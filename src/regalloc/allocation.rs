use std::{
  cmp::min,
  collections::{HashMap, HashSet},
};

use crate::{
  abs2asm::{MemLoc, X64Operand},
  asm::Instr,
  codegen::Context,
  regalloc::{interference::Node, liveness::Liveness, InterferenceGraph},
  registers::{
    color_from_reg,
    consts::{ALLOC_POOL_REGS_32, REG_ALLOC_POOL_SIZE, STACK_ELEM_SIZE},
    reg::X86_64Register,
  },
  utils::MinHeap,
};

use super::liveness::AbstractLine;

/// The allocator is responsible for allocating temps to registers or stack.
/// There is one allocator per function.
pub struct Allocator {
  /// The context of a function.
  pub ctx: Context,
  /// The allocation of temps to registers or stack.
  pub temp_allocation: HashMap<u32, X64Operand>,
  /// The color of each node in the interference graph.
  pub node_to_color: Vec<i32>,
  /// The number of temps that were spilled to the stack.
  pub num_spills: u32,
  /// The live-in set registers on each line that is a fn call.
  pub livein_sets_on_call: Option<Vec<HashSet<X86_64Register>>>,
  /// The abstract lines of the function, each represents a line of the abstract assembly
  pub abstract_lines: Option<Vec<AbstractLine>>,
}

impl Allocator {
  fn new(ctx: Context) -> Self {
    Allocator {
      ctx,
      temp_allocation: HashMap::new(),
      node_to_color: vec![],
      num_spills: 0,
      livein_sets_on_call: None,
      abstract_lines: None,
    }
  }

  /// Create an allocator from a context.
  pub fn from_context(context: Context) -> Self {
    let ntemps = context.temp_index;
    let mut allocator = Allocator::new(context);

    // Interference graph -> coloring order -> greedy coloring -> X64Operand
    allocator.precolor_arglist();

    let liveness = Liveness::from_context(&allocator.ctx);
    let mut igraph = InterferenceGraph::from_liveness(&liveness);
    let order = igraph.generate_simplicial_order(Some(&allocator.ctx));

    allocator.abstract_lines = Some(
      liveness
        .lines
        .into_iter()
        .filter(AbstractLine::not_eliminated)
        .collect::<Vec<_>>(),
    );

    // prespill
    if ntemps <= 100 {
      allocator.prespill(&igraph);
    }

    allocator.color_graph(&order, &igraph);

    // coalesce the registers
    if let Some(c_max) = allocator.largest_used_color() {
      allocator.greedy_coalesce(&mut igraph, c_max);
    }

    allocator.allocate_temp_position();

    // Get the livein registers on function calls, since we need to save/restore
    // those that are caller-saved registers.
    let mut livein_sets_on_call = vec![];
    for line in allocator.abstract_lines.as_ref().unwrap() {
      let instr = line.instr();
      let defines = line.defines();
      if matches!(instr, Instr::Call { .. }) {
        let livein_regs = line
          .live_in()
          .iter()
          .filter(|n| n.is_temp() && !defines.contains(n))
          .map(|t| {
            allocator
              .temp_allocation
              .get(t.as_temp().unwrap())
              .unwrap_or_else(|| panic!("Temp {:?} not found", t))
          })
          .filter(|op| op.is_register())
          .map(|op| op.as_register().cloned().unwrap())
          .collect::<HashSet<_>>();

        livein_sets_on_call.push(livein_regs);
      }
    }

    allocator.livein_sets_on_call = Some(livein_sets_on_call);

    allocator
  }

  /// Implements the graph coloring algorithm.
  ///
  /// For the given order of nodes, we visit every neighbor of that node, and
  /// assign that node the lowest color not used by any neighbor.
  ///
  /// Returns the max color used
  fn color_graph(&mut self, order: &Vec<u32>, igraph: &InterferenceGraph) {
    let mut node_to_color = vec![-1_i32; igraph.num_temp()];

    for (temp, reg) in self.temp_allocation.iter() {
      if let X64Operand::Register(reg) = reg {
        node_to_color[*temp as usize] = color_from_reg(*reg) as i32;
      } else {
        node_to_color[*temp as usize] = REG_ALLOC_POOL_SIZE as i32;
      }
    }

    for node in order {
      if node_to_color[*node as usize] != -1 || self.temp_allocation.contains_key(node) {
        continue;
      }

      // collect all the colored neighbors' colors
      let mut neighbor_colors = MinHeap::new();
      if let Some(neighbors) = igraph.neighbors(&Node::Temp(*node)) {
        neighbors
          .iter()
          .filter(|n| match n {
            Node::Temp(n) => node_to_color[*n as usize] != -1,
            _ => true,
          })
          .for_each(|n| {
            let color = match n {
              Node::Temp(n) => node_to_color[*n as usize],
              Node::Reg(reg) => color_from_reg(*reg) as i32,
            };
            neighbor_colors.push(color);
          });
      }

      node_to_color[*node as usize] = neighbor_colors.find_smallest_absent();
    }

    self.node_to_color = node_to_color;
  }

  fn allocate_temp_position(&mut self) {
    let operands = self.color_to_operand(self.node_to_color.clone());
    for (dest, op) in operands.into_iter().enumerate() {
      self.temp_allocation.insert(dest as u32, op);
    }
    self.allocate_on_stack_arguments();
  }
}

/// helpers
impl Allocator {
  fn largest_used_color(&self) -> Option<i32> {
    self.node_to_color.iter().max().copied()
  }

  /// Maps the colors to X64Operands.
  fn color_to_operand(&mut self, node_to_color: Vec<i32>) -> Vec<X64Operand> {
    let mut operands = vec![];
    let mut callee_saved = HashSet::new();
    for color in node_to_color.into_iter() {
      // in coalescing, we might have some nodes that are removed
      if color == -1 {
        continue;
      }
      if color < REG_ALLOC_POOL_SIZE as i32 {
        let reg = ALLOC_POOL_REGS_32[color as usize];
        if reg.is_callee_saved() {
          callee_saved.insert(reg);
        }
        operands.push(reg.into());
      } else {
        operands.push(self.get_next_spill_operand());
      }
    }
    operands
  }

  pub(crate) fn get_next_spill_operand(&mut self) -> X64Operand {
    let operand = X64Operand::mem(MemLoc::from_reg_offset(
      X86_64Register::RBP(64),
      -((STACK_ELEM_SIZE * (self.num_spills + 1)) as i64),
      8, // we make every on stack temp 8 bytes
    ));
    self.num_spills += 1;
    operand
  }

  /// Precolor argument list of a function
  ///
  /// # Example
  ///
  /// __c0_fadd(T0, T1) -> __c0_fadd(%rdi, %rsi)
  fn precolor_arglist(&mut self) {
    let nargs = self
      .ctx
      .src_ctx
      .get_func_decl(&self.ctx.fn_name)
      .unwrap()
      .param_list
      .0
      .len();
    for i in 0..min(nargs, 6) {
      let reg = X86_64Register::ith_argument(i as u8, 32);
      self
        .temp_allocation
        .insert(i as u32, X64Operand::Register(reg));
    }
  }

  /// Allocate the arguments that are on the stack by mapping the 7th,
  /// 8th, ... arguments to the stack.
  fn allocate_on_stack_arguments(&mut self) {
    let fn_args = self.ctx.get_self_arglist();
    for (i, arg) in fn_args.iter().enumerate().rev() {
      if i < 6 {
        break;
      }
      let offset = 8 * (2 + (i - 6));
      self.temp_allocation.insert(
        arg.0,
        X64Operand::mem(MemLoc::from_reg_offset(
          X86_64Register::RBP(64),
          offset as i64,
          8, // store arguments in 8 bytes
        )),
      );
    }
  }
}

impl Allocator {
  // Maps all the temp variables to the stack. Used to handle programs with a large number of temps.
  #[allow(dead_code)]
  pub fn all_stack_allocator(ctx: Context) -> Self {
    let mut allocator = Allocator::new(ctx);

    // Interference graph -> coloring order -> greedy coloring -> X64Operand
    allocator.precolor_arglist();
    allocator.all_stack_color_graph();
    allocator.livein_sets_on_call = None;

    allocator
  }

  pub fn all_stack_color_graph(&mut self) {
    let mut node_to_color = vec![-1_i32; self.ctx.temp_index as usize];

    for (temp, reg) in self.temp_allocation.iter() {
      if let X64Operand::Register(reg) = reg {
        node_to_color[*temp as usize] = color_from_reg(*reg) as i32;
      }
    }

    for node in 0..self.ctx.temp_index {
      if node_to_color[node as usize] != -1 || self.temp_allocation.contains_key(&node) {
        continue;
      }
      let color_for_node = ALLOC_POOL_REGS_32.len() as i32;
      node_to_color[node as usize] = color_for_node;
    }

    let operands = self.color_to_operand(node_to_color);
    for (dest, op) in operands.into_iter().enumerate() {
      self.temp_allocation.insert(dest as u32, op);
    }
    self.allocate_on_stack_arguments();
  }
}
