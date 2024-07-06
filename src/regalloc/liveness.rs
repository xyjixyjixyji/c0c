// Vec<instr> -> liveness analysis -> Vec<AbstractLine>
// Vec<AbstractLine> -> build interference raw graph -> InterferenceGraph

use std::collections::{HashMap, HashSet, VecDeque};

use crate::{
  asm::{Dest, Instr, InstrLabel},
  codegen::Context,
  regalloc::interference::Node,
  OPTIM_OPTIONS,
};

type AbstractLineRef = usize; // we use index to refer to the line

/// The abstract line of the liveness analysis, each represents a line of
/// the abstract assembly.
#[derive(Debug, Clone)]
pub struct AbstractLine {
  /// The original instruction, for precoloring, like idiv etc.
  instr: Instr,
  /// The temps used in this instruction
  uses: Vec<Node>,
  /// The temps defined in this instruction
  defines: Vec<Node>,
  /// The necessary temps for this instruction
  necessary: Vec<Node>,
  /// The needed temps derived for this instruction.
  needed: HashSet<Node>,
  /// The live-in temps at this instruction
  live_in: HashSet<Node>,
  /// The default successor of this line, for normal control flow, it is the next line
  /// for form like if-then-else, it is the then branch
  default_succ: Option<AbstractLineRef>,
  /// The branched successor of this line, for unconditional jump, it is the target line
  /// for form like if-then-else, it is the else branch
  branch_succ: Option<AbstractLineRef>,
  /// Whether this line is eliminated by deadcode elimination
  is_eliminated: bool,
}

impl AbstractLine {
  pub fn defines(&self) -> &Vec<Node> {
    &self.defines
  }

  pub fn uses(&self) -> &Vec<Node> {
    &self.uses
  }

  pub fn live_in(&self) -> &HashSet<Node> {
    &self.live_in
  }

  pub fn necessary(&self) -> &Vec<Node> {
    &self.necessary
  }

  pub fn needed(&self) -> &HashSet<Node> {
    &self.needed
  }

  pub fn instr(&self) -> &Instr {
    &self.instr
  }

  pub fn not_eliminated(&self) -> bool {
    !self.is_eliminated
  }

  pub fn successors(&self) -> Option<Vec<AbstractLineRef>> {
    match (self.default_succ, self.branch_succ) {
      (Some(s1), Some(s2)) => Some(vec![s1, s2]),
      (Some(s1), None) => Some(vec![s1]),
      (None, Some(s2)) => Some(vec![s2]),
      (None, None) => None,
    }
  }

  pub fn rewrite_temp(&mut self, from: &Dest, to: &Dest) {
    for def in self.defines.iter_mut() {
      if let Node::Temp(t) = def {
        if t == &from.0 {
          *def = Node::Temp(to.0);
        }
      }
    }

    for use_ in self.uses.iter_mut() {
      if let Node::Temp(t) = use_ {
        if t == &from.0 {
          *use_ = Node::Temp(to.0);
        }
      }
    }

    if self.live_in.contains(&Node::Temp(from.0)) {
      self.live_in.remove(&Node::Temp(from.0));
      self.live_in.insert(Node::Temp(to.0));
    }

    self.instr.rewrite_temp(from, to);
  }
}

impl AbstractLine {
  #[allow(clippy::too_many_arguments)]
  pub fn new(
    instr: Instr,
    uses: Vec<Node>,
    defines: Vec<Node>,
    necessary: Vec<Node>,
    needed: HashSet<Node>,
    live_in: HashSet<Node>,
    default_succ: Option<AbstractLineRef>,
    branch_succ: Option<AbstractLineRef>,
  ) -> Self {
    Self {
      instr,
      uses,
      defines,
      necessary,
      needed,
      live_in,
      default_succ,
      branch_succ,
      is_eliminated: false,
    }
  }
}

/// The liveness analysis result of a function
pub struct Liveness {
  /// The abstract lines of the function, each represents a line of
  /// the abstract assembly
  pub lines: Vec<AbstractLine>,
  /// The number of temps used in the function
  num_temp: u32,
}

impl Liveness {
  /// Create a liveness analysis information from the given context
  pub fn from_context(ctx: &Context) -> Self {
    let instrs = &ctx.instrs;
    let mut abstr_lines = vec![];

    let mut curr_line = None;
    let label_to_lineindex = ctx.build_label_to_line_index();
    for (line_num, instr) in instrs.iter().enumerate().rev() {
      let (default_succ, branch_succ) = Self::get_successors(instr, &label_to_lineindex, curr_line);

      abstr_lines.insert(
        0,
        AbstractLine::new(
          instr.clone(),
          instr.used(),
          instr.defined(),
          instr.neccessary(),
          HashSet::new(),
          HashSet::new(),
          default_succ,
          branch_succ,
        ),
      );

      curr_line = Some(line_num);
    }

    let liveness_ctx = Self {
      lines: abstr_lines,
      num_temp: ctx.temp_index,
    };
    let pred_map = liveness_ctx.generate_predecessor_map();
    // Precompute which lines each variable is defined on for quick lookup
    let mut var_def_lines: HashMap<Node, Vec<AbstractLineRef>> = HashMap::new();
    for (line_num, line) in liveness_ctx.lines().iter().enumerate() {
      for var in line.defines() {
        var_def_lines.entry(*var).or_default().push(line_num);
      }
    }

    if OPTIM_OPTIONS.lock().unwrap().dead_code_elim {
      liveness_ctx
        .derive_needed_set(&pred_map)
        .mark_deadcode()
        .derive_livein_set(&pred_map, &var_def_lines)
    } else {
      liveness_ctx.derive_livein_set(&pred_map, &var_def_lines)
    }
  }

  /// Variable based search to derive the live-in set for each line
  fn derive_livein_set(
    mut self,
    pred_map: &[HashSet<usize>],
    var_def_lines: &HashMap<Node, Vec<AbstractLineRef>>,
  ) -> Self {
    let mut livein_sets = vec![HashSet::new(); self.lines.len()];
    for (line_num, line) in self.lines().iter().enumerate().rev() {
      if line.is_eliminated {
        continue;
      }
      for var in line.uses() {
        if livein_sets[line_num].contains(var) {
          continue; // Variable is already live, no need to traverse
        }

        let mut visited = HashSet::new();
        let mut to_visit = VecDeque::new();
        to_visit.push_back(line_num);

        while let Some(current_line_num) = to_visit.pop_front() {
          if visited.contains(&current_line_num) {
            continue; // Already visited this line for the current variable
          }
          visited.insert(current_line_num);

          if !livein_sets[current_line_num].contains(var) {
            livein_sets[current_line_num].insert(*var);
            for pred in &pred_map[current_line_num] {
              // If a variable is defined at this predecessor, no need to add to visit
              if self.lines()[*pred].is_eliminated
                || var_def_lines
                  .get(var)
                  .map_or(true, |defs| !defs.contains(pred))
              {
                to_visit.push_back(*pred);
              }
            }
          }
        }
      }
    }

    for (line_num, line) in self.lines_mut().iter_mut().enumerate() {
      line.live_in = livein_sets[line_num].clone();
    }

    self
  }

  /// Iteratively derive the needed set for each line
  fn derive_needed_set(mut self, pred_map: &[HashSet<usize>]) -> Self {
    let mut needed_sets = vec![HashSet::new(); self.lines.len()];
    let mut not_saturated = true;

    // collect defined vars for each line
    let defined_vars = self
      .lines()
      .iter()
      .map(|line| line.defines().iter().copied().collect::<HashSet<_>>())
      .collect::<Vec<_>>();

    while not_saturated {
      not_saturated = false;
      for (line_num, line) in self.lines().iter().enumerate().rev() {
        // necessary nodes are needed
        needed_sets[line_num].extend(line.necessary());

        let mut succ_needed_set = &needed_sets[line_num];
        for &pred_line in &pred_map[line_num] {
          let pred_defines = &defined_vars[pred_line];
          let mut pred_needed = HashSet::<Node>::new();
          let before_size = needed_sets[pred_line].len();

          // if pred line doesn't define succ's needed var, add succ's needed var to pred's needed set
          pred_needed.extend(succ_needed_set.difference(pred_defines));

          // if pred line defines succ's needed var, add pred's used var to pred's needed set
          if succ_needed_set.intersection(pred_defines).any(|_| true) {
            pred_needed.extend(self.lines()[pred_line].uses());
          }

          needed_sets[pred_line].extend(pred_needed);
          let after_size = needed_sets[pred_line].len();
          not_saturated |= before_size != after_size;
          succ_needed_set = &needed_sets[line_num];
        }
      }
    }

    for (line_num, line) in self.lines_mut().iter_mut().enumerate() {
      line.needed = needed_sets[line_num].clone();
    }

    self
  }

  /// Performs deadcode elimination on the needed-ness analysis result
  fn mark_deadcode(mut self) -> Self {
    let mut deadcode_lines = HashSet::new();
    for (idx, line) in self.lines().iter().enumerate() {
      if line.necessary().is_empty() {
        if let Some(succ) = line.default_succ {
          let succ_line = &self.lines()[succ];
          if !line
            .defines()
            .iter()
            .any(|def| succ_line.needed().contains(def))
          {
            deadcode_lines.insert(idx);
          }
        } else {
          deadcode_lines.insert(idx);
        }
      }
    }
    for line in deadcode_lines {
      self.lines_mut()[line].is_eliminated = true;
    }
    self
  }

  pub fn lines(&self) -> &Vec<AbstractLine> {
    &self.lines
  }

  pub fn lines_mut(&mut self) -> &mut Vec<AbstractLine> {
    &mut self.lines
  }

  #[inline]
  pub fn num_temp(&self) -> u32 {
    self.num_temp
  }

  #[allow(dead_code)]
  fn debug_show_livein(&self) {
    for (idx, line) in self.lines.iter().enumerate() {
      let livein = &line.live_in;
      let instr = line.instr();
      log::debug!("Line {}: {}, live: {:?}", idx, instr, livein);
    }
  }

  /// Get the successors of the given instruction
  ///
  /// # Arguments
  ///
  /// * `ctx` - The context of the codegen
  /// * `instr` - The instruction to get successors
  /// * `nextline_idx`
  ///
  /// # Returns
  ///
  /// (default_succ, branch_succ)
  /// `default_succ` is the default successor, for normal control flow, it is the next line
  /// for form like if-then-else, it is the then branch
  /// `branch_succ` is the branched successor, for unconditional jump, it is `None`
  /// for form like if-then-else, it is the else branch
  fn get_successors(
    instr: &Instr,
    label_to_lineindex: &HashMap<InstrLabel, usize>,
    nextline_idx: Option<usize>,
  ) -> (Option<usize>, Option<usize>) {
    match instr {
      Instr::Jmp { target } => (
        Some(
          label_to_lineindex
            .get(target)
            .copied()
            .unwrap_or_else(|| nextline_idx.unwrap()),
        ),
        None,
      ),
      Instr::CondJmp {
        src: _,
        target_false,
        ..
      } => (
        nextline_idx,
        Some(
          label_to_lineindex
            .get(target_false)
            .copied()
            .expect("Jump target's label not found"),
        ),
      ),
      _ => (nextline_idx, None),
    }
  }

  /// Generate the predecessor map of the lines
  ///
  /// # Returns
  ///
  /// Predecessor map, a vector of hashset, each hashset contains the predecessors of the
  /// line, index by the current line index
  ///
  /// e.g. pred_map[3] contains the predecessors of line 3
  fn generate_predecessor_map(&self) -> Vec<HashSet<AbstractLineRef>> {
    let mut pred_map = vec![HashSet::new(); self.lines.len()];
    for (idx, line) in self.lines.iter().enumerate() {
      if let Some(succs) = line.successors() {
        for succ in succs {
          pred_map[succ].insert(idx);
        }
      }
    }

    pred_map
  }
}

#[allow(dead_code)]
pub fn debug_show_predmap(pred_map: &[HashSet<AbstractLineRef>]) {
  for (idx, preds) in pred_map.iter().enumerate() {
    log::debug!("Line {}: {:?}", idx, preds);
  }
}
