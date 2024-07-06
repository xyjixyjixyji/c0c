use std::collections::{hash_map, HashMap, HashSet};

use crate::{
  asm::{Cond, Dest, Instr, InstrLabel, Operand},
  ast::FuncName,
  codegen::Context,
  utils::Graph,
};

/// Context for inline function call substitution.
/// When performing inline substitution, we need to keep track of the mapping between
/// the callee's temps and labels to the caller's temps and labels.
pub(crate) struct InlineSubstitutionContext {
  temp_callee_to_caller: HashMap<Dest, Dest>,
  label_callee_to_caller: HashMap<InstrLabel, InstrLabel>,
  loop_id_callee_to_caller: HashMap<u32, u32>,
  ret_operand: Option<Operand>,
  instrs: Vec<Instr>,
}

impl InlineSubstitutionContext {
  fn new() -> Self {
    InlineSubstitutionContext {
      temp_callee_to_caller: HashMap::new(),
      label_callee_to_caller: HashMap::new(),
      loop_id_callee_to_caller: HashMap::new(),
      ret_operand: None,
      instrs: vec![],
    }
  }

  fn operand_callee_to_caller(&mut self, ctx: &mut Context, operand: Operand) -> Operand {
    match operand {
      Operand::Temp(dest) => Operand::Temp(self.temp_callee_to_caller(ctx, dest)),
      _ => operand,
    }
  }

  fn temp_callee_to_caller(&mut self, ctx: &mut Context, dest: Dest) -> Dest {
    if self.temp_callee_to_caller.contains_key(&dest) {
      return *self.temp_callee_to_caller.get(&dest).unwrap();
    }
    let new_temp = ctx.temp(dest.1);
    self.temp_callee_to_caller.insert(dest, new_temp);
    new_temp
  }

  fn label_callee_to_caller(&mut self, ctx: &mut Context, target: &InstrLabel) -> InstrLabel {
    if self.label_callee_to_caller.contains_key(target) {
      return self.label_callee_to_caller.get(target).unwrap().clone();
    }

    let loop_id = target.loop_id().map(|loop_id| {
      if let hash_map::Entry::Vacant(e) = self.loop_id_callee_to_caller.entry(loop_id) {
        let new_loop_id = ctx.loop_id();
        e.insert(new_loop_id);
        new_loop_id
      } else {
        *self.loop_id_callee_to_caller.get(&loop_id).unwrap()
      }
    });

    let new_label = ctx.label(loop_id, target.loop_state());
    self
      .label_callee_to_caller
      .insert(target.clone(), new_label.clone());
    new_label
  }
}

/// Translate a single function call from the caller context, to a sequence of instructions representing the inline substituted callee.
pub(crate) fn inline_fn_call(
  caller_ctx: &mut Context,
  callee_ctx: &Context,
  fn_args: Vec<Operand>,
  ret_dest: Option<Dest>,
) {
  let callee_params = callee_ctx.get_self_arglist();
  let callee_used_params = callee_params
    .iter()
    .filter(|d| callee_ctx.try_get_temp_used_count(d.0).is_some())
    .map(|d| d.0)
    .collect::<HashSet<_>>();
  let mut inline_ctx = InlineSubstitutionContext::new();
  inline_ctx.label_callee_to_caller.insert(
    callee_ctx.memerror_label.clone(),
    caller_ctx.memerror_label.clone(),
  );
  // skips the first few instructions in the callee body, which only moves the first 6 arguments to fresh temp,
  // and sets up the label for self-call
  let instr_skip_num = callee_params.len().min(6) + 1;
  for instr in callee_ctx.instrs.iter().skip(instr_skip_num) {
    let new_instr = translate_inline_instr(caller_ctx, instr, &mut inline_ctx);
    if let Some(new_instr) = new_instr {
      inline_ctx.instrs.push(new_instr);
    } else {
      break;
    }
  }

  for (i, param) in callee_params.iter().enumerate() {
    if callee_used_params.contains(&param.0) {
      let instr = Instr::Mov {
        src: fn_args[i],
        dest: *inline_ctx.temp_callee_to_caller.get(param).unwrap(),
      };
      caller_ctx.add_instr(instr);
    }
  }
  for instr in inline_ctx.instrs.iter() {
    caller_ctx.add_instr(instr.clone());
  }
  if let Some(ret_operand) = inline_ctx.ret_operand {
    let instr = Instr::Mov {
      src: ret_operand,
      dest: ret_dest.unwrap(),
    };
    caller_ctx.add_instr(instr);
  }
}

/// Translates a single instruction from the callee to the caller context when performing inline substitution.
fn translate_inline_instr(
  caller_ctx: &mut Context,
  instr: &Instr,
  inline_ctx: &mut InlineSubstitutionContext,
) -> Option<Instr> {
  match instr {
    Instr::BinOp {
      op,
      dest,
      src1,
      src2,
    } => Some(Instr::BinOp {
      op: *op,
      dest: inline_ctx.temp_callee_to_caller(caller_ctx, *dest),
      src1: inline_ctx.operand_callee_to_caller(caller_ctx, *src1),
      src2: inline_ctx.operand_callee_to_caller(caller_ctx, *src2),
    }),
    Instr::UnOp { op, dest, src } => Some(Instr::UnOp {
      op: *op,
      dest: inline_ctx.temp_callee_to_caller(caller_ctx, *dest),
      src: inline_ctx.operand_callee_to_caller(caller_ctx, *src),
    }),
    Instr::Mov { dest, src } => Some(Instr::Mov {
      dest: inline_ctx.temp_callee_to_caller(caller_ctx, *dest),
      src: inline_ctx.operand_callee_to_caller(caller_ctx, *src),
    }),
    Instr::Return(operand) => {
      inline_ctx.ret_operand = operand.map(|o| inline_ctx.operand_callee_to_caller(caller_ctx, o));
      None
    }
    Instr::Call { name, args, dest } => {
      let new_args = args
        .iter()
        .map(|arg| inline_ctx.operand_callee_to_caller(caller_ctx, *arg))
        .collect();
      Some(Instr::Call {
        name: name.clone(),
        args: new_args,
        dest: dest.map(|d| inline_ctx.temp_callee_to_caller(caller_ctx, d)),
      })
    }
    Instr::ReadMem { dest, read_addr } => Some(Instr::ReadMem {
      dest: inline_ctx.temp_callee_to_caller(caller_ctx, *dest),
      read_addr: inline_ctx.temp_callee_to_caller(caller_ctx, *read_addr),
    }),
    Instr::WriteMem { dest_addr, src } => Some(Instr::WriteMem {
      dest_addr: inline_ctx.temp_callee_to_caller(caller_ctx, *dest_addr),
      src: inline_ctx.operand_callee_to_caller(caller_ctx, *src),
    }),
    Instr::Lea {
      dest,
      base,
      index,
      elem_size,
    } => Some(Instr::Lea {
      dest: inline_ctx.temp_callee_to_caller(caller_ctx, *dest),
      base: inline_ctx.operand_callee_to_caller(caller_ctx, *base),
      index: index.map(|i| inline_ctx.operand_callee_to_caller(caller_ctx, i)),
      elem_size: *elem_size,
    }),
    Instr::Jmp { target } => Some(Instr::Jmp {
      target: inline_ctx.label_callee_to_caller(caller_ctx, target),
    }),
    Instr::CondJmp {
      src,
      target_true,
      target_false,
    } => Some(Instr::CondJmp {
      src: match src {
        Cond::Simp(operand) => {
          Cond::Simp(inline_ctx.operand_callee_to_caller(caller_ctx, *operand))
        }
        Cond::BinOp(op1, op, op2) => Cond::BinOp(
          inline_ctx.operand_callee_to_caller(caller_ctx, *op1),
          *op,
          inline_ctx.operand_callee_to_caller(caller_ctx, *op2),
        ),
      },
      target_true: inline_ctx.label_callee_to_caller(caller_ctx, target_true),
      target_false: inline_ctx.label_callee_to_caller(caller_ctx, target_false),
    }),
    Instr::Label { name } => Some(Instr::Label {
      name: inline_ctx.label_callee_to_caller(caller_ctx, name),
    }),
    Instr::Nop => Some(Instr::Nop),
  }
}

/// Considers the func call dependencies in the contexts, and generates an optimal order.
///
/// This is necessary in the context of inline substitution, because some inline-able functions
/// may call other inline-able functions, in this case, we need to generate an order that translates
/// all inline-able functions to the simpliest form.
pub(crate) fn generate_optimization_order(
  ctxs: &mut [Context],
) -> (Vec<usize>, HashMap<FuncName, usize>) {
  // directed edges from callee to caller
  let mut precedence_graph = Graph::<usize>::new();
  let mut called_fn = HashMap::<FuncName, Option<FuncName>>::new();

  let fn_name_to_id = ctxs
    .iter()
    .enumerate()
    .map(|(i, ctx)| {
      called_fn.insert(
        ctx.fn_name.clone(),
        ctx.called_functions_ref().iter().next().cloned(),
      );
      (ctx.fn_name.clone(), i)
    })
    .collect::<HashMap<FuncName, usize>>();

  let mut ids = vec![];
  called_fn.iter().for_each(|(caller, callee)| {
    let caller_id = fn_name_to_id.get(caller).unwrap();
    if let Some(callee) = callee {
      if let Some(callee_id) = fn_name_to_id.get(callee) {
        precedence_graph.add_directed_edge(*callee_id, *caller_id);
      }
    } else {
      ids.push(*caller_id);
    }
  });
  let mut order = vec![];
  let mut idx_set = ctxs
    .iter()
    .enumerate()
    .map(|(i, _)| i)
    .collect::<HashSet<usize>>();

  for id in ids {
    order.push(id);
    idx_set.remove(&id);
    let mut node = id;
    let temp_hashset = HashSet::new();
    let mut neighbors = precedence_graph.neighbors(&node).unwrap_or(&temp_hashset);
    while !neighbors.is_empty() {
      node = *neighbors.iter().next().unwrap();
      neighbors = precedence_graph.neighbors(&node).unwrap_or(&temp_hashset);
      order.push(node);
      idx_set.remove(&node);
    }
  }
  order.extend(idx_set);

  (order, fn_name_to_id)
}
