use std::collections::{HashMap, HashSet};

use crate::{
  asm::{Cond, Instr, InstrLabel, Operand},
  ast::{BinOp, UnOp},
  codegen::fold_binops_i32,
  regalloc::Node,
  OPTIM_OPTIONS,
};

use super::{cfg::ControlFlowGraph, BasicBlock};

pub(crate) fn peephole_const_prop(mut cfg: ControlFlowGraph) -> ControlFlowGraph {
  if !OPTIM_OPTIONS.lock().unwrap().peephole_const_prop {
    return cfg;
  }
  let start_label = cfg.blocks.front().unwrap().label.clone();
  peephold_const_prop_cfg_path(&mut cfg, start_label, &mut HashSet::new(), HashMap::new());
  cfg
}

fn peephold_const_prop_cfg_path(
  cfg: &mut ControlFlowGraph,
  label_start: InstrLabel,
  visited_blocks: &mut HashSet<InstrLabel>,
  mut temp_const_map: HashMap<u32, i64>,
) {
  // return directly if we've already processed this block.
  if visited_blocks.contains(&label_start) {
    return;
  }
  const_prop_inner(cfg.get_block_ref_mut(&label_start), &mut temp_const_map);
  visited_blocks.insert(label_start.clone());
  let successors = cfg.get_successors(&label_start);
  for succ in [successors.0, successors.1].iter().flatten() {
    let succ_predecessors = cfg.get_predecessors(succ);
    if succ.label_name() == ".main_15" {
      println!(
        "cur label: {:?}, succ: {:?}, succ_predecessors: {:?}",
        label_start, succ, succ_predecessors
      );
    }
    // if the current block and successor block can be combined into a single block, we keep using the
    // same mapping.
    if succ_predecessors.len() == 1 && succ_predecessors.contains(&label_start) {
      peephold_const_prop_cfg_path(cfg, succ.clone(), visited_blocks, temp_const_map.clone());
    } else {
      peephold_const_prop_cfg_path(cfg, succ.clone(), visited_blocks, HashMap::new());
    }
  }
}

fn const_prop_inner(block: &mut BasicBlock, temp_const_map: &mut HashMap<u32, i64>) {
  for instr in &mut block.instructions {
    // rewrite the instructions
    rewrite_instr(instr, temp_const_map);
    try_fold_const(instr);

    if let Instr::Mov { dest, src } = instr {
      if src.is_const() {
        temp_const_map.insert(dest.0, *src.as_const().unwrap());
      } else {
        temp_const_map.remove(&dest.0);
      }
    } else {
      for def in instr.defined() {
        if let Node::Temp(d) = def {
          temp_const_map.remove(&d);
        }
      }
    }
  }
}

fn rewrite_instr(instr: &mut Instr, current_temp_const_map: &HashMap<u32, i64>) {
  log::debug!("Rewriting instr: {:?}", instr);
  match instr {
    Instr::BinOp { src1, src2, .. } => {
      try_rewrite_temp(src1, current_temp_const_map);
      try_rewrite_temp(src2, current_temp_const_map);
    }
    Instr::UnOp { src, .. } => {
      try_rewrite_temp(src, current_temp_const_map);
    }
    Instr::Mov { src, .. } => {
      try_rewrite_temp(src, current_temp_const_map);
    }
    Instr::CondJmp { src, .. } => match src {
      Cond::Simp(o) => try_rewrite_temp(o, current_temp_const_map),
      Cond::BinOp(o1, _, o2) => {
        try_rewrite_temp(o1, current_temp_const_map);
        try_rewrite_temp(o2, current_temp_const_map);
      }
    },
    Instr::Return(o) => {
      if let Some(o) = o {
        try_rewrite_temp(o, current_temp_const_map);
      }
    }
    Instr::Call { args, .. } => {
      for arg in args {
        try_rewrite_temp(arg, current_temp_const_map);
      }
    }
    Instr::Jmp { .. } => {}
    Instr::Label { .. } => {}
    Instr::ReadMem { .. } => {}
    Instr::WriteMem { src, .. } => {
      try_rewrite_temp(src, current_temp_const_map);
    }
    Instr::Lea {
      base,
      index,
      elem_size,
      ..
    } => {
      try_rewrite_temp(base, current_temp_const_map);
      if let Some(i) = index {
        try_rewrite_temp(i, current_temp_const_map);
      }
      if let Some(e) = elem_size {
        try_rewrite_temp(e, current_temp_const_map);
      }
    }
    Instr::Nop => {}
  }
  log::debug!("After rewriting instr: {:?}", instr);
}

fn try_rewrite_temp(temp: &mut Operand, current_temp_const_map: &HashMap<u32, i64>) {
  if let Operand::Temp(dest) = temp {
    if let Some(const_val) = current_temp_const_map.get(&dest.0) {
      *temp = Operand::Const(*const_val);
    }
  }
}

fn try_fold_const(instr: &mut Instr) {
  match instr {
    Instr::BinOp {
      dest,
      src1,
      src2,
      op,
    } => {
      if src1.is_const() && src2.is_const() {
        if dest.1 == 4 {
          if let Some(result) =
            fold_binops_i32(*src1.as_const().unwrap(), *src2.as_const().unwrap(), *op)
          {
            *instr = Instr::Mov {
              dest: *dest,
              src: Operand::Const(result as i64),
            };
          }
        } else if let Some(result) =
          fold_binops_i64(*src1.as_const().unwrap(), *src2.as_const().unwrap(), *op)
        {
          *instr = Instr::Mov {
            dest: *dest,
            src: Operand::Const(result),
          };
        }
      }
    }
    Instr::UnOp { dest, src, op } => {
      if src.is_const() {
        let val = src.as_const().unwrap();
        let result = match op {
          UnOp::Neg => -val,
          UnOp::Not => {
            if *val == 0 {
              1
            } else {
              0
            }
          }
          UnOp::BitNot => !val,
        };
        *instr = Instr::Mov {
          dest: *dest,
          src: Operand::Const(result),
        };
      }
    }
    _ => {}
  }
}

pub fn fold_binops_i64(lhs: i64, rhs: i64, op: BinOp) -> Option<i64> {
  match op {
    BinOp::Add => Some(lhs + rhs),
    BinOp::Sub => Some(lhs - rhs),
    BinOp::Mul => Some(lhs * rhs),
    BinOp::Div => {
      if rhs == 0 || (lhs == std::i64::MIN && rhs == -1) {
        None
      } else {
        Some(lhs / rhs)
      }
    }
    BinOp::Mod => {
      if rhs == 0 || (lhs == std::i64::MIN && rhs == -1) {
        None
      } else {
        Some(lhs % rhs)
      }
    }
    BinOp::Shl => {
      if (0..32).contains(&rhs) {
        Some(lhs << rhs)
      } else {
        None
      }
    }
    BinOp::Shr => {
      if (0..32).contains(&rhs) {
        Some(lhs >> rhs)
      } else {
        None
      }
    }
    BinOp::BitAnd | BinOp::LogicalAnd => Some(lhs & rhs),
    BinOp::BitOr | BinOp::LogicalOr => Some(lhs | rhs),
    BinOp::BitXor => Some(lhs ^ rhs),
    BinOp::EqEq => Some(if lhs == rhs { 1 } else { 0 }),
    BinOp::Uneq => Some(if lhs != rhs { 1 } else { 0 }),
    BinOp::Geq => Some(if lhs >= rhs { 1 } else { 0 }),
    BinOp::GreaterThan => Some(if lhs > rhs { 1 } else { 0 }),
    BinOp::LessThan => Some(if lhs < rhs { 1 } else { 0 }),
    BinOp::Leq => Some(if lhs <= rhs { 1 } else { 0 }),
  }
}
