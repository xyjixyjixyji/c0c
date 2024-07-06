use std::collections::HashMap;

use crate::{
  asm::{Cond, Dest, Instr, Operand},
  regalloc::Node,
  OPTIM_OPTIONS,
};

use super::{cfg::ControlFlowGraph, BasicBlock};

/// Perform peephole copy propagation on the given CFG.
pub(crate) fn peephole_copy_prop(mut cfg: ControlFlowGraph) -> ControlFlowGraph {
  if !OPTIM_OPTIONS.lock().unwrap().peephole_copy_prop {
    return cfg;
  }

  for block in cfg.blocks.iter_mut() {
    copy_prop_inner(block);
  }

  cfg
}

fn copy_prop_inner(block: &mut BasicBlock) {
  let mut current_temp_map = HashMap::new();

  for instr in &mut block.instructions {
    try_rewrite_instr(instr, &current_temp_map);

    if let Instr::Mov { dest, src } = instr {
      update_mapping(dest.0, &mut current_temp_map);
      if let Operand::Temp(src_temp) = src {
        let original_src_temp = find_most_original_temp(src_temp.0, &current_temp_map);
        // Prevent circular reference by ensuring the dest is not the same as the src's original
        if original_src_temp != dest.0 {
          current_temp_map.insert(dest.0, original_src_temp);
        }
      }
    } else {
      for def in instr.defined() {
        if let Node::Temp(d) = def {
          // Update mapping for defined temp to prevent it from mapping back to itself or creating loops
          update_mapping(d, &mut current_temp_map);
        }
      }
    }
  }
}

fn update_mapping(defined_var: u32, current_temp_map: &mut HashMap<u32, u32>) {
  current_temp_map.retain(|&c, &mut o| o != defined_var && c != defined_var);
}

fn try_rewrite_instr(instr: &mut Instr, current_temp_map: &HashMap<u32, u32>) {
  match instr {
    Instr::BinOp { src1, src2, .. } => {
      try_rewrite_temp(src1, current_temp_map);
      try_rewrite_temp(src2, current_temp_map);
    }
    Instr::UnOp { src, .. } => {
      try_rewrite_temp(src, current_temp_map);
    }
    Instr::Mov { src, .. } => {
      try_rewrite_temp(src, current_temp_map);
    }
    Instr::CondJmp { src, .. } => match src {
      Cond::Simp(o) => try_rewrite_temp(o, current_temp_map),
      Cond::BinOp(o1, _, o2) => {
        try_rewrite_temp(o1, current_temp_map);
        try_rewrite_temp(o2, current_temp_map);
      }
    },
    Instr::Return(o) => {
      if let Some(o) = o {
        try_rewrite_temp(o, current_temp_map);
      }
    }
    Instr::Call { args, .. } => {
      for arg in args {
        try_rewrite_temp(arg, current_temp_map);
      }
    }
    Instr::Jmp { .. } => {}
    Instr::Label { .. } => {}
    Instr::ReadMem { .. } => {}
    Instr::WriteMem { src, .. } => {
      try_rewrite_temp(src, current_temp_map);
    }
    Instr::Lea {
      base,
      index,
      elem_size,
      ..
    } => {
      try_rewrite_temp(base, current_temp_map);
      if let Some(i) = index {
        try_rewrite_temp(i, current_temp_map);
      }
      if let Some(e) = elem_size {
        try_rewrite_temp(e, current_temp_map);
      }
    }
    Instr::Nop => {}
  }
}

fn try_rewrite_temp(temp: &mut Operand, current_temp_map: &HashMap<u32, u32>) {
  if let Operand::Temp(dest) = temp {
    let current_id = find_most_original_temp(dest.0, current_temp_map);
    *temp = Operand::Temp(Dest(current_id, dest.1));
  }
}

fn find_most_original_temp(temp_id: u32, current_temp_map: &HashMap<u32, u32>) -> u32 {
  let mut current_id = temp_id;
  while let Some(&original_id) = current_temp_map.get(&current_id) {
    current_id = original_id;
  }
  current_id
}
