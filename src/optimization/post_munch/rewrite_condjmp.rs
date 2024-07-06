use crate::{
  asm::{Cond, Instr, Operand},
  codegen::fold_binops_i32,
  OPTIM_OPTIONS,
};

use super::{cfg::ControlFlowGraph, BasicBlock};

/// For all condjmp with a constant condition, rewrite it to a jmp.
pub(crate) fn peephole_rewrite_condjmp(mut cfg: ControlFlowGraph) -> ControlFlowGraph {
  if !OPTIM_OPTIONS.lock().unwrap().peephole_rewrite_condjmp {
    return cfg;
  }

  let mut blocks = cfg.blocks.iter_mut().collect::<Vec<_>>();
  loop {
    let (new_blocks, rewritten) = rewrite_one(blocks);
    if !rewritten {
      break;
    }
    blocks = new_blocks;
  }
  cfg.rebuild_cfg();
  cfg
}

/// Rewrite a single condjmp, returns rewritten basicblocks and whether the block has been rewritten.
fn rewrite_one(mut blocks: Vec<&mut BasicBlock>) -> (Vec<&mut BasicBlock>, bool) {
  let mut rewritten = false;

  for idx in 0..blocks.len() {
    let mut pop_last_instr = false;
    if let Some(last_instr) = blocks[idx].instructions.last_mut() {
      match last_instr.clone() {
        Instr::CondJmp {
          src: cond,
          target_false,
          target_true,
        } => {
          if let Some(c) = fold_cond(&cond) {
            rewritten = true;

            // condjmp IR forms in condjmp + true blocks + false blocks + end label
            // after cond folding, the unused blocked will be removed by reachability analysis,
            // and we can simply pop the last instr in this block.
            let new_target = if c { target_true } else { target_false };

            // rewrite the basic block successors
            blocks[idx].successor_1 = Some(new_target.clone());
            blocks[idx].successor_2 = None;

            // After unreachable block elimination, the next instruction will be the correct block,
            // so we can safely remove the jmp instruction.
            pop_last_instr = true;
          }
        }
        Instr::Jmp { target } => {
          // remove jmp if the target is the immediate next block
          if let Some(next_block) = blocks.get(idx + 1) {
            if next_block.label == target {
              pop_last_instr = true;
            }
          }
        }
        _ => {}
      }
    }
    if pop_last_instr {
      blocks[idx].instructions.pop();
    }
  }

  (blocks, rewritten)
}

fn fold_cond(cond: &Cond) -> Option<bool> {
  match cond {
    Cond::Simp(s) => {
      if let Operand::Const(c) = s {
        Some(*c != 0)
      } else {
        None
      }
    }
    Cond::BinOp(lhs, op, rhs) => {
      if lhs.is_const() && rhs.is_const() {
        let lhs = lhs.as_const().unwrap();
        let rhs = rhs.as_const().unwrap();
        Some(fold_binops_i32(*lhs, *rhs, *op).unwrap() != 0)
      } else {
        None
      }
    }
  }
}
