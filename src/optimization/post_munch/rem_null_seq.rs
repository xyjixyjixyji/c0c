use crate::{
  asm::{Instr, Operand},
  OPTIM_OPTIONS,
};

use super::cfg::ControlFlowGraph;

pub(crate) fn peephole_remove_null_seq(mut cfg: ControlFlowGraph) -> ControlFlowGraph {
  if !OPTIM_OPTIONS.lock().unwrap().peephole_rem_null_seq {
    return cfg;
  }

  // null sequences:
  // jmp L1; L1: -> remove jmp L1
  // a <- b; b <- a; -> remove the second mov
  for block in cfg.blocks.iter_mut() {
    let ninstr = block.instructions.len();
    let mut removals = vec![false; ninstr];
    for i in 0..ninstr {
      if i + 1 < ninstr {
        let instr = &block.instructions[i];
        let next_instr = &block.instructions[i + 1];
        if let Instr::Mov {
          dest: d1,
          src: Operand::Temp(s1),
        } = instr
        {
          if let Instr::Mov {
            dest: d2,
            src: Operand::Temp(s2),
          } = next_instr
          {
            if s1 == d2 && s2 == d1 {
              removals[i + 1] = true;
            }
          }
        }
      }

      if let Instr::Mov {
        dest,
        src: Operand::Temp(src),
      } = &block.instructions[i]
      {
        if dest == src {
          removals[i] = true;
        }
      }
    }

    for (i, instr) in block.instructions.iter_mut().enumerate() {
      if removals[i] {
        *instr = Instr::Nop;
      }
    }
  }

  cfg
}
