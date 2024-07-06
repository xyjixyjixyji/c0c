use std::collections::HashSet;

use crate::{
  asm::{Instr, Operand},
  codegen::Context,
};

pub(crate) fn do_tco(caller_ctx: &mut Context, call_instr: &Instr) {
  if let Instr::Call { args, .. } = call_instr {
    let params = caller_ctx.get_self_arglist();
    let used_params = params
      .iter()
      .filter(|d| caller_ctx.try_get_temp_used_count(d.0).is_some())
      .map(|d| d.0)
      .collect::<HashSet<_>>();

    // removes the previous Call instruction
    caller_ctx.instrs.pop();

    // allocate fresh temps for the arguments, if it might interfere with the parameters
    let mut updated_args = vec![];
    for (src, dest) in args.iter().zip(params.iter()) {
      if let Operand::Temp(src_temp) = *src {
        if src_temp != *dest && used_params.contains(&src_temp.0) {
          let placeholder = caller_ctx.temp(src_temp.1);
          caller_ctx.add_instr(Instr::Mov {
            src: *src,
            dest: placeholder,
          });
          updated_args.push(Operand::Temp(placeholder));
          continue;
        }
      }
      updated_args.push(*src);
    }

    // update the param temps with correct values
    for (src, dest) in updated_args.into_iter().zip(params.iter()) {
      if caller_ctx.try_get_temp_used_count(dest.0).is_some() {
        caller_ctx.add_instr(Instr::Mov { src, dest: *dest });
      }
    }

    caller_ctx.add_instr(Instr::Jmp {
      target: caller_ctx.self_call_label.clone(),
    });

    caller_ctx.mark_tco_applied();
  } else {
    panic!("do_tco called with non-Call instruction")
  }
}
