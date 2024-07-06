//! Post-munch pass
//!   This optimization pass is performed immediately after codegen, before SSA
//!   and register allocation. It performs the following optimizations on the
//!   generated IR:
//!
//!   - Tail call optimization (TCO): replaces tail recursions with jumps to
//!     the beginning of the function
//!
//!   - Inline substitution: replaces function calls with the body of the
//!     callee, if the callee is a simple, straightline function

mod basic_blocks;
mod cfg;
mod const_prop;
mod copy_prop;
mod func_inline;
mod rem_null_seq;
mod rewrite_condjmp;
mod tco;

pub use basic_blocks::{basic_blocks_vec_from_ctx, build_basic_block_vec, BasicBlock};
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
  asm::Instr,
  ast::FuncName,
  codegen::Context,
  optimization::post_munch::{
    func_inline::{generate_optimization_order, inline_fn_call},
    rem_null_seq::peephole_remove_null_seq,
  },
  OPTIM_OPTIONS,
};

use self::{
  cfg::ControlFlowGraph, const_prop::peephole_const_prop, copy_prop::peephole_copy_prop,
  rewrite_condjmp::peephole_rewrite_condjmp, tco::do_tco,
};

/// Conditionally called after the munch pass, to perform some pre-SSA optimizations.
pub fn post_munch_pass(mut ctxs: Vec<Context>) -> Vec<Context> {
  let (order, fn_name_to_id) = generate_optimization_order(&mut ctxs);

  let mut ctxs = ctxs.into_iter().enumerate().collect::<BTreeMap<_, _>>();

  for i in order.iter() {
    let mut caller_ctx = ctxs.remove(i).unwrap();
    inline_and_tco(&ctxs, &mut caller_ctx, &fn_name_to_id);
    ctxs.insert(*i, caller_ctx);
  }

  // after inlining the functions, we can safely remove the context that are not called
  let mut called_fn_set = HashSet::new();
  called_fn_set.insert(String::from("main"));
  for ctx in &ctxs {
    let called_fn = ctx.1.build_called_functions();
    called_fn_set.extend(called_fn);
  }

  let mut ctxs = ctxs
    .into_iter()
    .filter(|(_, ctx)| {
      called_fn_set.contains(&ctx.fn_name)
        || ["main", "run", "prepare", "init", "checksum"].contains(&ctx.fn_name.as_str())
    })
    .collect::<BTreeMap<_, _>>();

  // peephole passes
  let keys = ctxs.keys().cloned().collect::<Vec<_>>();
  for k in keys {
    let ctx = ctxs.remove(&k).unwrap();

    let mut cfg = ControlFlowGraph::from_context(ctx);
    cfg = peephole_copy_prop(cfg);
    cfg = peephole_const_prop(cfg);
    cfg = peephole_rewrite_condjmp(cfg);
    cfg = peephole_remove_null_seq(cfg);

    let ctx = cfg.into_context();
    ctxs.insert(k, ctx);
  }

  ctxs.into_values().collect()
}

/// Optimizes a single function in the context with tco and inlining.
fn inline_and_tco(
  ctxs: &BTreeMap<usize, Context>,
  caller_ctx: &mut Context,
  fn_name_to_id: &HashMap<FuncName, usize>,
) {
  let params = caller_ctx.get_self_arglist();
  let fn_name = caller_ctx.fn_name.clone();
  let instrs = caller_ctx.instrs.clone();
  let mut prev_instr: Option<Instr> = None;
  caller_ctx.instrs.clear();
  caller_ctx.used_temp_counts_ref_mut().clear();

  let mut instr_to_skip = params.len().min(6) + 1;
  for instr in instrs.into_iter() {
    // skips the first few instructions in the callee body, which only moves
    // the first 6 arguments to fresh temp
    if instr_to_skip > 0 {
      instr_to_skip -= 1;
      caller_ctx.instrs.push(instr.clone());
      prev_instr = Some(instr);
      continue;
    }

    match &instr {
      Instr::Return(ret_temp) => {
        // tco
        if let Some(Instr::Call { name, dest, .. }) = prev_instr.clone() {
          let can_tco = ret_temp.is_none()
            || (dest.is_some()
              && ret_temp.unwrap().is_temp()
              && dest.unwrap() == *ret_temp.unwrap().as_temp().unwrap());
          if fn_name == *name && can_tco && OPTIM_OPTIONS.lock().unwrap().tco {
            do_tco(caller_ctx, &prev_instr.unwrap());
            prev_instr = Some(instr.clone());
            continue;
          }
        }
      }
      Instr::Call { name, args, dest } => {
        // inline
        if let Some(idx) = fn_name_to_id.get(name) {
          if let Some(callee_ctx) = ctxs.get(idx) {
            if callee_ctx.can_inline() && OPTIM_OPTIONS.lock().unwrap().inline {
              inline_fn_call(caller_ctx, callee_ctx, args.clone(), *dest);
              prev_instr = Some(instr.clone());
              continue;
            }
          }
        }
      }
      _ => {}
    }
    caller_ctx.add_instr(instr.clone());
    prev_instr = Some(instr);
  }
}
