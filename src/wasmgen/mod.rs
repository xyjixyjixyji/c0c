use std::collections::{HashMap, HashSet};

use crate::{
  ast::{Fdefn, Gdecl, Program, ReturnType},
  typecheck::TypecheckContext,
};

use self::{
  munch_stmt::munch_stmts,
  wasm::{WasmFn, WasmInstr, TEMP_VAR1_NAME, TEMP_VAR2_NAME},
};

mod munch_expr;
mod munch_stmt;
mod wasm;

pub(super) struct MunchContext {
  src_ctx: Box<TypecheckContext>,
  /// The set of declared parameters in the current function, and whether they are function arguments.
  declared_params: HashMap<String, bool>,
  loop_id: i32,
}

impl MunchContext {
  pub fn new(src_ctx: Box<TypecheckContext>, fn_params: HashSet<String>) -> Self {
    Self {
      src_ctx,
      declared_params: fn_params.into_iter().map(|param| (param, true)).collect(),
      loop_id: 0,
    }
  }

  pub fn loop_id(&mut self) -> i32 {
    let id = self.loop_id;
    self.loop_id += 1;
    id
  }
}

fn munch_fdefn(fdecn: Fdefn, src_ctx: Box<TypecheckContext>) -> WasmFn {
  let fn_params = fdecn
    .param_list
    .0
    .iter()
    .map(|(_, param)| param.to_string())
    .collect::<Vec<String>>();
  let mut munch_ctx = MunchContext::new(src_ctx, fn_params.iter().cloned().collect());
  munch_ctx
    .declared_params
    .insert(TEMP_VAR1_NAME.to_string(), false);
  munch_ctx
    .declared_params
    .insert(TEMP_VAR2_NAME.to_string(), false);

  let mut instrs = vec![];
  munch_stmts(&mut munch_ctx, &mut instrs, fdecn.body);

  let instrs = munch_ctx
    .declared_params
    .iter()
    .filter_map(|(var, is_arg)| {
      if !is_arg {
        Some(WasmInstr::DeclVar(var.clone()))
      } else {
        None
      }
    })
    .chain(instrs)
    .collect::<Vec<WasmInstr>>();

  WasmFn {
    name: fdecn.func_name.clone(),
    params: fn_params,
    instrs,
    has_return: matches!(fdecn.ret_type, ReturnType::Type(_)),
  }
}

fn munch_gdecl(gdecl: Gdecl, src_ctx: Box<TypecheckContext>) -> Option<WasmFn> {
  match gdecl {
    Gdecl::Fdefn(fdefn) => Some(munch_fdefn(fdefn, src_ctx)),
    _ => None,
  }
}
/// Converts an AST into a list of WebAssembly functions.
pub fn ast2wasm(program: Program, src_ctx: TypecheckContext) -> Vec<WasmFn> {
  let src_ctx = Box::new(src_ctx);
  program
    .into_iter()
    .filter_map(|gdecl| munch_gdecl(gdecl, src_ctx.clone()))
    .collect()
}
