use anyhow::{anyhow, Result};

use crate::ast::Program;
pub use context::{Context as TypecheckContext, FunctionContext, FunctionStatus, StructContext};
use context::{Context, TypeContext, VarContext};
use gdecl::tc_program;

mod context;
mod expr;
mod gdecl;
mod stmt;

pub fn valid_header_ast(program: &mut Program) -> Result<Context> {
  let mut ctx = valid_ast(program, None, true)?;
  ctx.define_lib_functions();
  ctx.define_all_func();
  ctx.mark_all_func_as_header();
  if ctx.is_func_declared("main") {
    Err(anyhow!("main cannot be declared in header file"))
  } else {
    Ok(ctx)
  }
}

pub fn valid_source_ast(program: &mut Program, mut header_ctx: Context) -> Result<Context> {
  header_ctx.declare_main();

  valid_ast(program, Some(header_ctx), false)
}

/// Typecheck!
fn valid_ast(program: &mut Program, ctx_opt: Option<Context>, is_hdr: bool) -> Result<Context> {
  let mut ctx = if let Some(ctx) = ctx_opt {
    ctx
  } else {
    Context::new(
      VarContext::new(),
      FunctionContext::new(),
      TypeContext::new(),
      StructContext::new(),
    )
  };

  match tc_program(&mut ctx, program, is_hdr) {
    Ok(_) => Ok(ctx),
    Err(e) => Err(anyhow!("Invalid program: error: {}", e)),
  }
}
