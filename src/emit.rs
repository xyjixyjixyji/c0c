// L2 Compiler
//! File emission
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>

use crate::abs2asm::fname_to_label;
use crate::regalloc::Allocator;
use crate::typecheck::TypecheckContext;
use crate::wasmgen::ast2wasm;
use crate::{abs2asm, ast, codegen};
use std::fs::File;
use std::io::prelude::*;

/// Return the wasm assembly code as a string.
pub fn return_wasm(program: ast::Program, src_ctx: TypecheckContext) -> String {
  let mut wasm = vec![];
  writeln!(wasm, "(\nmodule").unwrap();

  import_wasm_runtime_lib(&mut wasm);

  let wasm_fns = ast2wasm(program, src_ctx);
  for wasm_fn in wasm_fns {
    writeln!(wasm, "{}", wasm_fn).unwrap();
  }
  writeln!(wasm, ")").unwrap();
  String::from_utf8(wasm).unwrap()
}

fn import_wasm_runtime_lib(file: &mut impl Write) {
  writeln!(
    file,
    "(import \"alloc\" \"allocate\" (func $__15411_allocate (param i32) (result i32)))"
  )
  .unwrap();
  writeln!(file, "(import \"alloc\" \"memory\" (memory 1))").unwrap();

  // since we cannot raise signals directly, we use 'div by zero' to abort the program
  writeln!(
    file,
    "(func $15411abort (i32.const 1) (i32.const 0) (i32.div_u) (drop))"
  )
  .unwrap();
  writeln!(
    file,
    "(func $15411memerror (i32.const -1) (i32.load) (drop))"
  )
  .unwrap();
}

pub fn return_x86(ctxs: Vec<codegen::Context>) -> String {
  let mut x86 = vec![];
  for ctx in ctxs.into_iter() {
    let label = fname_to_label(
      ctx.fn_name.as_str(),
      ctx.get_func_status(&ctx.fn_name).unwrap().is_header(),
    );
    writeln!(x86, ".globl {}\n", &label).unwrap();

    x86_function_instr(ctx).into_iter().for_each(|inst| {
      writeln!(x86, "{}", inst).unwrap();
    });

    writeln!(x86).unwrap();
  }
  String::from_utf8(x86).unwrap()
}

/// Emit the wasm assembly code to a file.
///
/// The file is in the WAT format.
pub fn emit_wasm(
  filename: &str,
  program: ast::Program,
  src_ctx: TypecheckContext,
) -> std::io::Result<()> {
  let mut file = File::create(format!("{}.wat", filename)).unwrap();
  writeln!(file, "(\nmodule")?;

  import_wasm_runtime_lib(&mut file);

  let wasm_fns = ast2wasm(program, src_ctx);
  for wasm_fn in wasm_fns {
    writeln!(file, "{}", wasm_fn)?;
  }

  writeln!(file, ")")?;
  Ok(())
}

/// Emit the assembly code to a file.
///
/// # Arguments
///
/// * `filename` - The name of the file to write to.
/// * `ctxs` - The contexts to emit. Each context represents a function.
pub fn emit_x86(filename: &str, ctxs: Vec<codegen::Context>) -> std::io::Result<()> {
  let mut file = File::create(format!("{}.s", filename)).unwrap();
  writeln!(file, ".file\t{:?}\t", filename)?;

  // we only retain those functions that are at least called once

  for ctx in ctxs.into_iter() {
    let label = fname_to_label(
      ctx.fn_name.as_str(),
      ctx.get_func_status(&ctx.fn_name).unwrap().is_header(),
    );
    writeln!(file, ".globl {}\n", &label)?;

    x86_function_instr(ctx).into_iter().for_each(|inst| {
      writeln!(file, "{}", inst).unwrap();
    });

    writeln!(file)?;
  }

  Ok(())
}

// Generate x86 assembly instructions from the given context(per function).
fn x86_function_instr(ctx: codegen::Context) -> Vec<String> {
  let allocator = if ctx.temp_index > 5000 {
    Allocator::all_stack_allocator(ctx.clone())
  } else {
    Allocator::from_context(ctx.clone())
  };

  let is_header = ctx.get_func_status(&ctx.fn_name).unwrap().is_header();
  let fn_name = ctx.fn_name;

  let translator = abs2asm::X86Translator::from_allocator(allocator);

  let x86instrs = translator.translate_subroutine(&fname_to_label(&fn_name, is_header));

  x86instrs
    .iter()
    .map(|inst| inst.to_string())
    .filter(|s| !s.trim_end().is_empty())
    .collect()
}

pub fn emit_abs(filename: &str, ctxs: Vec<codegen::Context>) -> std::io::Result<()> {
  let mut file = File::create(format!("{}.abs", filename)).unwrap();
  writeln!(file, "// 15-411 L3 Compiler\n")?;

  for ctx in ctxs.into_iter() {
    let num_args = ctx
      .src_ctx
      .get_func_decl(&ctx.fn_name)
      .unwrap()
      .param_list
      .0
      .len();

    write!(file, "{}(", ctx.fn_name)?;
    for i in 0..num_args {
      write!(file, "T{}", i)?;
      if i != num_args - 1 {
        write!(file, ", ")?;
      }
    }
    writeln!(file, ")")?;

    for ins in &ctx.instrs {
      writeln!(file, "\t\t{}", ins)?;
    }
    writeln!(file)?;
  }
  Ok(())
}
