// L1 Compiler
//! Top Level Environment
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>

#[allow(clippy::all, unused_imports)]
mod c0;

mod abs2asm;
mod args;
mod asm;
mod ast;
mod codegen;
mod elaboration;
mod emit;
mod lex;
mod optimization;
mod parse;
mod regalloc;
mod registers;
mod typecheck;
mod utils;
mod wasmgen;

use anyhow::Result;
use args::EmitTarget;
use lazy_static::lazy_static;
use wasm_bindgen::prelude::*;

use std::sync::Arc;
use std::sync::Mutex;
use std::thread;
use std::time;

use crate::lex::LexerState;
use crate::parse::parser;
use crate::utils::do_optimize;

lazy_static! {
  static ref UNSAFE: Arc<Mutex<bool>> = Arc::new(Mutex::new(false));
  static ref OPTIMIZE: Arc<Mutex<bool>> = Arc::new(Mutex::new(false));
  static ref OPTIM_OPTIONS: Arc<Mutex<args::OptimizationConfig>> =
    Arc::new(Mutex::new(args::OptimizationConfig::from_config()));
}

fn main() {
  let cfg = args::parse_args();

  /// Take a filename and parse it into an AST!
  fn make_program(
    filename: Option<&str>,
    hdr_lexer_state: Option<LexerState>,
  ) -> Result<(ast::Program, Option<LexerState>)> {
    match filename {
      None => Ok((vec![], None)),
      Some(filename) => parser::parse(filename.to_string(), hdr_lexer_state, None),
    }
  }

  // Helper macro to time evaluating an expression (like a function call.)
  macro_rules! time {
    ( $x:expr ) => {{
      let t1 = time::SystemTime::now();
      let result = $x;
      (result, t1.elapsed().unwrap())
    }};
  }

  // We call with a large stack to avoid any overflows caused
  // by overzealous 15-411 students with large test cases.
  let child = thread::Builder::new()
    .stack_size(128 * 1024 * 1024)
    .spawn(move || {
      // make envlogger stdout
      env_logger::builder()
        .target(env_logger::Target::Stdout)
        .format_timestamp(None)
        .init();

      // Set the global UNSAFE and OPTIMIZE flags.
      if cfg.r#unsafe {
        *(UNSAFE.lock().unwrap()) = true;
      }
      if cfg.optimize {
        *(OPTIMIZE.lock().unwrap()) = true;
      }

      let filename = &cfg.file.unwrap();
      let (hdr_program, hdr_parse_time) = time!(make_program(cfg.hdr_file.as_deref(), None));
      let (mut hdr_prog, hdr_lexer_state) = match hdr_program {
        Err(e) => {
          eprintln!("{}", e);
          return 1; // Parse failed!
        }
        Ok((hdr_prog, lexer_state)) => (hdr_prog, lexer_state),
      };

      let (program, parse_time) = time!(make_program(Some(filename), hdr_lexer_state));
      let mut prog = match program {
        Err(e) => {
          eprintln!("{}", e);
          return 1; // Parse failed!
        }
        Ok((prog, _)) => prog,
      };

      let (hdr_ctx, hdr_tc_time) = time!(typecheck::valid_header_ast(&mut hdr_prog));
      if let Err(e) = hdr_ctx {
        eprintln!("{}", e);
        return 1; // Tc failed (sad!)
      }

      let (prog_ctx, tc_time) = time!(typecheck::valid_source_ast(&mut prog, hdr_ctx.unwrap()));
      if let Err(e) = prog_ctx {
        eprintln!("{}", e);
        return 1; // Tc failed (sad!)
      }

      if cfg.dump_ast {
        for line in &hdr_prog {
          println!("{:?}", line);
        }
        for line in &prog {
          println!("{:?}", line);
        }
      }

      if cfg.tc_only {
        println!("Pass typecheck.");
        return 0;
      }
      let prog_ctx_unwrap = prog_ctx.unwrap();
      if matches!(cfg.emit, EmitTarget::Wasm) {
        return emit::emit_wasm(filename, prog, prog_ctx_unwrap).is_err() as i32;
      }

      let do_optimize = do_optimize();
      let (ctxs, cg_time) = time!(codegen::munch_ast(prog, prog_ctx_unwrap, do_optimize));

      if cfg.dump_assem {
        for ctx in &ctxs {
          println!("Function: {}", ctx.fn_name);
          ctx.print_instrs();

          println!("\nBasic Blocks:");
          let basic_blocks_vec = optimization::basic_blocks_vec_from_ctx(ctx);
          for bb in basic_blocks_vec {
            bb.display("");
          }
          println!();
        }
      }

      if cfg.verbose {
        println!(
          "Parse time: {} us",
          parse_time.as_micros() + hdr_parse_time.as_micros()
        );
        println!(
          "Typecheck: {} us",
          tc_time.as_micros() + hdr_tc_time.as_micros()
        );
        println!("Codegen: {} us", cg_time.as_micros());
      }

      match cfg.emit {
        EmitTarget::Abstract => emit::emit_abs(filename, ctxs).is_err() as i32,
        EmitTarget::X86 => emit::emit_x86(filename, ctxs).is_err() as i32,
        EmitTarget::Wasm => 0,
      }
    })
    .unwrap();
  // Return the value from the child thread as the return value of the compiler.
  std::process::exit(child.join().expect("Couldn't join spawned thread"));
}

#[wasm_bindgen]
pub fn compile_to_wasm(header_file: &str, main_file: &str) -> String {
  match compile_to_wasm_inner(header_file, main_file) {
    Ok(wasm) => wasm,
    Err(e) => e.to_string(),
  }
}

fn compile_to_wasm_inner(header_file: &str, main_file: &str) -> Result<String> {
  let (mut hdr_prog, lexer_state) = if header_file.is_empty() {
    (vec![], None)
  } else {
    parser::parse("".to_string(), None, Some(header_file.to_string()))?
  };
  let (mut prog, _) = parser::parse("".to_string(), lexer_state, Some(main_file.to_string()))?;
  let hdr_ctx = typecheck::valid_header_ast(&mut hdr_prog)?;
  let source_ctx = typecheck::valid_source_ast(&mut prog, hdr_ctx)?;

  Ok(emit::return_wasm(prog, source_ctx))
}

#[wasm_bindgen]
pub fn compile_to_x86(header_file: &str, main_file: &str) -> String {
  match compile_to_x86_inner(header_file, main_file) {
    Ok(x86) => x86,
    Err(e) => e.to_string(),
  }
}

fn compile_to_x86_inner(header_file: &str, main_file: &str) -> Result<String> {
  let (mut hdr_prog, lexer_state) = if header_file.is_empty() {
    (vec![], None)
  } else {
    parser::parse("".to_string(), None, Some(header_file.to_string()))?
  };
  let (mut prog, _) = parser::parse("".to_string(), lexer_state, Some(main_file.to_string()))?;
  let hdr_ctx = typecheck::valid_header_ast(&mut hdr_prog)?;
  let source_ctx = typecheck::valid_source_ast(&mut prog, hdr_ctx)?;

  let ctxs = codegen::munch_ast(prog, source_ctx, true);
  Ok(emit::return_x86(ctxs))
}
