// L1 Compiler
//! Parse command line arguments
//! We expect this to be good enough for this course.
//! You could also use something like clap at the expense of bad compile times.
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>

use std::env;

use serde::Deserialize;

use crate::utils::do_optimize;

pub enum EmitTarget {
  Abstract,
  X86,
  Wasm,
}

fn get_optim_config_file_path() -> String {
  let source_dir = env!("CARGO_MANIFEST_DIR");
  format!("{}/{}", source_dir, "optim_opt.toml")
}

/// Configuration options for this compiler run.
pub struct Config {
  pub verbose: bool,
  pub regalloc_only: bool,
  pub tc_only: bool,
  pub dump_ast: bool,
  pub dump_assem: bool,
  pub r#unsafe: bool,
  pub optimize: bool,

  pub emit: EmitTarget,
  pub hdr_file: Option<String>,
  pub file: Option<String>,
}

/// Configuration options for optimization passes.
#[derive(Deserialize)]
pub struct OptimizationConfig {
  pub tco: bool,
  pub inline: bool,
  pub strength_reduction: bool,
  pub peephole_const_prop: bool,
  pub peephole_copy_prop: bool,
  pub peephole_rewrite_condjmp: bool,
  pub peephole_rem_null_seq: bool,
  pub dead_code_elim: bool,

  pub mcs_heuristic: bool,
  pub prespilling: bool,
  pub coaleacing: bool,
}

impl OptimizationConfig {
  pub fn from_config() -> Self {
    let config_file = get_optim_config_file_path();
    let config = std::fs::read_to_string(config_file);
    if let Ok(config) = config {
      if do_optimize() {
        toml::from_str(&config).unwrap()
      } else {
        OptimizationConfig {
          tco: false,
          inline: false,
          strength_reduction: false,
          peephole_const_prop: false,
          peephole_copy_prop: false,
          peephole_rewrite_condjmp: false,
          peephole_rem_null_seq: false,
          dead_code_elim: false,

          mcs_heuristic: false,
          prespilling: false,
          coaleacing: false,
        }
      }
    } else {
      OptimizationConfig {
        tco: true,
        inline: true,
        strength_reduction: true,
        peephole_const_prop: true,
        peephole_copy_prop: true,
        peephole_rewrite_condjmp: true,
        peephole_rem_null_seq: true,
        dead_code_elim: true,

        mcs_heuristic: true,
        prespilling: true,
        coaleacing: true,
      }
    }
  }
}

impl Config {
  /// Set your defaults here!
  fn default() -> Self {
    Config {
      verbose: false,       // Get extra output from the compiler
      regalloc_only: false, // Register allocate for checkpoint
      tc_only: false,       // Stop after parsing & checking types.
      dump_ast: false,      // Print the AST
      dump_assem: false,    // Print the generated abstract assembly.
      r#unsafe: false,      // Allow unsafe code.
      optimize: false,      // Optimize the generated code.

      emit: EmitTarget::Abstract, // Type of file to output
      hdr_file: None,             // Header file to include
      file: None,                 // Source file to compile.
    }
  }
}

/// Parses command line input into a configuration. Panics on invalid args.
pub fn parse_args() -> Config {
  let args: Vec<String> = env::args().collect();
  let mut config = Config::default();
  let mut index = 1;
  while index < args.len() {
    match args[index].as_str() {
      "--unsafe" => config.r#unsafe = true,
      "-O1" => config.optimize = true,
      "-v" | "--verbose" => config.verbose = true,
      "--dump-ast" => config.dump_ast = true,
      "--dump-assem" => config.dump_assem = true,
      "-t" | "--typecheck-only" => config.tc_only = true,
      "-r" | "--regalloc-only" => config.regalloc_only = true,
      "-e" | "--emit" => {
        // Allow for the emit type to be the next space-delimited token.
        if index + 1 < args.len() {
          match args[index + 1].as_str() {
            "abs" => config.emit = EmitTarget::Abstract,
            "x86-64" => config.emit = EmitTarget::X86,
            "was" => config.emit = EmitTarget::Wasm,
            other => {
              panic!("Unknown emit type : {}", other);
            }
          };
          index += 1;
        } else {
          panic!("Expected emit type");
        };
      }
      // Account for funky spacing in the grading script.
      "-eabs" => config.emit = EmitTarget::Abstract,
      "-ex86-64" => config.emit = EmitTarget::X86,
      "-ewasm" => config.emit = EmitTarget::Wasm,
      "-l" => {
        if index + 1 < args.len() {
          let hdr_file = args[index + 1].as_str();
          if let Some('-') = hdr_file.chars().next() {
          } else {
            config.hdr_file = Some(hdr_file.to_string())
          };
          index += 1;
        } else {
          panic!("Expected header file");
        };
      }
      file => {
        if let Some('-') = file.chars().next() {
        } else {
          config.file = Some(file.to_string())
        }
      }
    };
    index += 1;
  }

  if config.file.is_none() {
    panic!("Expected file input");
  }

  config
}
