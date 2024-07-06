// The module translate the abstract assembly into x86_64 assembly

mod asmline;
mod operand;

pub use operand::{MemLoc, X64Operand};
mod trans;
pub use trans::X86Translator;

use crate::{
  elaboration::{LIBRARY_ABORT, LIBRARY_CALLOC, LIBRARY_RAISE},
  utils::is_macos,
};
mod x86instr;

// Fetches the env variable name. For those developing on Macs, you'll
// want to export the UNAME env variable as 'Darwin' in ~/.bashrc or ~/.bash_profile.
pub fn fname_to_label(fname: &str, is_header: bool) -> String {
  // this is for benchmark
  if std::env::var("NORENAMEMAIN").is_ok() && fname == "main" {
    if is_macos() {
      return "_main".to_string();
    }
    return "main".to_string();
  }

  let fname = match fname {
    LIBRARY_ABORT => "abort".to_string(),
    LIBRARY_CALLOC => "calloc".to_string(),
    LIBRARY_RAISE => "raise".to_string(),
    _ => fname.to_string(),
  };

  if !is_header {
    if is_macos() {
      format!("__c0_{}", fname)
    } else {
      format!("_c0_{}", fname)
    }
  } else if is_macos() {
    format!("_{}", fname)
  } else {
    fname
  }
}
