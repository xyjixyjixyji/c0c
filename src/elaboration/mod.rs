mod elab;
pub use elab::elab_program;

/// The library function name for abort, this is an invalid name for function.
/// We use this as a marker for the runtime-implemented abort function.
///
/// This is rewritten to "abort" function name in the final emission stage.
pub const LIBRARY_ABORT: &str = "15411abort";
pub const LIBRARY_CALLOC: &str = "15411calloc";
pub const LIBRARY_RAISE: &str = "15411raise";
pub const LIBRARY_MEMERROR: &str = "15411memerror";
