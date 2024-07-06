mod graph;
pub use graph::Graph;
mod minheap;
pub use minheap::MinHeap;

use crate::{OPTIMIZE, UNSAFE};

pub fn is_macos() -> bool {
  std::env::var("UNAME") == Ok(String::from("Darwin"))
}

pub fn is_unsafe() -> bool {
  let u = UNSAFE.lock().unwrap();
  *u
}

pub fn do_optimize() -> bool {
  let o = OPTIMIZE.lock().unwrap();
  *o
}
