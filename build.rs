// Build file to produce c0.rs
// Author: Miles Conn <mconn@andrew.cmu.edu>

use lalrpop::Configuration;

extern crate lalrpop;

fn main() {
  Configuration::new()
    .generate_in_source_tree()
    .process_current_dir()
    .unwrap();
}
