use std::collections::{BinaryHeap, HashMap};

use crate::{registers::consts::REG_ALLOC_POOL_SIZE, OPTIM_OPTIONS};

/// Implement the prespill algorithm for register allocation
use super::{Allocator, InterferenceGraph, Node};

impl Allocator {
  pub(crate) fn prespill(&mut self, igraph: &InterferenceGraph) {
    if !OPTIM_OPTIONS.lock().unwrap().prespilling {
      return;
    }

    let mut cliques = igraph.find_maximum_cliques();

    let mut temp_to_freq = HashMap::new();
    for clique in cliques.iter() {
      for node in clique.iter() {
        if let Node::Temp(t) = node {
          let freq = temp_to_freq.entry(t).or_insert(0);
          *freq += 1;
        }
      }
    }

    // max heap
    let mut node_freq_heap = BinaryHeap::new();
    for (temp, freq) in temp_to_freq.iter() {
      node_freq_heap.push((*freq, **temp));
    }

    loop {
      // if all < REG_ALLOC_POOL_SIZE, we are done with prespilling
      if cliques
        .iter()
        .all(|clique| clique.len() <= REG_ALLOC_POOL_SIZE)
      {
        break;
      }

      // find the vertex that appears most frequently in the cliques
      let (_, temp) = node_freq_heap.pop().unwrap();
      self.spill_temp(temp);

      // remove temp from all clique
      for clique in cliques.iter_mut() {
        clique.remove(&Node::Temp(temp));
      }
    }
  }

  fn spill_temp(&mut self, temp: u32) {
    let spilled = self.get_next_spill_operand();
    self.temp_allocation.insert(temp, spilled);
  }
}
