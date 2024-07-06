/// Implements the register coaleasing for register allocator
use std::collections::HashSet;

use crate::{
  asm::{Instr, Operand},
  registers::{color_from_reg, consts::REG_ALLOC_POOL_SIZE},
  utils::MinHeap,
  OPTIM_OPTIONS,
};

use super::{Allocator, InterferenceGraph, Node};

impl Allocator {
  pub(crate) fn greedy_coalesce(&mut self, igraph: &mut InterferenceGraph, c_max: i32) {
    if !OPTIM_OPTIONS.lock().unwrap().coaleacing {
      return;
    }

    // if no interference between src and dest, we try to coalesce
    // 1. we find a color c <= c_max that is not used in the neighborhood
    // 2. we coallesce src and dest
    let mut i = 0;
    while i < self.ctx.instrs.len() {
      let instr = self.ctx.instrs[i].clone();

      if let Instr::Mov {
        src: Operand::Temp(src),
        dest,
      } = instr
      {
        if let Some(neighs) = igraph.neighbors(&Node::Temp(src.0)) {
          if neighs.contains(&Node::Temp(dest.0)) {
            i += 1;
            continue;
          }

          // we cannot coaleace the function arguments
          if self.ctx.is_function_argument(&src) || self.ctx.is_function_argument(&dest) {
            i += 1;
            continue;
          }

          if let Some(color) = self.find_neigh_smallest_unused_color(igraph, src.0, dest.0, c_max) {
            let new_neighs = self.get_neighbors_union_exclusive(igraph, src.0, dest.0);
            igraph.remove_node(Node::Temp(src.0));
            igraph.remove_node(Node::Temp(dest.0));

            // we remove dest, but reuse the temp number of src as the new node
            igraph.add_node(Node::Temp(src.0), new_neighs);
            self.node_to_color[src.0 as usize] = color as i32;

            // rewrite the instructions using the new temp
            for instr in self.ctx.instrs.iter_mut() {
              instr.rewrite_temp(&dest, &src);
            }

            for line in self.abstract_lines.as_mut().unwrap() {
              line.rewrite_temp(&dest, &src);
            }
          }
        }
      }
      i += 1;
    }
  }

  /// Return the smallest color that is unused by the neighbors of the union of the neighborhoods
  /// If the color do not match the criteria, return None
  fn find_neigh_smallest_unused_color(
    &self,
    igraph: &InterferenceGraph,
    node1: u32,
    node2: u32,
    c_max: i32,
  ) -> Option<u32> {
    let mut neighbor_colors = MinHeap::new();
    let neigh_union = self.get_neighbors_union_exclusive(igraph, node1, node2);
    neigh_union
      .iter()
      .filter(|n| match n {
        Node::Temp(n) => *n != node1,
        _ => true,
      })
      .for_each(|n| {
        let color = match n {
          Node::Temp(n) => self.node_to_color[*n as usize],
          Node::Reg(reg) => color_from_reg(*reg) as i32,
        };
        neighbor_colors.push(color);
      });

    // we need find to a color that <= c_max and is unused *or* the color is a register
    // if the color is a stack location, we stop coallescing if none of the node is already in a register
    let color = neighbor_colors.find_smallest_absent();
    if color > c_max && color >= REG_ALLOC_POOL_SIZE as i32 {
      None
    } else {
      Some(color as u32)
    }
  }
}

// helpers
impl Allocator {
  /// get neigh(a).union(neigh(b)) - {a, b}
  fn get_neighbors_union_exclusive(
    &self,
    igraph: &InterferenceGraph,
    a: u32,
    b: u32,
  ) -> HashSet<Node> {
    let mut neighs = HashSet::new();
    if let Some(neighs_a) = igraph.neighbors(&Node::Temp(a)) {
      neighs.extend(neighs_a.iter().cloned());
    }
    if let Some(neighs_b) = igraph.neighbors(&Node::Temp(b)) {
      neighs.extend(neighs_b.iter().cloned());
    }
    neighs.remove(&Node::Temp(a));
    neighs.remove(&Node::Temp(b));
    neighs
  }
}
