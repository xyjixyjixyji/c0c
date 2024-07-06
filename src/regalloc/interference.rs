use enum_as_inner::EnumAsInner;

use crate::{
  asm::Instr,
  ast,
  codegen::Context,
  regalloc::liveness::Liveness,
  registers::{
    consts::{RESERVED_REG2_64BIT, RESERVED_REG3_64BIT},
    reg::X86_64Register,
  },
  utils::Graph,
  OPTIM_OPTIONS,
};

use std::{
  collections::{HashMap, HashSet},
  iter::FromIterator,
};

/// A node in the interference graph.
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, EnumAsInner)]
pub enum Node {
  Temp(u32),
  Reg(X86_64Register),
}

impl std::default::Default for Node {
  fn default() -> Self {
    Node::Temp(0)
  }
}

impl std::fmt::Display for Node {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Node::Temp(temp) => write!(f, "{}", temp),
      Node::Reg(reg) => write!(f, "{}", reg),
    }
  }
}

// Represent the interference graph built from anstract assembly.
#[derive(Debug)]
pub struct InterferenceGraph {
  graph: Graph<Node>,
}

impl InterferenceGraph {
  pub fn new(graph: Graph<Node>) -> Self {
    InterferenceGraph { graph }
  }

  /// Build the interference graph from the liveness analysis result.
  pub fn from_liveness(liveness: &Liveness) -> Self {
    let mut graph = Graph::new();
    for i in 0..liveness.num_temp() {
      graph.add_node(Node::Temp(i));
    }

    let lines = liveness.lines().clone();
    for line in &lines {
      let succ_live_in: Option<Vec<Node>> = line.successors().map(|succs| {
        succs
          .iter()
          .flat_map(|succ| lines[*succ].live_in().clone())
          .collect()
      });

      if let Some(live_in) = succ_live_in {
        let succ_live_in = HashSet::from_iter(live_in.into_iter());
        for def in line.defines().iter() {
          graph.add_graph_edges(*def, &succ_live_in);
        }
      }

      Self::precolor(
        line.instr(),
        &HashSet::from_iter(line.live_in().clone().into_iter()),
        &mut graph,
      );
    }

    Self::new(graph)
  }

  /// Generate Simplicial Elimination Order using Maximum Cardinality Search
  ///
  /// Note that for a chordal graph, the MCS algorithm is guaranteed to generate
  /// a perfect elimination order. But for a non-chordal graph, the MCS still
  /// generate a valid order.
  pub fn generate_simplicial_order(&self, ctx: Option<&Context>) -> Vec<u32> {
    let num_temps = self.num_temp();
    let mut temp_bucket = HashMap::new();
    let mut buckets = vec![vec![]];
    let mut c = 0;
    for i in 0..num_temps {
      temp_bucket.insert(i, 0);
      buckets[0].push(i);
    }

    let mut simplicial_order = Vec::new();
    for _ in 0..num_temps {
      // find a node in largest bucket, if it empty, decrement largest_bucket
      while buckets[c].is_empty() {
        c -= 1;
      }

      let picked_temp = if !OPTIM_OPTIONS.lock().unwrap().mcs_heuristic {
        buckets[c][0]
      } else if let Some(ctx) = ctx {
        // TODO: we can optimize this using nested loop heuristic
        // to break the tie, we choose the node that is most often used
        let candidates = &buckets[c];
        *candidates
          .iter()
          .max_by_key(|&temp| ctx.try_get_temp_used_count(*temp as u32))
          .unwrap_or(&0)
      } else {
        buckets[c][0]
      };

      // remove the picked node from the bucket
      let index = buckets[c].iter().position(|x| *x == picked_temp).unwrap();
      buckets[c].remove(index);

      simplicial_order.push(picked_temp as u32);

      // move all neighbors of max_temp to the next bucket
      let neighbors = self
        .graph
        .neighbors(&Node::Temp(picked_temp as u32))
        .unwrap()
        .iter()
        .filter(|n| matches!(n, Node::Temp(_)))
        .filter(|n| !simplicial_order.contains(n.as_temp().unwrap()))
        .map(|n| *n.as_temp().unwrap() as usize)
        .collect::<Vec<_>>();

      for neigh in neighbors.into_iter() {
        let orig_bucket = temp_bucket[&neigh];
        let new_bucket = orig_bucket + 1;
        c = c.max(new_bucket);

        temp_bucket.insert(neigh, new_bucket);
        if new_bucket >= buckets.len() {
          buckets.push(vec![]);
        }
        buckets[new_bucket].push(neigh);
        buckets[orig_bucket].retain(|&temp| temp != neigh);
      }
    }

    simplicial_order
  }

  /// Precoloring, essentially is to marking extra interference to prevent
  /// some instructions overwriting some Dest.
  fn precolor(instr: &Instr, live_in: &HashSet<Node>, graph: &mut Graph<Node>) {
    match instr {
      Instr::BinOp { op, src2, .. } => match op {
        ast::BinOp::Div | ast::BinOp::Mod | ast::BinOp::Shr | ast::BinOp::Shl => {
          graph.add_node(Node::Reg(X86_64Register::RAX(32)));
          graph.add_node(Node::Reg(X86_64Register::RDX(32)));

          // we optimize mod and div into shift and freeup RDX
          if OPTIM_OPTIONS.lock().unwrap().strength_reduction
            && matches!(op, ast::BinOp::Mod | ast::BinOp::Div)
            && src2.exponent_of_two().is_some()
          {
            for node in live_in {
              graph.add_graph_edge(Node::Reg(X86_64Register::RAX(32)), *node);
            }
            return;
          }

          for node in live_in {
            graph.add_graph_edge(Node::Reg(X86_64Register::RAX(32)), *node);
            graph.add_graph_edge(Node::Reg(X86_64Register::RDX(32)), *node);
          }

          if matches!(op, ast::BinOp::Shr | ast::BinOp::Shl) {
            graph.add_node(Node::Reg(X86_64Register::RCX(32)));
            for node in live_in {
              graph.add_graph_edge(Node::Reg(X86_64Register::RCX(32)), *node);
            }
          }
        }
        _ => {}
      },
      Instr::Call { .. } => {
        let arg_nodes = X86_64Register::argument_regs(32)
          .map(Node::Reg)
          .iter()
          .cloned()
          .collect::<HashSet<_>>();

        // all live in needs to be connected to the argument registers
        for t in live_in {
          graph.add_graph_edges(*t, &arg_nodes);
        }
      }
      Instr::WriteMem { .. } => {
        for node in live_in {
          graph.add_graph_edge(
            *node,
            Node::Reg(*RESERVED_REG2_64BIT.as_register().unwrap()),
          );
        }
      }
      Instr::Lea { .. } => {
        for node in live_in {
          graph.add_graph_edge(
            *node,
            Node::Reg(*RESERVED_REG2_64BIT.as_register().unwrap()),
          );
          graph.add_graph_edge(
            *node,
            Node::Reg(*RESERVED_REG3_64BIT.as_register().unwrap()),
          );
        }
      }
      _ => {}
    }
  }

  pub fn neighbors(&self, node: &Node) -> Option<&HashSet<Node>> {
    self.graph.neighbors(node)
  }

  pub fn num_temp(&self) -> usize {
    // use self.graph.nodes_ref() and filter all temps in a functional way
    self
      .graph
      .nodes_ref()
      .iter()
      .filter(|n| matches!(n, Node::Temp(_)))
      .count()
  }

  pub fn remove_node(&mut self, node: Node) {
    self.graph.remove_node(node);
  }

  pub fn add_node(&mut self, node: Node, neighbors: HashSet<Node>) {
    self.graph.add_node(node);
    self.graph.add_graph_edges(node, &neighbors);
  }

  pub fn find_maximum_cliques(&self) -> Vec<HashSet<Node>> {
    self.graph.find_maximum_cliques()
  }
}

#[allow(dead_code)]
fn debug_show_interf_graph(graph: &Graph<Node>) {
  for node in graph.nodes_ref() {
    let neighbors = graph.neighbors(node).unwrap();
    log::debug!("T{}: {:?}", node, neighbors);
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_generate_basic_seo() {
    // 0 -> 1 -> 2 -> 3 -> 4
    let mut graph = Graph::new();
    for i in 0..5 {
      graph.add_node(Node::Temp(i));
    }

    graph.add_graph_edge(Node::Temp(0), Node::Temp(1));
    graph.add_graph_edge(Node::Temp(1), Node::Temp(2));
    graph.add_graph_edge(Node::Temp(2), Node::Temp(3));
    graph.add_graph_edge(Node::Temp(3), Node::Temp(4));

    let igraph = InterferenceGraph { graph };
    let seo = igraph.generate_simplicial_order(None);

    assert_eq!(seo, vec![0, 1, 2, 3, 4]);
  }

  #[test]
  fn test_generate_detached_seo() {
    // 0 -> 1 -> 2 -> 3 -> 4
    // 5 -> 6 -> 7 -> 8 -> 9
    let mut graph = Graph::new();
    for i in 0..10 {
      graph.add_node(Node::Temp(i));
    }

    graph.add_graph_edge(Node::Temp(0), Node::Temp(1));
    graph.add_graph_edge(Node::Temp(1), Node::Temp(2));
    graph.add_graph_edge(Node::Temp(2), Node::Temp(3));
    graph.add_graph_edge(Node::Temp(3), Node::Temp(4));
    graph.add_graph_edge(Node::Temp(5), Node::Temp(6));
    graph.add_graph_edge(Node::Temp(6), Node::Temp(7));
    graph.add_graph_edge(Node::Temp(7), Node::Temp(8));
    graph.add_graph_edge(Node::Temp(8), Node::Temp(9));

    let igraph = InterferenceGraph { graph };
    let seo = igraph.generate_simplicial_order(None);

    assert_eq!(seo, vec![0, 1, 2, 3, 4, 5, 6, 7, 8, 9]);
  }

  #[test]
  fn test_generate_z_seo() {
    let mut graph = Graph::new();
    for i in 0..5 {
      graph.add_node(Node::Temp(i));
    }

    graph.add_graph_edge(Node::Temp(1), Node::Temp(2));
    graph.add_graph_edge(Node::Temp(2), Node::Temp(3));
    graph.add_graph_edge(Node::Temp(3), Node::Temp(4));

    let igraph = InterferenceGraph { graph };
    let seo = igraph.generate_simplicial_order(None);

    assert_eq!(seo, vec![0, 1, 2, 3, 4]);
  }
}
