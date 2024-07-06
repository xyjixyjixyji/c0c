use std::collections::{HashMap, HashSet};
use std::hash::Hash;

/// Graph structure:
/// A Vec of length num_temps, each graph[i] contains
/// a HashSet of nodes connected to node i.
#[derive(Clone, Debug)]
pub struct Graph<T: Hash>(HashMap<T, HashSet<T>>);

impl<T: Hash + Eq + PartialEq + Clone + Copy> Graph<T> {
  pub fn new() -> Self {
    Graph(HashMap::new())
  }

  /// Return the neighbors of a node
  ///
  /// # Return
  /// If the node does not exist, return None
  /// If there is no neighbor for this node, return an empty HashSet
  pub fn neighbors(&self, node: &T) -> Option<&HashSet<T>> {
    if let Some(neighbors) = self.0.get(node) {
      Some(neighbors)
    } else {
      None
    }
  }

  pub fn contains(&self, node: &T) -> bool {
    self.0.contains_key(node)
  }

  /// Return the vector of the references to all nodes
  pub fn nodes_ref(&self) -> Vec<&T> {
    self.0.keys().collect()
  }

  /// Idempotently add a node into the graph
  pub fn add_node(&mut self, node: T) {
    if !self.contains(&node) {
      self.0.insert(node, HashSet::new());
    }
  }

  /// Add edges between a node and its neighbors
  pub fn add_graph_edges(&mut self, node: T, neighbors: &HashSet<T>) {
    for neigh in neighbors {
      self.add_graph_edge(node, *neigh);
    }
  }

  /// Add an edge between two nodes, i.e. node1 and node2
  pub fn add_graph_edge(&mut self, node1: T, node2: T) {
    if self.0.get(&node1).is_none() {
      self.add_node(node1);
    }

    if self.0.get(&node2).is_none() {
      self.add_node(node2);
    }

    self.0.get_mut(&node1).unwrap().insert(node2);
    self.0.get_mut(&node2).unwrap().insert(node1);
  }

  pub fn add_directed_edge(&mut self, node1: T, node2: T) {
    if self.0.get(&node1).is_none() {
      self.add_node(node1);
    }

    if self.0.get(&node2).is_none() {
      self.add_node(node2);
    }

    self.0.get_mut(&node1).unwrap().insert(node2);
  }

  pub fn remove_node(&mut self, node: T) {
    self.0.remove(&node);
    for (_, neighbors) in self.0.iter_mut() {
      neighbors.remove(&node);
    }
  }

  pub fn find_maximum_cliques(&self) -> Vec<HashSet<T>> {
    let mut cliques: Vec<HashSet<T>> = Vec::new();
    let p = self.nodes_ref().into_iter().cloned().collect();
    self.bron_kerbosch_with_pivot(HashSet::new(), p, HashSet::new(), &mut cliques);
    cliques
  }

  fn bron_kerbosch_with_pivot(
    &self,
    mut r: HashSet<T>,
    p: HashSet<T>,
    mut x: HashSet<T>,
    cliques: &mut Vec<HashSet<T>>,
  ) {
    if p.is_empty() && x.is_empty() {
      cliques.push(r);
      return;
    }

    // Choosing a pivot (u) from P union X to minimize the size of P \ N(u)
    let u = p
      .union(&x)
      .max_by_key(|v| self.neighbors(*v).map_or(0, |neigh| neigh.len()))
      .cloned()
      .unwrap();
    let neighbors_of_u = self.neighbors(&u).unwrap_or(&HashSet::new()).clone();

    let p_without_neighbors_of_u = p
      .difference(&neighbors_of_u)
      .cloned()
      .collect::<HashSet<T>>();

    for v in p_without_neighbors_of_u {
      let neighbors = self.neighbors(&v).unwrap_or(&HashSet::new()).clone();
      self.bron_kerbosch_with_pivot(
        r.union(&HashSet::from([v])).cloned().collect(),
        p.intersection(&neighbors).cloned().collect(),
        x.intersection(&neighbors).cloned().collect(),
        cliques,
      );
      r.remove(&v);
      x.insert(v);
    }
  }
}
