use std::cmp::Reverse;
use std::collections::{BinaryHeap, HashSet};

pub struct MinHeap<T: Ord>(BinaryHeap<Reverse<T>>);

impl<T: Ord> MinHeap<T> {
  pub fn new() -> Self {
    MinHeap(BinaryHeap::<Reverse<T>>::new())
  }

  pub fn push(&mut self, item: T) {
    self.0.push(Reverse(item));
  }

  pub fn pop(&mut self) -> Option<T> {
    self.0.pop().map(|v| v.0)
  }
}

impl MinHeap<i32> {
  pub fn find_smallest_absent(&mut self) -> i32 {
    let mut popped = HashSet::new();
    let mut i = 0;
    while let Some(v) = self.pop() {
      if popped.contains(&v) {
        continue;
      }

      if v != i {
        return i;
      }

      popped.insert(v);
      i += 1;
    }
    i
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_minheap() {
    let mut heap = MinHeap::new();
    heap.push(3);
    heap.push(1);
    heap.push(2);
    heap.push(0);
    assert_eq!(heap.find_smallest_absent(), 4);
  }

  #[test]
  fn test_minheap2() {
    let mut heap = MinHeap::new();
    heap.push(3);
    heap.push(1);
    heap.push(0);
    assert_eq!(heap.find_smallest_absent(), 2);
  }
}
