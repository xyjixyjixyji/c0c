use std::{
  collections::{HashMap, HashSet, VecDeque},
  iter::FromIterator,
};

use crate::{
  asm::{Instr, InstrLabel},
  codegen::Context,
};

use super::{basic_blocks::BasicBlock, build_basic_block_vec};

#[derive(Debug)]
pub struct ControlFlowGraph {
  pub blocks: VecDeque<BasicBlock>,
  pub successors: HashMap<InstrLabel, (Option<InstrLabel>, Option<InstrLabel>)>,
  pub predecessors: HashMap<InstrLabel, HashSet<InstrLabel>>,
  pub reachable_blocks: HashSet<InstrLabel>,
  /// We do not use the instruction list in the context
  pub codegen_ctx: Context,
}

impl ControlFlowGraph {
  pub fn from_context(ctx: Context) -> Self {
    let blocks = build_basic_block_vec(&ctx);
    Self {
      blocks: VecDeque::new(),
      successors: HashMap::new(),
      predecessors: HashMap::new(),
      reachable_blocks: HashSet::new(),
      codegen_ctx: ctx,
    }
    .build_cfg(blocks)
  }

  pub fn into_context(mut self) -> Context {
    let reachable_blocks = &self.reachable_blocks;
    let instrs = self
      .blocks
      .into_iter()
      .filter(|b| reachable_blocks.contains(&b.label))
      .flat_map(|b| b.instructions)
      .filter(|instr| !matches!(instr, Instr::Nop))
      .collect::<Vec<_>>();

    self.codegen_ctx.instrs = instrs;
    self.codegen_ctx
  }

  pub fn get_block_ref_mut(&mut self, label: &InstrLabel) -> &mut BasicBlock {
    let block_id = self.blocks.iter().position(|b| &b.label == label).unwrap();
    self.blocks.get_mut(block_id).unwrap()
  }

  pub fn get_successors(&self, label: &InstrLabel) -> (Option<InstrLabel>, Option<InstrLabel>) {
    self.successors[label].clone()
  }

  pub fn get_predecessors(&self, label: &InstrLabel) -> HashSet<InstrLabel> {
    self.predecessors[label].clone()
  }

  /// After the const propagation pass, some blocks will become redundant, so we rebuild the CFG
  pub fn rebuild_cfg(&mut self) {
    self.successors.clear();
    self.predecessors.clear();
    let mut new_successors = HashMap::new();
    let mut new_predecessors = HashMap::new();
    for block in self.blocks.iter() {
      self.update_mapping(block, &mut new_successors, &mut new_predecessors);
    }
    self.successors = new_successors;
    self.predecessors = new_predecessors;
    self.populate_reachability();
  }

  #[allow(dead_code)]
  pub fn remove_block(&mut self, label: &InstrLabel) {
    let block_id = self.blocks.iter().position(|b| &b.label == label).unwrap();

    // remove from block list
    self.blocks.remove(block_id);

    // remove from the successor map
    self.successors.remove(label);
    for (_, (s1, s2)) in self.successors.iter_mut() {
      if let Some(s) = s1 {
        if s == label {
          *s1 = None;
        }
      }
      if let Some(s) = s2 {
        if s == label {
          *s2 = None;
        }
      }
    }

    // remove from predecessor map
    self.predecessors.remove(label);
    for (_, preds) in self.predecessors.iter_mut() {
      preds.remove(label);
    }
  }

  fn build_cfg(mut self, blocks: VecDeque<BasicBlock>) -> Self {
    let mut successors = HashMap::new();
    let mut predecessors = HashMap::new();
    for block in blocks.iter() {
      self.update_mapping(block, &mut successors, &mut predecessors);
    }
    self.blocks = blocks;
    self.predecessors = predecessors;
    self.successors = successors;
    self.populate_reachability();
    self
  }

  fn populate_reachability(&mut self) {
    let mut reachable_blocks = HashSet::new();
    let mut queue = VecDeque::new();
    queue.push_back(self.blocks.front().unwrap().label.clone());
    while let Some(label) = queue.pop_front() {
      if reachable_blocks.contains(&label) {
        continue;
      }
      reachable_blocks.insert(label.clone());
      let (s1, s2) = self.get_successors(&label);
      if let Some(s1) = s1 {
        queue.push_back(s1);
      }
      if let Some(s2) = s2 {
        queue.push_back(s2);
      }
    }
    self.reachable_blocks = reachable_blocks;
  }

  // fn add_basic_blocks(&mut self, blocks: &BasicBlocks) {
  //   match blocks {
  //     BasicBlocks::Regular(bbs) => {
  //       for block in bbs.iter() {
  //         self.add_single_block(block);
  //       }
  //     }
  //     BasicBlocks::Loop(init, predicate, body) => {
  //       if let Some(init) = init {
  //         self.add_single_block(init);
  //       }
  //       if let Some(predicate) = predicate {
  //         self.add_single_block(predicate);
  //       }
  //       for b in body.iter() {
  //         self.add_basic_blocks(b);
  //       }
  //     }
  //   }
  // }

  fn update_mapping(
    &self,
    block: &BasicBlock,
    successors: &mut HashMap<InstrLabel, (Option<InstrLabel>, Option<InstrLabel>)>,
    predecessors: &mut HashMap<InstrLabel, HashSet<InstrLabel>>,
  ) {
    let mut set = false;

    if let Some(s1) = block.successor_1.clone() {
      set = true;

      // setup successors
      if let Some(s) = successors.get_mut(&block.label) {
        s.0 = Some(s1.clone());
      } else {
        successors.insert(block.label.clone(), (Some(s1.clone()), None));
      }
      // setup predecessors
      if let Some(p) = predecessors.get_mut(&s1) {
        p.insert(block.label.clone());
      } else {
        predecessors.insert(s1, HashSet::from_iter([block.label.clone()]));
      }
    }

    if let Some(s2) = block.successor_2.clone() {
      set = true;

      if let Some(s) = successors.get_mut(&block.label) {
        s.1 = Some(s2.clone());
      } else {
        successors.insert(block.label.clone(), (None, Some(s2.clone())));
      }
      if let Some(p) = predecessors.get_mut(&s2) {
        p.insert(block.label.clone());
      } else {
        predecessors.insert(s2, HashSet::from_iter([block.label.clone()]));
      }
    }

    if !set {
      successors.insert(block.label.clone(), (None, None));
    }
  }
}
