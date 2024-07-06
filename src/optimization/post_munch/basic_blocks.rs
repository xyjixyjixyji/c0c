use std::collections::VecDeque;

use crate::asm::{Instr, InstrLabel, LoopState};

#[derive(Debug)]
pub struct BasicBlock {
  pub label: InstrLabel,
  pub instructions: Vec<Instr>,
  pub successor_1: Option<InstrLabel>,
  pub successor_2: Option<InstrLabel>,
  pub is_complete: bool,
}

#[derive(Debug)]
pub enum BasicBlocks {
  Regular(Vec<BasicBlock>),
  // init block, predicate block, body blocks (can contain nested loops)
  Loop(
    Option<Box<BasicBlock>>,
    Option<Box<BasicBlock>>,
    Vec<BasicBlocks>,
  ),
}

impl BasicBlocks {
  pub fn new_loop(
    init: Option<BasicBlock>,
    predicate: Option<BasicBlock>,
    body: Vec<BasicBlocks>,
  ) -> Self {
    BasicBlocks::Loop(init.map(Box::new), predicate.map(Box::new), body)
  }
}

impl BasicBlock {
  /// Create a new basic block with the given label
  pub fn new(label: InstrLabel, is_first_block: bool) -> Self {
    let instructions = if is_first_block {
      vec![]
    } else {
      vec![Instr::Label {
        name: label.clone(),
      }]
    };
    BasicBlock {
      label,
      instructions,
      successor_1: None,
      successor_2: None,
      is_complete: false,
    }
  }

  /// Add an instruction to the basic block
  pub fn add_instr(&mut self, instr: &Instr) {
    if self.is_complete {
      return;
    }
    if !matches!(instr, Instr::Label { .. }) {
      self.instructions.push(instr.clone());
    }
    self.is_complete = match instr {
      Instr::Jmp { target } => {
        self.successor_1 = Some(target.clone());
        true
      }
      Instr::CondJmp {
        target_true,
        target_false,
        ..
      } => {
        self.successor_1 = Some(target_true.clone());
        self.successor_2 = Some(target_false.clone());
        true
      }
      Instr::Return(_) => true,
      Instr::Label { name } => {
        self.successor_1 = Some(name.clone());
        true
      }
      _ => false,
    };
  }
}

/// Convert the context into a vector of basic blocks, encapsulating loops and nested loops
pub fn basic_blocks_vec_from_ctx(ctx: &crate::codegen::Context) -> Vec<BasicBlocks> {
  let mut blocks = build_basic_block_vec(ctx);
  build_basic_blocks_vec(&mut blocks, None)
}

/// Helper method to build a vector of single basic block from the context
pub fn build_basic_block_vec(ctx: &crate::codegen::Context) -> VecDeque<BasicBlock> {
  // Initial label is for the first basic block, since we do not add the function label
  // as an instruction to our context
  let initial_label = InstrLabel::new(format!("{}.entry", ctx.self_call_label.label_name()));
  let mut current_block = BasicBlock::new(initial_label, true);
  let mut blocks = VecDeque::new();
  for instr in &ctx.instrs {
    current_block.add_instr(instr);
    if let Instr::Label { name } = instr {
      blocks.push_back(current_block);
      current_block = BasicBlock::new(name.clone(), false);
    }
  }
  current_block.is_complete = true;
  blocks.push_back(current_block);

  blocks
}

fn build_basic_blocks_vec(
  blocks: &mut VecDeque<BasicBlock>,
  cur_loop_id: Option<u32>,
) -> Vec<BasicBlocks> {
  let mut all_blocks = vec![];
  while !blocks.is_empty() {
    let label = blocks.front().unwrap().label.clone();
    if let Some(state) = label.loop_state() {
      match state {
        LoopState::Init => {
          let loop_init = blocks.pop_front();
          let loop_predicate = blocks.pop_front().unwrap();
          let loop_body = build_basic_blocks_vec(blocks, label.loop_id());
          all_blocks.push(BasicBlocks::new_loop(
            loop_init,
            Some(loop_predicate),
            loop_body,
          ));
        }
        LoopState::Predicate => {
          let loop_predicate = blocks.pop_front().unwrap();
          let loop_body = build_basic_blocks_vec(blocks, label.loop_id());
          all_blocks.push(BasicBlocks::new_loop(None, Some(loop_predicate), loop_body));
        }
        LoopState::End => {
          if label.loop_id() == cur_loop_id {
            return all_blocks;
          } else {
            all_blocks.push(build_regular_basic_blocks(blocks, cur_loop_id));
          }
        }
        LoopState::Body => {
          all_blocks.push(build_regular_basic_blocks(blocks, cur_loop_id));
        }
      }
    } else {
      all_blocks.push(build_regular_basic_blocks(blocks, cur_loop_id));
    }
  }
  all_blocks
}

fn build_regular_basic_blocks(
  blocks: &mut VecDeque<BasicBlock>,
  loop_id: Option<u32>,
) -> BasicBlocks {
  let mut cur_blocks = vec![];
  while !blocks.is_empty() {
    let label = blocks.front().unwrap().label.clone();
    if matches!(
      label.loop_state(),
      Some(LoopState::Init) | Some(LoopState::Predicate)
    ) || (matches!(label.loop_state(), Some(LoopState::End))
      && label.loop_id().is_some()
      && loop_id.is_some()
      && label.loop_id().unwrap() == loop_id.unwrap())
    {
      break;
    }
    cur_blocks.push(blocks.pop_front().unwrap());
  }
  BasicBlocks::Regular(cur_blocks)
}

impl BasicBlock {
  pub fn display(&self, padding: &str) -> String {
    let mut res = format!("{}Block: {} ", padding, self.label);
    if let Some(s1) = &self.successor_1 {
      res.push_str(&format!("{}Successor 1: {} ", padding, s1));
    }
    if let Some(s2) = &self.successor_2 {
      res.push_str(&format!("{}Successor 2: {}", padding, s2));
    }
    res.push('\n');
    res
  }
}

impl BasicBlocks {
  pub fn display(&self, padding: &str) {
    match self {
      BasicBlocks::Regular(bbs) => {
        println!("{}Regular:", padding);
        for bb in bbs {
          println!("{}", bb.display(padding));
        }
      }
      BasicBlocks::Loop(init, predicate, body) => {
        println!("{}Loop:", padding);
        if let Some(init) = init {
          println!("{}Init:\n{}", padding, init.display(padding));
        }
        if let Some(predicate) = predicate {
          println!("{}Predicate:\n{}", padding, predicate.display(padding));
        }
        println!("{}Body:", padding);
        for bb in body {
          bb.display(&format!("\t{}", padding));
        }
      }
    }
  }
}
