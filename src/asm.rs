// L3 Compiler
//! Abstract Assembly Type (Triples)

use crate::{
  ast, elaboration::LIBRARY_RAISE, regalloc::Node, registers::reg::X86_64Register, utils::is_macos,
};
use std::{
  fmt::{Debug, Display, Error, Formatter},
  vec,
};

use enum_as_inner::EnumAsInner;

/// Argument (can be either a temp or a constant.)
#[derive(Debug, Copy, Clone, PartialEq, Eq, EnumAsInner)]
pub enum Operand {
  Null,
  Const(i64),
  Temp(Dest),
}

impl Operand {
  /// If the operand is an immediate of 2 ** n, returns n
  /// Note that we exclude the zero
  pub fn exponent_of_two(&self) -> Option<Self> {
    match self {
      Self::Const(x) => {
        if (*x != 0) && ((*x & (*x - 1)) == 0) {
          Some(Self::Const(x.trailing_zeros() as i64))
        } else {
          None
        }
      }
      _ => None,
    }
  }
}

/// Destination (Temp number with temp size)
#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct Dest(pub u32, pub u32);

/// Label in the abstract assembly, may contain loop information
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct InstrLabel {
  label_name: String,
  loop_id: Option<u32>,
  loop_state: Option<LoopState>,
}

impl InstrLabel {
  pub fn new(label_name: String) -> Self {
    Self {
      label_name,
      loop_id: None,
      loop_state: None,
    }
  }

  pub fn with_loop_id(mut self, loop_id: Option<u32>) -> Self {
    self.loop_id = loop_id;
    self
  }

  pub fn with_loop_state(mut self, loop_state: Option<LoopState>) -> Self {
    self.loop_state = loop_state;
    self
  }

  pub fn label_name(&self) -> &str {
    &self.label_name
  }

  pub fn loop_id(&self) -> Option<u32> {
    self.loop_id
  }

  pub fn loop_state(&self) -> Option<LoopState> {
    self.loop_state
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub enum Cond {
  Simp(Operand),
  BinOp(Operand, ast::BinOp, Operand),
}

/// Special marker in IR representing loop init, predicate, body, and end.
#[derive(Clone, Debug, PartialEq, Eq, Copy, Hash)]
pub enum LoopState {
  Init,
  Predicate,
  Body,
  End,
}

/// Abstract Assembly Instruction
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Instr {
  BinOp {
    op: ast::BinOp,
    dest: Dest,
    src1: Operand,
    src2: Operand,
  },
  UnOp {
    op: ast::UnOp,
    dest: Dest,
    src: Operand,
  },
  Mov {
    dest: Dest,
    src: Operand,
  },
  CondJmp {
    src: Cond,
    target_true: InstrLabel,
    target_false: InstrLabel,
  },
  Jmp {
    target: InstrLabel,
  },
  Label {
    name: InstrLabel,
  },
  Return(Option<Operand>),
  Call {
    name: String,
    args: Vec<Operand>,
    dest: Option<Dest>,
  },
  ReadMem {
    dest: Dest,
    read_addr: Dest,
  },
  WriteMem {
    dest_addr: Dest,
    src: Operand,
  },
  // Load Effective Address for easier array access.
  // step_size must be a constant.
  Lea {
    dest: Dest,
    base: Operand,
    index: Option<Operand>,
    elem_size: Option<Operand>,
  },
  Nop,
}

impl Instr {
  pub fn mem_error() -> Self {
    let sigusr2 = if is_macos() { 31 } else { 12 };
    Instr::Call {
      name: LIBRARY_RAISE.to_string(),
      args: vec![Operand::Const(sigusr2)],
      dest: None,
    }
  }

  pub fn neccessary(&self) -> Vec<Node> {
    let necessary = match self {
      Instr::BinOp { op, src1, src2, .. } => match op {
        ast::BinOp::Div | ast::BinOp::Mod | ast::BinOp::Shl | ast::BinOp::Shr => {
          vec![*src1, *src2]
        }
        _ => vec![],
      },
      Instr::CondJmp { src, .. } => match src {
        Cond::Simp(s) => vec![*s],
        Cond::BinOp(s1, _, s2) => vec![*s1, *s2],
      },
      Instr::Return(t) => t.map_or(vec![], |t| vec![t]),
      Instr::ReadMem { read_addr, .. } => vec![Operand::Temp(*read_addr)],
      Instr::WriteMem { dest_addr, src } => vec![*src, Operand::Temp(*dest_addr)],
      Instr::Call { args, .. } => args.clone(),
      _ => vec![],
    };
    necessary
      .into_iter()
      .filter(|s| matches!(s, Operand::Temp(_)))
      .map(|s| *s.as_temp().unwrap())
      .map(|d| Node::Temp(d.0))
      .collect()
  }

  pub fn used(&self) -> Vec<Node> {
    let sources = match *self {
      Instr::BinOp { src1, src2, .. } => vec![src1, src2],
      Instr::UnOp { src, .. } => vec![src],
      Instr::Mov { src, .. } => vec![src],
      Instr::Return(src) => src.map_or(vec![], |src| vec![src]),
      Instr::CondJmp { src, .. } => match src {
        Cond::Simp(s) => vec![s],
        Cond::BinOp(s1, _, s2) => vec![s1, s2],
      },
      Instr::Call { ref args, .. } => args.clone(),
      Instr::ReadMem { read_addr: src, .. } => vec![Operand::Temp(src)],
      Instr::WriteMem { src, dest_addr } => vec![src, Operand::Temp(dest_addr)],
      Instr::Lea {
        base,
        index,
        elem_size,
        ..
      } => {
        let mut v = vec![base];
        if let Some(s) = elem_size {
          v.push(s);
        }
        if let Some(m) = index {
          v.push(m);
        }
        v
      }
      _ => vec![],
    };

    sources
      .into_iter()
      .filter(|s| matches!(s, Operand::Temp(_)))
      .map(|s| *s.as_temp().unwrap())
      .map(|d| Node::Temp(d.0))
      .collect()
  }

  pub fn defined(&self) -> Vec<Node> {
    match *self {
      Instr::BinOp { dest, .. } => vec![Node::Temp(dest.0)],
      Instr::UnOp { dest, .. } => vec![Node::Temp(dest.0)],
      Instr::Mov { dest, .. } => vec![Node::Temp(dest.0)],
      Instr::Call { dest, .. } => {
        // call defines all caller saved registers and dest
        let mut v = dest.map_or(vec![], |d| vec![Node::Temp(d.0)]);
        v.extend(X86_64Register::caller_saved().into_iter().map(Node::Reg));
        v
      }
      Instr::Lea { dest, .. } => vec![Node::Temp(dest.0)],
      Instr::ReadMem { dest, .. } => vec![Node::Temp(dest.0)],
      _ => vec![],
    }
  }

  pub fn rewrite_temp(&mut self, from: &Dest, to: &Dest) {
    match self {
      Instr::BinOp {
        dest, src1, src2, ..
      } => {
        if let Operand::Temp(d) = src1 {
          if d == from {
            *src1 = Operand::Temp(*to);
          }
        }
        if let Operand::Temp(d) = src2 {
          if d == from {
            *src2 = Operand::Temp(*to);
          }
        }
        if dest == from {
          *dest = *to;
        }
      }
      Instr::UnOp { dest, src, .. } => {
        if let Operand::Temp(d) = src {
          if d == from {
            *src = Operand::Temp(*to);
          }
        }
        if dest == from {
          *dest = *to;
        }
      }
      Instr::Mov { dest, src } => {
        if let Operand::Temp(d) = src {
          if d == from {
            *src = Operand::Temp(*to);
          }
        }
        if dest == from {
          *dest = *to;
        }
      }
      Instr::CondJmp { src, .. } => match src {
        Cond::Simp(s) => {
          if let Operand::Temp(d) = s {
            if d == from {
              *src = Cond::Simp(Operand::Temp(*to));
            }
          }
        }
        Cond::BinOp(s1, _, s2) => {
          if let Operand::Temp(d) = s1 {
            if d == from {
              *s1 = Operand::Temp(*to);
            }
          }
          if let Operand::Temp(d) = s2 {
            if d == from {
              *s2 = Operand::Temp(*to);
            }
          }
        }
      },
      Instr::Return(op) => {
        if let Some(Operand::Temp(d)) = op {
          if d == from {
            *op = Some(Operand::Temp(*to));
          }
        }
      }
      Instr::Call { args, dest, .. } => {
        if let Some(d) = dest {
          if d == from {
            *dest = Some(*to);
          }
        }
        for arg in args {
          if let Operand::Temp(d) = arg {
            if d == from {
              *arg = Operand::Temp(*to);
            }
          }
        }
      }
      Instr::ReadMem { dest, read_addr } => {
        if dest == from {
          *dest = *to;
        }
        if read_addr == from {
          *read_addr = *to;
        }
      }
      Instr::WriteMem { dest_addr, src } => {
        if let Operand::Temp(d) = src {
          if d == from {
            *src = Operand::Temp(*to);
          }
        }
        if dest_addr == from {
          *dest_addr = *to;
        }
      }
      Instr::Lea {
        dest,
        base,
        index,
        elem_size,
      } => {
        if let Operand::Temp(d) = base {
          if d == from {
            *base = Operand::Temp(*to);
          }
        }
        if let Some(Operand::Temp(d)) = index {
          if d == from {
            *index = Some(Operand::Temp(*to));
          }
        }
        if let Some(Operand::Temp(d)) = elem_size {
          if d == from {
            *elem_size = Some(Operand::Temp(*to));
          }
        }
        if dest == from {
          *dest = *to;
        }
      }
      _ => {}
    }
  }
}

impl Dest {
  pub fn with_size(&self, size: u32) -> Dest {
    Dest(self.0, size)
  }
}

impl Operand {
  pub fn get_size_bytes(&self) -> u32 {
    match self {
      Operand::Const(_) => 4,
      Operand::Null => 8,
      Operand::Temp(d) => d.1,
    }
  }
}

impl Display for InstrLabel {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "{}", self.label_name)
  }
}

impl Display for Dest {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "T{}", self.0)
  }
}

impl Display for Operand {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use Operand::*;
    match *self {
      Null => write!(fmt, "NULL"),
      Const(n) => write!(fmt, "${}", n),
      Temp(s) => write!(fmt, "{}", s),
    }
  }
}

impl Display for Cond {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    use Cond::*;
    match self {
      Simp(op) => write!(fmt, "{}", op),
      BinOp(op1, binop, op2) => write!(fmt, "{} {} {}", op1, binop, op2),
    }
  }
}

impl Display for Instr {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match self {
      Instr::BinOp {
        op,
        dest,
        src1,
        src2,
      } => write!(fmt, "\t{} <- {} {} {}", dest, src1, op, src2),
      Instr::UnOp { op, dest, src } => write!(fmt, "\t{} <- {}{}", dest, op, src),
      Instr::Mov { dest, src } => write!(fmt, "\t{} <- {}", dest, src),
      Instr::Return(dest) => {
        if let Some(dest) = dest {
          write!(fmt, "\tRET {}", dest)
        } else {
          write!(fmt, "\tRET")
        }
      }
      Instr::Label { name } => write!(fmt, "{}", name),
      Instr::Jmp { target } => write!(fmt, "\tgoto l{}", target),
      Instr::CondJmp {
        src,
        target_true: label_true,
        target_false: label_false,
      } => {
        write!(
          fmt,
          "\tIF {} THEN l{} ELSE l{}",
          src, label_true, label_false
        )
      }
      Instr::Call { name, args, dest } => {
        if let Some(dest) = dest {
          write!(fmt, "\t{} <- CALL {}(", dest, name)?;
        } else {
          write!(fmt, "\tCALL {}(", name)?;
        }

        for (i, arg) in args.iter().enumerate() {
          if i > 0 {
            write!(fmt, ", ")?;
          }
          write!(fmt, "{}", arg)?;
        }
        write!(fmt, ")")
      }
      Instr::ReadMem {
        dest,
        read_addr: src,
      } => write!(fmt, "\t{} <- *{}", dest, src),
      Instr::WriteMem {
        dest_addr: dest,
        src,
      } => write!(fmt, "\t*{} <- {}", dest, src),
      Instr::Lea {
        dest,
        base,
        index,
        elem_size,
      } => {
        write!(
          fmt,
          "\t{} <- LEA {}{}",
          dest,
          base,
          elem_size.map_or("".to_string(), |s| format!(", {}", s))
        )?;
        if let Some(m) = index {
          write!(fmt, ", {}", m)?;
        }
        Ok(())
      }
      Instr::Nop => Ok(()),
    }
  }
}
