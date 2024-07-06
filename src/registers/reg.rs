use std::fmt;

use crate::abs2asm::X64Operand;

/// All registers for x86_64
#[allow(clippy::upper_case_acronyms)]
#[derive(Eq, PartialEq, Debug, Copy, Clone, Hash)]
pub enum X86_64Register {
  RAX(u8),
  RBX(u8),
  RCX(u8),
  RDX(u8),
  RSI(u8),
  RDI(u8),
  RBP(u8),
  RSP(u8),
  R8(u8),
  R9(u8),
  R10(u8),
  R11(u8),
  R12(u8),
  R13(u8),
  R14(u8),
  R15(u8),
}

impl X86_64Register {
  pub const fn argument_regs(width: u8) -> [X86_64Register; 6] {
    [
      X86_64Register::RDI(width),
      X86_64Register::RSI(width),
      X86_64Register::RDX(width),
      X86_64Register::RCX(width),
      X86_64Register::R8(width),
      X86_64Register::R9(width),
    ]
  }

  pub const fn ith_argument(i: u8, width: u8) -> Self {
    match i {
      0 => X86_64Register::RDI(width),
      1 => X86_64Register::RSI(width),
      2 => X86_64Register::RDX(width),
      3 => X86_64Register::RCX(width),
      4 => X86_64Register::R8(width),
      5 => X86_64Register::R9(width),
      _ => panic!("Invalid argument index"),
    }
  }

  pub fn caller_saved() -> Vec<Self> {
    vec![
      X86_64Register::RAX(64),
      X86_64Register::RDI(64),
      X86_64Register::RSI(64),
      X86_64Register::RDX(64),
      X86_64Register::RCX(64),
      X86_64Register::R8(64),
      X86_64Register::R9(64),
      X86_64Register::R10(64),
    ]
  }

  pub const fn is_caller_saved(&self) -> bool {
    matches!(
      self,
      X86_64Register::RAX(_)
        | X86_64Register::RDI(_)
        | X86_64Register::RSI(_)
        | X86_64Register::RDX(_)
        | X86_64Register::RCX(_)
        | X86_64Register::R8(_)
        | X86_64Register::R9(_)
        | X86_64Register::R10(_)
    )
  }

  pub const fn is_callee_saved(&self) -> bool {
    matches!(
      self,
      X86_64Register::RBX(_)
        | X86_64Register::RBP(_)
        | X86_64Register::R12(_)
        | X86_64Register::R13(_)
        | X86_64Register::R14(_)
        | X86_64Register::R15(_)
    )
  }

  /// Convert to 64-bit caller-saved registers
  pub const fn as_caller_saved(&self) -> X86_64Register {
    match self {
      X86_64Register::RAX(_) => X86_64Register::RAX(64),
      X86_64Register::RDI(_) => X86_64Register::RDI(64),
      X86_64Register::RSI(_) => X86_64Register::RSI(64),
      X86_64Register::RDX(_) => X86_64Register::RDX(64),
      X86_64Register::RCX(_) => X86_64Register::RCX(64),
      X86_64Register::R8(_) => X86_64Register::R8(64),
      X86_64Register::R9(_) => X86_64Register::R9(64),
      X86_64Register::R10(_) => X86_64Register::R10(64),
      _ => panic!("Invalid caller-saved register"),
    }
  }

  pub const fn as_callee_saved(&self) -> X86_64Register {
    match self {
      X86_64Register::RBX(_) => X86_64Register::RBX(64),
      X86_64Register::RBP(_) => X86_64Register::RBP(64),
      X86_64Register::R12(_) => X86_64Register::R12(64),
      X86_64Register::R13(_) => X86_64Register::R13(64),
      X86_64Register::R14(_) => X86_64Register::R14(64),
      X86_64Register::R15(_) => X86_64Register::R15(64),
      _ => panic!("Invalid callee-saved register"),
    }
  }

  pub const fn width(&self) -> u8 {
    match self {
      X86_64Register::RAX(w) => *w,
      X86_64Register::RBX(w) => *w,
      X86_64Register::RCX(w) => *w,
      X86_64Register::RDX(w) => *w,
      X86_64Register::RSI(w) => *w,
      X86_64Register::RDI(w) => *w,
      X86_64Register::RBP(w) => *w,
      X86_64Register::RSP(w) => *w,
      X86_64Register::R8(w) => *w,
      X86_64Register::R9(w) => *w,
      X86_64Register::R10(w) => *w,
      X86_64Register::R11(w) => *w,
      X86_64Register::R12(w) => *w,
      X86_64Register::R13(w) => *w,
      X86_64Register::R14(w) => *w,
      X86_64Register::R15(w) => *w,
    }
  }
}

impl From<X86_64Register> for X64Operand {
  fn from(reg: X86_64Register) -> Self {
    match reg.width() {
      8 => X64Operand::reg(reg),
      16 => X64Operand::reg(reg),
      32 => X64Operand::reg(reg),
      64 => X64Operand::reg(reg),
      _ => panic!("Invalid width"),
    }
  }
}

impl fmt::Display for X86_64Register {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      X86_64Register::RAX(_) => match self.width() {
        8 => write!(f, "%al"),
        16 => write!(f, "%ax"),
        32 => write!(f, "%eax"),
        64 => write!(f, "%rax"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::RBX(_) => match self.width() {
        8 => write!(f, "%bl"),
        16 => write!(f, "%bx"),
        32 => write!(f, "%ebx"),
        64 => write!(f, "%rbx"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::RCX(_) => match self.width() {
        8 => write!(f, "%cl"),
        16 => write!(f, "%cx"),
        32 => write!(f, "%ecx"),
        64 => write!(f, "%rcx"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::RDX(_) => match self.width() {
        8 => write!(f, "%dl"),
        16 => write!(f, "%dx"),
        32 => write!(f, "%edx"),
        64 => write!(f, "%rdx"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::RSI(_) => match self.width() {
        8 => write!(f, "%sil"),
        16 => write!(f, "%si"),
        32 => write!(f, "%esi"),
        64 => write!(f, "%rsi"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::RDI(_) => match self.width() {
        8 => write!(f, "%dil"),
        16 => write!(f, "%di"),
        32 => write!(f, "%edi"),
        64 => write!(f, "%rdi"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::RBP(_) => match self.width() {
        8 => write!(f, "%bpl"),
        16 => write!(f, "%bp"),
        32 => write!(f, "%ebp"),
        64 => write!(f, "%rbp"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::RSP(_) => match self.width() {
        8 => write!(f, "%spl"),
        16 => write!(f, "%sp"),
        32 => write!(f, "%esp"),
        64 => write!(f, "%rsp"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::R8(_) => match self.width() {
        8 => write!(f, "%r8b"),
        16 => write!(f, "%r8w"),
        32 => write!(f, "%r8d"),
        64 => write!(f, "%r8"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::R9(_) => match self.width() {
        8 => write!(f, "%r9b"),
        16 => write!(f, "%r9w"),
        32 => write!(f, "%r9d"),
        64 => write!(f, "%r9"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::R10(_) => match self.width() {
        8 => write!(f, "%r10b"),
        16 => write!(f, "%r10w"),
        32 => write!(f, "%r10d"),
        64 => write!(f, "%r10"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::R11(_) => match self.width() {
        8 => write!(f, "%r11b"),
        16 => write!(f, "%r11w"),
        32 => write!(f, "%r11d"),
        64 => write!(f, "%r11"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::R12(_) => match self.width() {
        8 => write!(f, "%r12b"),
        16 => write!(f, "%r12w"),
        32 => write!(f, "%r12d"),
        64 => write!(f, "%r12"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::R13(_) => match self.width() {
        8 => write!(f, "%r13b"),
        16 => write!(f, "%r13w"),
        32 => write!(f, "%r13d"),
        64 => write!(f, "%r13"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::R14(_) => match self.width() {
        8 => write!(f, "%r14b"),
        16 => write!(f, "%r14w"),
        32 => write!(f, "%r14d"),
        64 => write!(f, "%r14"),
        _ => panic!("Invalid width"),
      },
      X86_64Register::R15(_) => match self.width() {
        8 => write!(f, "%r15b"),
        16 => write!(f, "%r15w"),
        32 => write!(f, "%r15d"),
        64 => write!(f, "%r15"),
        _ => panic!("Invalid width"),
      },
    }
  }
}
