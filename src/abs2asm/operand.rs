use enum_as_inner::EnumAsInner;

use super::{asmline::X64Asmline, x86instr::mov};
use crate::registers::{
  consts::{RESERVED_REG_32BIT, RESERVED_REG_64BIT},
  reg::X86_64Register,
};

use std::fmt;

/// Represents the memory location with size, used as [`X64Operand::Memory`]
/// for x86_64 assembly code.
///
/// The string inside takes the following forms...
/// e.g. [%rax], 8(%rsp)
#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct MemLoc(pub String, pub u32);

impl MemLoc {
  /// Creates a new MemLoc from a register and an offset
  /// e.g. %rax, 4 -> 4(%rax)
  pub fn from_reg_offset(reg: X86_64Register, offset: i64, byte_size: u32) -> Self {
    if offset == 0 {
      Self(format!("({})", reg), byte_size)
    } else {
      Self(format!("{}({})", offset, reg), byte_size)
    }
  }
}

impl fmt::Display for MemLoc {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "{}", self.0)
  }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, EnumAsInner)]
pub enum X64Operand {
  Null,
  /// Immediate 32-bit/64-bit value
  Imm(i64),
  /// Register
  Register(X86_64Register),
  /// Memory location
  Memory(MemLoc),
}

impl X64Operand {
  /// Constructs a new operand from a 8-bit register
  pub const fn reg(reg: X86_64Register) -> Self {
    Self::Register(reg)
  }

  /// Constructs a new operand from a memory location
  pub const fn mem(mem: MemLoc) -> Self {
    Self::Memory(mem)
  }

  pub fn to_8bit(&self) -> X64Operand {
    match self {
      X64Operand::Register(reg) => X64Operand::Register(reg.as_8bit()),
      _ => self.clone(),
    }
  }

  pub fn to_32bit(&self) -> X64Operand {
    match self {
      X64Operand::Register(reg) => X64Operand::Register(reg.as_32bit()),
      X64Operand::Memory(mem) => X64Operand::Memory(MemLoc(mem.0.clone(), 4)),
      _ => self.clone(),
    }
  }

  pub fn to_64bit(&self) -> X64Operand {
    match self {
      X64Operand::Register(reg) => X64Operand::Register(reg.as_64bit()),
      X64Operand::Memory(mem) => X64Operand::Memory(MemLoc(mem.0.clone(), 8)),
      _ => self.clone(),
    }
  }

  pub fn with_size(self, byte_size: u32) -> Self {
    match byte_size {
      4 => self.to_32bit(),
      8 => self.to_64bit(),
      _ => panic!("Invalid byte size: {}", byte_size),
    }
  }

  pub fn size_eq(&self, other: &Self) -> bool {
    let left_size = match self {
      X64Operand::Register(reg) => reg.width() / 8,
      X64Operand::Memory(mem) => mem.1 as u8,
      X64Operand::Null => 8,
      _ => 4,
    };
    let right_size = match other {
      X64Operand::Register(reg) => reg.width() / 8,
      X64Operand::Memory(mem) => mem.1 as u8,
      X64Operand::Null => 8,
      _ => 4,
    };
    left_size == right_size
      || matches!(self, X64Operand::Imm(_))
      || matches!(other, X64Operand::Imm(_))
  }

  /// Returns whether this operand could be represented in 32-bit
  pub fn imm_represented_in_32b(&self) -> bool {
    matches!(self, X64Operand::Imm(imm) if (i32::MIN as i64) <= *imm && *imm <= (i32::MAX as i64))
  }

  /// If the operand is an immediate of 2 ** n, returns n
  /// Note that we exclude the zero
  pub fn exponent_of_two(&self) -> Option<Self> {
    match self {
      Self::Imm(x) => {
        if (*x != 0) && ((*x & (*x - 1)) == 0) {
          Some(Self::Imm(x.trailing_zeros() as i64))
        } else {
          None
        }
      }
      _ => None,
    }
  }

  /// If op is an immediate, we ensure it is valid and return the immediate.
  /// Otherwise, we just do nothing and return the operand.
  pub fn ensure_imm_valid(&self, instrs: &mut Vec<X64Asmline>) -> X64Operand {
    match self {
      X64Operand::Imm(_) if !self.imm_represented_in_32b() => {
        let temp_reg = RESERVED_REG_64BIT.clone();
        instrs.push(mov(self, &temp_reg));
        temp_reg
      }
      _ => self.clone(),
    }
  }

  /// Returns whether this operand is valid for 64-bit operation
  /// e.g. movq %rax, %rbx -> rbx is valid, ebx is not
  pub fn valid_for_quad(&self) -> bool {
    match self {
      X64Operand::Register(reg) => reg.is_64bit(),
      X64Operand::Memory(mem) => mem.1 == 8,
      _ => true,
    }
  }

  /// Returns whether this operand is valid for 32-bit operation
  /// e.g. movl %eax, %ebx -> ebx is valid, rbx is not
  pub fn valid_for_long(&self) -> bool {
    match self {
      X64Operand::Register(reg) => reg.is_32bit(),
      X64Operand::Memory(mem) => mem.1 == 4,
      _ => true,
    }
  }

  pub fn zero_extend_mem_if_4byte(&self, instrs: &mut Vec<X64Asmline>) -> X64Operand {
    match self {
      X64Operand::Memory(mem) => {
        if mem.1 == 4 {
          let memloc = X64Operand::Memory(MemLoc(mem.0.clone(), 8));
          let temp_reg = RESERVED_REG_32BIT.clone();
          instrs.push(mov(self, &temp_reg));
          instrs.push(mov(&temp_reg.to_64bit(), &memloc));
          memloc
        } else {
          self.clone()
        }
      }
      _ => self.clone(),
    }
  }
}

impl fmt::Display for X64Operand {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    match self {
      X64Operand::Null => write!(f, "$0"),
      X64Operand::Imm(imm) => write!(f, "${}", *imm),
      X64Operand::Register(reg) => write!(f, "{}", reg),
      X64Operand::Memory(mem) => write!(f, "{}", mem),
    }
  }
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_memloc_string() {
    let memloc = MemLoc::from_reg_offset(X86_64Register::RAX(64), 4, 4);
    assert_eq!(format!("{}", memloc), "4(%rax)");
  }

  #[test]
  fn test_operand_string() {
    assert_eq!(format!("{}", X64Operand::Imm(1)), "$1");
    assert_eq!(
      format!("{}", X64Operand::Register(X86_64Register::RAX(64))),
      "%rax"
    );
    assert_eq!(
      format!("{}", X64Operand::Register(X86_64Register::RAX(32))),
      "%eax"
    );
    assert_eq!(
      format!("{}", X64Operand::Memory(MemLoc("4(%rax)".to_string(), 4))),
      "4(%rax)"
    );
  }

  #[test]
  fn imm_valid_for_long() {
    assert!(X64Operand::Imm(1).valid_for_long());
    assert!(X64Operand::Imm(i32::MIN as i64).valid_for_long());
    assert!(X64Operand::Imm(i32::MAX as i64).valid_for_long());
    assert!(!X64Operand::Imm(i64::MAX).valid_for_long());
    assert!(!X64Operand::Imm(i32::MAX as i64 + 1).valid_for_long());
    assert!(!X64Operand::Imm(i32::MIN as i64 - 1).valid_for_long());
  }

  #[test]
  fn imm_represented_in_32b_test() {
    assert!(X64Operand::Imm(1).imm_represented_in_32b());
    assert!(X64Operand::Imm(i32::MIN as i64).imm_represented_in_32b());
    assert!(X64Operand::Imm(i32::MAX as i64).imm_represented_in_32b());
    assert!(!X64Operand::Imm(i64::MAX).imm_represented_in_32b());
    assert!(!X64Operand::Imm(i64::MIN).imm_represented_in_32b());
    assert!(!X64Operand::Imm(i32::MAX as i64 + 1).imm_represented_in_32b());
    assert!(!X64Operand::Imm(i32::MIN as i64 - 1).imm_represented_in_32b());
  }
}
