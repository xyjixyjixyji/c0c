use crate::{abs2asm::X64Operand, registers::reg::X86_64Register};

// IMPORTANT: The ordering of ALLOC_POOL_REGS_32 and ALLOC_POOL_REGS_64 must be the same.
//
// ====================
// Upon change, please make sure the order is maintained!
// ====================
pub const REG_ALLOC_POOL_SIZE: usize = 13;
pub const ALLOC_POOL_REGS_32: [X86_64Register; REG_ALLOC_POOL_SIZE] = [
  X86_64Register::RAX(32),
  X86_64Register::RDI(32),
  X86_64Register::RSI(32),
  X86_64Register::RDX(32),
  X86_64Register::RCX(32),
  X86_64Register::R8(32),
  X86_64Register::R9(32),
  X86_64Register::R10(32),
  X86_64Register::RBX(32),
  X86_64Register::R12(32),
  X86_64Register::R13(32),
  X86_64Register::R14(32),
  X86_64Register::R15(32),
];

pub const ALLOC_POOL_REGS_64: [X86_64Register; REG_ALLOC_POOL_SIZE] = [
  X86_64Register::RAX(64),
  X86_64Register::RDI(64),
  X86_64Register::RSI(64),
  X86_64Register::RDX(64),
  X86_64Register::RCX(64),
  X86_64Register::R8(64),
  X86_64Register::R9(64),
  X86_64Register::R10(64),
  X86_64Register::RBX(64),
  X86_64Register::R12(64),
  X86_64Register::R13(64),
  X86_64Register::R14(64),
  X86_64Register::R15(64),
];

pub const RESERVED_REG_32BIT: &X64Operand = &X64Operand::reg(X86_64Register::R11(32));
pub const RESERVED_REG_64BIT: &X64Operand = &X64Operand::reg(X86_64Register::R11(64));
/// Used in write_mem and lea
pub const RESERVED_REG2_32BIT: &X64Operand = &X64Operand::reg(X86_64Register::R10(32));
pub const RESERVED_REG2_64BIT: &X64Operand = &X64Operand::reg(X86_64Register::R10(64));
/// Used in lea
pub const RESERVED_REG3_64BIT: &X64Operand = &X64Operand::reg(X86_64Register::R9(64));

// 0b11111
pub const VALID_SHIFT_MASK: &X64Operand = &X64Operand::Imm(0b11111u32 as i64);

pub const IMM_ZERO: &X64Operand = &X64Operand::Imm(0);

pub const STACK_ELEM_SIZE: u32 = 8;
