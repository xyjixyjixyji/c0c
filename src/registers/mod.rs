pub(crate) mod consts;
mod macros;
pub(crate) mod reg;

#[allow(dead_code)]
pub fn reg_from_color(color: u32, width: u32) -> reg::X86_64Register {
  match width {
    32 => consts::ALLOC_POOL_REGS_32[color as usize],
    64 => consts::ALLOC_POOL_REGS_64[color as usize],
    _ => panic!("Invalid width"),
  }
}

pub fn color_from_reg(reg: reg::X86_64Register) -> u32 {
  match reg.width() {
    32 => consts::ALLOC_POOL_REGS_32
      .iter()
      .position(|&x| x == reg)
      .unwrap() as u32,
    64 => consts::ALLOC_POOL_REGS_64
      .iter()
      .position(|&x| x == reg)
      .unwrap() as u32,
    _ => panic!("Invalid register"),
  }
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::registers::consts::{RESERVED_REG2_64BIT, RESERVED_REG3_64BIT};

  #[test]
  fn test_reg_index_order() {
    for i in 0..consts::REG_ALLOC_POOL_SIZE as u32 {
      assert_eq!(color_from_reg(reg_from_color(i, 32)), i);
      assert_eq!(color_from_reg(reg_from_color(i, 64)), i);
    }
  }

  #[test]
  fn reserved2_and_reserved3_are_caller_saved() {
    assert!(RESERVED_REG2_64BIT.as_register().unwrap().is_caller_saved());
    assert!(RESERVED_REG3_64BIT.as_register().unwrap().is_caller_saved());
  }
}
