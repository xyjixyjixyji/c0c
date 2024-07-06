use std::convert::TryInto;

use crate::{
  abs2asm::{asmline::X64Asmline, X64Operand},
  asm::InstrLabel,
  ast::BinOp,
  registers::{
    consts::{
      IMM_ZERO, RESERVED_REG2_32BIT, RESERVED_REG2_64BIT, RESERVED_REG3_64BIT, RESERVED_REG_32BIT,
      RESERVED_REG_64BIT, VALID_SHIFT_MASK,
    },
    reg::X86_64Register,
  },
  utils::is_unsafe,
  OPTIM_OPTIONS,
};

use super::MemLoc;

// ====================================================
// ====================================================
// =                   Primitives                     =
// ====================================================
// ====================================================

fn get_instr_byte_width(src: &X64Operand, dst: &X64Operand) -> u32 {
  let src_size = match src {
    X64Operand::Register(reg) => reg.width() as u32 / 8,
    X64Operand::Memory(mem) => mem.1,
    X64Operand::Null => 8,
    X64Operand::Imm(_) => 4,
  };
  let dst_size = match dst {
    X64Operand::Register(reg) => reg.width() as u32 / 8,
    X64Operand::Memory(mem) => mem.1,
    X64Operand::Null => 8,
    X64Operand::Imm(_) => 4,
  };
  src_size.max(dst_size)
}

pub fn cmp(src1: &X64Operand, src2: &X64Operand) -> X64Asmline {
  assert!(src1.size_eq(src2));

  let inst_width = get_instr_byte_width(src1, src2);
  let src1 = src1.clone().with_size(inst_width);
  let src2 = src2.clone().with_size(inst_width);
  if inst_width == 8 {
    X64Asmline::cmpq(&src1, &src2)
  } else {
    X64Asmline::cmpl(&src1, &src2)
  }
}

pub fn mov(src: &X64Operand, dst: &X64Operand) -> X64Asmline {
  assert!(src.size_eq(dst));

  let inst_width = get_instr_byte_width(src, dst);
  let src = src.clone().with_size(inst_width);
  let dst = dst.clone().with_size(inst_width);
  if inst_width == 8 {
    X64Asmline::movq(&src, &dst)
  } else {
    X64Asmline::movl(&src, &dst)
  }
}

pub(crate) fn add(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  let mut instrs = vec![];

  let instr_width = get_instr_byte_width(src, dst);
  let add_f = if instr_width == 8 {
    X64Asmline::addq
  } else {
    X64Asmline::addl
  };

  // there are atmost 1 imm in src and src2, so r11 will not be overwritten
  let dst = dst.clone().with_size(instr_width);
  let src = src
    .clone()
    .with_size(instr_width)
    .ensure_imm_valid(&mut instrs);
  let src2 = src2
    .clone()
    .with_size(instr_width)
    .ensure_imm_valid(&mut instrs);

  if src2 == dst {
    instrs.push(add_f(&src, &dst))
  } else {
    instrs.push(add_f(&src, &src2));
    instrs.push(mov(&src2, &dst));
  }

  instrs
}

pub(crate) fn sub(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  let mut instrs = vec![];

  let instr_width = get_instr_byte_width(src, dst);

  // there are atmost 1 imm in src and src2, so r11 will not be overwritten
  let dst = dst.clone().with_size(instr_width);
  let src = src
    .clone()
    .with_size(instr_width)
    .ensure_imm_valid(&mut instrs);
  let src2 = src2
    .clone()
    .with_size(instr_width)
    .ensure_imm_valid(&mut instrs);

  let sub_f = if instr_width == 8 {
    X64Asmline::subq
  } else {
    X64Asmline::subl
  };

  if src2 == dst {
    instrs.push(sub_f(&src, &dst));
  } else {
    instrs.push(sub_f(&src, &src2));
    instrs.push(mov(&src2, &dst));
  }

  instrs
}

// helper method for processing imull3 speial case
fn imul_const(
  dst: &X64Operand,
  src1: &X64Operand,
  src2: &X64Operand,
  instr_width: u32,
) -> Vec<X64Asmline> {
  assert!(src1.is_imm());

  let imul3_f = if instr_width == 8 {
    X64Asmline::imulq3
  } else {
    X64Asmline::imull3
  };

  let reserved_reg = if instr_width == 8 {
    RESERVED_REG_64BIT.clone()
  } else {
    RESERVED_REG_32BIT.clone()
  };

  if dst.is_register() {
    if src2.is_imm() || src2 == dst {
      return vec![mov(src2, &reserved_reg), imul3_f(src1, &reserved_reg, dst)];
    }
    return vec![imul3_f(src1, src2, dst)];
  }
  if src2.is_imm() {
    return vec![
      mov(src2, dst),
      imul3_f(src1, dst, &reserved_reg),
      mov(&reserved_reg, dst),
    ];
  }
  vec![imul3_f(src1, src2, &reserved_reg), mov(&reserved_reg, dst)]
}

pub(crate) fn imul(dst: &X64Operand, src1: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  let mut instrs = vec![];

  let instr_width = get_instr_byte_width(src1, dst);
  let reserved_reg = if instr_width == 8 {
    RESERVED_REG_64BIT.clone()
  } else {
    RESERVED_REG_32BIT.clone()
  };
  let imul_f = if instr_width == 8 {
    X64Asmline::imulq2
  } else {
    X64Asmline::imull2
  };

  // special case: use imull_const when at least one of the operands is an immediate
  if src1.is_imm() {
    instrs.extend(imul_const(dst, src1, src2, instr_width));
    return instrs;
  } else if src2.is_imm() {
    instrs.extend(imul_const(dst, src2, src1, instr_width));
    return instrs;
  }

  // swap operands order if right_src is the same as dst to save a move
  let (left_src, right_src) = if src2 == dst {
    (src2, src1)
  } else {
    (src1, src2)
  };

  // use imull2 when none of src1 and src2 is an immediate
  instrs.extend(
    mov_operand_to_operand(left_src, dst)
      .into_iter()
      .chain(if !dst.is_register() {
        vec![
          mov(dst, &reserved_reg),
          imul_f(right_src, &reserved_reg),
          mov(&reserved_reg, dst),
        ]
      } else {
        vec![imul_f(right_src, dst)]
      }),
  );

  instrs
}

pub(crate) fn idiv(dst: &X64Operand, src1: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  let mut lines = vec![];
  let mut src2 = src2;

  // a / (2 ** n) -> a >> n
  if OPTIM_OPTIONS.lock().unwrap().strength_reduction {
    if let Some(exp) = src2.exponent_of_two() {
      return div_to_shr(dst, src1, &exp);
    }

    if src2.is_imm() {
      let imm = *src2.as_imm().unwrap();
      if (imm > 2 && imm < i32::MAX as i64) || (imm < -2 && imm > i32::MIN as i64) {
        return const_div_to_mul(dst, src1, imm as i32);
      }
    }
  }

  if src2.is_imm() {
    lines.push(mov(src2, RESERVED_REG_32BIT));
    src2 = RESERVED_REG_32BIT;
  }
  lines.extend(vec![
    mov(src1, &X64Operand::reg(X86_64Register::RAX(32))),
    X64Asmline::cdq(),
    X64Asmline::idivl(src2),
    mov(&X64Operand::reg(X86_64Register::RAX(32)), dst),
  ]);
  lines
}

pub(crate) fn imod(dst: &X64Operand, src1: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  let mut lines = vec![];
  let mut src2 = src2;

  if OPTIM_OPTIONS.lock().unwrap().strength_reduction {
    if let Some(exp) = src2.exponent_of_two() {
      return mod_to_shr(dst, src1, &exp);
    }
  }

  if src2.is_imm() {
    lines.push(mov(src2, RESERVED_REG_32BIT));
    src2 = RESERVED_REG_32BIT;
  }
  lines.append(&mut vec![
    mov(src1, &X64Operand::reg(X86_64Register::RAX(32))),
    X64Asmline::cdq(),
    X64Asmline::idivl(src2),
    mov(&X64Operand::reg(X86_64Register::RDX(32)), dst),
  ]);
  lines
}

/// If src is less than src2, set dst to 1, else 0
pub(crate) fn less_than(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  vec![cmp(src, src2), mov(IMM_ZERO, dst), X64Asmline::setl(dst)]
}

/// If src is less than or equal to src2, set dst to 1, else 0
pub(crate) fn leq(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  vec![cmp(src, src2), mov(IMM_ZERO, dst), X64Asmline::setle(dst)]
}

/// If src is greater than src2, set dst to 1, else 0
pub(crate) fn greater_than(
  dst: &X64Operand,
  src: &X64Operand,
  src2: &X64Operand,
) -> Vec<X64Asmline> {
  vec![cmp(src, src2), mov(IMM_ZERO, dst), X64Asmline::setg(dst)]
}

/// If src is greater than or equal to src2, set dst to 1, else 0
pub(crate) fn geq(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  vec![cmp(src, src2), mov(IMM_ZERO, dst), X64Asmline::setge(dst)]
}

/// If src is equal to src2, set dst to 1, else 0
pub(crate) fn eqeq(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  vec![cmp(src, src2), mov(IMM_ZERO, dst), X64Asmline::sete(dst)]
}

pub(crate) fn uneq(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  vec![cmp(src, src2), mov(IMM_ZERO, dst), X64Asmline::setne(dst)]
}

pub(crate) fn logical_and(
  dst: &X64Operand,
  src: &X64Operand,
  src2: &X64Operand,
) -> Vec<X64Asmline> {
  if src2 == dst {
    vec![X64Asmline::andl(src, src2)]
  } else {
    vec![X64Asmline::andl(src, src2), X64Asmline::movl(src2, dst)]
  }
}

pub(crate) fn logical_or(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  if src2 == dst {
    vec![X64Asmline::orl(src, src2)]
  } else {
    vec![X64Asmline::orl(src, src2), X64Asmline::movl(src2, dst)]
  }
}

pub(crate) fn bit_and(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  if src2 == dst {
    vec![X64Asmline::andl(src, src2)]
  } else {
    vec![X64Asmline::andl(src, src2), X64Asmline::movl(src2, dst)]
  }
}

pub(crate) fn bit_xor(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  if src2 == dst {
    vec![X64Asmline::xorl(src, src2)]
  } else {
    vec![X64Asmline::xorl(src, src2), X64Asmline::movl(src2, dst)]
  }
}

pub(crate) fn bit_or(dst: &X64Operand, src: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  if src2 == dst {
    vec![X64Asmline::orl(src, src2)]
  } else {
    vec![X64Asmline::orl(src, src2), X64Asmline::movl(src2, dst)]
  }
}

pub(crate) fn shl(dst: &X64Operand, src1: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  if is_unsafe() {
    vec![
      mov(src1, RESERVED_REG_32BIT),
      mov(src2, &X64Operand::reg(X86_64Register::RCX(32))),
      X64Asmline::sall(&X64Operand::reg(X86_64Register::RCX(8)), RESERVED_REG_32BIT),
      mov(RESERVED_REG_32BIT, dst),
    ]
  } else {
    vec![
      // checks valid range, throw div-by-0 if out of shift range
      mov(VALID_SHIFT_MASK, RESERVED_REG_32BIT),
      X64Asmline::andl(src2, RESERVED_REG_32BIT),
      X64Asmline::cmpl(src2, RESERVED_REG_32BIT),
      X64Asmline::sete(RESERVED_REG_32BIT),
      mov(src2, &X64Operand::reg(X86_64Register::RAX(32))),
      X64Asmline::cdq(),
      X64Asmline::idivl(RESERVED_REG_32BIT),
      mov(src1, RESERVED_REG_32BIT),
      mov(
        &X64Operand::reg(X86_64Register::RAX(32)),
        &X64Operand::reg(X86_64Register::RCX(32)),
      ),
      X64Asmline::sall(&X64Operand::reg(X86_64Register::RCX(8)), RESERVED_REG_32BIT),
      mov(RESERVED_REG_32BIT, dst),
    ]
  }
}

pub(crate) fn shr(dst: &X64Operand, src1: &X64Operand, src2: &X64Operand) -> Vec<X64Asmline> {
  if is_unsafe() {
    vec![
      mov(src1, RESERVED_REG_32BIT),
      mov(src2, &X64Operand::reg(X86_64Register::RCX(32))),
      X64Asmline::sarl(&X64Operand::reg(X86_64Register::RCX(8)), RESERVED_REG_32BIT),
      mov(RESERVED_REG_32BIT, dst),
    ]
  } else {
    vec![
      mov(VALID_SHIFT_MASK, RESERVED_REG_32BIT),
      X64Asmline::andl(src2, RESERVED_REG_32BIT),
      X64Asmline::cmpl(src2, RESERVED_REG_32BIT),
      X64Asmline::sete(RESERVED_REG_32BIT),
      // checks valid range, throw div-by-0 if out of shift range
      mov(src2, &X64Operand::reg(X86_64Register::RAX(32))),
      X64Asmline::cdq(),
      X64Asmline::idivl(RESERVED_REG_32BIT),
      mov(src1, RESERVED_REG_32BIT),
      mov(
        &X64Operand::reg(X86_64Register::RAX(32)),
        &X64Operand::reg(X86_64Register::RCX(32)),
      ),
      X64Asmline::sarl(&X64Operand::reg(X86_64Register::RCX(8)), RESERVED_REG_32BIT),
      mov(RESERVED_REG_32BIT, dst),
    ]
  }
}

// T0 = *T1;
pub(crate) fn read_mem(
  dst: &X64Operand,
  read_addr: &X64Operand,
  dst_size_bytes: u32,
) -> Vec<X64Asmline> {
  let mut instrs = vec![];

  // if source is an address, we get its value first
  // if source is an register, we can directly use it
  let src = match read_addr {
    X64Operand::Register(r) => X64Operand::mem(MemLoc::from_reg_offset(*r, 0, dst_size_bytes)),
    X64Operand::Memory(_) => {
      let val_placeholder = RESERVED_REG_64BIT.clone().with_size(dst_size_bytes);
      instrs.push(mov(read_addr, RESERVED_REG_64BIT));
      let m = MemLoc::from_reg_offset(
        *RESERVED_REG_64BIT.as_register().unwrap(),
        0,
        dst_size_bytes,
      );
      instrs.push(mov(&X64Operand::mem(m), &val_placeholder));
      val_placeholder
    }
    X64Operand::Imm(_) | X64Operand::Null => panic!("Cannot read from an immediate addr"),
  };
  instrs.append(&mut mov_operand_to_operand(&src, dst));

  instrs
}

// *T0 = T1;
pub(crate) fn write_mem(dst_addr: &X64Operand, src: &X64Operand) -> Vec<X64Asmline> {
  let mut instrs = vec![];
  let src_byte_size = get_instr_byte_width(src, IMM_ZERO);
  // write_mem is actually more tricky, when dst and src are both in memory,
  // we need two temporary registers to achieve this.
  let s = match src {
    X64Operand::Register(_) | X64Operand::Imm(_) | X64Operand::Null => src.clone(),
    X64Operand::Memory(_) => {
      let temp_register = RESERVED_REG_64BIT.clone().with_size(src_byte_size);
      instrs.push(mov(src, &temp_register));
      temp_register
    }
  };
  let d = match dst_addr {
    X64Operand::Register(r) => *r,
    X64Operand::Memory(_) => {
      instrs.push(mov(dst_addr, RESERVED_REG2_64BIT));
      *RESERVED_REG2_64BIT.as_register().unwrap()
    }
    X64Operand::Imm(_) | X64Operand::Null => panic!("Cannot write to an immediate addr"),
  };

  let d = X64Operand::mem(MemLoc::from_reg_offset(d, 0, src_byte_size));

  instrs.push(mov(&s, &d));

  instrs
}

/// lea (base, index, scale_factor), dst
/// This operation stores the quantity base + elem_size * scale_factor in dst.
///
/// Note that at the extreme this operation can use up to three temporary registers.
pub(crate) fn lea(
  dst: &X64Operand,
  base: &X64Operand, // Base is a reg/memory
  index: Option<&X64Operand>,
  scale_factor: Option<&X64Operand>, // This is an immediate
  displ: Option<i32>,
) -> Vec<X64Asmline> {
  // in lea, dst, base, elem_size must be registers and index must be an immediate
  let mut instrs = vec![];
  if let Some(scale) = scale_factor {
    assert!(scale.is_imm());
    assert!([1, 2, 4, 8].contains(scale.as_imm().unwrap()));
    assert!(index.is_some());
  }

  // base has to be in a register
  let base = match base {
    X64Operand::Register(r) => *r,
    X64Operand::Memory(_) => {
      instrs.push(mov(base, RESERVED_REG_64BIT));
      *RESERVED_REG_64BIT.as_register().unwrap()
    }
    X64Operand::Imm(_) | X64Operand::Null => panic!("Cannot use an immediate as base"),
  };

  // index has to be in a register
  let index = index.map(|operand| match operand {
    X64Operand::Register(_) => operand.to_64bit(),
    X64Operand::Memory(_) | X64Operand::Imm(_) => {
      instrs.push(mov(&operand.clone().with_size(4), RESERVED_REG2_32BIT));
      RESERVED_REG2_64BIT.clone()
    }
    X64Operand::Null => panic!("Cannot use NULL as index"),
  });

  // dst has to be in a register
  let dst_on_stack = matches!(dst, X64Operand::Memory(_));
  let lea_dst = match dst {
    X64Operand::Register(r) => *r,
    X64Operand::Memory(_) => {
      instrs.push(mov(dst, RESERVED_REG3_64BIT));
      *RESERVED_REG3_64BIT.as_register().unwrap()
    }
    X64Operand::Imm(_) | X64Operand::Null => panic!("Cannot use an immediate as dst"),
  };

  // lea (base, index, elem_size), dst
  instrs.push(X64Asmline::lea(
    &X64Operand::reg(lea_dst),
    &X64Operand::reg(base),
    index.as_ref(),
    scale_factor,
    displ,
  ));

  // move the final address back to stack location
  if dst_on_stack {
    instrs.push(mov(&X64Operand::reg(lea_dst), dst));
  }
  instrs
}

// ====================================================
// ====================================================
// =                   Helpers                        =
// ====================================================
// ====================================================

/// The prologue for each subroutine
///
/// do this IMMEDIATELY when entering the subroutine
pub fn subroutine_prologue(
  stack_size: i64,
  used_callee_regs: &Vec<X86_64Register>,
  has_spilled_args: bool,
) -> Vec<X64Asmline> {
  let mut v = vec![];
  if stack_size > 0 || has_spilled_args {
    // push the stack base pointer
    v.push(X64Asmline::pushq(&X64Operand::reg(X86_64Register::RBP(64))));

    // set the stack base pointer
    v.push(mov(
      &X64Operand::reg(X86_64Register::RSP(64)),
      &X64Operand::reg(X86_64Register::RBP(64)),
    ));
  }

  // making space for the stack
  if stack_size > 0 {
    v.push(X64Asmline::subq(
      &X64Operand::Imm(stack_size),
      &X64Operand::reg(X86_64Register::RSP(64)),
    ));
  }

  // save the callee saved registers THAT WE USED IN THIS SUBROUTINE
  for reg in used_callee_regs {
    v.push(X64Asmline::pushq(&X64Operand::reg(*reg)));
  }

  v
}

/// The epilogue for each subroutine
///
/// do this IMMEDIATELY before ret instruction
pub fn subroutine_epilogue(
  stack_size: i64,
  used_callee_regs: &[X86_64Register],
  has_spilled_args: bool,
) -> Vec<X64Asmline> {
  let mut v = vec![];

  // restore the callee saved registers
  for reg in used_callee_regs.iter().rev() {
    v.push(X64Asmline::popq(&X64Operand::reg(*reg)));
  }

  // restore the stack pointer
  if stack_size > 0 {
    v.push(X64Asmline::addq(
      &X64Operand::Imm(stack_size),
      &X64Operand::reg(X86_64Register::RSP(64)),
    ));
  }

  if stack_size > 0 || has_spilled_args {
    // restore the stack base pointer
    v.push(X64Asmline::popq(&X64Operand::reg(X86_64Register::RBP(64))));
  }

  v
}

/// Move an operand to another operand
///
/// # Example
/// movl %eax, %ebx, can be expressed by
/// mov_operand_to_operand(&X64Operand::reg32(X32Register::EAX), &X64Operand::reg32(X32Register::EBX))
pub(crate) fn mov_operand_to_operand(src: &X64Operand, dst: &X64Operand) -> Vec<X64Asmline> {
  assert!(src.size_eq(dst));
  let instr_size_bytes = get_instr_byte_width(src, dst);
  if src == dst {
    return vec![]; // no self mov
  }

  let mut x64asmlines = vec![];
  if src.is_memory() && dst.is_memory() {
    let reserved_reg = RESERVED_REG_64BIT.clone().with_size(instr_size_bytes);
    x64asmlines.push(mov(src, &reserved_reg));
    x64asmlines.push(mov(&reserved_reg, dst));
  } else {
    x64asmlines.push(mov(src, dst));
  }

  x64asmlines
}

/// Returns a sequence of x86_64 assembly instructions that
/// implements op src1, src2, dst
pub(crate) fn binop_operand_to_operand(
  op: &BinOp,
  src1: &X64Operand,
  src2: &X64Operand,
  dst: &X64Operand,
) -> Vec<X64Asmline> {
  let mut instrs: Vec<X64Asmline> = vec![];

  let (src, src2) = rearrange_src_operands(&mut instrs, op, dst, src1, src2);

  instrs.append(&mut match op {
    BinOp::Add => add(dst, &src, &src2),
    BinOp::Sub => sub(dst, &src, &src2),
    BinOp::BitAnd => bit_and(dst, &src, &src2),
    BinOp::BitOr => bit_or(dst, &src, &src2),
    BinOp::BitXor => bit_xor(dst, &src, &src2),
    BinOp::LessThan => less_than(dst, &src, &src2),
    BinOp::Leq => leq(dst, &src, &src2),
    BinOp::GreaterThan => greater_than(dst, &src, &src2),
    BinOp::Geq => geq(dst, &src, &src2),
    BinOp::EqEq => eqeq(dst, &src, &src2),
    BinOp::Uneq => uneq(dst, &src, &src2),
    BinOp::LogicalAnd => logical_and(dst, &src, &src2),
    BinOp::LogicalOr => logical_or(dst, &src, &src2),
    BinOp::Mul => imul(dst, &src, &src2),
    BinOp::Shl => shl(dst, &src, &src2),
    BinOp::Shr => shr(dst, &src, &src2),
    BinOp::Div => idiv(dst, &src, &src2),
    BinOp::Mod => imod(dst, &src, &src2),
  });

  instrs
}

pub(crate) fn cond_jmp(src: &X64Operand, target_false: &InstrLabel) -> Vec<X64Asmline> {
  if src.is_imm() {
    return if *src.as_imm().unwrap() == 0 {
      vec![X64Asmline::jmp(target_false)]
    } else {
      vec![]
    };
  }
  vec![
    X64Asmline::cmpb(IMM_ZERO, &src.to_8bit()),
    X64Asmline::je(target_false),
  ]
}

// Returns two sources that can be used for the further binop
//
// # Example
// let (src, src2) = rearrange_src_operands(&mut instrs, op, dst, src1, src2);
// instrs.append(&mut binop_operand_to_operand(op, &src, &src2, dst));
fn rearrange_src_operands(
  instrs: &mut Vec<X64Asmline>,
  op: &BinOp,
  dst: &X64Operand,
  src1: &X64Operand,
  src2: &X64Operand,
) -> (X64Operand, X64Operand) {
  let instr_length = get_instr_byte_width(src1, dst).max(get_instr_byte_width(src2, dst));
  let (src1, src2) = if instr_length == 8 {
    (
      src1.zero_extend_mem_if_4byte(instrs).with_size(8),
      src2.zero_extend_mem_if_4byte(instrs).with_size(8),
    )
  } else {
    (src1.clone().with_size(4), src2.clone().with_size(4))
  };
  let dst = &dst.clone().with_size(instr_length);
  match op {
    BinOp::Add
    | BinOp::Sub
    | BinOp::BitAnd
    | BinOp::BitOr
    | BinOp::BitXor
    | BinOp::LessThan
    | BinOp::Leq
    | BinOp::GreaterThan
    | BinOp::Geq
    | BinOp::EqEq
    | BinOp::Uneq
    | BinOp::LogicalAnd
    | BinOp::LogicalOr => {
      // we always want to move left_src to dst before the operation,
      // so if src2 is the same as dst, we swap them and save a movl
      let (left_src, right_src) = if op.is_commutative() && src2 == dst.clone() {
        (src2, src1)
      } else {
        (src1, src2)
      };

      // temp_dst has to be a register when
      // 1. We need to move left_src to a register first then we can do the
      //    operation on right_src and the register
      // 2. We need to move left_src to a register first since we cannot
      //    do the operation on two memory location
      // 3. for imul: temp must be a register when src is not an immediate
      let temp_dst = if right_src == dst.clone()
        || (right_src.is_memory() && dst.is_memory())
        || (*op == BinOp::Mul && dst.is_memory() && !right_src.is_imm())
      {
        RESERVED_REG_64BIT
      } else {
        dst
      }
      .clone()
      .with_size(instr_length);

      // move the left_src to temp_dst, temp_dst will be register if necessary
      instrs.append(&mut mov_operand_to_operand(&left_src, &temp_dst));

      // new src1 will be right_src and new src2 will be temp_dst since
      // left_src is already in temp_dst
      (right_src, temp_dst)
    }
    BinOp::Mul => (src1, src2),
    BinOp::Div => (src1, src2),
    BinOp::Mod => (src1, src2),
    BinOp::Shl => (src1, src2),
    BinOp::Shr => (src1, src2),
  }
}

// ====================================================
// ====================================================
// =                Optimizations                     =
// ====================================================
// ====================================================

/// exp is the exponent of the origin divisor
/// For example: dst = src1 / 8 -> exp = 3
fn div_to_shr(dst: &X64Operand, src1: &X64Operand, exp: &X64Operand) -> Vec<X64Asmline> {
  // for di, due to interference, we have extra register to use, i.e. RAX and RDX
  // but we have mark that if we can optimize, we free up RDX, instead, we use RESERVED_REG_32BIT
  let mut instrs = vec![];

  let exp_value = 1 << *exp.as_imm().unwrap();
  instrs.push(mov(src1, &X64Operand::reg(X86_64Register::RAX(32))));

  let src1 = X64Operand::reg(X86_64Register::RAX(32));
  if X64Operand::Imm(exp_value - 1).imm_represented_in_32b() {
    instrs.extend(lea(
      RESERVED_REG_32BIT,
      &src1.to_64bit(),
      None,
      None,
      Some((exp_value - 1).try_into().unwrap()),
    ));
  } else {
    instrs.extend(add(
      RESERVED_REG_32BIT,
      &X64Operand::Imm(exp_value - 1),
      &src1,
    ));
  }

  instrs.push(X64Asmline::testl(&src1, &src1));
  instrs.push(X64Asmline::cmovs(RESERVED_REG_32BIT, &src1));
  instrs.push(X64Asmline::sarl(exp, &src1));
  instrs.push(mov(&src1, dst));

  instrs
}

fn mod_to_shr(dst: &X64Operand, src1: &X64Operand, exp: &X64Operand) -> Vec<X64Asmline> {
  // for imod, due to interference, we have extra register to use, i.e. RAX and RDX
  // but we have mark that if we can optimize, we free up RDX, instead, we use RESERVED_REG_32BIT
  let mut instrs = vec![];

  let exponent = *exp.as_imm().unwrap();

  instrs.push(mov(src1, RESERVED_REG_32BIT));
  instrs.push(mov(
    RESERVED_REG_32BIT,
    &X64Operand::reg(X86_64Register::RAX(32)),
  ));
  instrs.push(X64Asmline::sarl(
    &X64Operand::Imm(31),
    &X64Operand::reg(X86_64Register::RAX(32)),
  ));
  instrs.push(X64Asmline::shrl(
    &X64Operand::Imm(32 - exponent),
    &X64Operand::reg(X86_64Register::RAX(32)),
  ));
  instrs.push(X64Asmline::addl(
    &X64Operand::reg(X86_64Register::RAX(32)),
    RESERVED_REG_32BIT,
  ));
  instrs.push(X64Asmline::andl(
    &X64Operand::Imm((1 << exponent) - 1),
    RESERVED_REG_32BIT,
  ));
  instrs.push(X64Asmline::subl(
    &X64Operand::reg(X86_64Register::RAX(32)),
    RESERVED_REG_32BIT,
  ));
  instrs.push(mov(RESERVED_REG_32BIT, dst));

  instrs
}

struct MagicNumber {
  magic: i32,
  shift: i32,
}

fn get_magic(d: i32) -> MagicNumber {
  let mut delta: u32;
  const TWO31: u32 = 0x80000000; // 2^31.
  let ad = d.unsigned_abs();
  let t = TWO31 + (d as u32 >> 31);
  let anc = t - 1 - t % ad;
  let mut p = 31;
  let mut q1 = TWO31 / anc;
  let mut r1 = TWO31 % anc;
  let mut q2 = TWO31 / ad;
  let mut r2 = TWO31 % ad;

  loop {
    p += 1;
    q1 *= 2;
    r1 *= 2;
    if r1 >= anc {
      q1 += 1;
      r1 -= anc;
    }
    q2 *= 2;
    r2 *= 2;
    if r2 >= ad {
      q2 += 1;
      r2 -= ad;
    }
    delta = ad - r2;
    if q1 < delta || (q1 == delta && r1 == 0) {
      continue;
    }
    break;
  }

  let mut magic = MagicNumber {
    magic: (q2 + 1) as i32,
    shift: p - 32,
  };

  if d < 0 {
    magic.magic = -magic.magic;
  }

  magic
}

fn const_div_to_mul(dst: &X64Operand, src1: &X64Operand, imm: i32) -> Vec<X64Asmline> {
  // For div, we can use RDX, RAX and R11D
  let mut instrs = vec![];
  let magic = get_magic(imm.abs());
  let op1 = X64Operand::reg(X86_64Register::RAX(64));
  let op2 = X64Operand::reg(X86_64Register::RDX(64));
  let op3 = RESERVED_REG_64BIT.clone();

  instrs.push(mov(src1, &op1.to_32bit()));
  instrs.push(X64Asmline::movslq(&op1.to_32bit(), &op2.to_64bit()));
  instrs.push(X64Asmline::imulq3(
    &X64Operand::Imm(magic.magic as i64),
    &op2.to_64bit(),
    &op2.to_64bit(),
  ));

  if imm != 3 {
    instrs.push(X64Asmline::shrq(&X64Operand::Imm(32), &op2.to_64bit()));
    if magic.magic < 0 {
      instrs.push(X64Asmline::addl(&op1.to_32bit(), &op2.to_32bit()));
    }
  }

  if imm > 0 {
    if imm == 3 {
      instrs.push(mov(&op2.to_64bit(), &op3.to_64bit()));
      instrs.push(X64Asmline::shrq(&X64Operand::Imm(32), &op3.to_64bit()));
    } else {
      instrs.push(mov(&op2.to_32bit(), &op3.to_32bit()));
    }

    if magic.shift > 0 {
      instrs.push(X64Asmline::sarl(
        &X64Operand::Imm(magic.shift as i64),
        &op3.to_32bit(),
      ));
    }
    instrs.push(X64Asmline::cdq());
    instrs.push(mov(&op3.to_32bit(), &op1.to_32bit()));
    instrs.push(X64Asmline::subl(&op2.to_32bit(), &op1.to_32bit()));
    instrs.push(mov(&op1.to_32bit(), dst));
  } else {
    instrs.push(X64Asmline::sarl(
      &X64Operand::Imm(magic.shift as i64),
      &op2.to_32bit(),
    ));
    instrs.push(X64Asmline::sarl(&X64Operand::Imm(31), &op1.to_32bit()));
    instrs.push(X64Asmline::subl(&op2.to_32bit(), &op1.to_32bit()));
    instrs.push(mov(&op1.to_32bit(), dst));
  }
  instrs
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn test_magic() {
    let magic = get_magic(3);
    assert_eq!(magic.magic, 0x55555556);
    assert_eq!(magic.shift, 0);

    let magic2 = get_magic(5);
    assert_eq!(magic2.magic, 0x66666667);
    assert_eq!(magic2.shift, 1);

    let magic3 = get_magic(7);
    assert_eq!(magic3.magic, -1840700269);
    assert_eq!(magic3.shift, 2);

    let magic3 = get_magic(9);
    assert_eq!(magic3.magic, 954437177);
    assert_eq!(magic3.shift, 1);
  }
}
