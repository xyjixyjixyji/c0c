use crate::abs2asm::operand::X64Operand;
use crate::asm::InstrLabel;

/// Represents one assembly line of a x86_64 program
///
/// When calling functions, please make sure the operands are valid.
/// For example, X64Asmline::pushq() can only be called with 64-bit operands.
#[derive(Debug, Clone)]
pub enum X64Asmline {
  /// regular instruction
  Instr(String),
  /// e.g. __c0_main:
  Label(String),
}

impl std::fmt::Display for X64Asmline {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      X64Asmline::Instr(instr) => write!(f, "\t{}", instr),
      X64Asmline::Label(label) => write!(f, "{}:", label),
    }
  }
}

impl X64Asmline {
  pub fn new_instr(instr: String) -> Self {
    Self::Instr(instr)
  }

  pub fn new_label(label: String) -> Self {
    Self::Label(label)
  }

  pub fn call(label: &str) -> Self {
    Self::new_instr(format!("call {}", label))
  }

  pub fn pushq(operand: &X64Operand) -> Self {
    assert!(operand.valid_for_quad());

    match operand {
      X64Operand::Register(reg) => {
        if reg.is_64bit() {
          Self::new_instr(format!("pushq {}", operand))
        } else {
          panic!("pushq {}, can only operates on 64-bit registers", reg)
        }
      }
      _ => Self::new_instr(format!("pushq {}", operand)),
    }
  }

  pub fn popq(operand: &X64Operand) -> Self {
    assert!(operand.valid_for_quad());

    match operand {
      X64Operand::Register(reg) => {
        if reg.is_64bit() {
          Self::new_instr(format!("popq {}", operand))
        } else {
          panic!("popq {}, can only operates on 64-bit registers", reg)
        }
      }
      _ => Self::new_instr(format!("popq {}", operand)),
    }
  }

  pub fn movq(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(!(src.is_memory() && dst.is_memory()));
    assert!(!dst.is_imm());
    assert!(src.valid_for_quad() && dst.valid_for_quad());

    if src == dst {
      Self::new_instr("".to_string())
    } else {
      Self::new_instr(format!("movq {}, {}", src, dst))
    }
  }

  pub fn movl(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(!(src.is_memory() && dst.is_memory()));
    assert!(!dst.is_imm());
    assert!(src.valid_for_long() && dst.valid_for_long());

    if src == dst {
      Self::new_instr("".to_string())
    } else {
      Self::new_instr(format!("movl {}, {}", src, dst))
    }
  }

  pub fn subq(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(!(src.is_memory() && dst.is_memory()));
    assert!(!dst.is_imm());
    assert!(src.valid_for_quad() && dst.valid_for_quad());

    Self::new_instr(format!("subq {}, {}", src, dst))
  }

  pub fn subl(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(!(src.is_memory() && dst.is_memory()));
    assert!(!dst.is_imm());
    assert!(src.valid_for_long() && dst.valid_for_long());

    Self::new_instr(format!("subl {}, {}", src, dst))
  }

  pub fn addq(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(!(src.is_memory() && dst.is_memory()));
    assert!(!dst.is_imm());
    assert!(src.valid_for_quad() && dst.valid_for_quad());

    Self::new_instr(format!("addq {}, {}", src, dst))
  }

  pub fn addl(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(!(src.is_memory() && dst.is_memory()));
    assert!(!dst.is_imm());
    assert!(src.valid_for_long() && dst.valid_for_long());

    Self::new_instr(format!("addl {}, {}", src, dst))
  }

  pub fn imulq2(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(src.valid_for_quad() && dst.valid_for_quad());
    assert!(dst.is_register() && !src.is_imm());

    Self::new_instr(format!("imulq {}, {}", src, dst))
  }

  pub fn imull2(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(src.valid_for_long() && dst.valid_for_long());
    assert!(dst.is_register() && !src.is_imm());

    Self::new_instr(format!("imull {}, {}", src, dst))
  }

  pub fn imulq3(num: &X64Operand, src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(src.valid_for_quad() && dst.valid_for_quad());
    assert!(num.is_imm() && dst.is_register() && !src.is_imm());

    Self::new_instr(format!("imulq {}, {}, {}", num, src, dst))
  }

  pub fn imull3(num: &X64Operand, src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(src.valid_for_long() && dst.valid_for_long());
    assert!(num.is_imm() && dst.is_register() && !src.is_imm());

    Self::new_instr(format!("imull {}, {}, {}", num, src, dst))
  }

  pub fn idivl(src: &X64Operand) -> Self {
    assert!(src.valid_for_long());
    assert!(!src.is_imm());

    Self::new_instr(format!("idivl {}", src))
  }

  pub fn cdq() -> Self {
    Self::new_instr("cdq".to_string())
  }

  pub fn negl(operand: &X64Operand) -> Self {
    assert!(operand.valid_for_long());
    Self::new_instr(format!("negl {}", operand))
  }

  pub fn xorl(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(src.valid_for_long() && dst.valid_for_long());
    Self::new_instr(format!("xorl {}, {}", src, dst))
  }

  pub fn notl(operand: &X64Operand) -> Self {
    assert!(operand.valid_for_long());
    Self::new_instr(format!("notl {}", operand))
  }

  pub fn andl(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(src.valid_for_long() && dst.valid_for_long());
    Self::new_instr(format!("andl {}, {}", src, dst))
  }

  pub fn orl(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(src.valid_for_long() && dst.valid_for_long());
    Self::new_instr(format!("orl {}, {}", src, dst))
  }

  pub fn cmpb(operand1: &X64Operand, operand2: &X64Operand) -> Self {
    Self::new_instr(format!("cmpb {}, {}", operand1, operand2))
  }

  pub fn cmpl(operand1: &X64Operand, operand2: &X64Operand) -> Self {
    Self::new_instr(format!("cmpl {}, {}", operand1, operand2))
  }

  pub fn cmpq(operand1: &X64Operand, operand2: &X64Operand) -> Self {
    Self::new_instr(format!("cmpq {}, {}", operand1, operand2))
  }

  pub fn setl(operand: &X64Operand) -> Self {
    assert!(operand.is_register() || operand.is_memory());
    Self::new_instr(format!("setl {}", operand.to_8bit()))
  }

  pub fn setle(operand: &X64Operand) -> Self {
    assert!(operand.is_register() || operand.is_memory());
    Self::new_instr(format!("setle {}", operand.to_8bit()))
  }

  pub fn setg(operand: &X64Operand) -> Self {
    assert!(operand.is_register() || operand.is_memory());
    Self::new_instr(format!("setg {}", operand.to_8bit()))
  }

  pub fn setge(operand: &X64Operand) -> Self {
    assert!(operand.is_register() || operand.is_memory());
    Self::new_instr(format!("setge {}", operand.to_8bit()))
  }

  pub fn sete(operand: &X64Operand) -> Self {
    assert!(operand.is_register() || operand.is_memory());
    Self::new_instr(format!("sete {}", operand.to_8bit()))
  }

  pub fn setne(operand: &X64Operand) -> Self {
    assert!(operand.is_register() || operand.is_memory());
    Self::new_instr(format!("setne {}", operand.to_8bit()))
  }

  pub fn sarl(shift: &X64Operand, src: &X64Operand) -> Self {
    assert!(src.valid_for_long());
    Self::new_instr(format!("sarl {}, {}", shift.to_8bit(), src))
  }

  pub fn shrl(shift: &X64Operand, src: &X64Operand) -> Self {
    assert!(src.valid_for_long());
    Self::new_instr(format!("shrl {}, {}", shift.to_8bit(), src))
  }

  pub fn shrq(shift: &X64Operand, src: &X64Operand) -> Self {
    assert!(src.valid_for_quad());
    Self::new_instr(format!("shrq {}, {}", shift.to_8bit(), src))
  }

  pub fn sall(shift: &X64Operand, src: &X64Operand) -> Self {
    assert!(src.valid_for_long());
    Self::new_instr(format!("sall {}, {}", shift.to_8bit(), src))
  }

  /// leaq (base, index, elem_size), dst
  pub fn lea(
    dst: &X64Operand,
    base: &X64Operand,
    index: Option<&X64Operand>,
    elem_size: Option<&X64Operand>,
    displ: Option<i32>,
  ) -> Self {
    assert!(dst.is_register());
    assert!(base.is_register() && base.valid_for_quad());
    assert!(index.is_none() || (index.unwrap().is_register() && index.unwrap().valid_for_quad()));
    assert!(elem_size.is_none() || elem_size.unwrap().is_imm());
    assert!((elem_size.is_none() && index.is_none()) || (elem_size.is_some() && index.is_some()));

    let mut instr_string = if let Some(displ) = displ {
      format!("lea {}({}", displ, base)
    } else {
      format!("lea ({}", base)
    };
    if let Some(index) = index {
      instr_string.push_str(&format!(",{}", index));
    }
    if let Some(elem_size) = elem_size {
      instr_string.push_str(&format!(",{}", elem_size.clone().into_imm().unwrap()));
    }
    instr_string.push_str(&format!("), {}", dst));

    Self::new_instr(instr_string)
  }

  pub fn jmp(target: &InstrLabel) -> Self {
    Self::new_instr(format!("jmp {}", target))
  }

  pub fn je(target: &InstrLabel) -> Self {
    Self::new_instr(format!("je {}", target))
  }

  pub fn jne(target: &InstrLabel) -> Self {
    Self::new_instr(format!("jne {}", target))
  }

  pub fn jg(target: &InstrLabel) -> Self {
    Self::new_instr(format!("jg {}", target))
  }

  pub fn jge(target: &InstrLabel) -> Self {
    Self::new_instr(format!("jge {}", target))
  }

  pub fn jl(target: &InstrLabel) -> Self {
    Self::new_instr(format!("jl {}", target))
  }

  pub fn jle(target: &InstrLabel) -> Self {
    Self::new_instr(format!("jle {}", target))
  }

  pub fn ret() -> Self {
    Self::new_instr("ret".to_string())
  }

  pub fn testl(operand1: &X64Operand, operand2: &X64Operand) -> Self {
    assert!(operand1.valid_for_long() && operand2.valid_for_long());
    Self::new_instr(format!("testl {}, {}", operand1, operand2))
  }

  pub fn cmovs(operand1: &X64Operand, operand2: &X64Operand) -> Self {
    assert!(operand1.valid_for_long() && operand2.valid_for_long());
    Self::new_instr(format!("cmovs {}, {}", operand1, operand2))
  }

  pub fn movslq(src: &X64Operand, dst: &X64Operand) -> Self {
    assert!(src.valid_for_long() && dst.valid_for_quad());
    Self::new_instr(format!("movslq {}, {}", src, dst))
  }
}
