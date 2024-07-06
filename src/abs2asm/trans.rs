use crate::{
  abs2asm::{
    asmline::X64Asmline,
    fname_to_label,
    operand::X64Operand,
    x86instr::{
      binop_operand_to_operand, cmp, cond_jmp, lea, mov, mov_operand_to_operand, read_mem,
      subroutine_epilogue, subroutine_prologue, write_mem,
    },
  },
  asm::{Cond, Dest, Instr, InstrLabel, Operand},
  ast::{BinOp, UnOp},
  codegen::Context,
  regalloc::Allocator,
  registers::{
    consts::{RESERVED_REG_32BIT, RESERVED_REG_64BIT, STACK_ELEM_SIZE},
    reg::X86_64Register,
  },
};

use std::collections::{HashMap, HashSet};

/// Lab1: This translator spills all temps to the stack,
///       and does not necessarily need a register allocator.
///       How: using rsp + offset to access, assumes each slot is 4 bytes.
///
/// NOTE: We use GNU syntax so the dst is always on the right.
pub struct X86Translator {
  ctx: Context,
  /// maps temp variables' index to their allocated register, or spilled stack location
  register_allocation_map: HashMap<u32, X64Operand>,
  /// The stack space used by the subroutine (temps)
  stack_size: i64,
  /// The livein sets for function calls [`Instr::Call`]. Caller-saved registers
  /// that are in the livein sets are going to be saved/restored.
  livein_sets_on_call: Option<Vec<HashSet<X86_64Register>>>,
}

impl X86Translator {
  /// Get the translator from the abstract assembly instructions.
  pub fn from_allocator(allocator: Allocator) -> Self {
    X86Translator {
      ctx: allocator.ctx,
      register_allocation_map: allocator.temp_allocation,
      stack_size: (allocator.num_spills * STACK_ELEM_SIZE) as i64,
      livein_sets_on_call: allocator.livein_sets_on_call,
    }
  }

  /// Translate a subroutine
  ///
  /// Lab3: we translate each defined function separately, and add global label i.e. __c0_main
  pub fn translate_subroutine(&self, label: &str) -> Vec<X64Asmline> {
    let mut x64asmlines = vec![X64Asmline::new_label(label.to_string())];
    let used_callee_regs = self.used_callee_registers();

    x64asmlines.extend(subroutine_prologue(
      self.stack_size,
      &used_callee_regs,
      self.ctx.get_self_arglist().len() > 6,
    ));
    let mut ith_call = 0; // mark how many calls we have encountered

    for instr in self.ctx.instrs.iter() {
      log::debug!("Translating instr: {:?}", instr);
      let x64asmline = match instr {
        Instr::BinOp {
          op,
          dest,
          src1,
          src2,
        } => self.translate_binop(op, dest, src1, src2),
        Instr::UnOp { op, dest, src } => self.translate_unop(op, dest, src),
        Instr::Mov { dest, src } => self.translate_mov(dest, src),
        Instr::Return(src) => self.translate_ret(src),
        Instr::Label { name: lab } => {
          // remove redundant labels
          if !self.ctx.tco_applied() && *lab == self.ctx.self_call_label {
            vec![]
          } else {
            vec![X64Asmline::new_label(instr.to_string())]
          }
        }
        Instr::Jmp { target } => vec![X64Asmline::jmp(target)],
        Instr::CondJmp {
          src, target_false, ..
        } => self.translate_condjmp(src, target_false),
        Instr::Call { name, args, dest } => {
          let instrs = self.translate_call(name, args, *dest, ith_call, used_callee_regs.len());
          ith_call += 1;
          instrs
        }
        Instr::ReadMem { dest, read_addr } => {
          let d = self.convert_dest_to_x64_operand(dest);
          let addr = self.convert_dest_to_x64_operand(read_addr);
          read_mem(&d, &addr, dest.1)
        }
        Instr::WriteMem { dest_addr, src } => {
          let d_addr = self.convert_dest_to_x64_operand(dest_addr);
          let s = self.convert_operand_to_x64(src);
          write_mem(&d_addr, &s)
        }
        Instr::Lea {
          dest,
          base,
          index,
          elem_size,
        } => {
          let dest = self.convert_dest_to_x64_operand(dest);
          let base = self.convert_operand_to_x64(base);
          let index = index.map(|i| self.convert_operand_to_x64(&i));
          let elem_size = elem_size.map(|i| self.convert_operand_to_x64(&i));
          lea(&dest, &base, index.as_ref(), elem_size.as_ref(), None)
        }
        Instr::Nop => vec![],
      };

      x64asmlines.extend(x64asmline);
    }

    x64asmlines
  }
}

impl X86Translator {
  /// Translates 2 forms of conditional jump
  /// 1. Conditional is a simple boolean temp
  /// 2. Conditional is a binary comparison of 2 temps
  fn translate_condjmp(&self, src: &Cond, target_false: &InstrLabel) -> Vec<X64Asmline> {
    match src {
      Cond::Simp(src) => {
        let operand = &self.convert_operand_to_x64(src);
        cond_jmp(operand, target_false)
      }
      Cond::BinOp(lhs, op, rhs) => {
        let operand1 = &self.convert_operand_to_x64(lhs);
        let mut operand2 = &self.convert_operand_to_x64(rhs);
        let mut instrs = vec![];
        if operand1.is_memory() && operand2.is_memory() {
          let reserved_reg = if operand1.valid_for_quad() {
            RESERVED_REG_64BIT
          } else {
            RESERVED_REG_32BIT
          };
          instrs.push(mov(operand2, reserved_reg));
          operand2 = reserved_reg;
        }
        let swap_operands = operand2.is_imm() || operand2.is_null();
        instrs.append(&mut match op {
          BinOp::LessThan => {
            if swap_operands {
              vec![cmp(operand2, operand1), X64Asmline::jge(target_false)]
            } else {
              vec![cmp(operand1, operand2), X64Asmline::jle(target_false)]
            }
          }
          BinOp::Leq => {
            if swap_operands {
              vec![cmp(operand2, operand1), X64Asmline::jg(target_false)]
            } else {
              vec![cmp(operand1, operand2), X64Asmline::jl(target_false)]
            }
          }
          BinOp::GreaterThan => {
            if swap_operands {
              vec![cmp(operand2, operand1), X64Asmline::jle(target_false)]
            } else {
              vec![cmp(operand1, operand2), X64Asmline::jge(target_false)]
            }
          }
          BinOp::Geq => {
            if swap_operands {
              vec![cmp(operand2, operand1), X64Asmline::jl(target_false)]
            } else {
              vec![cmp(operand1, operand2), X64Asmline::jg(target_false)]
            }
          }
          BinOp::EqEq => {
            if swap_operands {
              vec![cmp(operand2, operand1), X64Asmline::jne(target_false)]
            } else {
              vec![cmp(operand1, operand2), X64Asmline::jne(target_false)]
            }
          }
          BinOp::Uneq => {
            if swap_operands {
              vec![cmp(operand2, operand1), X64Asmline::je(target_false)]
            } else {
              vec![cmp(operand1, operand2), X64Asmline::je(target_false)]
            }
          }
          _ => {
            panic!("Invalid binop for condjmp: {:?}", op);
          }
        });
        instrs
      }
    }
  }

  fn translate_binop(
    &self,
    op: &BinOp,
    dest: &Dest,
    src1: &Operand,
    src2: &Operand,
  ) -> Vec<X64Asmline> {
    let src1 = self.convert_operand_to_x64(src1);
    let src2 = self.convert_operand_to_x64(src2);
    let dst = self.convert_dest_to_x64_operand(dest);

    binop_operand_to_operand(op, &src1, &src2, &dst)
  }

  fn translate_unop(&self, op: &UnOp, dest: &Dest, src: &Operand) -> Vec<X64Asmline> {
    match op {
      UnOp::Neg => {
        let src = self.convert_operand_to_x64(src);
        let dst = self.convert_dest_to_x64_operand(dest);

        mov_operand_to_operand(&src, &dst)
          .into_iter()
          .chain(vec![X64Asmline::negl(&dst)])
          .collect()
      }
      UnOp::Not => {
        let src = self.convert_operand_to_x64(src);
        let dst = self.convert_dest_to_x64_operand(dest);

        mov_operand_to_operand(&src, &dst)
          .into_iter()
          .chain(vec![X64Asmline::xorl(&X64Operand::Imm(1), &dst)])
          .collect()
      }
      UnOp::BitNot => {
        let src = self.convert_operand_to_x64(src);
        let dst = self.convert_dest_to_x64_operand(dest);

        mov_operand_to_operand(&src, &dst)
          .into_iter()
          .chain(vec![X64Asmline::notl(&dst)])
          .collect()
      }
    }
  }

  fn translate_mov(&self, dest: &Dest, src: &Operand) -> Vec<X64Asmline> {
    let src = self.convert_operand_to_x64(src);
    let dst = self.convert_dest_to_x64_operand(dest);

    mov_operand_to_operand(&src, &dst)
  }

  fn translate_call(
    &self,
    name: &str,
    args: &[Operand],
    dest: Option<Dest>,
    ith_call: usize,
    num_callee_saved: usize,
  ) -> Vec<X64Asmline> {
    let mut instrs = vec![];
    let caller_saved = self.get_caller_saved_regs(ith_call);

    // push caller-saved
    for reg in caller_saved.iter() {
      instrs.push(X64Asmline::pushq(&X64Operand::Register(*reg)));
    }

    // align stack before function call
    let align_size = self.get_align_size(args.len(), caller_saved.len(), num_callee_saved);
    if align_size != 0 {
      instrs.push(X64Asmline::subq(
        &X64Operand::Imm(align_size.into()),
        &X64Operand::Register(X86_64Register::RSP(64)),
      ));
    }

    // setup arguments, first 6 in registers, the rest on the stack
    instrs.extend(self.setup_func_args(args));

    // call the function and move the return value to the destination.
    instrs.push(X64Asmline::call(&fname_to_label(
      name,
      self
        .ctx
        .get_func_status(name)
        .unwrap_or_else(|| panic!("Function {} is not found", name))
        .is_header(),
    )));

    if let Some(dest) = dest {
      instrs.push(mov(
        &X64Operand::Register(X86_64Register::RAX(64)),
        &self.convert_dest_to_x64_operand(&dest).to_64bit(),
      ));
    }

    // restore the space that we used for the on-stack arguments, and for function call alignment
    let num_args = args.len() as i32;
    let restore_size = ((std::cmp::max(num_args, 6) - 6) * 8) + align_size;
    if restore_size != 0 {
      instrs.push(X64Asmline::addq(
        &X64Operand::Imm(restore_size.into()),
        &X64Operand::Register(X86_64Register::RSP(64)),
      ));
    }

    // pop caller-saved
    for reg in caller_saved.iter().rev() {
      instrs.push(X64Asmline::popq(&X64Operand::Register(*reg)));
    }

    instrs
  }

  /// Note that the two assembly lines generated can NOT be consecutive,
  /// since we have to follow the sequence of mov -> epilogue -> ret
  ///
  /// See `translate_subroutine` for more details.
  fn translate_ret(&self, src: &Option<Operand>) -> Vec<X64Asmline> {
    // x86 uses `eax` to store the return value.
    let mut asmlines = Vec::new();
    if let Some(src) = src {
      let size_bytes = match src {
        Operand::Temp(Dest(_, size)) => *size,
        Operand::Null => 8,
        _ => 4,
      };
      let ret_register = match size_bytes {
        4 => X64Operand::reg(X86_64Register::RAX(32)),
        8 => X64Operand::reg(X86_64Register::RAX(64)),
        _ => panic!("Invalid return size: {}", size_bytes),
      };
      asmlines.push(mov(&self.convert_operand_to_x64(src), &ret_register));
    }
    asmlines.extend(subroutine_epilogue(
      self.stack_size,
      &self.used_callee_registers(),
      self.ctx.get_self_arglist().len() > 6,
    ));
    asmlines.push(X64Asmline::ret());

    asmlines
  }
}

// Helpers
impl X86Translator {
  /// Convert the abstract assembly operand to a X64Operand
  fn convert_operand_to_x64(&self, operand: &Operand) -> X64Operand {
    match operand {
      Operand::Null => X64Operand::Null,
      Operand::Const(n) => X64Operand::Imm(*n),
      Operand::Temp(dest) => self.convert_dest_to_x64_operand(dest),
    }
  }

  /// Convert a Dest temp to a X64 Operand for the assembly
  ///
  /// For example, T0 -> 4(%rsp), T1 -> %eax, etc.
  fn convert_dest_to_x64_operand(&self, dest: &Dest) -> X64Operand {
    self
      .register_allocation_map
      .get(&dest.0)
      .unwrap_or_else(|| panic!("Cannot find temp {:?}'s offset", dest))
      .clone()
      .with_size(dest.1)
  }

  /// Get the caller saved registers that are livein on the i-th call
  fn get_caller_saved_regs(&self, ith_call: usize) -> Vec<X86_64Register> {
    let regs = match self
      .livein_sets_on_call
      .as_ref()
      .unwrap_or(&vec![])
      .get(ith_call)
    {
      Some(livein_regs) => livein_regs.clone(),
      None => HashSet::new(),
    };

    regs
      .iter()
      .filter(|reg| reg.is_caller_saved())
      .map(|reg| reg.as_caller_saved())
      .collect()
  }

  // moves the first 6 arguments to the registers, and the rest to the stack, and aligns the stack to 8 mod 16
  // before function call.
  // returns the stack offset for alignment
  fn setup_func_args(&self, arg_list: &[Operand]) -> Vec<X64Asmline> {
    let mut instrs = vec![];
    for (i, arg) in arg_list.iter().enumerate().rev() {
      if i < 6 {
        instrs.push(mov(
          &self.convert_operand_to_x64(arg).to_64bit(),
          &X64Operand::Register(X86_64Register::ith_argument(i as u8, 64)),
        ));
      } else {
        let operand = self.convert_operand_to_x64(arg);
        if operand.is_register() {
          instrs.push(X64Asmline::pushq(
            &self.convert_operand_to_x64(arg).to_64bit(),
          ))
        } else {
          instrs.push(X64Asmline::pushq(&operand.to_64bit()));
        }
      }
    }
    instrs
  }

  fn used_callee_registers(&self) -> Vec<X86_64Register> {
    self
      .register_allocation_map
      .values()
      .filter(|op| op.is_register())
      .map(|op| op.as_register().unwrap())
      .filter(|reg| reg.is_callee_saved())
      .map(|r| r.as_callee_saved())
      .collect()
  }

  fn get_align_size(&self, nargs: usize, ncaller_saved: usize, ncallee_saved: usize) -> i32 {
    let stack_alignment_dword = ncallee_saved        // callee saved
      + ncaller_saved                                // caller saved
      + (std::cmp::max(nargs, 6) - 6)                // stack args
      + self.stack_size as usize / 8                 // spilled temps
      + if nargs > 6 || self.stack_size > 0 {        // rbp (pushed?)
        1
      } else {
        0
      };

    // rsp % 16 = 8 before callsite
    if stack_alignment_dword % 2 == 0 {
      8
    } else {
      0
    }
  }
}
