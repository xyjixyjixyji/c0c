use crate::asm::{self, Cond, Dest, Instr, Operand};
use crate::ast::{self, BinOp, Typ};
use crate::elaboration::LIBRARY_CALLOC;
use crate::utils::is_unsafe;
use crate::OPTIM_OPTIONS;

use super::{fold_binops_i32, munch_conditional, Context, TEMP_SIZE_4B, TEMP_SIZE_8B};

/// Transform an expression into a series of instructions in a context.
/// returns the size of the expression type in bytes
/// is_lvalue: whether the expression is an lvalue of a memasgn
pub fn munch_expr(
  ctx: &mut Context,
  dest: Option<Dest>,
  expr: ast::Expr,
  is_lvalue: bool,
) -> Operand {
  use ast::Expr::*;
  use Operand::*;

  match expr {
    // function calls
    FunctionCall(fname, arglist) => munch_fn_call(ctx, dest, fname, arglist),
    Alloc(typ) => munch_alloc(ctx, typ, dest),
    AllocArray(typ, len) => munch_alloc_array(ctx, typ, *len, dest),

    // normal expressions
    Binop(lhs, op, rhs) => munch_binop(ctx, dest, *lhs, op, *rhs),
    Unop(op, rhs) => munch_unop(ctx, dest, op, *rhs),
    Ternary(cond, t, f) => munch_ternary(ctx, dest, *cond, *t, *f),

    // memory access
    StructAccess(struct_expr, field, struct_typ) => {
      munch_struct_access(ctx, dest, *struct_expr, field, *struct_typ, is_lvalue)
    }
    PointerDeref(e, ptr_typ) => munch_pointer_deref(ctx, dest, *e, *ptr_typ, is_lvalue),
    ArrayAccess(arr_expr, idx_expr, elem_typ) => {
      munch_array_access(ctx, dest, *arr_expr, *idx_expr, *elem_typ, is_lvalue)
    }

    // simple expressions
    Number(n) => {
      if let Some(dest) = dest {
        ctx.add_instr(Instr::Mov {
          src: Const(n as i64),
          dest,
        });
        Temp(dest)
      } else {
        Const(n as i64)
      }
    }
    True => {
      if let Some(dest) = dest {
        ctx.add_instr(Instr::Mov {
          src: Const(1),
          dest,
        });
        Temp(dest)
      } else {
        Const(1)
      }
    }
    False => {
      if let Some(dest) = dest {
        ctx.add_instr(Instr::Mov {
          src: Const(0),
          dest,
        });
        Temp(dest)
      } else {
        Const(0)
      }
    }
    Variable(v) => {
      let var = Temp(ctx.var(v));
      if let Some(dest) = dest {
        let dest = dest.with_size(var.get_size_bytes());
        ctx.add_instr(Instr::Mov { src: var, dest });
        Temp(dest)
      } else {
        var
      }
    }
    ast::Expr::Null => {
      if let Some(dest) = dest {
        let dest = dest.with_size(TEMP_SIZE_8B);
        ctx.add_instr(Instr::Mov {
          src: Operand::Null,
          dest,
        });
        Temp(dest)
      } else {
        Operand::Null
      }
    }
    _ => panic!("Expr: {:?} cannot appear in the elaborated AST", expr),
  }
}

fn munch_fn_call(
  ctx: &mut Context,
  dest: Option<Dest>,
  fname: ast::FuncName,
  arglist: ast::ArgList,
) -> Operand {
  let mut args = Vec::new();

  // left to right evaluation
  for arg in arglist.0 {
    args.push(munch_expr(ctx, None, arg, false));
  }

  let ret_type = ctx.src_ctx.get_func_decl(&fname).unwrap().ret_type;
  let (dest, _) = match ret_type {
    ast::ReturnType::Void => (None, 0),
    ast::ReturnType::Type(typ) => {
      let size = ctx.src_ctx.get_type_dest_size(&typ);
      (Some(dest.unwrap_or(ctx.temp(size)).with_size(size)), size)
    }
  };

  ctx.add_instr(Instr::Call {
    name: fname.clone(),
    args,
    dest,
  });
  if fname != ctx.fn_name {
    ctx.metadata.called_fns.insert(fname);
  } else {
    ctx.mark_cannot_inline()
  }
  dest.map_or(Operand::Null, Operand::Temp)
}

fn munch_alloc(ctx: &mut Context, typ: ast::Typ, dest: Option<Dest>) -> Operand {
  let dest = dest.unwrap_or(ctx.temp_default()).with_size(TEMP_SIZE_8B);
  let size = ctx.src_ctx.get_type_size(&typ);
  ctx.add_instr(Instr::Call {
    name: LIBRARY_CALLOC.to_string(),
    args: vec![Operand::Const(1), Operand::Const(size as i64)],
    dest: Some(dest),
  });
  Operand::Temp(dest)
}

fn munch_alloc_array(
  ctx: &mut Context,
  typ: ast::Typ,
  length: ast::Expr,
  dest: Option<Dest>,
) -> Operand {
  let label_true = ctx.basic_label();
  let dest = dest
    .unwrap_or(ctx.temp(TEMP_SIZE_8B))
    .with_size(TEMP_SIZE_8B);

  let elem_size = ctx.src_ctx.get_type_size(&typ);
  let length_operand = munch_expr(ctx, None, length, false);

  if let Operand::Const(l) = length_operand {
    if l >= 0 {
      ctx.record_array_length(dest, l);
    }
  }

  // ensures array length is non-negative
  let is_nonneg = ctx.temp_default();
  ctx.add_instr(Instr::BinOp {
    op: ast::BinOp::Geq,
    dest: is_nonneg,
    src1: length_operand,
    src2: Operand::Const(0),
  });
  ctx.add_instr(Instr::CondJmp {
    src: Cond::Simp(Operand::Temp(is_nonneg)),
    target_true: label_true.clone(),
    target_false: ctx.memerror_label.clone(),
  });
  ctx.add_instr(Instr::Label { name: label_true });
  // allocate space for the array and extra 8 Bytes for the length
  let byte_length = if let Operand::Const(len) = length_operand {
    Operand::Const(len * elem_size as i64 + 8)
  } else {
    let byte_length = ctx.temp(TEMP_SIZE_8B);
    ctx.add_instr(Instr::BinOp {
      op: ast::BinOp::Mul,
      dest: byte_length,
      src1: length_operand,
      src2: Operand::Const(elem_size as i64),
    });
    ctx.add_instr(Instr::BinOp {
      op: ast::BinOp::Add,
      dest: byte_length,
      src1: Operand::Temp(byte_length),
      src2: Operand::Const(8),
    });
    asm::Operand::Temp(byte_length)
  };

  // 8 + length * elem_size
  // calloc(length + 8, elem_size) -> calloc(length * elem_size + 8, 1)
  let begin_addr = ctx.temp(TEMP_SIZE_8B);
  ctx.add_instr(Instr::Call {
    name: LIBRARY_CALLOC.to_string(),
    args: vec![byte_length, Operand::Const(1)],
    dest: Some(begin_addr),
  });

  // write the length of the array to the first 8 Bytes
  ctx.add_instr(Instr::WriteMem {
    dest_addr: begin_addr,
    src: length_operand,
  });

  // write the address of the array to dest
  ctx.add_instr(Instr::BinOp {
    op: ast::BinOp::Add,
    dest,
    src1: Operand::Temp(begin_addr),
    src2: Operand::Const(8),
  });

  Operand::Temp(dest)
}

fn munch_binop(
  ctx: &mut Context,
  dest: Option<Dest>,
  lhs: ast::Expr,
  op: ast::BinOp,
  rhs: ast::Expr,
) -> Operand {
  let operand1 = munch_expr(ctx, None, lhs, false);
  let operand2 = munch_expr(ctx, None, rhs, false);
  if operand1.is_const() && operand2.is_const() {
    let folded = fold_binops_i32(
      *operand1.as_const().unwrap(),
      *operand2.as_const().unwrap(),
      op,
    );
    if let Some(folded) = folded {
      let mut operand = Operand::Const(folded as i64);
      if let Some(dest) = dest {
        ctx.add_instr(Instr::Mov {
          src: Operand::Const(folded as i64),
          dest,
        });
        operand = Operand::Temp(dest);
      }
      return operand;
    }
  }

  let dest = dest.unwrap_or(ctx.temp_default());

  // a = b * (2 ** n) -> a = b << n
  if OPTIM_OPTIONS.lock().unwrap().strength_reduction {
    if let BinOp::Mul = op {
      if let Some(exp) = operand2.exponent_of_two() {
        ctx.add_instr(Instr::BinOp {
          op: BinOp::Shl,
          dest: dest.with_size(TEMP_SIZE_4B),
          src1: operand1,
          src2: exp,
        });
        return Operand::Temp(dest);
      }
    }
  }

  ctx.add_instr(Instr::BinOp {
    op,
    dest: dest.with_size(TEMP_SIZE_4B),
    src1: operand1,
    src2: operand2,
  });
  Operand::Temp(dest)
}

fn munch_unop(ctx: &mut Context, dest: Option<Dest>, op: ast::UnOp, rhs: ast::Expr) -> Operand {
  let operand = munch_expr(ctx, None, rhs, false);
  if operand.is_const() {
    let folded = match op {
      ast::UnOp::Neg => Some(-*operand.as_const().unwrap()),
      ast::UnOp::Not => Some(1 - *operand.as_const().unwrap()),
      ast::UnOp::BitNot => Some(!*operand.as_const().unwrap()),
    };
    if let Some(folded) = folded {
      let mut operand = Operand::Const(folded);
      if let Some(dest) = dest {
        ctx.add_instr(Instr::Mov {
          src: Operand::Const(folded),
          dest,
        });
        operand = Operand::Temp(dest);
      }
      return operand;
    }
  }
  let dest = dest.unwrap_or(ctx.temp_default());
  ctx.add_instr(Instr::UnOp {
    op,
    dest,
    src: operand,
  });
  Operand::Temp(dest)
}

fn munch_ternary(
  ctx: &mut Context,
  dest: Option<Dest>,
  cond: ast::Expr,
  true_expr: ast::Expr,
  false_expr: ast::Expr,
) -> Operand {
  let label_true = ctx.basic_label();
  let label_false = ctx.basic_label();
  let label_next = ctx.basic_label();
  let checked_cond: Cond;
  let (cond_operand1, op, cond_operand2) = munch_conditional(ctx, cond);
  if let Some(op) = op {
    checked_cond = Cond::BinOp(cond_operand1, op, cond_operand2.unwrap());
  } else {
    // always evaluate one branch
    if cond_operand1.is_const() {
      if *cond_operand1.as_const().unwrap() == 0 {
        return munch_expr(ctx, dest, false_expr, false);
      } else {
        return munch_expr(ctx, dest, true_expr, false);
      }
    }
    checked_cond = Cond::Simp(cond_operand1);
  }

  let mut dest = dest.unwrap_or(ctx.temp_default());
  ctx.add_instr(Instr::CondJmp {
    src: checked_cond,
    target_true: label_true.clone(),
    target_false: label_false.clone(),
  });
  ctx.add_instr(Instr::Label { name: label_true });
  dest = *munch_expr(ctx, Some(dest), true_expr, false)
    .as_temp()
    .unwrap();
  ctx.add_instr(Instr::Jmp {
    target: label_next.clone(),
  });
  ctx.add_instr(Instr::Label { name: label_false });
  munch_expr(ctx, Some(dest), false_expr, false);
  ctx.add_instr(Instr::Jmp {
    target: label_next.clone(),
  });
  ctx.add_instr(Instr::Label { name: label_next });

  Operand::Temp(dest)
}

/// Munch the struct_access AST, the form is like struct.field
/// If the struct is an lvalue or the field the a big type, we write the address
/// into dest. Otherwise, we write the value into dest.
///
/// # Example
/// struct a {
///   int b;
///   struct b c;
/// }
/// munch(a.b): write the value of b into dest
/// munch(a.c): write the address of c into dest
fn munch_struct_access(
  ctx: &mut Context,
  dest: Option<Dest>,
  expr: ast::Expr,
  field: String,
  struct_typ: Typ,
  is_lvalue: bool,
) -> Operand {
  let dest = dest.unwrap_or(ctx.temp_default());
  let struct_name = match struct_typ {
    Typ::Struct(name) => name,
    _ => {
      unreachable!()
    }
  };
  let (field_typ, field_offset) = ctx
    .src_ctx
    .get_field_typ_offset(&struct_name, &field)
    .unwrap();

  // if the field is a struct, then we put the resulting address in dest
  // without dereferencing it
  let need_deref = !(matches!(field_typ, Typ::Struct(_)) || is_lvalue);
  let struct_base_addr = if !need_deref {
    dest.with_size(TEMP_SIZE_8B)
  } else {
    ctx.temp(TEMP_SIZE_8B)
  };
  munch_expr(ctx, Some(struct_base_addr), expr, false);

  // find the correct field offset
  ctx.add_instr(Instr::BinOp {
    op: ast::BinOp::Add,
    dest: struct_base_addr,
    src1: Operand::Temp(struct_base_addr),
    src2: Operand::Const(field_offset as i64),
  });

  // dereference the field address if the field is not an array, pointer, or struct,
  // or if the expression is NOT an lvalue
  let field_temp_size = ctx.src_ctx.get_type_dest_size(&field_typ);
  if need_deref {
    let dest = dest.with_size(field_temp_size);
    ctx.add_instr(Instr::ReadMem {
      dest,
      read_addr: struct_base_addr,
    });
    Operand::Temp(dest)
  } else {
    Operand::Temp(struct_base_addr)
  }
}

/// Munch the pointer_deref AST, the form is like *expr
/// If the pointer is an lvalue, we write the address into dest. Otherwise, we
/// write the value into dest.
///
/// # Example
/// *ptr = 1; => AsgnMem(PointerDeref(ptr), Const(1))
/// This will become a Write_mem instruction of Write_mem(ptr, 1)
/// See munch_asgnmem() in [`crate::codegen::stmt`] for more details
fn munch_pointer_deref(
  ctx: &mut Context,
  dest: Option<Dest>,
  expr: ast::Expr,
  ptr_typ: Typ,
  is_lvalue: bool,
) -> Operand {
  let dest = dest.unwrap_or(ctx.temp_default());
  let need_deref = !(matches!(ptr_typ, Typ::Struct(_)) || is_lvalue);
  let addr_temp = if !need_deref {
    dest.with_size(TEMP_SIZE_8B)
  } else {
    ctx.temp(TEMP_SIZE_8B)
  };
  munch_expr(ctx, Some(addr_temp), expr, false);

  // for *ptr = 1, we do not need a null pointer check
  // but for other forms like **ptr, we need to check if ptr is null
  if !is_lvalue {
    null_ptr_check(ctx, addr_temp);
  }

  let val_temp_size = ctx.src_ctx.get_type_dest_size(&ptr_typ);
  if need_deref {
    let dest = dest.with_size(val_temp_size);
    ctx.add_instr(Instr::ReadMem {
      dest,
      read_addr: addr_temp,
    });
    return Operand::Temp(dest);
  }
  Operand::Temp(addr_temp)
}

/// Munch the array_access AST, the form is like arr[expr]
/// If the array access result is an lvalue, we write the address into dest.
/// Otherwise, we write the array access' value into dest.
///
/// # Example
/// arr[1] = 2; => AsgnMem(ArrayAccess(arr, Const(1)), Const(2))
/// This will become a Write_mem instruction of
/// Write_mem(arr + 1 * sizeof(typ), 2).
///
/// a = arr[1]; => Asgn(a, ArrayAccess(arr, Const(1)))
/// This will become a Read_mem instruction of
/// Read_mem(a, arr + 1 * sizeof(typ))
fn munch_array_access(
  ctx: &mut Context,
  dest: Option<Dest>,
  arr_expr: ast::Expr,
  idx_expr: ast::Expr,
  elem_typ: Typ,
  is_lvalue: bool,
) -> Operand {
  let dest = dest.unwrap_or(ctx.temp_default());
  let need_deref = !(matches!(elem_typ, Typ::Struct(_)) || is_lvalue);
  let array_dest = if !need_deref {
    dest.with_size(TEMP_SIZE_8B)
  } else {
    ctx.temp(TEMP_SIZE_8B)
  };
  munch_expr(ctx, Some(array_dest), arr_expr, false);
  let idx_operand = munch_expr(ctx, None, idx_expr, false);

  if !is_unsafe() {
    // check dest != NULL
    null_ptr_check(ctx, array_dest);
  }

  // Skip when idx and length are constants and 0 <= idx < length
  if let Operand::Const(idx) = idx_operand {
    if let Some(length) = ctx.try_get_array_length(&array_dest) {
      if idx >= length || idx < 0 {
        bound_check(ctx, array_dest, idx_operand);
      }
    } else {
      bound_check(ctx, array_dest, idx_operand);
    }
  } else {
    // check if the array access is out of bound
    bound_check(ctx, array_dest, idx_operand);
  }

  let typ_size = ctx.src_ctx.get_type_size(&elem_typ);
  if typ_size > TEMP_SIZE_8B as u64 {
    if let Operand::Const(idx) = idx_operand {
      let offset = idx * typ_size as i64;
      ctx.add_instr(Instr::BinOp {
        op: ast::BinOp::Add,
        dest: array_dest,
        src1: Operand::Temp(array_dest),
        src2: Operand::Const(offset),
      });
    } else {
      let offset_temp = ctx.temp(TEMP_SIZE_8B);
      ctx.add_instr(Instr::BinOp {
        op: ast::BinOp::Mul,
        dest: offset_temp,
        src1: idx_operand,
        src2: Operand::Const(typ_size as i64),
      });
      ctx.add_instr(Instr::BinOp {
        op: ast::BinOp::Add,
        dest: array_dest,
        src1: Operand::Temp(array_dest),
        src2: Operand::Temp(offset_temp),
      });
    }
  } else {
    ctx.add_instr(Instr::Lea {
      dest: array_dest,
      base: Operand::Temp(array_dest),
      index: Some(idx_operand),
      elem_size: Some(Operand::Const(typ_size as i64)),
    });
  }

  let typ_temp_size = ctx.src_ctx.get_type_dest_size(&elem_typ);
  if need_deref {
    let dest = dest.with_size(typ_temp_size);
    ctx.add_instr(Instr::ReadMem {
      dest,
      read_addr: array_dest,
    });
    return Operand::Temp(dest);
  }
  Operand::Temp(array_dest)
}

pub(crate) fn null_ptr_check(ctx: &mut Context, ptr_dest: Dest) {
  if is_unsafe() {
    return;
  }
  let label_true = ctx.basic_label();
  ctx.add_instr(Instr::CondJmp {
    src: Cond::BinOp(Operand::Temp(ptr_dest), BinOp::Uneq, Operand::Null),
    target_true: label_true.clone(),
    target_false: ctx.memerror_label.clone(),
  });
  ctx.add_instr(Instr::Label { name: label_true });
}

/// Add instruction sequence to check if the array access is out of bound
///
/// # Arguments
/// * `ctx` - The context to add instructions to
/// * `array_dest` - The destination storing the array
/// * `idx_dest` - The destination storing the index
fn bound_check(ctx: &mut Context, array_dest: Dest, idx_operand: Operand) {
  if is_unsafe() {
    return;
  }

  // first get the length of the array, which is stored in the 8 bytes before the array
  let length_addr_dest = ctx.temp(TEMP_SIZE_8B);
  ctx.add_instr(Instr::BinOp {
    op: ast::BinOp::Sub,
    dest: length_addr_dest,
    src1: Operand::Temp(array_dest),
    src2: Operand::Const(8),
  });

  // Now read the length out
  let length_dest = ctx.temp(TEMP_SIZE_4B);
  ctx.add_instr(Instr::ReadMem {
    dest: length_dest,
    read_addr: length_addr_dest,
  });

  // if not index < length, jump to error
  let label_true = ctx.basic_label();
  ctx.add_instr(Instr::CondJmp {
    src: Cond::BinOp(idx_operand, BinOp::LessThan, Operand::Temp(length_dest)),
    target_true: label_true.clone(),
    target_false: ctx.memerror_label.clone(),
  });
  ctx.add_instr(Instr::Label { name: label_true });

  // if not index >= 0, jump to error
  if idx_operand.is_const() {
    if let Operand::Const(idx) = idx_operand {
      if idx < 0 {
        ctx.add_instr(Instr::Jmp {
          target: ctx.memerror_label.clone(),
        });
      }
      return;
    }
  }
  let label_true = ctx.basic_label();
  ctx.add_instr(Instr::CondJmp {
    src: Cond::BinOp(idx_operand, BinOp::Geq, Operand::Const(0)),
    target_true: label_true.clone(),
    target_false: ctx.memerror_label.clone(),
  });
  ctx.add_instr(Instr::Label { name: label_true });
}
