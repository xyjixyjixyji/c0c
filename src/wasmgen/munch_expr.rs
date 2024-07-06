use crate::{
  ast::{self, ArgList, BinOp, Expr, FuncName, Typ, UnOp},
  codegen::fold_binops_i32,
  elaboration::LIBRARY_MEMERROR,
  registers::consts::VALID_SHIFT_MASK,
};

use super::{
  wasm::{WasmBinop, WasmInstr, TEMP_VAR1_NAME, TEMP_VAR2_NAME},
  MunchContext,
};

const ALLOCATE_FUNC: &str = "__15411_allocate";

#[derive(Debug, Clone)]
pub enum MunchExprResult {
  Constant(i32),
  Instructions(Vec<WasmInstr>),
}

/// Transforms an AST expression into a series of WebAssembly instructions.
/// If the AST expression can be folded into a constant, the constant is returned.
///
/// Returns (folded constant, wasm instructions)
pub(super) fn munch_expr(ctx: &mut MunchContext, expr: Expr, is_lvalue: bool) -> MunchExprResult {
  match expr {
    Expr::True => MunchExprResult::Constant(1),
    Expr::False => MunchExprResult::Constant(0),
    Expr::Null => MunchExprResult::Constant(0),
    Expr::Number(e) => MunchExprResult::Constant(e),
    Expr::Variable(var) => MunchExprResult::Instructions(vec![WasmInstr::GetVar(var.clone())]),
    Expr::Binop(e1, op, e2) => munch_binop(ctx, *e1, op, *e2),
    Expr::Unop(op, e) => munch_unop(ctx, *e, op),
    Expr::Ternary(cond, true_expr, false_expr) => {
      munch_ternary(ctx, *cond, *true_expr, *false_expr)
    }
    Expr::FunctionCall(name, arg_list) => munch_func_call(ctx, name, arg_list),
    Expr::Alloc(typ) => munch_alloc(ctx, typ),
    Expr::AllocArray(typ, length_expr) => munch_alloc_array(ctx, typ, *length_expr),
    Expr::StructAccess(struct_expr, field, struct_typ) => {
      munch_struct_access(ctx, *struct_expr, field, *struct_typ, is_lvalue)
    }
    Expr::PointerDeref(expr, typ) => munch_pointer_deref(ctx, *expr, *typ, is_lvalue),
    Expr::ArrayAccess(arr_expr, idx_expr, elem_typ) => {
      munch_array_access(ctx, *arr_expr, *idx_expr, *elem_typ, is_lvalue)
    }
    _ => panic!("Expr: {:?} cannot appear in the elaborated AST", expr),
  }
}

fn munch_struct_access(
  ctx: &mut MunchContext,
  struct_expr: ast::Expr,
  field: String,
  struct_typ: ast::Typ,
  is_lvalue: bool,
) -> MunchExprResult {
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

  let need_deref = !(matches!(field_typ, Typ::Struct(_)) || is_lvalue);

  // put the field's offset on the stack, i.e. base + offset
  let mut instrs = vec![];

  match munch_expr(ctx, struct_expr, false) {
    MunchExprResult::Constant(addr) => instrs.push(WasmInstr::Const(addr)),
    MunchExprResult::Instructions(struct_instrs) => instrs.extend(struct_instrs),
  }
  instrs.push(WasmInstr::Const(field_offset as i32));
  instrs.push(WasmInstr::BinOp(WasmBinop::Add));

  if need_deref {
    instrs.push(WasmInstr::Load);
  }

  MunchExprResult::Instructions(instrs)
}

fn munch_array_access(
  ctx: &mut MunchContext,
  arr_expr: Expr,
  idx_expr: Expr,
  elem_typ: Typ,
  is_lvalue: bool,
) -> MunchExprResult {
  let need_deref = !(matches!(elem_typ, Typ::Struct(_)) || is_lvalue);
  // calculate the memory offset, i.e. arr_expr + idx_expr * sizeof(elem_typ)
  let elem_size = ctx.src_ctx.get_type_size(&elem_typ);

  let mut instrs = vec![];
  // stack: base_addr
  match munch_expr(ctx, arr_expr, false) {
    MunchExprResult::Constant(addr) => instrs.push(WasmInstr::Const(addr)),
    MunchExprResult::Instructions(arr_instrs) => instrs.extend(arr_instrs),
  }

  // stack: addr_offset, base_addr
  match munch_expr(ctx, idx_expr, false) {
    MunchExprResult::Constant(idx) => {
      instrs.push(WasmInstr::Const(idx));
    }
    MunchExprResult::Instructions(idx_instrs) => {
      instrs.extend(idx_instrs);
    }
  }

  // check the array base address for null
  instrs.push(WasmInstr::SetVar(TEMP_VAR2_NAME.to_string()));
  instrs.extend(check_null_ptr());
  instrs.push(WasmInstr::GetVar(TEMP_VAR2_NAME.to_string()));

  instrs.push(WasmInstr::Const(elem_size as i32));
  instrs.push(WasmInstr::BinOp(WasmBinop::Mul));

  // store the offset to temp var 2, and base_addr to temp var 1
  instrs.push(WasmInstr::SetVar(TEMP_VAR2_NAME.to_string()));
  instrs.push(WasmInstr::TeeVar(TEMP_VAR1_NAME.to_string()));
  instrs.push(WasmInstr::GetVar(TEMP_VAR2_NAME.to_string()));
  instrs.extend(check_array_access());
  instrs.push(WasmInstr::BinOp(WasmBinop::Add));

  if need_deref {
    instrs.push(WasmInstr::Load);
  }

  MunchExprResult::Instructions(instrs)
}

fn munch_alloc_array(ctx: &mut MunchContext, typ: ast::Typ, length: ast::Expr) -> MunchExprResult {
  let size = ctx.src_ctx.get_type_size(&typ);
  let length_munched = munch_expr(ctx, length, false);

  let mut instrs = vec![];

  // push the length instructions to the stack
  match length_munched {
    MunchExprResult::Constant(length) => instrs.push(WasmInstr::Const(length)),
    MunchExprResult::Instructions(length_instrs) => instrs.extend(length_instrs),
  }

  // if length is non-positive, trigger memerror
  instrs.push(WasmInstr::TeeVar(TEMP_VAR1_NAME.to_string()));
  instrs.push(WasmInstr::GetVar(TEMP_VAR1_NAME.to_string()));
  instrs.push(WasmInstr::Const(0));
  instrs.push(WasmInstr::BinOp(WasmBinop::Lt));
  instrs.push(WasmInstr::If {
    has_return: false,
    then_block: vec![WasmInstr::Call(LIBRARY_MEMERROR.to_string())],
    else_block: None,
  });

  instrs.push(WasmInstr::Const(size as i32));
  instrs.push(WasmInstr::BinOp(WasmBinop::Mul));

  // save array size to temp var
  instrs.push(WasmInstr::TeeVar(TEMP_VAR1_NAME.to_string()));

  // extra 4 bytes to store the array length
  instrs.push(WasmInstr::Const(4));
  instrs.push(WasmInstr::BinOp(WasmBinop::Add));
  instrs.push(WasmInstr::Call(ALLOCATE_FUNC.to_string()));
  instrs.push(WasmInstr::TeeVar(TEMP_VAR2_NAME.to_string()));

  // store array size to array ptr
  instrs.push(WasmInstr::GetVar(TEMP_VAR1_NAME.to_string()));
  instrs.push(WasmInstr::Store);

  // return the actual address of the array, which is 4 bytes after the allocated address
  instrs.push(WasmInstr::GetVar(TEMP_VAR2_NAME.to_string()));
  instrs.push(WasmInstr::Const(4));
  instrs.push(WasmInstr::BinOp(WasmBinop::Add));

  MunchExprResult::Instructions(instrs)
}

fn munch_pointer_deref(
  ctx: &mut MunchContext,
  expr: Expr,
  ptr_typ: ast::Typ,
  is_lvalue: bool,
) -> MunchExprResult {
  let need_deref = !(matches!(ptr_typ, Typ::Struct(_)) || is_lvalue);
  let mut new_instrs = vec![];
  // push the address
  let expr_munched = munch_expr(ctx, expr, false);
  // read the memory
  match expr_munched {
    MunchExprResult::Constant(addr) => {
      new_instrs.push(WasmInstr::Const(addr));
    }
    MunchExprResult::Instructions(instrs) => {
      new_instrs.extend(instrs);
    }
  }
  if !is_lvalue {
    new_instrs.extend(check_null_ptr());
  }
  if need_deref {
    new_instrs.push(WasmInstr::Load);
  }
  MunchExprResult::Instructions(new_instrs)
}

fn munch_alloc(ctx: &mut MunchContext, typ: Typ) -> MunchExprResult {
  let size = ctx.src_ctx.get_type_size(&typ);
  MunchExprResult::Instructions(vec![
    WasmInstr::Const(size as i32),
    WasmInstr::Call(ALLOCATE_FUNC.to_string()),
  ])
}

fn munch_binop(ctx: &mut MunchContext, e1: Expr, op: BinOp, e2: Expr) -> MunchExprResult {
  let lhs_munched = munch_expr(ctx, e1, false);
  let rhs_munched = munch_expr(ctx, e2, false);

  let mut instrs = vec![];

  // special case: if both operands are constants, we can fold the operation
  if matches!(lhs_munched, MunchExprResult::Constant(_))
    && matches!(rhs_munched, MunchExprResult::Constant(_))
  {
    if let (MunchExprResult::Constant(lhs), MunchExprResult::Constant(rhs)) =
      (lhs_munched.clone(), rhs_munched.clone())
    {
      if let Some(result) = fold_binops_i32(lhs as i64, rhs as i64, op) {
        return MunchExprResult::Constant(result);
      }
    }
  }

  let mut check_shift_bound = true;
  // push the lhs instructions to the stack
  match lhs_munched {
    MunchExprResult::Constant(lhs) => instrs.push(WasmInstr::Const(lhs)),
    MunchExprResult::Instructions(lhs_instrs) => instrs.extend(lhs_instrs),
  }

  match rhs_munched {
    MunchExprResult::Constant(rhs) => {
      if (0..32).contains(&rhs) {
        check_shift_bound = false;
      }
      instrs.push(WasmInstr::Const(rhs))
    }
    MunchExprResult::Instructions(rhs_instrs) => instrs.extend(rhs_instrs),
  }

  let wasm_op = match op {
    BinOp::Add => WasmBinop::Add,
    BinOp::Sub => WasmBinop::Sub,
    BinOp::Mul => WasmBinop::Mul,
    BinOp::Div => WasmBinop::Div,
    BinOp::Mod => WasmBinop::Rem,
    BinOp::BitAnd | BinOp::LogicalAnd => WasmBinop::And,
    BinOp::BitOr | BinOp::LogicalOr => WasmBinop::Or,
    BinOp::BitXor => WasmBinop::Xor,
    BinOp::Shl => {
      if check_shift_bound {
        instrs.extend(check_shift_val());
      }
      WasmBinop::Shl
    }
    BinOp::Shr => {
      if check_shift_bound {
        instrs.extend(check_shift_val());
      }
      WasmBinop::Shr
    }
    BinOp::EqEq => WasmBinop::Eq,
    BinOp::Uneq => WasmBinop::Ne,
    BinOp::Leq => WasmBinop::Le,
    BinOp::LessThan => WasmBinop::Lt,
    BinOp::Geq => WasmBinop::Ge,
    BinOp::GreaterThan => WasmBinop::Gt,
  };

  instrs.push(WasmInstr::BinOp(wasm_op));

  MunchExprResult::Instructions(instrs)
}

fn munch_unop(ctx: &mut MunchContext, e: Expr, op: UnOp) -> MunchExprResult {
  let expr_munched = munch_expr(ctx, e, false);

  match expr_munched {
    MunchExprResult::Constant(e) => {
      let result = match op {
        UnOp::Neg => -e,
        UnOp::Not => 1 - e,
        UnOp::BitNot => !e,
      };
      MunchExprResult::Constant(result)
    }
    MunchExprResult::Instructions(mut instrs) => match op {
      UnOp::Neg => {
        instrs.push(WasmInstr::Const(-1));
        instrs.push(WasmInstr::BinOp(WasmBinop::Mul));
        MunchExprResult::Instructions(instrs)
      }
      UnOp::Not => {
        let mut new_instrs = vec![WasmInstr::Const(1)];
        new_instrs.extend(instrs);
        new_instrs.push(WasmInstr::BinOp(WasmBinop::Sub));
        MunchExprResult::Instructions(new_instrs)
      }
      UnOp::BitNot => {
        instrs.push(WasmInstr::Const(-1));
        instrs.push(WasmInstr::BinOp(WasmBinop::Xor));
        MunchExprResult::Instructions(instrs)
      }
    },
  }
}

fn munch_ternary(
  ctx: &mut MunchContext,
  cond: Expr,
  true_expr: Expr,
  false_expr: Expr,
) -> MunchExprResult {
  let cond_munched = munch_expr(ctx, cond, false);
  match cond_munched {
    MunchExprResult::Constant(cond_val) => {
      if cond_val == 0 {
        munch_expr(ctx, false_expr, false)
      } else {
        munch_expr(ctx, true_expr, false)
      }
    }
    MunchExprResult::Instructions(mut cond_instrs) => {
      let t_munched = munch_expr(ctx, true_expr, false);
      let f_munched = munch_expr(ctx, false_expr, false);

      let true_instrs = match t_munched {
        MunchExprResult::Constant(val) => {
          vec![WasmInstr::Const(val)]
        }
        MunchExprResult::Instructions(instrs) => instrs,
      };

      let false_instrs = match f_munched {
        MunchExprResult::Constant(val) => {
          vec![WasmInstr::Const(val)]
        }
        MunchExprResult::Instructions(instrs) => instrs,
      };

      cond_instrs.push(WasmInstr::If {
        has_return: true,
        then_block: true_instrs,
        else_block: Some(false_instrs),
      });

      MunchExprResult::Instructions(cond_instrs)
    }
  }
}

fn munch_func_call(ctx: &mut MunchContext, name: FuncName, arg_list: ArgList) -> MunchExprResult {
  let mut instrs = vec![];

  for arg in arg_list.0 {
    let arg_munched = munch_expr(ctx, arg, false);
    match arg_munched {
      MunchExprResult::Constant(arg) => instrs.push(WasmInstr::Const(arg)),
      MunchExprResult::Instructions(arg_instrs) => instrs.extend(arg_instrs),
    }
  }

  instrs.push(WasmInstr::Call(name));

  MunchExprResult::Instructions(instrs)
}

/// Special helper function to enforce C0's runtime semantics when the shift amount is out of bounds (+- 32)
///
/// When called, the top of the stack should contain the shift amount.
pub(super) fn check_shift_val() -> Vec<WasmInstr> {
  vec![
    WasmInstr::TeeVar(TEMP_VAR1_NAME.to_string()),
    WasmInstr::GetVar(TEMP_VAR1_NAME.to_string()),
    WasmInstr::Const(*VALID_SHIFT_MASK.as_imm().unwrap() as i32),
    WasmInstr::BinOp(WasmBinop::And),
    WasmInstr::GetVar(TEMP_VAR1_NAME.to_string()),
    WasmInstr::BinOp(WasmBinop::Eq),
    WasmInstr::BinOp(WasmBinop::Div),
  ]
}

/// Special helper function to enforce C0's runtime semantics when dereferencing a null pointer
/// When called, the top of the stack should contain the address to be checked
pub(super) fn check_null_ptr() -> Vec<WasmInstr> {
  vec![
    WasmInstr::TeeVar(TEMP_VAR1_NAME.to_string()),
    WasmInstr::GetVar(TEMP_VAR1_NAME.to_string()),
    WasmInstr::Const(0),
    WasmInstr::BinOp(WasmBinop::Eq),
    WasmInstr::If {
      has_return: false,
      then_block: vec![WasmInstr::Call(LIBRARY_MEMERROR.to_string())],
      else_block: None,
    },
  ]
}

/// Special helper function to enforce C0's runtime semantics when accessing an array
/// When called, the array's base address should be stored in TEMP_VAR1_NAME, and the offset should be stored in TEMP_VAR2_NAME
fn check_array_access() -> Vec<WasmInstr> {
  vec![
    // get address to array size
    WasmInstr::GetVar(TEMP_VAR1_NAME.to_string()),
    WasmInstr::Const(4),
    WasmInstr::BinOp(WasmBinop::Sub),
    WasmInstr::Load,
    // compare offset with array size. If offset >= size, then error
    WasmInstr::GetVar(TEMP_VAR2_NAME.to_string()),
    WasmInstr::BinOp(WasmBinop::Le),
    WasmInstr::If {
      has_return: false,
      then_block: vec![WasmInstr::Call(LIBRARY_MEMERROR.to_string())],
      else_block: None,
    },
    // ensures offset >= 0
    WasmInstr::GetVar(TEMP_VAR2_NAME.to_string()),
    WasmInstr::Const(0),
    WasmInstr::BinOp(WasmBinop::Lt),
    WasmInstr::If {
      has_return: false,
      then_block: vec![WasmInstr::Call(LIBRARY_MEMERROR.to_string())],
      else_block: None,
    },
  ]
}
