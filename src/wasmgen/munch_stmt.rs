use crate::ast::{self, AsnOp, Expr, Stmt, Stmts};

use super::{
  munch_expr::{check_null_ptr, check_shift_val, munch_expr, MunchExprResult},
  wasm::{WasmBinop, WasmInstr, TEMP_VAR1_NAME, TEMP_VAR2_NAME},
  MunchContext,
};

/// Munches a statement into WebAssembly instructions.
///
/// Returns true if the statement is a return statement.
fn munch_stmt(ctx: &mut MunchContext, instrs: &mut Vec<WasmInstr>, stmt: Stmt) -> bool {
  match stmt {
    Stmt::Decl(_, var, expr) => munch_decl(ctx, instrs, var, expr),
    Stmt::Asgn(var, expr) => munch_asgn(ctx, instrs, var, expr),
    Stmt::Expr(expr) => {
      let need_drop = match &expr {
        Expr::FunctionCall(name, _) => {
          let func_decl = ctx.src_ctx.get_func_decl(name).unwrap();
          func_decl.ret_type != ast::ReturnType::Void
        }
        _ => true,
      };

      match munch_expr(ctx, expr, false) {
        MunchExprResult::Constant(_) => {}
        MunchExprResult::Instructions(expr_instrs) => {
          instrs.extend(expr_instrs);
          if need_drop {
            instrs.push(WasmInstr::Drop);
          }
        }
      }

      false
    }
    Stmt::Ret(expr) => munch_ret(ctx, instrs, expr),
    Stmt::Block(stmts, _) => {
      munch_stmts(ctx, instrs, stmts);
      false
    }
    Stmt::If(cond, stmts_true, stmts_false) => munch_if(ctx, instrs, cond, stmts_true, stmts_false),
    Stmt::While(cond, body) => munch_while(ctx, instrs, cond, body),
    Stmt::AsgnMem(lvalue, expr, asnop) => munch_asgnmem(ctx, instrs, lvalue, expr, asnop),
  }
}

fn munch_asgnmem(
  ctx: &mut MunchContext,
  instrs: &mut Vec<WasmInstr>,
  lvalue: ast::Expr,
  expr: ast::Expr,
  asnop: ast::AsnOp,
) -> bool {
  let is_eq = asnop == AsnOp::Eq;

  // stack: lvalue_addr
  match munch_expr(ctx, lvalue.clone(), true) {
    MunchExprResult::Constant(_) => panic!("Lvalue cannot be a constant"),
    MunchExprResult::Instructions(expr_instrs) => {
      instrs.extend(expr_instrs);
    }
  }

  if !is_eq {
    instrs.push(WasmInstr::TeeVar(TEMP_VAR1_NAME.to_string()));
    instrs.push(WasmInstr::GetVar(TEMP_VAR1_NAME.to_string()));
  }

  // stack: rhs, (laddr?), laddr
  match munch_expr(ctx, expr, false) {
    MunchExprResult::Constant(c) => {
      instrs.push(WasmInstr::Const(c));
    }
    MunchExprResult::Instructions(expr_instrs) => {
      instrs.extend(expr_instrs);
    }
  }

  // stack: (laddr?), laddr; temp var 2: rhs
  instrs.push(WasmInstr::SetVar(TEMP_VAR2_NAME.to_string()));

  // check laddr for null value
  instrs.extend(check_null_ptr());
  instrs.push(WasmInstr::GetVar(TEMP_VAR2_NAME.to_string()));

  if !is_eq {
    // store the rhs into temp
    instrs.push(WasmInstr::SetVar(TEMP_VAR1_NAME.to_string()));
    // stack: lval, laddr
    instrs.push(WasmInstr::Load);
    // stack: rhs, lval, laddr
    instrs.push(WasmInstr::GetVar(TEMP_VAR1_NAME.to_string()));
    if matches!(asnop, AsnOp::ShlEq | AsnOp::ShrEq) {
      instrs.extend(check_shift_val());
    }
    // stack: result, addr
    instrs.push(WasmInstr::BinOp(asnop_to_wasm_binop(asnop)));
  }

  // store the result in the lvalue
  instrs.push(WasmInstr::Store);

  false
}

fn munch_decl(
  ctx: &mut MunchContext,
  instrs: &mut Vec<WasmInstr>,
  var: String,
  expr: Option<ast::Expr>,
) -> bool {
  ctx.declared_params.insert(var.clone(), false);
  if let Some(e) = expr {
    match munch_expr(ctx, e, false) {
      MunchExprResult::Constant(val) => {
        instrs.push(WasmInstr::Const(val));
      }
      MunchExprResult::Instructions(expr_instrs) => {
        instrs.extend(expr_instrs);
      }
    }
    instrs.push(WasmInstr::SetVar(var));
  }
  false
}

fn munch_asgn(
  ctx: &mut MunchContext,
  instrs: &mut Vec<WasmInstr>,
  var: String,
  expr: ast::Expr,
) -> bool {
  match munch_expr(ctx, expr, false) {
    MunchExprResult::Constant(val) => {
      instrs.push(WasmInstr::Const(val));
    }
    MunchExprResult::Instructions(expr_instrs) => {
      instrs.extend(expr_instrs);
    }
  }
  instrs.push(WasmInstr::SetVar(var));
  false
}

fn munch_ret(ctx: &mut MunchContext, instrs: &mut Vec<WasmInstr>, expr: Option<ast::Expr>) -> bool {
  if let Some(expr) = expr {
    match munch_expr(ctx, expr, false) {
      MunchExprResult::Constant(val) => {
        instrs.push(WasmInstr::Const(val));
      }
      MunchExprResult::Instructions(expr_instrs) => {
        instrs.extend(expr_instrs);
      }
    }
  }
  instrs.push(WasmInstr::Return);
  true
}

fn munch_if(
  ctx: &mut MunchContext,
  instrs: &mut Vec<WasmInstr>,
  cond: Expr,
  stmts_true: Stmts,
  stmts_false: Option<Stmts>,
) -> bool {
  let cond_munched = munch_expr(ctx, cond, false);
  match cond_munched {
    MunchExprResult::Constant(c) => {
      if c == 0 {
        if let Some(stmts_false) = stmts_false {
          return munch_stmts(ctx, instrs, stmts_false);
        } else {
          return false;
        }
      } else {
        return munch_stmts(ctx, instrs, stmts_true);
      }
    }
    MunchExprResult::Instructions(cond_instrs) => {
      instrs.extend(cond_instrs);
    }
  }

  // build the "if" WasmInstr
  let mut true_instrs = vec![];
  let mut has_return = munch_stmts(ctx, &mut true_instrs, stmts_true);
  let false_instrs = match stmts_false {
    Some(stmts_false) => {
      let mut false_instrs = vec![];
      has_return = munch_stmts(ctx, &mut false_instrs, stmts_false) && has_return;
      Some(false_instrs)
    }
    None => {
      has_return = false;
      None
    }
  };

  let if_instr = WasmInstr::If {
    has_return: false,
    then_block: true_instrs,
    else_block: false_instrs,
  };
  instrs.push(if_instr);
  has_return
}

fn munch_while(
  ctx: &mut MunchContext,
  instrs: &mut Vec<WasmInstr>,
  cond: Expr,
  stmts: Stmts,
) -> bool {
  let loop_id = ctx.loop_id();

  // build the condition
  let mut expr_instrs = vec![];
  let cond_munched = munch_expr(ctx, cond, false);
  match cond_munched {
    MunchExprResult::Constant(cond) => {
      if cond == 0 {
        return false;
      } else {
        expr_instrs.push(WasmInstr::Const(1));
      }
    }
    MunchExprResult::Instructions(cond_instrs) => {
      expr_instrs.extend(cond_instrs);
    }
  }

  // build the loop body
  let mut loop_instrs = vec![];

  munch_stmts(ctx, &mut loop_instrs, stmts);
  let loop_instr = WasmInstr::While {
    cond: expr_instrs,
    body: loop_instrs,
    loop_id,
  };
  instrs.push(loop_instr);

  false
}

/// Munches a list of statements into WebAssembly instructions.
///
/// Returns true if any of the statements is a return statement.
pub(super) fn munch_stmts(
  munch_ctx: &mut MunchContext,
  instrs: &mut Vec<WasmInstr>,
  stmts: Stmts,
) -> bool {
  for stmt in stmts.into_stmts() {
    if munch_stmt(munch_ctx, instrs, stmt) {
      return true;
    }
  }
  false
}

fn asnop_to_wasm_binop(asnop: AsnOp) -> WasmBinop {
  match asnop {
    AsnOp::PlusEq => WasmBinop::Add,
    AsnOp::MinusEq => WasmBinop::Sub,
    AsnOp::TimesEq => WasmBinop::Mul,
    AsnOp::DivEq => WasmBinop::Div,
    AsnOp::ModEq => WasmBinop::Rem,
    AsnOp::AndEq => WasmBinop::And,
    AsnOp::OrEq => WasmBinop::Or,
    AsnOp::XorEq => WasmBinop::Xor,
    AsnOp::ShlEq => WasmBinop::Shl,
    AsnOp::ShrEq => WasmBinop::Shr,
    AsnOp::Eq => WasmBinop::Eq,
  }
}
