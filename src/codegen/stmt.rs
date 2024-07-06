use crate::asm::{Cond, Instr, LoopState, Operand};
use crate::ast::{self, to_op, AsnOp, Typ};
use crate::codegen::expr::munch_expr;
use crate::typecheck::TypecheckContext;

use super::expr::null_ptr_check;
use super::{munch_conditional, Context, INLINE_SUBSTITION_THRESHOLD, TEMP_SIZE_8B};

/// Transform a statement into a series of instructions in a context.
///
/// # Arguments
///
/// * `ctx` - The context to add the instructions to
/// * `stmt` - The statement to transform
/// * `loop_id` - The loop id if the statement is in a loop, it is used for inner
///               loop statement to locate whether and which loop it's in
pub(super) fn munch_stmt(ctx: &mut Context, stmt: ast::Stmt, loop_id: Option<u32>) {
  use ast::Stmt::*;
  match stmt {
    Decl(typ, var, expr) => munch_decl(ctx, typ, var, expr),
    Asgn(var, expr) => munch_asgn(ctx, var, expr),
    Ret(expr) => munch_ret(ctx, expr),
    Block(stmts, is_for_loop) => munch_stmts(ctx, stmts, is_for_loop),
    If(expr, stmts_true, stmts_false) => munch_if(ctx, expr, stmts_true, stmts_false),
    While(expr, stmts) => munch_while(ctx, expr, stmts, loop_id),
    Expr(expr) => {
      munch_expr(ctx, None, expr, false);
    }
    AsgnMem(lvalue, expr, asnop) => munch_asgnmem(ctx, lvalue, expr, asnop),
  };
}

fn munch_decl(ctx: &mut Context, typ: Typ, var: String, expr: Option<ast::Expr>) {
  let var_size = ctx.src_ctx.get_type_dest_size(&typ);
  if let Some(e) = expr {
    let dest = ctx.var_with_size(var, var_size);
    munch_expr(ctx, Some(dest), e, false);
    ctx.mark_cannot_inline()
  } else {
    ctx.var_with_size(var, var_size);
  }
}

fn munch_asgn(ctx: &mut Context, var: String, expr: ast::Expr) {
  let dest = ctx.var(var);
  munch_expr(ctx, Some(dest), expr, false);
}

fn munch_ret(ctx: &mut Context, expr: Option<ast::Expr>) {
  let instr = if let Some(expr) = expr {
    let dest_operand = munch_expr(ctx, None, expr, false);
    Instr::Return(Some(dest_operand))
  } else {
    Instr::Return(None)
  };

  ctx.add_instr(instr);
}

fn munch_if(
  ctx: &mut Context,
  cond_expr: ast::Expr,
  stmts_true: ast::Stmts,
  stmts_false: Option<ast::Stmts>,
) {
  let label_false = ctx.basic_label();
  let label_next = match stmts_false {
    Some(_) => ctx.basic_label(),
    None => label_false.clone(),
  };
  let (cond_operand1, op, cond_operand2) = munch_conditional(ctx, cond_expr);
  let checked_cond: Cond;
  if let Some(op) = op {
    checked_cond = Cond::BinOp(cond_operand1, op, cond_operand2.unwrap());
  } else {
    // always evaluate one branch
    if let Operand::Const(cond_operand1) = cond_operand1 {
      if cond_operand1 == 0 && stmts_false.is_some() {
        if let Some(stmts_false) = stmts_false {
          munch_stmts(ctx, stmts_false, false);
          return;
        }
      }
      if cond_operand1 == 1 {
        munch_stmts(ctx, stmts_true, false);
        return;
      }
    }
    checked_cond = Cond::Simp(cond_operand1);
  }

  // we set can_inline here because the when it returns early, the control flow is still linear
  ctx.mark_cannot_inline();
  let label_true = ctx.basic_label();

  ctx.add_instr(Instr::CondJmp {
    src: checked_cond,
    target_true: label_true.clone(),
    target_false: label_false.clone(),
  });
  ctx.add_instr(Instr::Label { name: label_true });
  munch_stmts(ctx, stmts_true, false);
  ctx.add_instr(Instr::Jmp {
    target: label_next.clone(),
  });

  if let Some(stmts_false) = stmts_false {
    ctx.add_instr(Instr::Label { name: label_false });
    munch_stmts(ctx, stmts_false, false);
    ctx.add_instr(Instr::Jmp {
      target: label_next.clone(),
    });
  }

  ctx.add_instr(Instr::Label { name: label_next });
}

fn munch_while(ctx: &mut Context, cond_expr: ast::Expr, stmts: ast::Stmts, loop_id: Option<u32>) {
  let loop_id = loop_id.unwrap_or(ctx.loop_id());
  let loop_predicate_label = ctx.label(Some(loop_id), Some(LoopState::Predicate));
  let loop_end_label = ctx.label(Some(loop_id), Some(LoopState::End));

  ctx.add_instr(Instr::Label {
    name: loop_predicate_label.clone(),
  });

  let (cond_operand1, op, cond_operand2) = munch_conditional(ctx, cond_expr);
  let checked_cond = if let Some(op) = op {
    Cond::BinOp(cond_operand1, op, cond_operand2.unwrap())
  } else {
    // skip when loop condition is always false
    if cond_operand1.is_const() && *cond_operand1.as_const().unwrap() == 0 {
      ctx.add_instr(Instr::Label {
        name: loop_end_label,
      });
      return;
    }
    Cond::Simp(cond_operand1)
  };

  // we set can_inline here because the when it returns early, the control flow is still linear
  ctx.mark_cannot_inline();
  let loop_body_label = ctx.label(Some(loop_id), Some(LoopState::Body));

  ctx.add_instr(Instr::CondJmp {
    src: checked_cond,
    target_true: loop_body_label.clone(),
    target_false: loop_end_label.clone(),
  });
  ctx.add_instr(Instr::Label {
    name: loop_body_label,
  });
  munch_stmts(ctx, stmts, false);

  ctx.add_instr(Instr::Jmp {
    target: loop_predicate_label,
  });

  ctx.add_instr(Instr::Label {
    name: loop_end_label,
  });
}

fn munch_asgnmem(ctx: &mut Context, lvalue: ast::Expr, expr: ast::Expr, asnop: AsnOp) {
  let is_eq = asnop == AsnOp::Eq;
  let lvalue_addr = ctx.temp(TEMP_SIZE_8B);
  let _ = munch_expr(ctx, Some(lvalue_addr), lvalue, true);

  // evaluate rhs first to be consistent with dynamic semantics
  let rvalue_operand = munch_expr(ctx, None, expr, false);

  null_ptr_check(ctx, lvalue_addr);
  let total_val_operand = if !is_eq {
    let lvalue_val = ctx.temp(rvalue_operand.get_size_bytes());
    ctx.add_instr(Instr::ReadMem {
      dest: lvalue_val,
      read_addr: lvalue_addr,
    });
    ctx.add_instr(Instr::BinOp {
      op: to_op(asnop).unwrap(),
      dest: lvalue_val,
      src1: Operand::Temp(lvalue_val),
      src2: rvalue_operand,
    });
    Operand::Temp(lvalue_val)
  } else {
    rvalue_operand
  };

  ctx.add_instr(Instr::WriteMem {
    dest_addr: lvalue_addr,
    src: total_val_operand,
  });
}

pub(super) fn munch_stmts(ctx: &mut Context, stmts: ast::Stmts, is_for_loop: bool) {
  let loop_id = if is_for_loop {
    let loop_id = ctx.loop_id();
    let loop_init_label = ctx.label(Some(loop_id), Some(LoopState::Init));
    ctx.add_instr(Instr::Label {
      name: loop_init_label,
    });
    Some(loop_id)
  } else {
    None
  };
  for stmt in stmts.into_stmts() {
    munch_stmt(ctx, stmt, loop_id);
  }
}

fn munch_fdefn(fdefn: ast::Fdefn, src_ctx: Box<TypecheckContext>) -> Option<Context> {
  let mut ctx = Context::new(fdefn.func_name.clone(), src_ctx);

  for (typ, param) in fdefn.param_list.0.iter() {
    let param_size = ctx.src_ctx.get_type_dest_size(typ);
    ctx.var_with_size(param.to_string(), param_size);
  }

  // we move the first 6 arguments to the new temp registers to maintain
  // the interference
  for (i, (_, param)) in fdefn.param_list.0.iter().enumerate() {
    if i < 6 {
      let dest = *ctx.var_temp_map.get(param).unwrap();
      let fresh = ctx.temp(dest.1);

      ctx.instrs.push(Instr::Mov {
        dest: fresh,
        src: Operand::Temp(dest),
      });
      ctx.var_temp_map.insert(param.to_string(), fresh);
    }
  }
  ctx.add_instr(Instr::Label {
    name: ctx.self_call_label.clone(),
  });

  munch_stmts(&mut ctx, fdefn.body, false);

  ctx.setup_memerror_mount_point();

  if ctx.instrs.len() > INLINE_SUBSTITION_THRESHOLD || ctx.num_called_functions() > 1 {
    ctx.mark_cannot_inline()
  }
  Some(ctx)
}

pub(super) fn munch_gdecl(gdecl: ast::Gdecl, src_ctx: Box<TypecheckContext>) -> Option<Context> {
  match gdecl {
    ast::Gdecl::Fdefn(fdefn) => munch_fdefn(fdefn, src_ctx),
    _ => None,
  }
}
