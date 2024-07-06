use crate::{
  ast::{self, Var},
  typecheck::expr::tc_expr,
};

use anyhow::{anyhow, Result};

use super::context::{Context, VarStatus};

/// Validate a series of statements, updating the context of defined variables
/// according to the results we have from validating each statement.
pub(super) fn tc_stmts(
  ctx: &mut Context,
  stmts: &mut ast::Stmts,
  required_ret_type: &ast::ReturnType,
) -> Result<(bool, Vec<Var>)> {
  let mut all_new_defn = vec![];
  let old_var_ctx = ctx.clone_var_context();

  let mut has_ret = false;
  for stmt in stmts.get_stmts_ref() {
    let (ret, mut new_defn) = tc_stmt(ctx, stmt, required_ret_type)?;
    has_ret |= ret;
    ctx.update_defined_var(&new_defn);
    all_new_defn.append(&mut new_defn);
  }

  if matches!(stmts, ast::Stmts::NewScopeStmts(_)) {
    ctx.replace_var_context(old_var_ctx);
  }

  Ok((has_ret, all_new_defn))
}

/// Check the validity of a statement. Returns (ret, defined_set)
fn tc_stmt(
  ctx: &mut Context,
  stmt: &mut ast::Stmt,
  required_ret_type: &ast::ReturnType,
) -> Result<(bool, Vec<Var>)> {
  use ast::Stmt::*;

  match stmt {
    Decl(typ, var, expr_opt) => tc_stmt_decl(ctx, typ, var, expr_opt),
    // If it's defined, anything goes. If it's just declared, we're not allowed to use its value.
    Asgn(var, e) => tc_stmt_asgn(ctx, var, e),
    AsgnMem(e1, e2, _) => tc_stmt_asgnmem(ctx, e1, e2),
    Ret(e) => tc_stmt_ret(ctx, e, required_ret_type),
    Block(block_stmts, _) => tc_stmts(ctx, block_stmts, required_ret_type),
    Expr(e) => tc_expr(ctx, e, true).map(|typ| {
      if matches!(typ, ast::Typ::Struct(_)) {
        return Err(anyhow!(
          "top level expression cannot have struct type: {:?}",
          e
        ));
      }
      Ok((false, vec![]))
    })?,
    If(e, then_stmts, else_stmts) => tc_stmt_if(ctx, e, then_stmts, else_stmts, required_ret_type),
    While(e, body_stmts) => tc_stmt_while(ctx, e, body_stmts, required_ret_type),
  }
}

fn tc_stmt_decl(
  ctx: &mut Context,
  typ: &ast::Typ,
  var: &str,
  expr: &mut Option<ast::Expr>,
) -> Result<(bool, Vec<Var>)> {
  if ctx.is_var_declared(var) || ctx.type_exists(var) {
    return Err(anyhow!(
      "Variable name: {} declared in current context",
      var
    ));
  }
  if let Some(typ) = &ctx.to_reduced_typ(typ) {
    // variable with large type not allowed
    if matches!(typ, ast::Typ::Struct(_)) {
      return Err(anyhow!(
        "Cannot declare variable with struct type: {:?}",
        typ
      ));
    }
    if let Some(expr) = expr {
      // int x = 1;
      let expr_typ = tc_expr(ctx, expr, false)?;
      if ctx.typ_eq(&expr_typ, typ) {
        ctx.define_var(var.to_string(), typ.clone());
        Ok((false, vec![var.to_string()]))
      } else {
        Err(anyhow!(
          "Invalid declaration: {} = {:?}, with different types assigned",
          var,
          expr
        ))
      }
    } else {
      // int x;
      ctx.declare_var(var.to_string(), typ.clone());
      Ok((false, vec![]))
    }
  } else {
    Err(anyhow!("Invalid type: {:?}", typ))
  }
}

fn tc_stmt_asgn(ctx: &mut Context, var: &str, e: &mut ast::Expr) -> Result<(bool, Vec<Var>)> {
  match ctx.get_var_status(var) {
    Some(VarStatus { typ, .. }) => {
      let expr_typ = tc_expr(ctx, e, false)?;
      if !ctx.typ_eq(&expr_typ, &typ) {
        Err(anyhow!("Invalid assignment: {:?} = {:?}", var, e))
      } else {
        ctx.define_var(var.to_string(), typ);
        Ok((false, vec![var.to_string()]))
      }
    }
    None => Err(anyhow!("Assigning undeclared variable {:?}", var,)),
  }
}

fn tc_stmt_asgnmem(
  ctx: &mut Context,
  e1: &mut ast::Expr,
  e2: &mut ast::Expr,
) -> Result<(bool, Vec<Var>)> {
  let typ1 = tc_expr(ctx, e1, false)?;
  let typ2 = tc_expr(ctx, e2, false)?;
  if matches!(typ1, ast::Typ::Struct(_)) {
    return Err(anyhow!(
      "Invalid assignment: cannot use struct type {:?} = {:?}",
      e1,
      e2
    ));
  }

  if ctx.typ_eq(&typ1, &typ2) {
    Ok((false, vec![]))
  } else {
    Err(anyhow!("Invalid assignment: {:?} = {:?}", e1, e2))
  }
}

fn tc_stmt_ret(
  ctx: &mut Context,
  e: &mut Option<ast::Expr>,
  required_ret_type: &ast::ReturnType,
) -> Result<(bool, Vec<Var>)> {
  let ret_type = if let Some(expr) = e {
    let expr_typ = tc_expr(ctx, expr, false)?;
    ctx.typ_to_return_type(&expr_typ)
  } else {
    ast::ReturnType::Void
  };

  if ctx.return_type_eq(&ret_type, required_ret_type) {
    Ok((true, ctx.declared_vars()))
  } else {
    Err(anyhow!("Invalid return type: {:?}", ret_type))
  }
}

fn tc_stmt_if(
  ctx: &mut Context,
  e: &mut ast::Expr,
  then_stmts: &mut ast::Stmts,
  else_stmts: &mut Option<ast::Stmts>,
  required_ret_type: &ast::ReturnType,
) -> Result<(bool, Vec<Var>)> {
  let expr_typ = tc_expr(ctx, e, false)?;
  if !ctx.typ_eq(&expr_typ, &ast::Typ::Bool) {
    return Err(anyhow!("Condition expecting type boolean, expr: {:?}", e));
  }

  let (ret_then, defn_then) = tc_stmts(ctx, then_stmts, required_ret_type)?;
  let (ret_else, defn_else) = match else_stmts {
    Some(stmts) => tc_stmts(ctx, stmts, required_ret_type)?,
    None => (false, vec![]),
  };

  let defn_both = defn_then
    .iter()
    .filter(|var| defn_else.contains(var))
    .cloned()
    .collect();

  // if both stmts return, this if is guaranteed to return
  Ok((ret_then && ret_else, defn_both))
}

fn tc_stmt_while(
  ctx: &mut Context,
  e: &mut ast::Expr,
  body_stmts: &mut ast::Stmts,
  required_ret_type: &ast::ReturnType,
) -> Result<(bool, Vec<Var>)> {
  let typ_expr = tc_expr(ctx, e, false)?;
  if ctx.typ_eq(&typ_expr, &ast::Typ::Bool) {
    tc_stmts(ctx, body_stmts, required_ret_type)?;
    Ok((false, vec![])) // while does not return
  } else {
    Err(anyhow!(
      "while (expression) ..., expected boolean expression"
    ))
  }
}
