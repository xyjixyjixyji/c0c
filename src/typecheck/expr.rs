use crate::ast::{self, Typ};
use anyhow::{anyhow, Ok, Result};

use super::context::Context;

pub(super) fn tc_expr(
  ctx: &mut Context,
  expr: &mut ast::Expr,
  is_top_level_stmt: bool,
) -> Result<ast::Typ> {
  use ast::Expr::*;
  match expr {
    True | False => Ok(ast::Typ::Bool),
    Null => Ok(ast::Typ::Null),
    Number(_) => Ok(ast::Typ::Int),
    Variable(v) => tc_expr_var(ctx, v),
    FunctionCall(fname, arglist) => tc_expr_fn_call(ctx, fname, arglist, is_top_level_stmt),
    Binop(e1, binop, e2) => tc_expr_binop(ctx, e1, binop, e2),
    Unop(unop, e) => tc_expr_unop(ctx, unop, e),
    Ternary(e1, e2, e3) => tc_expr_tenary(ctx, e1, e2, e3),
    PointerDeref(e, typ) => tc_expr_ptr_deref(ctx, e, typ),
    StructAccess(e, field, typ) => tc_expr_struct_access(ctx, e, field, typ),
    ArrayAccess(e1, e2, typ) => tc_expr_array_access(ctx, e1, e2, typ),
    Alloc(t) => tc_expr_alloc(ctx, t),
    AllocArray(t, e) => tc_expr_alloc_array(ctx, t, e),
    PostOp(_, _) => Err(anyhow!("PostOp should not appear in final elaborated AST")),
    StructDeref(_, _, _) => Err(anyhow!(
      "StructDeref should not appear in final elaborated AST"
    )),
  }
}

// 1. expression must be pointer type
fn tc_expr_ptr_deref(
  ctx: &mut Context,
  e: &mut ast::Expr,
  ptr_typ: &mut Box<Typ>,
) -> Result<ast::Typ> {
  let typ = tc_expr(ctx, e, false)?;
  match typ {
    ast::Typ::Pointer(t) => {
      *ptr_typ = Box::new(*t.clone());
      Ok(*t)
    }
    _ => Err(anyhow!("Expression {:?} must have type pointer", e)),
  }
}

// 1. expression must be struct type
// 2. struct must be defined
// 3. field must be in struct definition
// 4. returns the type for the field
fn tc_expr_struct_access(
  ctx: &mut Context,
  e: &mut ast::Expr,
  field: &str,
  struct_typ: &mut Box<Typ>,
) -> Result<ast::Typ> {
  let str_typ = tc_expr(ctx, e, false)?;
  match &str_typ {
    ast::Typ::Struct(s) => {
      if let Some((expr_typ, _)) = ctx.get_field_typ_offset(s, field) {
        *struct_typ = Box::new(str_typ);
        Ok(expr_typ)
      } else {
        Err(anyhow!("Field {} not defined in struct {}", field, s))
      }
    }
    _ => Err(anyhow!("Expression {:?} must have type struct", e)),
  }
}

// 1. e1 must be an array type
// 2. e2 must be an int type
// 3. returns the type of the array
fn tc_expr_array_access(
  ctx: &mut Context,
  e1: &mut ast::Expr,
  e2: &mut ast::Expr,
  array_type: &mut Box<Typ>,
) -> Result<ast::Typ> {
  let typ1 = tc_expr(ctx, e1, false)?;
  let typ2 = tc_expr(ctx, e2, false)?;
  if !ctx.typ_eq(&typ2, &ast::Typ::Int) {
    return Err(anyhow!(
      "Array index expression {:?} must have type int",
      e2
    ));
  }
  match typ1 {
    ast::Typ::Array(t) => {
      *array_type = Box::new(*t.clone());
      Ok(*t)
    }
    _ => Err(anyhow!(
      "Array body expression {:?} must have type array",
      e1
    )),
  }
}

// checks that t is the correct type for an alloc expression, and
// that it is defined (if it is a struct type)
// returns Pointer(t) type
fn tc_expr_alloc(ctx: &mut Context, t: &ast::Typ) -> Result<ast::Typ> {
  use ast::Typ::*;
  let reduced_type = ctx.to_reduced_typ(t).unwrap_or(ast::Typ::Null);
  match reduced_type.clone() {
    Null => Err(anyhow!("Invalid type in alloc expression")),
    Struct(name) => {
      if ctx.struct_defined(&name) {
        Ok(Pointer(Box::new(reduced_type)))
      } else {
        Err(anyhow!(
          "undefined struct type {} cannot be allocated",
          name
        ))
      }
    }
    _ => Ok(Pointer(Box::new(reduced_type))),
  }
}

// checks that t is the correct type for an alloc_array expression, and
// that it is defined (if it is a struct type). Also checks that e is an int
// returns Array(t) type
fn tc_expr_alloc_array(ctx: &mut Context, t: &ast::Typ, e: &mut ast::Expr) -> Result<ast::Typ> {
  let typ = tc_expr(ctx, e, false)?;
  if typ != ast::Typ::Int {
    return Err(anyhow!("Array size expression {:?} must have type int", e));
  }
  if let Some(arr_type) = ctx.to_reduced_typ(t) {
    return match arr_type.clone() {
      ast::Typ::Struct(name) => {
        if ctx.struct_defined(&name) {
          Ok(ast::Typ::Array(Box::new(arr_type)))
        } else {
          Err(anyhow!(
            "undefined struct type {} cannot be allocated",
            name
          ))
        }
      }
      _ => Ok(ast::Typ::Array(Box::new(arr_type))),
    };
  }
  Err(anyhow!("Invalid array type in alloc_array expression"))
}

// var in an expression must be defined
fn tc_expr_var(ctx: &Context, var: &str) -> Result<ast::Typ> {
  if ctx.is_var_defined(var) {
    let typ = match ctx.get_var_status(var) {
      Some(status) => status.typ,
      None => return Err(anyhow!("Variable {} not defined within scope", var)),
    };
    Ok(typ)
  } else {
    Err(anyhow!("Variable {} not defined within scope", var))
  }
}

// typecheck a functional as an expression. Void returning functions are only
// allowed as top level statements.
fn tc_expr_fn_call(
  ctx: &mut Context,
  fname: &ast::FuncName,
  arglist: &mut ast::ArgList,
  is_top_level_stmt: bool,
) -> Result<ast::Typ> {
  // check if function is declared and retrieve its signature
  if !ctx.is_func_declared(fname) {
    return Err(anyhow!("Function {} not declared", fname));
  }

  // check shadowing
  if ctx.is_var_declared(fname) {
    return Err(anyhow!(
      "Function name {} shadowed in current context",
      fname
    ));
  }

  let mut fdecl = ctx.get_func_decl(fname).unwrap();

  // check if the number of arguments match the function signature
  if fdecl.param_list.0.len() != arglist.0.len() {
    return Err(anyhow!(
      "Function {} called with wrong number of arguments",
      fname
    ));
  }

  // check if the types of the arguments match the function signature
  for ((typ, arg_name), expr) in fdecl.param_list.0.iter_mut().zip(arglist.0.iter_mut()) {
    let expr_typ = tc_expr(ctx, expr, false)?;
    if !ctx.typ_eq(&expr_typ, typ) {
      return Err(anyhow!(
        "Function {} called with wrong argument type for arg: {}",
        fname,
        arg_name
      ));
    }
  }

  ctx.mark_func_as_used(fname.to_string());

  match fdecl.ret_type {
    ast::ReturnType::Void => {
      if is_top_level_stmt {
        Ok(ast::Typ::Custom("void".to_string()))
      } else {
        Err(anyhow!(
          "Function {} with void return type cannot be used as non-top level expression",
          fname
        ))
      }
    }
    ast::ReturnType::Type(t) => {
      let reduced_typ = ctx.to_reduced_typ(&t);
      if reduced_typ.is_none() {
        return Err(anyhow!("Return type {} does not exist", t));
      }
      Ok(reduced_typ.unwrap())
    }
  }
}

// 1. expressions much be valid
// 2. e1 must be a boolean expression
// 3. e2 and e3 must have the same type
// 4. e2 and e3 cannot be struct types
fn tc_expr_tenary(
  ctx: &mut Context,
  e1: &mut ast::Expr,
  e2: &mut ast::Expr,
  e3: &mut ast::Expr,
) -> Result<ast::Typ> {
  let typ1 = tc_expr(ctx, e1, false)?;
  let typ2 = tc_expr(ctx, e2, false)?;
  let typ3 = tc_expr(ctx, e3, false)?;
  if !ctx.typ_eq(&typ1, &ast::Typ::Bool) {
    Err(anyhow!("Expression {:?} must have type boolean", e1))
  } else if !ctx.typ_eq(&typ2, &typ3) {
    Err(anyhow!(
      "Expression {:?} and {:?} must have the same type",
      e2,
      e3
    ))
  } else if matches!(typ2, ast::Typ::Struct(_)) {
    Err(anyhow!("Cannot have struct type in ternary expression"))
  } else {
    Ok(if typ2 == Typ::Null { typ3 } else { typ2 })
  }
}

// 1. expressions much be valid
// 2. Not only takes boolean
// 3. Neg and BitNot only takes int
fn tc_expr_unop(ctx: &mut Context, unop: &ast::UnOp, e: &mut ast::Expr) -> Result<ast::Typ> {
  let typ = tc_expr(ctx, e, false)?;
  match unop {
    ast::UnOp::Not => {
      if ctx.typ_eq(&typ, &ast::Typ::Bool) {
        Ok(ast::Typ::Bool)
      } else {
        Err(anyhow!("Expression {:?} must have type boolean", e))
      }
    }
    ast::UnOp::Neg | ast::UnOp::BitNot => {
      if ctx.typ_eq(&typ, &ast::Typ::Int) {
        Ok(ast::Typ::Int)
      } else {
        Err(anyhow!("Expression {:?} must have type int", e))
      }
    }
  }
}

// 1. expressions much be valid
// 2. e1 and e2 must have the same type
// 3. Must follow the binop rules
// 4. e1 and e2 cannot be struct types
fn tc_expr_binop(
  ctx: &mut Context,
  e1: &mut ast::Expr,
  binop: &ast::BinOp,
  e2: &mut ast::Expr,
) -> Result<ast::Typ> {
  let typ1 = tc_expr(ctx, e1, false)?;
  let typ2 = tc_expr(ctx, e2, false)?;
  if !ctx.typ_eq(&typ1, &typ2) {
    Err(anyhow!(
      "Expression {:?} and {:?} must have the same type",
      e1,
      e2
    ))
  } else if matches!(typ1, ast::Typ::Struct(_)) {
    Err(anyhow!("Cannot have struct type in binary expression"))
  } else {
    match binop {
      // binop
      ast::BinOp::Add
      | ast::BinOp::Sub
      | ast::BinOp::Mul
      | ast::BinOp::Div
      | ast::BinOp::Mod
      | ast::BinOp::BitAnd
      | ast::BinOp::BitXor
      | ast::BinOp::BitOr
      | ast::BinOp::Shl
      | ast::BinOp::Shr => {
        if ctx.typ_eq(&typ1, &ast::Typ::Int) {
          Ok(ast::Typ::Int)
        } else {
          Err(anyhow!("Expression {:?} must have type int", e1))
        }
      }
      // relop
      ast::BinOp::LessThan | ast::BinOp::Leq | ast::BinOp::GreaterThan | ast::BinOp::Geq => {
        if ctx.typ_eq(&typ1, &ast::Typ::Int) {
          Ok(ast::Typ::Bool)
        } else {
          Err(anyhow!("Expression {:?} must have type int", e1))
        }
      }
      // polyeq
      ast::BinOp::EqEq | ast::BinOp::Uneq => Ok(ast::Typ::Bool),
      // logop
      ast::BinOp::LogicalAnd | ast::BinOp::LogicalOr => {
        if ctx.typ_eq(&typ1, &ast::Typ::Bool) {
          Ok(ast::Typ::Bool)
        } else {
          Err(anyhow!("Expression {:?} must have type boolean", e1))
        }
      }
    }
  }
}
