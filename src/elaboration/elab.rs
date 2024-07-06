use crate::ast::{
  self, to_op, ArgList, AsnOp, BinOp, Expr, PostOp, Program, Stmt, Stmts, SurfaceProgram,
  SurfaceStmt, UnOp,
};

use super::LIBRARY_ABORT;

/// Elaborate a surface program to a program, i.e. final AST.
pub fn elab_program(surface_program: SurfaceProgram) -> Program {
  surface_program.into_iter().map(elab_gdecl).collect()
}

fn elab_gdecl(gdecl: ast::SurfaceGdecl) -> ast::Gdecl {
  match gdecl {
    ast::SurfaceGdecl::Fdecl(fdecl) => ast::Gdecl::Fdecl(fdecl),
    ast::SurfaceGdecl::Fdefn(fdefn) => ast::Gdecl::Fdefn(elab_fdefn(fdefn)),
    ast::SurfaceGdecl::Typedef(tdef) => ast::Gdecl::Typedef(tdef),
    ast::SurfaceGdecl::Sdecl(sdecl) => ast::Gdecl::Sdecl(sdecl),
    ast::SurfaceGdecl::Sdefn(sdefn) => ast::Gdecl::Sdefn(sdefn),
  }
}

fn elab_fdefn(fdefn: ast::SurfaceFdefn) -> ast::Fdefn {
  let ast::SurfaceFdefn {
    ret_type,
    func_name,
    param_list,
    body,
  } = fdefn;
  let mut body = match body {
    SurfaceStmt::Block(b) => b,
    _ => panic!("Function body must be a block!"),
  };

  // if body does not end with a return statement, we implicitly add one
  if ret_type == ast::ReturnType::Void && !matches!(body.last(), Some(SurfaceStmt::Ret(_))) {
    body.push(SurfaceStmt::Ret(None));
  }

  ast::Fdefn {
    ret_type,
    func_name,
    param_list,
    body: Stmts::NewScopeStmts(elab_stmts(body)),
  }
}

// currently dummy elaboration
fn elab_stmts(surface_stmts: Vec<SurfaceStmt>) -> Vec<Stmt> {
  surface_stmts.into_iter().map(elab_stmt).collect()
}

fn elab_stmt(s: SurfaceStmt) -> Stmt {
  match s {
    SurfaceStmt::Decl(typ, var, expr) => Stmt::Decl(typ, var, expr.map(elab_expr)),
    SurfaceStmt::Asgn(lvalue, asnop, expr) => elab_asgn(elab_expr(lvalue), asnop, elab_expr(expr)),
    SurfaceStmt::Block(stmts) => Stmt::Block(Stmts::NewScopeStmts(elab_stmts(stmts)), false),
    SurfaceStmt::If(expr, s1, s2) => elab_if(elab_expr(expr), *s1, s2),
    SurfaceStmt::While(expr, stmt) => Stmt::While(
      elab_expr(expr),
      Stmts::NewScopeStmts(surface_block_to_vec(*stmt)),
    ),
    SurfaceStmt::For(init, e, step, body) => elab_for(init, elab_expr(e), step, *body),
    SurfaceStmt::Ret(expr) => {
      if let Some(expr) = expr {
        Stmt::Ret(Some(elab_expr(expr)))
      } else {
        Stmt::Ret(None)
      }
    }
    SurfaceStmt::Expr(expr) => {
      if let Expr::PostOp(e, op) = expr {
        elab_postop(elab_expr(*e), op)
      } else {
        Stmt::Expr(elab_expr(expr))
      }
    }
    SurfaceStmt::Assert(expr) => elab_assert(elab_expr(expr)),
  }
}

/// Elaborate a += b to a = a + b, and similar asnops
fn elab_asgn(lvalue: Expr, asnop: AsnOp, expr: Expr) -> Stmt {
  if let Expr::Variable(v) = lvalue {
    if asnop == AsnOp::Eq {
      Stmt::Asgn(v, expr)
    } else {
      let binop = to_op(asnop).unwrap();
      Stmt::Asgn(
        v.clone(),
        Expr::Binop(Box::new(Expr::Variable(v)), binop, Box::new(expr)),
      )
    }
  } else if lvalue.is_lvalue() {
    Stmt::AsgnMem(lvalue, expr, asnop)
  } else {
    panic!("Invalid lvalue: {:?}", lvalue)
  }
}

/// Elaborate a++ to a = a + 1, a-- to a = a - 1
fn elab_postop(lvalue: Expr, postop: PostOp) -> Stmt {
  if let Expr::Variable(v) = lvalue {
    let binop = postop.to_op();
    Stmt::Asgn(
      v.clone(),
      Expr::Binop(
        Box::new(Expr::Variable(v)),
        binop,
        Box::new(Expr::Number(1)),
      ),
    )
  } else if lvalue.is_lvalue() {
    let asnop = postop.to_asnop();
    Stmt::AsgnMem(lvalue.clone(), Expr::Number(1), asnop)
  } else {
    panic!("Invalid lvalue: {:?}", lvalue)
  }
}

/// Elaborate If statement
fn elab_if(expr: Expr, s1: SurfaceStmt, s2: Option<Box<SurfaceStmt>>) -> Stmt {
  Stmt::If(
    expr,
    Stmts::NewScopeStmts(surface_block_to_vec(s1)),
    s2.map(|b| Stmts::NewScopeStmts(surface_block_to_vec(*b))),
  )
}

/// Elaborate For statement
fn elab_for(
  init: Option<Box<SurfaceStmt>>,
  e: Expr,
  step: Option<Box<SurfaceStmt>>,
  body: SurfaceStmt,
) -> Stmt {
  let mut main_block = vec![];
  let mut while_block = vec![Stmt::Block(
    Stmts::NewScopeStmts(surface_block_to_vec(body)),
    false,
  )];

  if let Some(init) = init {
    main_block.push(elab_stmt(*init));
  }
  if let Some(step) = step {
    if let SurfaceStmt::Decl(_, _, _) = *step {
      panic!("For-loop step cannot be a declaration");
    }
    while_block.push(elab_stmt(*step));
  }
  // in the for-loop, the init shares the scope with the body
  main_block.push(Stmt::While(e, Stmts::NormalStmts(while_block)));
  Stmt::Block(Stmts::NewScopeStmts(main_block), true)
}

fn elab_assert(e: Expr) -> Stmt {
  Stmt::If(
    Expr::Unop(UnOp::Not, Box::new(e)),
    Stmts::NewScopeStmts(vec![Stmt::Expr(Expr::FunctionCall(
      LIBRARY_ABORT.to_string(),
      ArgList(vec![]),
    ))]),
    None,
  )
}

/// Elaborate expressions
fn elab_expr(e: Expr) -> Expr {
  match e {
    Expr::Binop(e1, op, e2) => {
      let e1_after = elab_expr(*e1);
      let e2_after = elab_expr(*e2);
      match op {
        BinOp::LogicalAnd => Expr::Ternary(
          Box::new(e1_after),
          Box::new(e2_after),
          Box::new(Expr::False),
        ),
        BinOp::LogicalOr => {
          Expr::Ternary(Box::new(e1_after), Box::new(Expr::True), Box::new(e2_after))
        }
        _ => Expr::Binop(Box::new(e1_after), op, Box::new(e2_after)),
      }
    }
    Expr::Unop(op, e) => Expr::Unop(op, Box::new(elab_expr(*e))),
    Expr::Ternary(e1, e2, e3) => Expr::Ternary(
      Box::new(elab_expr(*e1)),
      Box::new(elab_expr(*e2)),
      Box::new(elab_expr(*e3)),
    ),
    Expr::FunctionCall(name, arglist) => Expr::FunctionCall(
      name,
      ArgList(arglist.0.into_iter().map(elab_expr).collect::<Vec<_>>()),
    ),
    // elab struct->a into (*struct).a
    Expr::StructDeref(struct_expr, field, struct_type) => Expr::StructAccess(
      Box::new(Expr::PointerDeref(
        Box::new(elab_expr(*struct_expr)),
        struct_type.clone(),
      )),
      field,
      struct_type,
    ),
    Expr::PointerDeref(e, typ) => Expr::PointerDeref(Box::new(elab_expr(*e)), typ),
    Expr::StructAccess(e, field, typ) => Expr::StructAccess(Box::new(elab_expr(*e)), field, typ),
    Expr::ArrayAccess(e1, e2, typ) => {
      Expr::ArrayAccess(Box::new(elab_expr(*e1)), Box::new(elab_expr(*e2)), typ)
    }
    Expr::AllocArray(t, e) => Expr::AllocArray(t, Box::new(elab_expr(*e))),
    Expr::PostOp(e, op) => Expr::PostOp(Box::new(elab_expr(*e)), op),
    _ => e,
  }
}

fn surface_block_to_vec(s: SurfaceStmt) -> Vec<Stmt> {
  match s {
    SurfaceStmt::Block(b) => elab_stmts(b),
    _ => vec![elab_stmt(s)],
  }
}
