//! The surface AST for the language. This is the data structure that the parser
//! will produce. Note that this is **before** elaboration, so it is not yet
//! the final AST that the rest of the compiler will consume.

use super::{
  prog::{AsnOp, Expr, Fdecl, FuncName, ParamList, ReturnType, Typ, Typedef, Var},
  Sdecl, Sdefn,
};

pub type SurfaceProgram = Vec<SurfaceGdecl>;

#[derive(Clone, Debug)]
pub enum SurfaceGdecl {
  Fdecl(Fdecl),
  Fdefn(SurfaceFdefn),
  Sdecl(Sdecl),
  Sdefn(Sdefn),
  Typedef(Typedef),
}

#[derive(Clone, Debug)]
pub struct SurfaceFdefn {
  pub ret_type: ReturnType,
  pub func_name: FuncName,
  pub param_list: ParamList,
  pub body: SurfaceStmt, // SurfaceStmt::block
}

// Surface statement before elaboration
#[derive(Clone, Debug)]
pub enum SurfaceStmt {
  // Simp
  Decl(Typ, Var, Option<Expr>),
  // we represent the lvalue as Expr in surface syntax for Asgn and PostOp
  // to make the lalrpop parser happy
  //
  // ((x)) = 2
  Asgn(Expr, AsnOp, Expr),

  Expr(Expr),

  // block
  Block(Vec<SurfaceStmt>),

  // control
  If(Expr, Box<SurfaceStmt>, Option<Box<SurfaceStmt>>),
  While(Expr, Box<SurfaceStmt>),
  For(
    Option<Box<SurfaceStmt>>,
    Expr,
    Option<Box<SurfaceStmt>>,
    Box<SurfaceStmt>,
  ),
  Ret(Option<Expr>),
  Assert(Expr),
}
