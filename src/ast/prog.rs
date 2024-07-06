//! The actual AST for the language. This is the data structure that the elaboration
//! will produce, and that the rest of the compiler will consume.

use std::fmt::Debug;

pub type Var = String;
pub type FuncName = String;
pub type TypeName = String;
pub type Param = (Typ, Var);
pub type Arg = Expr;

pub type Program = Vec<Gdecl>;

#[derive(Clone, Debug)]
pub enum Gdecl {
  Fdecl(Fdecl),
  Fdefn(Fdefn),
  Sdecl(Sdecl),
  Sdefn(Sdefn),
  Typedef(Typedef),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Fdecl {
  pub ret_type: ReturnType,
  pub func_name: FuncName,
  pub param_list: ParamList,
}

#[derive(Clone, Debug)]
pub struct Fdefn {
  pub ret_type: ReturnType,
  pub func_name: FuncName,
  pub param_list: ParamList,
  pub body: Stmts, // block
}

#[derive(Clone, Debug)]
pub struct Sdecl {
  pub struct_name: TypeName,
}

#[derive(Clone, Debug)]
pub struct Sdefn {
  pub struct_name: TypeName,
  pub field_list: FieldList,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Typ {
  Int,
  Bool,
  Custom(String),
  Struct(String),
  Pointer(Box<Typ>),
  Array(Box<Typ>),
  Null,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
  Add,         // +
  Sub,         // -
  Mul,         // *
  Div,         // /
  Mod,         // %
  LessThan,    // <
  Leq,         // <=
  GreaterThan, // >
  Geq,         // >=
  EqEq,        // ==
  Uneq,        // !=
  LogicalAnd,  // &&
  LogicalOr,   // ||
  BitAnd,      // &
  BitXor,      // ^
  BitOr,       // |
  Shl,         // <<
  Shr,         // >>
}

impl BinOp {
  pub fn is_commutative(&self) -> bool {
    use BinOp::*;
    matches!(
      self,
      Add | Mul | EqEq | Uneq | LogicalAnd | LogicalOr | BitAnd | BitXor | BitOr
    )
  }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
  Neg,    // -
  Not,    // !
  BitNot, // ~
}
#[allow(clippy::enum_variant_names)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub enum AsnOp {
  Eq,      // =
  PlusEq,  // +=
  MinusEq, // -=
  TimesEq, // *=
  DivEq,   // /=
  ModEq,   // %=
  AndEq,   // &=
  XorEq,   // ^=
  OrEq,    // |=
  ShlEq,   // <<=
  ShrEq,   // >>=
}

/// Helper function to convert assignment operators to their original operator.
pub fn to_op(op: AsnOp) -> Option<BinOp> {
  use AsnOp::*;
  use BinOp::*;

  match op {
    Eq => None,
    PlusEq => Some(Add),
    MinusEq => Some(Sub),
    TimesEq => Some(Mul),
    DivEq => Some(Div),
    ModEq => Some(Mod),
    AndEq => Some(BitAnd),
    XorEq => Some(BitXor),
    OrEq => Some(BitOr),
    ShlEq => Some(Shl),
    ShrEq => Some(Shr),
  }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum PostOp {
  Inc, // ++
  Dec, // --
}

/// Helper function to convert post operators to their original operator.
impl PostOp {
  pub fn to_asnop(self) -> AsnOp {
    use AsnOp::*;
    match self {
      PostOp::Inc => PlusEq,
      PostOp::Dec => MinusEq,
    }
  }

  pub fn to_op(self) -> BinOp {
    use BinOp::*;
    match self {
      PostOp::Inc => Add,
      PostOp::Dec => Sub,
    }
  }
}

#[derive(Clone, Debug)]
pub struct Typedef {
  pub orig_type: Typ,
  pub new_type_name: TypeName,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ReturnType {
  Void,
  Type(Typ),
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParamList(pub Vec<Param>);

pub type FieldList = ParamList;

#[derive(Clone, Debug)]
pub struct ArgList(pub Vec<Arg>);

#[derive(Clone, Debug)]
pub enum Stmts {
  NewScopeStmts(Vec<Stmt>),
  NormalStmts(Vec<Stmt>),
}

/// Statement (declare variable, define, assign, block, etc.) after elaboration
/// <Simp> / <Control> / <Block>
#[derive(Clone, Debug)]
pub enum Stmt {
  // Simp
  Decl(Typ, Var, Option<Expr>),
  // after elaboration, we should only have simple Asgn statements in the AST (e.g. var = expr)
  Asgn(Var, Expr),
  // *x = 1; x->y = 2; x[0] = 3; *x++; etc.
  AsgnMem(Expr, Expr, AsnOp),
  Expr(Expr),

  // block
  // bool represents whether this block contains an elaborated for-loop
  Block(Stmts, bool),

  // control
  If(Expr, Stmts, Option<Stmts>),
  While(Expr, Stmts),
  Ret(Option<Expr>),
}

/// Expression tree.
#[derive(Clone, Debug)]
pub enum Expr {
  Null,
  Number(i32),
  Variable(Var),
  Binop(Box<Expr>, BinOp, Box<Expr>),
  Unop(UnOp, Box<Expr>),
  Ternary(Box<Expr>, Box<Expr>, Box<Expr>),
  FunctionCall(FuncName, ArgList),
  True,
  False,

  Alloc(Typ),
  AllocArray(Typ, Box<Expr>),
  // struct.field
  // Typ represents the type of the struct
  StructAccess(Box<Expr>, Var, Box<Typ>),
  // struct_ptr->field
  // Typ represents the type of the struct
  StructDeref(Box<Expr>, Var, Box<Typ>),
  // *ptr
  // Typ represents the type of the pointer
  PointerDeref(Box<Expr>, Box<Typ>),
  // arr[expr]
  // Typ represents the type of array element
  ArrayAccess(Box<Expr>, Box<Expr>, Box<Typ>),

  // This is a parser hack to support precedence rules
  // such as *p++;
  // this variant should not appear in the final elaborated AST
  PostOp(Box<Expr>, PostOp),
}

impl Expr {
  pub fn is_lvalue(&self) -> bool {
    use Expr::*;
    match self {
      Variable(_) => true,
      StructAccess(e, _, _) => e.is_lvalue(),
      PointerDeref(e, _) => e.is_lvalue(),
      StructDeref(e, _, _) => e.is_lvalue(),
      ArrayAccess(e, _, _) => e.is_lvalue(),
      _ => false,
    }
  }
}

impl Stmts {
  pub fn into_stmts(self) -> Vec<Stmt> {
    match self {
      Stmts::NewScopeStmts(stmts) => stmts,
      Stmts::NormalStmts(stmts) => stmts,
    }
  }

  pub fn get_stmts_ref(&mut self) -> &mut Vec<Stmt> {
    match self {
      Stmts::NewScopeStmts(stmts) => stmts,
      Stmts::NormalStmts(stmts) => stmts,
    }
  }
}

impl std::fmt::Display for Fdecl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?} {}(", self.ret_type, self.func_name)?;
    for (i, (typ, var)) in self.param_list.0.iter().enumerate() {
      if i != 0 {
        write!(f, ", ")?;
      }
      write!(f, "{} {}", typ, var)?;
    }
    write!(f, ")")
  }
}

impl std::fmt::Display for Fdefn {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{:?} {}(", self.ret_type, self.func_name)?;
    for (i, (typ, var)) in self.param_list.0.iter().enumerate() {
      if i != 0 {
        write!(f, ", ")?;
      }
      write!(f, "{} {}", typ, var)?;
    }
    write!(f, ") {{\n{}\n}}", self.body)
  }
}

impl std::fmt::Display for Stmts {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Stmts::NewScopeStmts(stmts) => {
        writeln!(f, "{{")?;
        for stmt in stmts {
          writeln!(f, "{:?}", stmt)?;
        }
        write!(f, "}}")
      }
      Stmts::NormalStmts(stmts) => {
        for stmt in stmts {
          writeln!(f, "{:?}", stmt)?;
        }
        Ok(())
      }
    }
  }
}
