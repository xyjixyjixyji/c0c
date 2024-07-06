mod prog;
mod surface_prog;

use std::fmt::{Debug, Display, Error, Formatter};

pub use prog::*;
pub use surface_prog::*;

// Some display functionality. You'll likely want to implement the Display
// trait in a way that you can print your compiler's version of the AST
// in a way that closely resembles indented and annotated source code.

impl Display for Typ {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      Typ::Int => write!(fmt, "int"),
      Typ::Bool => write!(fmt, "bool"),
      Typ::Custom(ref s) => write!(fmt, "{}", s),
      Typ::Struct(ref s) => write!(fmt, "struct {}", s),
      Typ::Array(ref typ) => {
        write!(fmt, "{}[]", typ)
      }
      Typ::Pointer(ref typ) => {
        write!(fmt, "{}*", typ)
      }
      Typ::Null => write!(fmt, "NULL"),
    }
  }
}

impl Display for BinOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      BinOp::Add => write!(fmt, "+"),
      BinOp::Sub => write!(fmt, "-"),
      BinOp::Mul => write!(fmt, "*"),
      BinOp::Div => write!(fmt, "/"),
      BinOp::Mod => write!(fmt, "%"),
      BinOp::LessThan => write!(fmt, "<"),
      BinOp::Leq => write!(fmt, "<="),
      BinOp::GreaterThan => write!(fmt, ">"),
      BinOp::Geq => write!(fmt, ">="),
      BinOp::EqEq => write!(fmt, "=="),
      BinOp::Uneq => write!(fmt, "!="),
      BinOp::LogicalAnd => write!(fmt, "&&"),
      BinOp::LogicalOr => write!(fmt, "||"),
      BinOp::BitAnd => write!(fmt, "&"),
      BinOp::BitXor => write!(fmt, "^"),
      BinOp::BitOr => write!(fmt, "|"),
      BinOp::Shl => write!(fmt, "<<"),
      BinOp::Shr => write!(fmt, ">>"),
    }
  }
}

impl Display for UnOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      UnOp::Neg => write!(fmt, "-"),
      UnOp::Not => write!(fmt, "!"),
      UnOp::BitNot => write!(fmt, "~"),
    }
  }
}

impl Display for PostOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      PostOp::Inc => write!(fmt, "++"),
      PostOp::Dec => write!(fmt, "--"),
    }
  }
}

impl Display for AsnOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    match *self {
      AsnOp::Eq => write!(fmt, "="),
      AsnOp::PlusEq => write!(fmt, "+="),
      AsnOp::MinusEq => write!(fmt, "-="),
      AsnOp::TimesEq => write!(fmt, "*="),
      AsnOp::DivEq => write!(fmt, "/="),
      AsnOp::ModEq => write!(fmt, "%="),
      AsnOp::AndEq => write!(fmt, "&="),
      AsnOp::XorEq => write!(fmt, "^="),
      AsnOp::OrEq => write!(fmt, "|="),
      AsnOp::ShlEq => write!(fmt, "<<="),
      AsnOp::ShrEq => write!(fmt, ">>="),
    }
  }
}

impl Debug for Typ {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "{}", self)
  }
}

impl Debug for BinOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "{}", self)
  }
}
impl Debug for UnOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "{}", self)
  }
}

impl Debug for AsnOp {
  fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
    write!(fmt, "{}", self)
  }
}
