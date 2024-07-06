use std::collections::HashMap;

use crate::ast;

#[derive(Debug, Clone)]
pub struct TypeContext {
  typedef_table: HashMap<ast::TypeName, ast::Typ>,
}

impl TypeContext {
  pub fn new() -> Self {
    TypeContext {
      typedef_table: HashMap::new(),
    }
  }

  /// Adds a typedef to the context. mapped_type should be checked before, and should
  /// only include built-in types.
  pub(super) fn declare_typedef(&mut self, new_name: ast::TypeName, mapped_type: ast::Typ) {
    self.typedef_table.insert(new_name, mapped_type);
  }

  /// Returns true if a type name is declared.
  pub(super) fn type_exists(&self, name: &str) -> bool {
    self.typedef_table.contains_key(name)
  }

  pub fn to_reduced_typ(&self, typ: &ast::Typ) -> Option<ast::Typ> {
    match typ {
      ast::Typ::Int | ast::Typ::Bool | ast::Typ::Struct(_) | ast::Typ::Null => Some(typ.clone()),
      ast::Typ::Custom(name) => self.get_typedef(name),
      ast::Typ::Array(inner_typ) => self
        .to_reduced_typ(inner_typ)
        .map(|t| ast::Typ::Array(Box::new(t))),
      ast::Typ::Pointer(inner_typ) => self
        .to_reduced_typ(inner_typ)
        .map(|t| ast::Typ::Pointer(Box::new(t))),
    }
  }

  pub fn typ_to_return_type(&self, typ: &ast::Typ) -> ast::ReturnType {
    match typ {
      ast::Typ::Custom(name) => {
        let typ = self.get_typedef(name);
        match typ {
          Some(t) => ast::ReturnType::Type(t),
          None => panic!("Invalid type: {:?}", typ),
        }
      }
      _ => ast::ReturnType::Type(typ.clone()),
    }
  }

  pub fn typ_eq(&self, typ1: &ast::Typ, typ2: &ast::Typ) -> bool {
    let typ1 = self.to_reduced_typ(typ1).unwrap();
    let typ2 = self.to_reduced_typ(typ2).unwrap();
    typ1 == typ2
      || (matches!(typ1, ast::Typ::Pointer(_)) && typ2 == ast::Typ::Null)
      || (matches!(typ2, ast::Typ::Pointer(_)) && typ1 == ast::Typ::Null)
  }

  pub fn return_type_eq(&self, ret1: &ast::ReturnType, ret2: &ast::ReturnType) -> bool {
    use ast::ReturnType::Type;
    if ret1 == ret2 {
      true
    } else {
      match (ret1, ret2) {
        (Type(t1), Type(t2)) => self.typ_eq(t1, t2),
        _ => false,
      }
    }
  }

  /// Returns the mapped built-in type for a given type name, or None if it doesn't exist in the context.
  pub(super) fn get_typedef(&self, typename: &str) -> Option<ast::Typ> {
    self.typedef_table.get(typename).cloned()
  }
}
