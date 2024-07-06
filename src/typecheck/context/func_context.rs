use anyhow::{anyhow, Result};

use std::collections::{HashMap, HashSet};

use crate::{
  ast,
  elaboration::{LIBRARY_ABORT, LIBRARY_CALLOC, LIBRARY_RAISE},
};

use super::type_context::TypeContext;

/// The status of a function declaration.
#[derive(Clone, Debug)]
pub struct FunctionStatus {
  /// The function declaration.
  pub(super) fdecl: ast::Fdecl,
  /// Whether the function is defined.
  pub(super) defn: bool,
  /// Whether the function is defined in a header.
  pub(super) is_header: bool,
}

impl FunctionStatus {
  pub fn is_header(&self) -> bool {
    self.is_header
  }
}

/// We use a global typedef and fdecl context, modified as the typechecker progresses.
///
/// This is a global data structure, NOT per-scope.
#[derive(Clone, Debug)]
pub struct FunctionContext {
  function_status_table: HashMap<ast::FuncName, FunctionStatus>,
  used_functions: HashSet<ast::FuncName>,
}

impl FunctionContext {
  pub fn new() -> Self {
    FunctionContext {
      function_status_table: HashMap::new(),
      used_functions: HashSet::new(),
    }
  }

  pub fn get_func_status(&self, name: &str) -> Option<FunctionStatus> {
    self.function_status_table.get(name).cloned()
  }

  /// Adds a fdecl to the context. Signature should be checked before adding,
  /// and should only include built-in types.
  pub(super) fn declare(&mut self, name: ast::FuncName, fdecl: &ast::Fdecl) {
    let defn = self
      .function_status_table
      .get(&name)
      .map_or(false, |f| f.defn);

    self.function_status_table.insert(
      name,
      FunctionStatus {
        fdecl: fdecl.clone(),
        defn,
        is_header: false,
      },
    );
  }

  /// Marks a function as defined.
  pub(super) fn define(&mut self, name: ast::FuncName) {
    if let Some(fdecl) = self.function_status_table.get_mut(&name) {
      fdecl.defn = true;
    }
  }

  /// Returns true if a function name is declared.
  pub(super) fn is_declared(&self, name: &str) -> bool {
    self.function_status_table.contains_key(name)
  }

  /// Returns true if a function is defined.
  pub(super) fn is_defined(&self, name: ast::FuncName) -> bool {
    self
      .function_status_table
      .get(&name)
      .map_or(false, |fdecl| fdecl.defn)
  }

  pub fn get_decl(&self, name: &str) -> Option<ast::Fdecl> {
    self
      .function_status_table
      .get(name)
      .map(|fdecl| fdecl.fdecl.clone())
  }

  /// Marks a function as "used" in the program.
  pub(super) fn mark_as_used(&mut self, name: ast::FuncName) {
    self.used_functions.insert(name);
  }

  /// Checks whether all used functions are defined.
  pub(super) fn all_used_defined(&self) -> bool {
    self
      .used_functions
      .iter()
      .all(|fname| self.function_status_table.get(fname).unwrap().defn)
  }

  pub(super) fn remove_unused_fdecl(&mut self) {
    self.function_status_table.retain(|_, fdecl| fdecl.defn);
  }

  /// Marks all functions as defined.
  pub fn define_all(&mut self) {
    for (_, fdecl) in self.function_status_table.iter_mut() {
      fdecl.defn = true;
    }
  }

  pub fn declare_main(&mut self) {
    let main_fdecl = FunctionStatus {
      fdecl: ast::Fdecl {
        func_name: "main".to_string(),
        param_list: ast::ParamList(vec![]),
        ret_type: ast::ReturnType::Type(ast::Typ::Int),
      },
      defn: self
        .function_status_table
        .get("main")
        .map_or(false, |s| s.defn),
      is_header: self
        .function_status_table
        .get("main")
        .map_or(false, |s| s.defn),
    };
    self
      .function_status_table
      .insert("main".to_string(), main_fdecl);
  }

  pub fn define_lib_functions(&mut self) {
    for f in [LIBRARY_ABORT, LIBRARY_CALLOC, LIBRARY_RAISE].iter() {
      let decl = FunctionStatus {
        fdecl: ast::Fdecl {
          func_name: f.to_string(),
          param_list: ast::ParamList(vec![]),
          ret_type: ast::ReturnType::Void,
        },
        defn: true,
        is_header: false,
      };

      self.function_status_table.insert(f.to_string(), decl);
    }
  }

  pub fn mark_all_as_header(&mut self) {
    for (_, fdecl) in self.function_status_table.iter_mut() {
      fdecl.is_header = true;
    }
  }

  pub fn check_main_signature(&self, typ_ctx: &TypeContext) -> Result<()> {
    if let Some(fdecl) = self.get_decl("main") {
      if typ_ctx.return_type_eq(&fdecl.ret_type, &ast::ReturnType::Type(ast::Typ::Int))
        && fdecl.param_list.0.is_empty()
        && self.is_defined("main".to_string())
      {
        Ok(())
      } else {
        Err(anyhow!("Invalid main function signature"))
      }
    } else {
      Err(anyhow!("main function not defined"))
    }
  }

  pub fn check_param_names(&self, typ_ctx: &TypeContext, fdecl: &ast::Fdecl) -> bool {
    let mut param_names: HashSet<String> = HashSet::new();
    fdecl
      .param_list
      .0
      .iter()
      .all(|(_, name)| !typ_ctx.type_exists(name) && param_names.insert(name.clone()))
  }

  pub fn check_dup_decl(
    &self,
    typ_ctx: &TypeContext,
    fdecl1: &ast::Fdecl,
    fdecl2: &ast::Fdecl,
  ) -> bool {
    // check return type
    if !typ_ctx.return_type_eq(&fdecl1.ret_type, &fdecl2.ret_type) {
      return false;
    }

    // check param list
    let param_list1 = &fdecl1.param_list.0;
    let param_list2 = &fdecl2.param_list.0;

    if param_list1.len() != param_list2.len() {
      return false;
    }

    for i in 0..param_list1.len() {
      let typ1 = typ_ctx.to_reduced_typ(&param_list1[i].0);
      let typ2 = typ_ctx.to_reduced_typ(&param_list2[i].0);
      match (typ1, typ2) {
        (Some(t1), Some(t2)) => {
          if t1 != t2 {
            return false;
          }
        }
        _ => return false,
      }
    }

    true
  }
}
