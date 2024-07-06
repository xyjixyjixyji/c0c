use crate::ast;

use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct VarStatus {
  pub typ: ast::Typ,
  pub defn: bool,
}

/// We use a mutable type-checking context, modified as the typechecker progresses.
/// This is per-scope data structure
///
/// Note: no shadowing in the nested block is allowed
#[derive(Clone, Debug, Default)]
pub struct VarContext {
  var_table: HashMap<ast::Var, VarStatus>,
}

impl VarContext {
  pub fn new() -> Self {
    VarContext {
      var_table: HashMap::new(),
    }
  }

  /// Search through all of the currently available scopes for
  /// this variable, starting from the deepest and going up.
  pub fn get(&self, var: &str) -> Option<VarStatus> {
    self.var_table.get(var).cloned()
  }

  /// Declare a variable, in the current scope.
  pub fn declare(&mut self, s: ast::Var, t: ast::Typ) {
    let status = VarStatus {
      typ: t,
      defn: false,
    };
    self.var_table.insert(s, status);
  }

  /// Define a variable, in the current scope.
  pub fn define(&mut self, s: ast::Var, t: ast::Typ) {
    let status = VarStatus { typ: t, defn: true };
    self.var_table.insert(s, status);
  }

  pub fn remove(&mut self, s: &ast::Var) {
    self.var_table.remove(s);
  }

  /// Update the current scope with the new list of defined variables.
  pub fn update_defined_vars(&mut self, new_defn: &Vec<ast::Var>) {
    for var in new_defn {
      if let Some(status) = self.var_table.get_mut(var) {
        status.defn = true;
      }
    }
  }

  /// Returns true if a variable is declared in the current scope.
  pub fn is_declared(&self, var: &str) -> bool {
    self.var_table.get(var).is_some()
  }

  /// Returns true if a variable is defined in the current scope.
  pub fn is_defined(&self, var: &str) -> bool {
    matches!(self.var_table.get(var), Some(VarStatus { defn: true, .. }))
  }

  /// Returns a list of all declared variables in the current scope.
  pub fn declared(&self) -> Vec<ast::Var> {
    self.var_table.keys().cloned().collect()
  }
}
