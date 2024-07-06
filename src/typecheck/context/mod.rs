mod func_context;
mod struct_context;
mod type_context;
mod var_context;

use std::collections::HashMap;

pub use func_context::{FunctionContext, FunctionStatus};
pub use struct_context::StructContext;
pub use type_context::TypeContext;
pub use var_context::{VarContext, VarStatus};

use anyhow::Result;

use crate::ast::{self, Typ, Var};

#[derive(Debug, Clone)]
pub struct Context {
  pub var_context: VarContext,
  pub func_context: FunctionContext,
  pub type_context: TypeContext,
  pub struct_context: StructContext,
}

impl Context {
  pub fn new(
    var_context: VarContext,
    func_context: FunctionContext,
    type_context: TypeContext,
    struct_context: StructContext,
  ) -> Self {
    Context {
      var_context,
      func_context,
      type_context,
      struct_context,
    }
  }

  pub fn clone_var_context(&mut self) -> VarContext {
    self.var_context.clone()
  }

  pub fn replace_var_context(&mut self, new_var_context: VarContext) {
    self.var_context = new_var_context;
  }
}

impl Context {
  pub fn get_var_status(&self, var: &str) -> Option<VarStatus> {
    self.var_context.get(var)
  }

  pub fn declare_var(&mut self, s: ast::Var, t: ast::Typ) {
    self.var_context.declare(s, t);
  }

  pub fn define_var(&mut self, s: ast::Var, t: ast::Typ) {
    self.var_context.define(s, t);
  }

  pub fn remove_var(&mut self, s: &ast::Var) {
    self.var_context.remove(s);
  }

  pub fn update_defined_var(&mut self, new_defn: &Vec<ast::Var>) {
    self.var_context.update_defined_vars(new_defn);
  }

  pub fn declared_vars(&self) -> Vec<ast::Var> {
    self.var_context.declared()
  }

  pub fn is_var_declared(&self, var: &str) -> bool {
    self.var_context.is_declared(var)
  }

  pub fn is_var_defined(&self, var: &str) -> bool {
    self.var_context.is_defined(var)
  }
}

impl Context {
  pub fn declare_func(&mut self, name: ast::FuncName, fdecl: &ast::Fdecl) {
    self.func_context.declare(name, fdecl);
  }

  pub fn define_lib_functions(&mut self) {
    self.func_context.define_lib_functions();
  }

  pub fn declare_main(&mut self) {
    self.func_context.declare_main();
  }

  pub fn define_func(&mut self, name: ast::FuncName) {
    self.func_context.define(name);
  }

  pub fn define_all_func(&mut self) {
    self.func_context.define_all();
  }

  pub fn get_func_decl(&self, name: &str) -> Option<ast::Fdecl> {
    self.func_context.get_decl(name)
  }

  pub fn get_func_status(&self, name: &str) -> Option<FunctionStatus> {
    self.func_context.get_func_status(name)
  }

  pub fn all_used_fn_are_defined(&self) -> bool {
    self.func_context.all_used_defined()
  }

  pub fn remove_unused_fdecl(&mut self) {
    self.func_context.remove_unused_fdecl()
  }

  pub fn is_func_declared(&self, name: &str) -> bool {
    self.func_context.is_declared(name)
  }

  pub fn is_func_defined(&self, name: ast::FuncName) -> bool {
    self.func_context.is_defined(name)
  }

  pub fn mark_func_as_used(&mut self, name: ast::FuncName) {
    self.func_context.mark_as_used(name);
  }

  pub fn mark_all_func_as_header(&mut self) {
    self.func_context.mark_all_as_header();
  }

  pub fn check_main_signature(&self) -> Result<()> {
    self.func_context.check_main_signature(&self.type_context)
  }

  pub fn check_param_names(&self, fdecl: &ast::Fdecl) -> bool {
    self
      .func_context
      .check_param_names(&self.type_context, fdecl)
  }

  pub fn check_dup_decl(&self, fdecl: &ast::Fdecl, other_fdecl: &ast::Fdecl) -> bool {
    self
      .func_context
      .check_dup_decl(&self.type_context, fdecl, other_fdecl)
  }
}

impl Context {
  pub fn declare_typedef(&mut self, new_name: ast::TypeName, mapped_type: ast::Typ) {
    self.type_context.declare_typedef(new_name, mapped_type);
  }

  pub fn type_exists(&self, name: &str) -> bool {
    self.type_context.type_exists(name)
  }

  pub fn to_reduced_typ(&self, typ: &ast::Typ) -> Option<ast::Typ> {
    self.type_context.to_reduced_typ(typ)
  }

  pub fn typ_to_return_type(&self, typ: &ast::Typ) -> ast::ReturnType {
    self.type_context.typ_to_return_type(typ)
  }

  pub fn return_type_eq(&self, ret1: &ast::ReturnType, ret2: &ast::ReturnType) -> bool {
    self.type_context.return_type_eq(ret1, ret2)
  }

  pub fn typ_eq(&self, typ1: &ast::Typ, typ2: &ast::Typ) -> bool {
    self.type_context.typ_eq(typ1, typ2)
  }

  pub fn get_type_size(&self, typ: &ast::Typ) -> u64 {
    match typ {
      ast::Typ::Int | ast::Typ::Bool => 4,
      ast::Typ::Pointer(_) | ast::Typ::Array(_) | ast::Typ::Null => 8,
      ast::Typ::Struct(name) => self.struct_context.get_struct_size(name).unwrap(),
      ast::Typ::Custom(name) => self.get_type_size(&self.type_context.get_typedef(name).unwrap()),
    }
  }

  pub fn get_type_align_size(&self, typ: &ast::Typ) -> u64 {
    match typ {
      ast::Typ::Int | ast::Typ::Bool => 4,
      ast::Typ::Pointer(_) | ast::Typ::Array(_) | ast::Typ::Null => 8,
      ast::Typ::Struct(name) => self.struct_context.get_struct_align_size(name).unwrap() as u64,
      ast::Typ::Custom(name) => {
        self.get_type_align_size(&self.type_context.get_typedef(name).unwrap())
      }
    }
  }

  pub fn get_type_dest_size(&self, typ: &ast::Typ) -> u32 {
    match typ {
      ast::Typ::Int | ast::Typ::Bool => 4,
      ast::Typ::Pointer(_) | ast::Typ::Array(_) | ast::Typ::Null | ast::Typ::Struct(_) => 8,
      ast::Typ::Custom(name) => {
        self.get_type_dest_size(&self.type_context.get_typedef(name).unwrap())
      }
    }
  }
}

impl Context {
  pub fn declare_struct(&mut self, name: ast::TypeName) {
    self.struct_context.declare(name);
  }

  pub fn define_struct(
    &mut self,
    name: ast::TypeName,
    field_offsets: HashMap<Var, (Typ, u64)>,
    struct_size: u64,
    struct_align_size: u8,
  ) {
    self
      .struct_context
      .define(name, field_offsets, struct_size, struct_align_size);
  }

  pub fn struct_defined(&self, name: &str) -> bool {
    self.struct_context.is_defined(name)
  }

  pub fn get_field_typ_offset(
    &self,
    struct_name: &str,
    field_name: &str,
  ) -> Option<(ast::Typ, u64)> {
    self
      .struct_context
      .get_field_typ_offset(struct_name, field_name)
  }
}
