use crate::ast::{self, Typ, Var};
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct StructStatus {
  pub is_defined: bool,
  pub field_offsets: HashMap<Var, (Typ, u64)>,
  pub struct_size: u64,
  pub align_size: u8,
}

#[derive(Clone, Debug)]
pub struct StructContext {
  struct_table: HashMap<ast::TypeName, StructStatus>,
}

impl StructContext {
  pub fn new() -> Self {
    StructContext {
      struct_table: HashMap::new(),
    }
  }

  pub(super) fn declare(&mut self, name: ast::TypeName) {
    self.struct_table.entry(name).or_insert(StructStatus {
      is_defined: false,
      field_offsets: HashMap::new(),
      struct_size: 0,
      align_size: 0,
    });
  }

  pub(super) fn define(
    &mut self,
    name: ast::TypeName,
    field_offsets: HashMap<Var, (Typ, u64)>,
    struct_size: u64,
    struct_align_size: u8,
  ) {
    self.struct_table.insert(
      name,
      StructStatus {
        is_defined: true,
        field_offsets,
        struct_size,
        align_size: struct_align_size,
      },
    );
  }

  pub(super) fn is_defined(&self, name: &str) -> bool {
    self.struct_table.get(name).map_or(false, |s| s.is_defined)
  }

  pub(super) fn get_field_typ_offset(
    &self,
    struct_name: &str,
    field_name: &str,
  ) -> Option<(Typ, u64)> {
    if !self.struct_table.contains_key(struct_name) {
      return None;
    }
    self
      .struct_table
      .get(struct_name)
      .unwrap()
      .field_offsets
      .get(field_name)
      .cloned()
  }

  pub(super) fn get_struct_size(&self, struct_name: &str) -> Option<u64> {
    self.struct_table.get(struct_name).map(|s| s.struct_size)
  }

  pub(super) fn get_struct_align_size(&self, struct_name: &str) -> Option<u8> {
    self.struct_table.get(struct_name).map(|s| s.align_size)
  }
}
