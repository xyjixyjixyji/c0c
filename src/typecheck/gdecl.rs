use std::collections::HashMap;

use super::{context::Context, stmt::tc_stmts};
use crate::ast::{self, Fdecl};

use anyhow::{anyhow, Ok, Result};

pub(super) fn tc_program(
  ctx: &mut Context,
  program: &mut ast::Program,
  is_hdr: bool,
) -> Result<()> {
  for gdecl in program.iter_mut() {
    match gdecl {
      ast::Gdecl::Fdecl(fdecl) => tc_fdecl(ctx, fdecl)?,
      ast::Gdecl::Fdefn(fdefn) => {
        if is_hdr {
          return Err(anyhow!("Cannot define function in header file",));
        } else {
          tc_fdefn(ctx, fdefn)?
        }
      }
      ast::Gdecl::Typedef(typedef) => tc_typedef(ctx, typedef)?,
      ast::Gdecl::Sdecl(sdecl) => tc_sdecl(ctx, sdecl)?,
      ast::Gdecl::Sdefn(sdefn) => tc_sdefn(ctx, sdefn)?,
    }
  }

  if ctx.all_used_fn_are_defined() {
    if is_hdr {
      Ok(())
    } else {
      ctx.remove_unused_fdecl();
      ctx.check_main_signature()
    }
  } else {
    Err(anyhow!(
      "Some functions referenced in the program are not defined"
    ))
  }
}

/// Typecheck a struct declaration, just insert struct name into the context.
/// If struct already declared or defined, do nothing.
pub(super) fn tc_sdecl(ctx: &mut Context, sdecl: &ast::Sdecl) -> Result<()> {
  ctx.declare_struct(sdecl.struct_name.clone());
  Ok(())
}

/// Typecheck a struct definition. Checks the following things:
///
/// 1. struct is not already defined
/// 2. all fields have a valid type
/// 3. all fields of struct type are defined
/// 4. all field names are unique
pub(super) fn tc_sdefn(ctx: &mut Context, sdefn: &ast::Sdefn) -> Result<()> {
  // checks 1
  if ctx.struct_defined(&sdefn.struct_name) {
    return Err(anyhow!("Struct name {} already defined", sdefn.struct_name));
  }

  let mut field_offset = HashMap::new();
  let mut offset = 0;
  let mut max_align_size = 0;
  for (typ, name) in sdefn.field_list.0.iter() {
    // checks 2
    let reduced_typ = match ctx.to_reduced_typ(typ) {
      Some(t) => Ok(t),
      _ => Err(anyhow!("Invalid type in struct field")),
    }?;

    // checks 3
    if let ast::Typ::Struct(name) = reduced_typ.clone() {
      if !ctx.struct_defined(&name) {
        return Err(anyhow!("field type {} is not defined", name));
      }
    }

    // update offset and max_field_size
    let type_size = ctx.get_type_size(&reduced_typ);
    let align_size = ctx.get_type_align_size(&reduced_typ);
    max_align_size = max_align_size.max(align_size);
    // align the offset to 0 % align_size
    if align_size != 0 {
      // padding
      offset += (align_size - (offset % align_size)) % align_size
    };

    // checks 4
    if field_offset
      .insert(name.to_string(), (reduced_typ, offset))
      .is_some()
    {
      return Err(anyhow!("Field name {} already exists in struct", name));
    }
    offset += type_size;
  }
  // align the struct size to max_align_size
  if max_align_size != 0 {
    offset += (max_align_size - (offset % max_align_size)) % max_align_size
  }
  ctx.define_struct(
    sdefn.struct_name.clone(),
    field_offset,
    offset,
    max_align_size as u8,
  );
  Ok(())
}

/// Typecheck a function declaration. Checks the following:
///
/// 1. all param names are unique and not used in typedef.
/// 2. function name is not used in typedef.
/// 3. function name is not used by another fdecl with different signature.
/// 4. return type exists, and is not struct.
/// 5. param types exist, and are not struct.
pub(super) fn tc_fdecl(ctx: &mut Context, fdecl: &ast::Fdecl) -> Result<()> {
  if !ctx.check_param_names(fdecl) {
    return Err(anyhow!(
      "Function {} has duplicate param names",
      fdecl.func_name
    ));
  }

  if ctx.type_exists(fdecl.func_name.as_str()) {
    return Err(anyhow!(
      "Function name {} already exists in typedef",
      fdecl.func_name
    ));
  }

  if let Some(other_fdecl) = ctx.get_func_decl(&fdecl.func_name) {
    if ctx.check_dup_decl(fdecl, &other_fdecl) {
      return Ok(());
    } else {
      return Err(anyhow!(
        "Function {} already exists with different signature",
        fdecl.func_name
      ));
    }
  }

  let mut fdecl = fdecl.clone();
  if let ast::ReturnType::Type(ret_typ) = &fdecl.ret_type {
    let typ = ctx.to_reduced_typ(ret_typ);
    if typ.is_none() {
      return Err(anyhow!("Return type {} does not exist", ret_typ));
    }
    let ret_type = typ.unwrap();
    if matches!(ret_type, ast::Typ::Struct(_)) {
      return Err(anyhow!("Return type cannot be struct"));
    }
    fdecl.ret_type = ast::ReturnType::Type(ret_type);
  }

  for (typ, _) in fdecl.param_list.0.iter_mut() {
    let param_typ = ctx.to_reduced_typ(typ);
    if let Some(t) = param_typ {
      if matches!(t, ast::Typ::Struct(_)) {
        return Err(anyhow!("param type cannot be struct"));
      }
      *typ = t;
    } else {
      return Err(anyhow!("Invalid type in param list"));
    }
  }

  ctx.declare_func(fdecl.func_name.clone(), &fdecl);

  Ok(())
}

/// Typecheck a typedef. Checks the following:
///
/// 1. new type name is not used in fdecl.
/// 2. new type name is not used in another typedef.
/// 3. orig type exists.
pub(super) fn tc_typedef(ctx: &mut Context, typedef: &ast::Typedef) -> Result<()> {
  if ctx.is_func_declared(&typedef.new_type_name) || ctx.type_exists(&typedef.new_type_name) {
    return Err(anyhow!("Typedef {} already exists", typedef.new_type_name));
  }
  let mapped_typ = match ctx.to_reduced_typ(&typedef.orig_type) {
    Some(t) => t,
    _ => Err(anyhow!("Invalid type in typedef"))?,
  };
  ctx.declare_typedef(typedef.new_type_name.clone(), mapped_typ);
  Ok(())
}

/// Typecheck a function definition. Checkes the following:
///
/// 1. check fdecl
/// 2. check if function is already defined
/// 3. check if function returns, or has void type
pub(super) fn tc_fdefn(ctx: &mut Context, fdefn: &mut crate::ast::Fdefn) -> Result<()> {
  let fdecl = Fdecl {
    func_name: fdefn.func_name.clone(),
    ret_type: fdefn.ret_type.clone(),
    param_list: fdefn.param_list.clone(),
  };
  tc_fdecl(ctx, &fdecl)?;

  // for duplicated fdecl, we only check type, but we save the signature
  // only for fdefn
  ctx.declare_func(fdecl.func_name.clone(), &fdecl);

  if ctx.is_func_defined(fdefn.func_name.clone()) {
    return Err(anyhow!(
      "Function {} already defined",
      fdefn.func_name.clone()
    ));
  }

  // update the context with the function parameters
  for (typ, var) in fdefn.param_list.0.iter() {
    assert!(!ctx.is_var_defined(var));
    ctx.define_var(var.clone(), ctx.to_reduced_typ(typ).unwrap());
  }

  let (ret, _) = tc_stmts(ctx, &mut fdefn.body, &fdefn.ret_type)?;

  if ret || ctx.return_type_eq(&fdefn.ret_type, &ast::ReturnType::Void) {
    ctx.define_func(fdefn.func_name.clone());

    // remove the previously defined function arguments
    for (_, var) in fdefn.param_list.0.iter() {
      ctx.remove_var(var);
    }

    Ok(())
  } else {
    Err(anyhow!("Function {} does not return", fdefn.func_name))
  }
}
