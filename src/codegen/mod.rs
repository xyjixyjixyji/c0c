// L4 Compiler
// Assembly code generator for fake assembly, similar to the triples discussed in class.

mod expr;
mod stmt;

use crate::abs2asm::fname_to_label;
use crate::asm::{Cond, Dest, Instr, InstrLabel, LoopState, Operand};
use crate::ast::{self, BinOp, FuncName};
use crate::optimization;
use crate::typecheck::{FunctionStatus, TypecheckContext};
use std::collections::{HashMap, HashSet};

use self::expr::munch_expr;
use self::stmt::munch_gdecl;

pub const TEMP_SIZE_4B: u32 = 4;
pub const TEMP_SIZE_8B: u32 = 8;
pub const INLINE_SUBSTITION_THRESHOLD: usize = 200;

#[derive(Debug, Clone)]
pub struct ContextMetadata {
  /// Whether this function can be inlined
  /// criteria:
  ///   1. straightline function
  ///   2. <= 20 lines of absasm
  ///   3. doesn't define any new variables
  ///   4. calls at most 1 function
  ///   5. not recursive
  can_inline: bool,
  /// The set of functions that are called in this function
  called_fns: HashSet<FuncName>,
  /// Used temps -> occurence count
  used_temp_counts: HashMap<u32, u32>,
  /// Whether tail call optimization is applied to this function
  tco_applied: bool,
  /// Some compile-time known array size to avoid unnecessary checks
  known_array_length: HashMap<Dest, i64>,
}

impl std::default::Default for ContextMetadata {
  fn default() -> Self {
    ContextMetadata {
      can_inline: true,
      called_fns: HashSet::new(),
      used_temp_counts: HashMap::new(),
      tco_applied: false,
      known_array_length: HashMap::new(),
    }
  }
}

/// Code generation context that contains a counter for creating new temps,
/// the list of currently-generated instructions, and the mapping from variable
/// names to temps.
#[derive(Clone, Debug)]
pub struct Context {
  /// Metadata for optimization
  pub metadata: ContextMetadata,
  /// The function name for this context's function
  pub fn_name: FuncName,
  /// The counter for creating new temps
  pub temp_index: u32,
  /// The counter for creating new labels
  pub label_index: u32,
  /// The counter for creating new loop ids
  pub loop_id: u32,
  /// The mapping from variable names to temps
  pub var_temp_map: HashMap<String, Dest>,
  /// The list of currently-generated instructions
  pub instrs: Vec<Instr>,
  /// The global typecheck context
  pub src_ctx: Box<TypecheckContext>,
  /// The memerror label
  pub memerror_label: InstrLabel,
  /// The label for self call, used for tail recursion optimization
  pub self_call_label: InstrLabel,
}

impl Context {
  pub(super) fn new(fn_name: FuncName, src_ctx: Box<TypecheckContext>) -> Self {
    // pointer deref
    Context {
      metadata: ContextMetadata::default(),
      fn_name: fn_name.clone(),
      temp_index: 0,
      label_index: 0,
      loop_id: 0,
      instrs: Vec::new(),
      var_temp_map: HashMap::new(),
      src_ctx,
      memerror_label: InstrLabel::new(format!("._{}_memerror", fn_name)),
      self_call_label: InstrLabel::new(format!(".{}", fname_to_label(&fn_name, false))),
    }
  }

  pub(super) fn basic_label(&mut self) -> InstrLabel {
    self.label(None, None)
  }

  /// Create a new label in this context, used as control flow targets.
  pub(super) fn label(
    &mut self,
    loop_id: Option<u32>,
    loop_state: Option<LoopState>,
  ) -> InstrLabel {
    let result = self.label_index;
    self.label_index += 1;

    InstrLabel::new(format!(".{}_{}", self.fn_name.clone(), result))
      .with_loop_id(loop_id)
      .with_loop_state(loop_state)
  }

  pub(super) fn loop_id(&mut self) -> u32 {
    let result = self.loop_id;
    self.loop_id += 1;
    result
  }

  /// Create a new temp in this context.
  /// default size is 4 bytes
  pub(super) fn temp(&mut self, size: u32) -> Dest {
    let result = self.temp_index;
    self.temp_index += 1;
    Dest(result, size)
  }

  /// Create a new temp in this context with default size.
  pub(super) fn temp_default(&mut self) -> Dest {
    self.temp(TEMP_SIZE_4B)
  }

  /// Create or fetch a variable.
  pub(super) fn var(&mut self, var: String) -> Dest {
    match self.var_temp_map.get(&var) {
      Some(d) => *d,
      None => {
        let result = self.temp_default();
        self.var_temp_map.insert(var, result);
        result
      }
    }
  }

  /// Update the size of a variable.
  pub(super) fn var_with_size(&mut self, var: String, size: u32) -> Dest {
    let mut temp = match self.var_temp_map.get(&var) {
      Some(d) => *d,
      None => self.temp_default(),
    };
    temp.1 = size;
    self.var_temp_map.insert(var, temp);
    temp
  }

  pub(super) fn add_instr(&mut self, instr: Instr) {
    // record the used temps
    match &instr {
      Instr::Mov { dest, src, .. } => {
        if let Operand::Temp(t) = src {
          self.update_used_temp(t.0);
        }
        self.update_used_temp(dest.0);
      }
      Instr::BinOp {
        dest, src1, src2, ..
      } => {
        if let Operand::Temp(t) = src1 {
          self.update_used_temp(t.0);
        }
        if let Operand::Temp(t) = src2 {
          self.update_used_temp(t.0);
        }
        self.update_used_temp(dest.0);
      }
      Instr::ReadMem { dest, read_addr } => {
        self.update_used_temp(read_addr.0);
        self.update_used_temp(dest.0);
      }
      Instr::WriteMem { src, dest_addr } => {
        self.update_used_temp(dest_addr.0);
        if let Operand::Temp(t) = src {
          self.update_used_temp(t.0);
        }
      }
      Instr::Call { dest, args, .. } => {
        if let Some(d) = dest {
          self.update_used_temp(d.0);
        }
        for arg in args {
          if let Operand::Temp(t) = arg {
            self.update_used_temp(t.0);
          }
        }
      }
      Instr::UnOp { dest, src, .. } => {
        if let Operand::Temp(t) = src {
          self.update_used_temp(t.0);
        }
        self.update_used_temp(dest.0);
      }
      Instr::CondJmp { src, .. } => match src {
        Cond::BinOp(src1, _, src2) => {
          if let Operand::Temp(t) = src1 {
            self.update_used_temp(t.0);
          }
          if let Operand::Temp(t) = src2 {
            self.update_used_temp(t.0);
          }
        }
        Cond::Simp(src) => {
          if let Operand::Temp(t) = src {
            self.update_used_temp(t.0);
          }
        }
      },
      Instr::Return(Some(Operand::Temp(t))) => {
        self.update_used_temp(t.0);
      }
      Instr::Lea {
        dest, base, index, ..
      } => {
        if let Operand::Temp(t) = base {
          self.update_used_temp(t.0);
        }
        if let Some(Operand::Temp(t)) = index {
          self.update_used_temp(t.0);
        }
        self.update_used_temp(dest.0);
      }
      _ => {}
    }

    self.update_metadata_hook(&instr);

    self.instrs.push(instr);
  }

  fn update_metadata_hook(&mut self, instr: &Instr) {
    if let Instr::Mov {
      dest,
      src: Operand::Temp(t),
    } = instr
    {
      // overwritten
      if self.try_get_array_length(dest).is_some() {
        self.remove_array_length(dest);
      }
      if let Some(l) = self.try_get_array_length(t) {
        self.record_array_length(*dest, l);
      }
    }
  }

  pub(super) fn setup_memerror_mount_point(&mut self) {
    self.add_instr(Instr::Label {
      name: self.memerror_label.clone(),
    });
    self.add_instr(Instr::mem_error());
  }

  /// Get the temps that are the arguments of this function.
  pub fn get_self_arglist(&self) -> Vec<Dest> {
    self
      .src_ctx
      .get_func_decl(&self.fn_name)
      .unwrap()
      .param_list
      .0
      .iter()
      .map(|(_, name)| self.var_temp_map[name])
      .collect()
  }

  pub fn is_function_argument(&self, dest: &Dest) -> bool {
    self
      .src_ctx
      .get_func_decl(&self.fn_name)
      .unwrap()
      .param_list
      .0
      .iter()
      .any(|(_, name)| self.var_temp_map[name] == *dest)
  }

  pub fn print_instrs(&self) {
    for (i, inst) in self.instrs.iter().enumerate() {
      println!("{}:\t {}", i, inst);
    }
  }

  pub fn get_func_status(&self, func_name: &str) -> Option<FunctionStatus> {
    self.src_ctx.get_func_status(func_name)
  }

  pub fn try_get_temp_used_count(&self, temp: u32) -> Option<u32> {
    self.metadata.used_temp_counts.get(&temp).copied()
  }

  pub fn mark_cannot_inline(&mut self) {
    self.metadata.can_inline = false;
  }

  pub fn can_inline(&self) -> bool {
    self.metadata.can_inline
  }

  pub fn num_called_functions(&self) -> usize {
    self.metadata.called_fns.len()
  }

  pub fn mark_tco_applied(&mut self) {
    self.metadata.tco_applied = true;
  }

  pub fn tco_applied(&self) -> bool {
    self.metadata.tco_applied
  }

  pub fn record_array_length(&mut self, dest: Dest, size: i64) {
    self.metadata.known_array_length.insert(dest, size);
  }

  fn remove_array_length(&mut self, dest: &Dest) {
    self.metadata.known_array_length.remove(dest);
  }

  pub fn try_get_array_length(&self, dest: &Dest) -> Option<i64> {
    self.metadata.known_array_length.get(dest).copied()
  }

  pub fn called_functions_ref(&self) -> &HashSet<FuncName> {
    &self.metadata.called_fns
  }

  pub fn used_temp_counts_ref_mut(&mut self) -> &mut HashMap<u32, u32> {
    &mut self.metadata.used_temp_counts
  }

  fn update_used_temp(&mut self, temp: u32) {
    if let Some(count) = self.metadata.used_temp_counts.get_mut(&temp) {
      *count += 1;
    } else {
      self.metadata.used_temp_counts.insert(temp, 1);
    }
  }

  pub fn build_called_functions(&self) -> HashSet<FuncName> {
    let mut called_fns = HashSet::new();
    for instr in &self.instrs {
      if let Instr::Call { name, .. } = instr {
        called_fns.insert(name.clone());
      }
    }
    called_fns
  }

  pub fn build_label_to_line_index(&self) -> HashMap<InstrLabel, usize> {
    let mut label_to_lineindex = HashMap::new();
    for (i, instr) in self.instrs.iter().enumerate() {
      if let Instr::Label { name } = instr {
        label_to_lineindex.insert(name.clone(), i);
      }
    }
    label_to_lineindex
  }
}

/// Munches a conditional expression. If it's a binop, returns the operands and the operator
/// to construct a different variant of the conditional jump absasm, which compared 2 operands directly
///
/// If the conditional can be evaluated at compile time, returns the result as a constant in the first
/// return value
pub(super) fn munch_conditional(
  ctx: &mut Context,
  cond: ast::Expr,
) -> (Operand, Option<ast::BinOp>, Option<Operand>) {
  if let ast::Expr::Binop(
    lhs,
    op @ (BinOp::EqEq
    | BinOp::Uneq
    | BinOp::Geq
    | BinOp::GreaterThan
    | BinOp::LessThan
    | BinOp::Leq),
    rhs,
  ) = cond
  {
    let (operand1, operand2) = (
      munch_expr(ctx, None, *lhs, false),
      munch_expr(ctx, None, *rhs, false),
    );

    // special case of comparing nulls
    if operand1.is_null() && operand2.is_null() {
      let bool_val = (BinOp::EqEq == op) as i64;
      return (Operand::Const(bool_val), None, None);
    }

    if operand1.is_const() && operand2.is_const() {
      let folded = fold_binops_i32(
        *operand1.as_const().unwrap(),
        *operand2.as_const().unwrap(),
        op,
      );
      if let Some(folded) = folded {
        return (Operand::Const(folded as i64), None, None);
      }
    }
    return (operand1, Some(op), Some(operand2));
  }

  (munch_expr(ctx, None, cond, false), None, None)
}

// performs constant folding on binary operations, while preserving dynamic behavior
pub fn fold_binops_i32(lhs: i64, rhs: i64, op: ast::BinOp) -> Option<i32> {
  let lhs = lhs as i32;
  let rhs = rhs as i32;
  match op {
    BinOp::Add => Some(lhs + rhs),
    BinOp::Sub => Some(lhs - rhs),
    BinOp::Mul => Some(lhs * rhs),
    BinOp::Div => {
      if rhs == 0 || (lhs == std::i32::MIN && rhs == -1) {
        None
      } else {
        Some(lhs / rhs)
      }
    }
    BinOp::Mod => {
      if rhs == 0 || (lhs == std::i32::MIN && rhs == -1) {
        None
      } else {
        Some(lhs % rhs)
      }
    }
    BinOp::Shl => {
      if (0..32).contains(&rhs) {
        Some(lhs << rhs)
      } else {
        None
      }
    }
    BinOp::Shr => {
      if (0..32).contains(&rhs) {
        Some(lhs >> rhs)
      } else {
        None
      }
    }
    BinOp::BitAnd | BinOp::LogicalAnd => Some(lhs & rhs),
    BinOp::BitOr | BinOp::LogicalOr => Some(lhs | rhs),
    BinOp::BitXor => Some(lhs ^ rhs),
    BinOp::EqEq => Some(if lhs == rhs { 1 } else { 0 }),
    BinOp::Uneq => Some(if lhs != rhs { 1 } else { 0 }),
    BinOp::Geq => Some(if lhs >= rhs { 1 } else { 0 }),
    BinOp::GreaterThan => Some(if lhs > rhs { 1 } else { 0 }),
    BinOp::LessThan => Some(if lhs < rhs { 1 } else { 0 }),
    BinOp::Leq => Some(if lhs <= rhs { 1 } else { 0 }),
  }
}

/// Convert an AST into an abstract-assembly IR.
pub fn munch_ast(
  program: ast::Program,
  src_ctx: TypecheckContext,
  do_optimize: bool,
) -> Vec<Context> {
  let mut ctxs = Vec::new();
  let src_ctx = Box::new(src_ctx);
  for gdecl in program {
    if let Some(ctx) = munch_gdecl(gdecl, src_ctx.clone()) {
      ctxs.push(ctx);
    }
  }

  if do_optimize {
    optimization::post_munch_pass(ctxs)
  } else {
    ctxs
  }
}
