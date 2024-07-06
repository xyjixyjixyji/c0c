pub const TEMP_VAR1_NAME: &str = "__15411TEMP1__";
pub const TEMP_VAR2_NAME: &str = "__15411TEMP2__";

pub struct WasmFn {
  pub name: String,
  pub params: Vec<String>,
  pub instrs: Vec<WasmInstr>,
  pub has_return: bool,
}

#[derive(Debug, Clone)]
pub enum WasmInstr {
  // basic instructions
  Const(i32),
  BinOp(WasmBinop),
  DeclVar(String),
  SetVar(String),
  GetVar(String),
  TeeVar(String),
  Drop,
  Return,

  // control flow instructions
  If {
    has_return: bool, // whether both branches return
    then_block: Vec<WasmInstr>,
    else_block: Option<Vec<WasmInstr>>,
  },
  While {
    cond: Vec<WasmInstr>,
    body: Vec<WasmInstr>,
    loop_id: i32,
  },

  // function call
  Call(String),

  // memory instructions
  Load,
  Store,
}

#[derive(Debug, Clone, Copy)]
pub enum WasmBinop {
  Add,
  Sub,
  Mul,
  Div,
  Rem,
  And,
  Or,
  Xor,
  Shl,
  Shr,
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
}

impl std::fmt::Display for WasmFn {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "(func ${}", self.name)?;
    for param in &self.params {
      write!(f, " (param ${} i32)", param)?;
    }
    if self.has_return {
      write!(f, " (result i32)")?;
    }
    writeln!(f)?;

    for instr in &self.instrs {
      writeln!(f, "{}", instr)?;
    }
    if self.has_return {
      writeln!(f, "(unreachable)")?;
    }
    writeln!(f, ")")?;
    writeln!(f, "(export \"{}\" (func ${}))", self.name, self.name)
  }
}

impl std::fmt::Display for WasmInstr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      WasmInstr::Const(c) => write!(f, "(i32.const {})", c),
      WasmInstr::BinOp(op) => write!(f, "({})", op),
      WasmInstr::DeclVar(var) => write!(f, "(local ${} i32)", var),
      WasmInstr::SetVar(var) => write!(f, "(local.set ${})", var),
      WasmInstr::GetVar(var) => write!(f, "(local.get ${})", var),
      WasmInstr::TeeVar(var) => write!(f, "(local.tee ${})", var),
      WasmInstr::Load => write!(f, "(i32.load)"),
      WasmInstr::Store => write!(f, "(i32.store)"),
      WasmInstr::Return => write!(f, "(return)"),
      WasmInstr::Drop => write!(f, "(drop)"),
      WasmInstr::Call(func) => write!(f, "(call ${})", func),
      WasmInstr::If {
        has_return,
        then_block,
        else_block,
      } => WasmInstr::fmt_if(f, *has_return, then_block, else_block.as_deref()),
      WasmInstr::While {
        cond,
        body,
        loop_id,
      } => WasmInstr::fmt_while(f, cond, body, *loop_id),
    }
  }
}

impl WasmInstr {
  /// Writes the wasm for an if instruction
  fn fmt_if(
    f: &mut std::fmt::Formatter<'_>,
    has_return: bool,
    then_block: &[WasmInstr],
    else_block: Option<&[WasmInstr]>,
  ) -> std::fmt::Result {
    write!(f, "(if")?;
    if has_return {
      writeln!(f, " (result i32)")?;
    }
    writeln!(f, "\t(then")?;
    for instr in then_block {
      writeln!(f, "\t\t{}", instr)?;
    }
    writeln!(f, "\t)")?;
    if let Some(else_block) = else_block {
      writeln!(f, "\t(else")?;
      for instr in else_block {
        writeln!(f, "\t\t{}", instr)?;
      }
      writeln!(f, "\t)")?;
    }
    writeln!(f, ")")
  }

  /// Writes the wasm for a while instruction
  ///
  /// wasm only supports do {...} while(cond) semantics, so we change the while loop into the following:
  ///
  /// block {
  ///   if (!cond) break
  ///   do {...} while(cond)
  /// }
  ///
  fn fmt_while(
    f: &mut std::fmt::Formatter<'_>,
    cond: &[WasmInstr],
    body: &[WasmInstr],
    loop_id: i32,
  ) -> std::fmt::Result {
    // wraps a block around the loop
    writeln!(f, "(block ${}", loop_id)?;

    // initial condition check
    for instr in cond {
      writeln!(f, "\t{}", instr)?;
    }
    writeln!(f, "\t(i32.eqz)")?;
    // this is the break instruction
    writeln!(f, "\t(br_if ${})", loop_id)?;

    // start the do {...} while(cond) loop
    writeln!(f, "\t(loop ${}", loop_id)?;
    for instr in body {
      writeln!(f, "\t\t{}", instr)?;
    }
    for instr in cond {
      writeln!(f, "\t\t{}", instr)?;
    }
    writeln!(f, "\t\t(br_if ${})", loop_id)?;
    writeln!(f, "\t)")?;
    writeln!(f, ")")
  }
}

impl std::fmt::Display for WasmBinop {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      WasmBinop::Add => write!(f, "i32.add"),
      WasmBinop::Sub => write!(f, "i32.sub"),
      WasmBinop::Mul => write!(f, "i32.mul"),
      WasmBinop::Div => write!(f, "i32.div_s"),
      WasmBinop::Rem => write!(f, "i32.rem_s"),
      WasmBinop::And => write!(f, "i32.and"),
      WasmBinop::Or => write!(f, "i32.or"),
      WasmBinop::Xor => write!(f, "i32.xor"),
      WasmBinop::Shl => write!(f, "i32.shl"),
      WasmBinop::Shr => write!(f, "i32.shr_s"),
      WasmBinop::Eq => write!(f, "i32.eq"),
      WasmBinop::Ne => write!(f, "i32.ne"),
      WasmBinop::Lt => write!(f, "i32.lt_s"),
      WasmBinop::Le => write!(f, "i32.le_s"),
      WasmBinop::Gt => write!(f, "i32.gt_s"),
      WasmBinop::Ge => write!(f, "i32.ge_s"),
    }
  }
}
