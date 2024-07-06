// L4 Compiler
//! Parser
// Author: Miles Conn <mconn@andrew.cmu.edu>

// We rely on [lalrpop](https://github.com/lalrpop/lalrpop) for parsing.
// Lalrpop generates a LR(1) paraser the grammar can be found in c0.lalrpop
// and the generated code in c0.rs

pub mod parser {
  use logos::{Logos, Span};
  use std::cell::RefCell;
  use std::collections::HashSet;
  use std::fs;

  use anyhow::{anyhow, Result};

  use crate::ast::Program;
  use crate::c0;
  use crate::elaboration::elab_program;
  use crate::lex::{LexerState, Token};

  fn parse_string(
    input: String,
    hdr_lexer_state: Option<LexerState>,
  ) -> Result<(Program, Option<LexerState>)> {
    // You'll need lexer_with_extras later trust me :)
    let lexer_state_ref = RefCell::new(hdr_lexer_state.unwrap_or(LexerState {
      declared: HashSet::new(),
      typedef_in_progress: false,
      in_progress_ident: None,
    }));
    let lex_stream = Token::lexer_with_extras(&input, &lexer_state_ref)
      .spanned()
      .map(|(mut t, y): (Token, Span)| {
        t = match t {
          Token::Ident(s) => {
            if lexer_state_ref.borrow().declared.contains(s) {
              Token::DeclaredIdent(s)
            } else {
              t
            }
          }
          _ => t,
        };
        (y.start, t, y.end)
      });

    let result = c0::SurfaceProgramParser::new()
      .parse(lex_stream)
      .map_err(|e| anyhow!("Couldn't parse file. Failed with message {:?}", e));

    let program = result?;
    Ok((elab_program(program), Some(lexer_state_ref.into_inner())))
  }

  pub fn parse(
    file_name: String,
    hdr_lexer_state: Option<LexerState>,
    file_str: Option<String>,
  ) -> Result<(Program, Option<LexerState>)> {
    let str_file = if let Some(file) = file_str {
      file
    } else {
      fs::read_to_string(file_name).expect("Couldn't read file")
    };

    parse_string(str_file, hdr_lexer_state)
  }
}
