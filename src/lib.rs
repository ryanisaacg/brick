#![allow(clippy::result_large_err)]

use std::io;

use analyzer::{typecheck, TypecheckError};
use thiserror::Error;

mod id;

pub mod analyzer;
pub mod arena;
pub mod parser;
pub mod provenance;
pub mod tokenizer;

use parser::ParseError;
use typed_arena::Arena;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
    #[error("filesystem error: {0} {1}")]
    FilesystemError(io::Error, String),
}

pub fn compile_file(source_name: &'static str, contents: String) -> Result<(), CompileError> {
    let tokens = tokenizer::lex(source_name, contents);
    let mut parse_nodes = Arena::new();
    let parsed_module = parser::parse(&mut parse_nodes, tokens)?;
    typecheck(parsed_module).unwrap();

    Ok(())
}
