#![allow(clippy::result_large_err)]

use std::io;

use analyzer::typecheck;
use thiserror::Error;

mod id;

pub mod analyzer;
pub mod arena;
pub mod parser;
pub mod provenance;
pub mod tokenizer;

use parser::ParseError;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
    #[error("filesystem error: {0} {1}")]
    FilesystemError(io::Error, String),
}

pub fn compile_file(source_name: &'static str, contents: String) -> Result<(), CompileError> {
    let tokens = tokenizer::lex(source_name, contents);
    let parsed_module = parser::parse(tokens)?;
    typecheck(parsed_module);

    Ok(())
}
