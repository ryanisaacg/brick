#![allow(clippy::result_large_err)]

use std::io;

use thiserror::Error;

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
