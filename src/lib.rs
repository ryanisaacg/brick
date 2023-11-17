#![allow(clippy::result_large_err)]

use std::io;

use thiserror::Error;
use typecheck::{
    resolve::{name_to_declaration, resolve_top_level_declarations},
    typecheck,
};

mod id;

pub mod arena;
pub mod parser;
pub mod provenance;
pub mod tokenizer;
pub mod typecheck;

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
    let source = &parsed_module[..];
    // TODO: scan for imports and make the imports
    let names = name_to_declaration(source);
    let declarations = resolve_top_level_declarations(&names).unwrap();
    typecheck(source, declarations).unwrap();

    Ok(())
}
