#![allow(clippy::result_large_err)]

use std::{collections::HashMap, io};

pub use interpreter::Value;
use ir::lower_function;
use thiserror::Error;
use typecheck::{
    resolve::{name_to_declaration, resolve_top_level_declarations},
    typecheck,
};

mod arena;
mod id;
mod tokenizer;

pub mod interpreter;
pub mod ir;
pub mod parser;
pub mod provenance;
pub mod typecheck;

use parser::ParseError;
use typed_arena::Arena;

use crate::interpreter::evaluate_function;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
    #[error("filesystem error: {0} {1}")]
    FilesystemError(io::Error, String),
}

pub fn interpret_code(
    source_name: &'static str,
    contents: String,
) -> Result<Vec<Value>, CompileError> {
    let tokens = tokenizer::lex(source_name, contents);
    let mut parse_nodes = Arena::new();
    let parsed_module = parser::parse(&mut parse_nodes, tokens)?;
    let source = &parsed_module[..];
    // TODO: scan for imports and make the imports
    let names = name_to_declaration(source);
    let declarations = resolve_top_level_declarations(&names).unwrap();
    let functions = typecheck(source, declarations).unwrap();

    let ir_arena = Arena::new();
    let ir: HashMap<_, _> = functions
        .into_iter()
        .map(|func| {
            let func = lower_function(&ir_arena, func);
            (func.id, func)
        })
        .collect();
    let main = ir
        .iter()
        .find(|(_, func)| func.name == "main")
        .expect("function named main")
        .0;

    Ok(evaluate_function(&ir, *main, &[]))
}
