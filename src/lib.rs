#![allow(clippy::result_large_err)]

use std::{
    collections::{HashMap, HashSet},
    fs, io,
};

use id::ID;
use imports::find_imports;
pub use interpreter::Value;
use ir::{lower_function, IrFunction, IrNode};
use thiserror::Error;
use typecheck::{
    resolve::{name_to_declaration, resolve_top_level_declarations},
    typecheck,
};

mod arena;
mod id;
mod imports;
mod tokenizer;

pub mod interpreter;
pub mod ir;
pub mod parser;
pub mod provenance;
pub mod typecheck;

use parser::{AstNode, ParseError};
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
    let ir_arena = Arena::new();
    // TODO: "main"?
    let functions = compile_file(&ir_arena, "main", source_name, contents)?;
    let main = functions
        .iter()
        .find(|(_, func)| func.name == "main")
        .expect("function named main")
        .0;

    Ok(evaluate_function(&functions, *main, &[]))
}

pub fn compile_file<'a>(
    ir_arena: &'a Arena<IrNode<'a>>,
    module_name: &'a str,
    source_name: &'static str,
    contents: String,
) -> Result<HashMap<ID, IrFunction<'a>>, CompileError> {
    let parse_arena = Arena::new();
    let modules = collect_modules(&parse_arena, module_name.to_string(), source_name, contents)?;

    let mut declarations = HashMap::new();
    for (_name, module) in modules.iter() {
        let source = &module[..];
        let names = name_to_declaration(source.iter());
        declarations.extend(resolve_top_level_declarations(&names).unwrap().into_iter());
    }

    let ir = modules
        .values()
        .flat_map(|module| typecheck(module.iter(), &declarations).unwrap())
        .into_iter()
        .map(|func| {
            let func = lower_function(ir_arena, func);
            (func.id, func)
        })
        .collect();

    Ok(ir)
}

struct ParseQueueEntry {
    name: String,
    source_name: &'static str,
    contents: String,
}

fn collect_modules<'a>(
    arena: &'a Arena<AstNode<'a>>,
    module_name: String,
    source_name: &'static str,
    contents: String,
) -> Result<HashMap<String, Vec<AstNode<'a>>>, CompileError> {
    let mut modules_seen = HashSet::new();
    modules_seen.insert(module_name.clone());
    let mut modules_to_parse = vec![ParseQueueEntry {
        name: module_name,
        source_name,
        contents,
    }];
    let mut parsed_modules = HashMap::new();

    while let Some(ParseQueueEntry {
        name,
        source_name,
        contents,
    }) = modules_to_parse.pop()
    {
        let tokens = tokenizer::lex(source_name, contents);
        let parsed_module = parser::parse(arena, tokens)?;

        for import in find_imports(parsed_module.iter()) {
            if modules_seen.contains(import.name) {
                continue;
            }
            let contents = fs::read_to_string(import.source_path.as_str()).unwrap();
            modules_to_parse.push(ParseQueueEntry {
                name: import.name.to_string(),
                source_name: import.source_path.leak(),
                contents,
            });
            modules_seen.insert(import.name.to_string());
        }

        parsed_modules.insert(name, parsed_module);
    }

    Ok(parsed_modules)
}
