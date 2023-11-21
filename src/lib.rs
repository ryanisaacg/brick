#![allow(clippy::result_large_err)]

use std::{
    collections::{HashMap, HashSet},
    fs, io,
};

pub use interpreter::Value;
use interpreter::{evaluate_node, Context};
use ir::{IrModule, IrNode};
use thiserror::Error;
use typecheck::{resolve::resolve_module, typecheck};

mod arena;
mod id;
mod tokenizer;

pub mod interpreter;
pub mod ir;
pub mod parser;
pub mod provenance;
pub mod typecheck;

use parser::{AstNode, AstNodeValue, ParseError};
use typed_arena::Arena;

use crate::ir::lower_module;

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
    let modules = compile_file(&ir_arena, "main", source_name, contents)?;
    let mut statements = Vec::new();
    let mut functions = HashMap::new();
    for (name, module) in modules {
        // TODO: execute imported statements?
        if name == "main" {
            statements.extend(module.top_level_statements);
        }
        for function in module.functions {
            functions.insert(function.id, function);
        }
    }

    let mut context = Context::new(&functions);
    for statement in statements {
        let _ = evaluate_node(&mut context, &statement);
    }

    Ok(context.values())
}

pub fn compile_file<'a>(
    ir_arena: &'a Arena<IrNode<'a>>,
    module_name: &'a str,
    source_name: &'static str,
    contents: String,
) -> Result<HashMap<String, IrModule<'a>>, CompileError> {
    let parse_arena = Arena::new();
    let modules = collect_modules(&parse_arena, module_name.to_string(), source_name, contents)?;

    let mut declarations = HashMap::new();
    for (_name, module) in modules.iter() {
        declarations.extend(resolve_module(&module[..]).into_iter());
    }

    let ir = modules
        .iter()
        .map(|(name, contents)| {
            let types = typecheck(contents.iter(), &declarations).unwrap();
            let ir = lower_module(ir_arena, types);
            (name.clone(), ir)
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

        for node in parsed_module.iter() {
            if let AstNodeValue::Import(name) = &node.value {
                if modules_seen.contains(name) {
                    continue;
                }
                let source_path = format!("{}.brick", &name[..]);
                //resolve_file_path(name.as_str());
                let contents = fs::read_to_string(source_path.as_str()).unwrap();
                modules_to_parse.push(ParseQueueEntry {
                    name: name.to_string(),
                    source_name: source_path.leak(),
                    contents,
                });
                modules_seen.insert(name.to_string());
            }
        }

        parsed_modules.insert(name, parsed_module);
    }

    Ok(parsed_modules)
}
