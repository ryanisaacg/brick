#![allow(clippy::result_large_err)]

use std::{collections::HashMap, fs, io};

use thiserror::Error;

pub mod analyzer;
pub mod arena;
pub mod backend;
pub mod parser;
pub mod provenance;
pub mod tokenizer;

use analyzer::{
    resolve_type_names, scan_top_level, typecheck, IRContext, ScanResults, Scope, TypecheckError,
};
use parser::ParseError;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
    #[error("type error: {0}")]
    TypeError(#[from] TypecheckError),
    #[error("filesystem error: {0} {1}")]
    FilesystemError(io::Error, String),
}

pub fn compile_file(source_name: &'static str) -> Result<Vec<u8>, CompileError> {
    let mut parsed_files = HashMap::new();
    let mut files_to_parse = vec![source_name.to_string()];
    let mut declarations = HashMap::new();
    let mut ir_context = IRContext::new();
    // TODO: parallelize this?
    while let Some(file) = files_to_parse.pop() {
        let mut file_name = String::new();
        file_name.push_str(&file);
        file_name.push_str(".brick");
        let contents = fs::read_to_string(&file_name)
            .map_err(|e| CompileError::FilesystemError(e, format!("reading file {}", file_name)))?;
        let file = Box::leak(file.into_boxed_str());
        let tokens = tokenizer::lex(file, contents.to_string());
        let (statements, arena) = parser::parse(tokens)?;
        let ScanResults {
            imports,
            declarations: local_declarations,
        } = scan_top_level(statements.iter().copied(), &arena, &mut ir_context)?;
        parsed_files.insert(file_name, (statements, arena));
        for (export_name, export_type) in local_declarations {
            // TODO: handle collisions?
            declarations.insert(export_name, export_type);
        }
        files_to_parse.extend(
            imports
                .iter()
                .filter(|import| !parsed_files.contains_key(import.as_str()))
                .cloned(),
        );
    }
    resolve_type_names(&mut ir_context, &declarations)?;
    let mut ir = Vec::new();
    let global_scope = [Scope {
        declarations,
        return_type: None,
    }];
    // TODO: also, parallelize this?
    for (statements, arena) in parsed_files.values() {
        let new_ir = typecheck(
            statements.iter().copied(),
            &mut ir_context,
            arena,
            &global_scope,
        )?;
        ir.extend(new_ir.into_iter());
    }
    Ok(backend::emit(ir, &ir_context, true))
}

pub fn compile_source(source_name: &'static str, contents: &str) -> Result<Vec<u8>, CompileError> {
    let tokens = tokenizer::lex(source_name, contents.to_string());
    let (statements, arena) = parser::parse(tokens)?;
    let mut ir_context = IRContext::new();
    // TODO: support imports?
    let ScanResults {
        imports: _,
        declarations,
    } = scan_top_level(statements.iter().copied(), &arena, &mut ir_context)?;
    resolve_type_names(&mut ir_context, &declarations)?;
    let global_scope = [Scope {
        declarations,
        return_type: None,
    }];
    let ir = typecheck(
        statements.into_iter(),
        &mut ir_context,
        &arena,
        &global_scope,
    )?;
    Ok(backend::emit(ir, &ir_context, true))
}
