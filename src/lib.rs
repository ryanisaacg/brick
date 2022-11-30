use std::{collections::HashMap, fs, io};

use scan::{scan_top_level, ScanResults};
use thiserror::Error;

pub mod backend;
pub mod lexer;
pub mod parser;
pub mod provenance;
mod scan;
pub mod tree;
pub mod typecheck;

use parser::ParseError;
use typecheck::{traverse, Scope, TypecheckError};

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
    // TODO: parallelize this?
    while let Some(file) = files_to_parse.pop() {
        let mut file_name = String::new();
        file_name.push_str(&file);
        file_name.push_str(".brick");
        let contents = fs::read_to_string(&file_name)
            .map_err(|e| CompileError::FilesystemError(e, format!("reading file {}", file_name)))?;
        let file = Box::leak(file.into_boxed_str());
        let tokens = lexer::lex(file, contents.to_string());
        let (statements, arena) = parser::parse(tokens)?;
        let ScanResults { imports, exports } = scan_top_level(statements.iter().copied(), &arena);
        parsed_files.insert(file_name, (statements, arena));
        for (export_name, export_type) in exports {
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
    let mut ir_context = typecheck::IRContext::new(Box::new(traverse));
    let mut ir = Vec::new();
    let global_scope = [Scope { declarations }];
    for (statements, arena) in parsed_files.values() {
        let new_ir = typecheck::typecheck(
            statements.iter().copied(),
            &mut ir_context,
            arena,
            &global_scope,
        )?;
        ir.extend(new_ir.into_iter());
    }
    Ok(backend::emit(ir, &ir_context))
}

pub fn compile_source(source_name: &'static str, contents: &str) -> Result<Vec<u8>, CompileError> {
    let tokens = lexer::lex(source_name, contents.to_string());
    let (statements, arena) = parser::parse(tokens)?;
    // TODO: support imports?
    let ScanResults {
        imports: _,
        exports,
    } = scan_top_level(statements.iter().copied(), &arena);
    let mut ir_context = typecheck::IRContext::new(Box::new(traverse));
    let global_scope = [Scope {
        declarations: exports,
    }];
    let ir = typecheck::typecheck(
        statements.into_iter(),
        &mut ir_context,
        &arena,
        &global_scope,
    )?;
    Ok(backend::emit(ir, &ir_context))
}
