use thiserror::Error;

pub mod backend;
pub mod lexer;
pub mod parser;
pub mod provenance;
pub mod tree;
pub mod typecheck;

use parser::ParseError;
use typecheck::TypecheckError;

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
    #[error("type error: {0}")]
    TypeError(#[from] TypecheckError),
}

pub fn compile(source_name: &'static str, contents: &str) -> Result<Vec<u8>, CompileError> {
    let tokens = lexer::lex(source_name, contents.to_string());
    let (statement, arena) = parser::parse(tokens)?;
    let mut ir_context = typecheck::IRContext {
        statements: Vec::new(),
        expressions: Vec::new(),
    };
    let ir = typecheck::typecheck(statement.into_iter(), &mut ir_context, &arena, &[])?;
    Ok(backend::emit(ir, &ir_context))
}
