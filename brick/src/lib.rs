#![allow(clippy::result_large_err)]

use declaration_context::FileDeclarations;
pub use declaration_context::{DeclarationContext, TypeID};
use multi_error::merge_results;
use std::{collections::HashMap, io};

use borrowck::LifetimeError;
use hir::HirModule;
use interpreter::{Function, VM};
pub use linear_ir::{
    expr_ty_to_physical, DeclaredTypeLayout, LinearFunction, LinearNode, LinearNodeValue,
    PhysicalCollection, PhysicalPrimitive, PhysicalType, RuntimeFunction, TypeLayoutValue,
};
use linear_ir::{layout_types, LinearContext};
use parser::AstNode;
use thiserror::Error;
use typecheck::typecheck;
pub use typecheck::{ExpressionType, FuncType, TypeDeclaration};

mod borrowck;
mod declaration_context;
mod hir;
mod interpreter;
mod linear_ir;
mod multi_error;
pub mod parser;
mod provenance;
mod tokenizer;
mod typecheck;

use parser::ParseError;
use typed_arena::Arena;

use crate::{hir::lower_module, typecheck::TypecheckError};

pub mod id;
pub use hir::{ArithmeticOp, BinaryLogicalOp, ComparisonOp, HirNodeValue, UnaryLogicalOp};
pub use interpreter::{ExternBinding, Value};
pub use provenance::{SourceMarker, SourceRange};

#[derive(Debug, Error)]
pub enum IntepreterError {
    #[error("aborted during execution")]
    Abort,
    #[error("compile error: {0}")]
    CompileError(#[from] CompileError),
}

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
    #[error("filesystem error: {0} {1}")]
    FilesystemError(io::Error, String),
    #[error("typecheck errors: {0}")]
    TypecheckError(#[from] TypecheckError),
    #[error("lifetime errors: {0}")]
    LifetimeError(#[from] LifetimeError),
}

pub struct SourceFile {
    pub filename: &'static str,
    pub module_name: &'static str,
    pub contents: String,
}

impl SourceFile {
    pub fn from_filename(filename: &'static str) -> io::Result<SourceFile> {
        let after_last_slash = filename
            .rfind(std::path::MAIN_SEPARATOR)
            .map(|idx| idx + 1)
            .unwrap_or(0);
        let dot = filename.find('.').unwrap_or(filename.len());
        let module_name = &filename[after_last_slash..dot];
        let contents = std::fs::read_to_string(filename)?;
        Ok(SourceFile {
            filename,
            module_name,
            contents,
        })
    }
}

pub fn interpret_code(
    sources: Vec<SourceFile>,
    bindings: Vec<(&str, ExternBinding)>,
) -> Result<(Vec<Value>, Vec<u8>), IntepreterError> {
    let LowerResults {
        statements,
        statements_ty: _,
        functions,
        declarations,
        type_layouts: ty_declarations,
        constant_data,
    } = lower_code(sources, 1, std::mem::size_of::<usize>())?;
    let mut functions: HashMap<_, _> = functions
        .into_iter()
        .map(|func| (func.id, Function::Ir(func)))
        .collect();

    let binding_name_to_fn: HashMap<_, _> = declarations
        .extern_function_bindings
        .iter()
        .map(|(name, id)| (name.as_str(), *id))
        .collect();
    for (name, implementation) in bindings {
        if let Some(fn_id) = binding_name_to_fn.get(name) {
            functions.insert(*fn_id, Function::Extern(implementation));
        }
    }

    let vm = VM::new(ty_declarations, &functions, constant_data);
    match vm.evaluate_top_level_statements(&statements[..]) {
        Ok(results) => Ok(results),
        Err(_) => Err(IntepreterError::Abort),
    }
}

pub struct LowerResults {
    pub statements: Vec<LinearNode>,
    pub statements_ty: Option<PhysicalType>,
    pub functions: Vec<LinearFunction>,
    pub declarations: DeclarationContext,
    pub type_layouts: HashMap<TypeID, DeclaredTypeLayout>,
    pub constant_data: Vec<u8>,
}

pub fn lower_code(
    sources: Vec<SourceFile>,
    byte_size: usize,
    pointer_size: usize,
) -> Result<LowerResults, CompileError> {
    let parse_arena = Arena::new();
    // TODO: return more than one parse error
    let modules: Vec<_> = sources
        .into_iter()
        .map(
            |SourceFile {
                 filename,
                 module_name,
                 contents,
             }| {
                parse_file(&parse_arena, filename, contents).map(|ast| (module_name, ast))
            },
        )
        .collect::<Result<_, _>>()?;

    let CompilationResults {
        modules,
        mut declarations,
    } = typecheck_module(&modules[..])?;

    let mut type_layouts = HashMap::new();
    layout_types(
        &declarations.id_to_decl,
        &mut type_layouts,
        byte_size,
        pointer_size,
    );

    let mut statements = Vec::new();
    let mut functions = Vec::new();
    let mut constant_data = Vec::new();
    let mut indirect_function_types = HashMap::new();

    let mut linear_context = LinearContext {
        layouts: &type_layouts,
        constant_data_region: &mut constant_data,
        indirect_function_types: &mut indirect_function_types,
        byte_size,
        pointer_size,
        module: FileDeclarations::new(),
    };

    for (name, module) in modules {
        // TODO: execute imported statements?
        if name == "main" {
            statements.push(module.top_level_statements);
        }
        for function in module.functions {
            functions.push(linear_context.linearize_function(&declarations, function));
        }
    }

    let statements_ty = statements.last().and_then(|last| match &last.ty {
        ExpressionType::Void | ExpressionType::Unreachable => None,
        return_ty => Some(expr_ty_to_physical(return_ty)),
    });
    let statements = linear_context.linearize_nodes(statements);

    for (expr, fn_id) in indirect_function_types {
        let ExpressionType::FunctionReference {
            parameters,
            returns,
        } = expr
        else {
            unreachable!();
        };
        declarations.id_to_func.insert(
            fn_id,
            FuncType {
                id: fn_id,
                type_param_count: 0,
                params: parameters,
                returns: *returns,
                is_associated: false,
                is_coroutine: false,
                provenance: None,
            },
        );
    }

    Ok(LowerResults {
        statements,
        statements_ty,
        functions,
        declarations,
        type_layouts,
        constant_data,
    })
}

pub struct CompilationResults {
    pub modules: HashMap<&'static str, HirModule>,
    pub declarations: DeclarationContext,
}

pub fn check_types(source: &str) -> Result<CompilationResults, CompileError> {
    let parse_arena = Arena::new();
    let main = parse_file(&parse_arena, "main", source.to_string())?;

    typecheck_module(&[("main", main)])
}

pub fn typecheck_module<'a>(
    contents: &'a [(&'static str, Vec<AstNode<'a>>)],
) -> Result<CompilationResults, CompileError> {
    use rayon::prelude::*;

    let mut declarations = DeclarationContext::new();
    let mut decl_results = Ok(());
    for (name, source) in contents.iter() {
        merge_results(
            &mut decl_results,
            declarations.insert_file(name, &source[..]),
        );
    }
    declarations.propagate_viral_types();
    decl_results?;

    let module_results = contents
        .par_iter()
        .map(
            |(name, contents)| -> Result<(&'static str, HirModule), TypecheckError> {
                let types = typecheck(&contents[..], name, &declarations)?;
                let ir = lower_module(types, &declarations);
                Ok((name, ir))
            },
        )
        .collect::<Vec<_>>();
    let mut modules = HashMap::new();
    let mut typecheck_errors = Ok(());
    for module_result in module_results {
        if let Ok((name, module)) = module_result {
            modules.insert(name, module);
        } else {
            multi_error::merge_results(&mut typecheck_errors, module_result.map(|_| {}));
        }
    }
    typecheck_errors?;

    let mut lifetime_errors = Ok(());
    for module in modules.values_mut() {
        multi_error::merge_results(
            &mut lifetime_errors,
            borrowck::borrow_check(&declarations, module),
        );
    }
    lifetime_errors?;

    Ok(CompilationResults {
        modules,
        declarations,
    })
}

pub fn parse_file<'a>(
    arena: &'a Arena<AstNode<'a>>,
    filename: &'static str,
    contents: String,
) -> Result<Vec<AstNode<'a>>, CompileError> {
    let tokens = tokenizer::lex(filename, contents);
    let parsed_module = parser::parse(arena, tokens)?;

    Ok(parsed_module)
}
