#![allow(clippy::result_large_err)]

use declaration_context::FileDeclarations;
pub use declaration_context::{DeclarationContext, TypeID};
use diagnostic::DiagnosticContents;
use std::{collections::HashMap, error::Error, fmt::Display, io};

use hir::HirModule;
use interpreter::{Function, VM};
pub use linear_ir::{
    expr_ty_to_physical, DeclaredTypeLayout, LinearFunction, LinearNode, LinearNodeValue,
    PhysicalCollection, PhysicalPrimitive, PhysicalType, RuntimeFunction, TypeLayoutValue,
};
use linear_ir::{layout_types, LinearContext};
use parser::ParsedFile;
use typecheck::typecheck;
pub use typecheck::{typecheck_node, ExpressionType, FuncType, TypeDeclaration, TypecheckContext};

mod borrowck;
mod declaration_context;
pub mod diagnostic;
mod generate_destructors;
mod hir;
mod interpreter;
mod linear_ir;
mod multi_error;
pub mod parser;
mod provenance;
mod tokenizer;
mod type_validator;
mod typecheck;

use parser::ParseError;

use crate::{
    diagnostic::Diagnostic,
    generate_destructors::generate_destructors,
    hir::{constant_inlining, desugar_module},
    type_validator::validate_types,
    typecheck::TypecheckedFile,
};

pub mod id;
pub use hir::{ArithmeticOp, BinaryLogicalOp, ComparisonOp, HirNodeValue, UnaryLogicalOp};
pub use interpreter::{ExternBinding, Value};
pub use provenance::{SourceMarker, SourceRange};

pub use borrowck::LifetimeError;
pub use type_validator::TypeValidationError;
pub use typecheck::TypecheckError;

#[derive(Debug)]
pub enum IntepreterError {
    Abort,
    CompileError(CompileError),
}

impl Display for IntepreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntepreterError::Abort => write!(f, "Panic within interpreter"),
            IntepreterError::CompileError(e) => e.fmt(f),
        }
    }
}

impl std::error::Error for IntepreterError {}

#[derive(Debug)]
pub enum CompileError {
    ParseError(ParseError),
    TypeValidationError(TypeValidationError),
    TypecheckError(TypecheckError),
    LifetimeError(LifetimeError),
}

impl Error for CompileError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        Some(match self {
            CompileError::ParseError(err) => err,
            CompileError::TypeValidationError(err) => err,
            CompileError::TypecheckError(err) => err,
            CompileError::LifetimeError(err) => err,
        })
    }
}

impl Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let diagnostics = match self {
            CompileError::ParseError(e) => e.contents(),
            CompileError::TypeValidationError(e) => e.contents(),
            CompileError::TypecheckError(e) => e.contents(),
            CompileError::LifetimeError(e) => e.contents(),
        };
        match diagnostics {
            DiagnosticContents::Scalar(d) => d.fmt(f),
            DiagnosticContents::Vector(diagnostics) => {
                for diagnostic in diagnostics {
                    diagnostic.fmt(f)?;
                }
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug)]
pub struct SourceFile {
    pub filename: &'static str,
    pub module_name: &'static str,
    pub contents: String,
}

impl SourceFile {
    pub fn from_filename(filename: &'static str) -> io::Result<SourceFile> {
        let contents = std::fs::read_to_string(filename)?;
        Ok(Self::from_filename_and_contents(filename, contents))
    }

    pub fn from_filename_and_contents(filename: &'static str, contents: String) -> SourceFile {
        let after_last_slash = filename
            .rfind(std::path::MAIN_SEPARATOR)
            .map(|idx| idx + 1)
            .unwrap_or(0);
        let dot = filename.find('.').unwrap_or(filename.len());
        let module_name = &filename[after_last_slash..dot];
        SourceFile {
            filename,
            module_name,
            contents,
        }
    }
}

pub fn interpret_code(
    sources: &[SourceFile],
    bindings: Vec<(&str, ExternBinding)>,
) -> Result<(Vec<Value>, Vec<u8>), IntepreterError> {
    let LowerResults {
        statements,
        statements_ty: _,
        functions,
        declarations,
        type_layouts: ty_declarations,
        constant_data,
    } = lower_code(sources, 1, std::mem::size_of::<usize>())
        .map_err(IntepreterError::CompileError)?;
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
    sources: &[SourceFile],
    byte_size: usize,
    pointer_size: usize,
) -> Result<LowerResults, CompileError> {
    let single_source = sources.len() == 1;

    let CompilationResults {
        main,
        modules,
        mut declarations,
    } = check_types(sources)?;

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

    for (idx, module) in modules.into_iter().enumerate() {
        if Some(idx) == main || (single_source && idx == 0) {
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
    pub main: Option<usize>,
    pub modules: Vec<HirModule>,
    pub declarations: DeclarationContext,
}

pub fn check_types(sources: &[SourceFile]) -> Result<CompilationResults, CompileError> {
    // TODO: return more than one parse error
    let modules: Vec<_> = sources
        .iter()
        .map(
            |SourceFile {
                 filename,
                 module_name,
                 contents,
             }| {
                parse_file(filename, contents.as_str()).map(|ast| (module_name, ast))
            },
        )
        .collect::<Result<_, _>>()?;
    let module_refs: Vec<_> = modules.iter().map(|(name, ast)| (**name, ast)).collect();

    typecheck_module(&module_refs[..])
}

pub fn typecheck_module(
    contents: &[(&'static str, &ParsedFile)],
) -> Result<CompilationResults, CompileError> {
    use rayon::prelude::*;

    let mut declarations =
        DeclarationContext::new(contents).map_err(CompileError::TypecheckError)?;
    validate_types(&declarations).map_err(CompileError::TypeValidationError)?;

    let module_types = contents
        .par_iter()
        .map(
            |(name, contents)| -> Result<TypecheckedFile<'_, '_>, TypecheckError> {
                let types = typecheck(contents, name, &declarations)?;
                Ok(types)
            },
        )
        .collect::<Vec<_>>();

    let mut constant_values = HashMap::new();
    let mut typecheck_errors = Ok(());
    for (module_ty, (_, ast)) in module_types.iter().zip(contents.iter()) {
        let Some(module_ty) = multi_error::merge_results_or_value(
            &mut typecheck_errors,
            module_ty.as_ref().map_err(|e| e.clone()),
        ) else {
            continue;
        };
        constant_inlining::extract_constant_values(
            &ast.arena,
            module_ty,
            &declarations,
            &mut constant_values,
        );
    }
    typecheck_errors.map_err(CompileError::TypecheckError)?;

    let module_results = contents
        .par_iter()
        .zip(module_types.into_par_iter())
        .map(|((name, contents), types)| {
            let ir = desugar_module(
                &declarations,
                &contents.arena,
                types.expect("errors should bail earlier"),
                &constant_values,
            );
            (name, ir)
        })
        .collect::<Vec<_>>();
    let mut modules = Vec::new();
    let mut main = None;
    for (name, module) in module_results {
        if *name == "main" {
            main = Some(modules.len());
        }
        modules.push(module);
    }
    generate_destructors(&mut modules, &mut declarations);

    let mut lifetime_errors = Ok(());
    for module in modules.iter_mut() {
        multi_error::merge_results(
            &mut lifetime_errors,
            borrowck::borrow_check(&declarations, module),
        );
    }
    lifetime_errors.map_err(CompileError::LifetimeError)?;

    Ok(CompilationResults {
        main,
        modules,
        declarations,
    })
}

pub fn parse_file(filename: &str, contents: &str) -> Result<ParsedFile, CompileError> {
    let tokens = tokenizer::lex(filename.into(), contents.into());
    let parsed_module = ParsedFile::parse(tokens).map_err(CompileError::ParseError)?;

    Ok(parsed_module)
}
