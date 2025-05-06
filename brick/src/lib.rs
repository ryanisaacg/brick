#![allow(clippy::result_large_err)]

use declaration_context::FileDeclarations;
pub use declaration_context::{DeclarationContext, TypeID};
use diagnostic::DiagnosticContents;
use monomorph::monomorphize;
use std::{collections::HashMap, error::Error, fmt::Display, io};

use hir::{HirModule, HirNode};
use interpreter::{Function, VM};
pub use linear_ir::{
    expr_ty_to_physical, DeclaredTypeLayout, LinearFunction, LinearNode, LinearNodeValue,
    PhysicalCollection, PhysicalPrimitive, PhysicalType, RuntimeFunction, TypeLayoutValue,
};
use linear_ir::{layout_types, LinearContext};
use parser::ParsedFile;
use typecheck::typecheck;
pub use typecheck::{
    typecheck_node, ExpressionType, FuncType, PointerKind, TypeDeclaration, TypecheckContext,
};

mod borrowck;
mod declaration_context;
pub mod diagnostic;
mod generate_destructors;
mod hir;
mod interpreter;
mod linear_ir;
mod monomorph;
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
pub use hir::{
    ArithmeticOp, BinaryLogicalOp, ComparisonOp, HirNodeValue, UnaryArithmeticOp, UnaryLogicalOp,
};
pub use interpreter::{ExternBinding, Value};
pub use provenance::{SourceMarker, SourceRange};

pub use borrowck::LifetimeError;
pub use type_validator::TypeValidationError;
pub use typecheck::TypecheckError;

#[derive(Debug)]
pub enum IntepreterError {
    Abort,
    CompileError(CompileError),
    NoMainProvided,
}

impl Display for IntepreterError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IntepreterError::Abort => write!(f, "Panic within interpreter"),
            IntepreterError::CompileError(e) => e.fmt(f),
            IntepreterError::NoMainProvided => write!(f, "No main module provided to interpreter"),
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
    let compiled_sources = check_types(sources).map_err(IntepreterError::CompileError)?;
    let LowerResults {
        statements,
        statements_ty: _,
        functions,
        declarations,
        type_layouts: ty_declarations,
        constant_data,
    } = lower_code(compiled_sources, 1, std::mem::size_of::<usize>());

    let Some(statements) = statements else {
        return Err(IntepreterError::NoMainProvided);
    };

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
    match vm.evaluate_top_level_statements(&statements) {
        Ok(results) => Ok(results),
        Err(_) => Err(IntepreterError::Abort),
    }
}

pub struct LowerResults {
    pub statements: Option<LinearNode>,
    pub statements_ty: Option<PhysicalType>,
    pub functions: Vec<LinearFunction>,
    pub declarations: DeclarationContext,
    pub type_layouts: HashMap<TypeID, DeclaredTypeLayout>,
    pub constant_data: Vec<u8>,
}

pub fn lower_code(
    CompilationResults {
        main,
        modules,
        mut declarations,
    }: CompilationResults,
    byte_size: usize,
    pointer_size: usize,
) -> LowerResults {
    let mut top_level_statements: Option<HirNode> = None;
    let mut functions = HashMap::new();

    for (idx, module) in modules.into_iter().enumerate() {
        if Some(idx) == main {
            top_level_statements = Some(module.top_level_statements);
        }
        for function in module.functions {
            functions.insert(function.id, function);
        }
    }

    monomorphize(&mut declarations, &mut top_level_statements, &mut functions);

    let mut type_layouts = HashMap::new();
    layout_types(
        &declarations.id_to_decl,
        &mut type_layouts,
        byte_size,
        pointer_size,
    );

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

    let statements_ty = top_level_statements
        .as_ref()
        .and_then(|statement| match &statement.ty {
            ExpressionType::Void | ExpressionType::Unreachable => None,
            return_ty => Some(expr_ty_to_physical(return_ty)),
        });
    let statements = top_level_statements.map(|statement| linear_context.linearize_node(statement));
    let functions = functions
        .into_values()
        .map(|function| linear_context.linearize_function(&declarations, function))
        .collect();

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
                params: parameters,
                returns: *returns,
                is_associated: false,
                is_coroutine: false,
                is_unsafe: false,
                is_extern: false,
                provenance: None,
                type_parameters: HashMap::new(),
            },
        );
    }

    LowerResults {
        statements,
        statements_ty,
        functions,
        declarations,
        type_layouts,
        constant_data,
    }
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
    // In single-source mode, count the only source as the main module
    if main.is_none() && contents.len() == 1 {
        main = Some(0);
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
