#![allow(clippy::result_large_err)]

use declaration_context::Module;
pub use declaration_context::{DeclarationContext, TypeID};
use multi_error::merge_results;
use std::{
    collections::{HashMap, HashSet},
    fs, io,
};

use borrowck::LifetimeError;
use hir::HirModule;
use interpreter::{Function, VM};
pub use linear_ir::{
    expr_ty_to_physical, DeclaredTypeLayout, LinearFunction, LinearNode, LinearNodeValue,
    PhysicalCollection, PhysicalPrimitive, PhysicalType, RuntimeFunction, TypeLayoutValue,
};
use linear_ir::{layout_types, LinearContext};
use parser::{AstNode, AstNodeValue};
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

pub fn eval(source: &str) -> Result<Vec<Value>, IntepreterError> {
    let (val, _) = interpret_code("eval", source.to_string(), Default::default())?;

    Ok(val)
}

pub fn eval_preserve_vm(source: &str) -> Result<(Vec<Value>, Vec<u8>), IntepreterError> {
    interpret_code("eval", source.to_string(), Default::default())
}

pub fn eval_with_bindings(
    source: &str,
    bindings: Vec<(&str, ExternBinding)>,
) -> Result<Vec<Value>, IntepreterError> {
    let (val, _) = interpret_code("eval", source.to_string(), bindings)?;

    Ok(val)
}

pub fn interpret_code(
    source_name: &'static str,
    contents: String,
    bindings: Vec<(&str, ExternBinding)>,
) -> Result<(Vec<Value>, Vec<u8>), IntepreterError> {
    let LowerResults {
        statements,
        statements_ty: _,
        functions,
        declarations,
        type_layouts: ty_declarations,
        constant_data,
    } = lower_code(
        "main",
        source_name,
        contents,
        1,
        std::mem::size_of::<usize>(),
    )?;
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
    module_name: &str,
    source_name: &'static str,
    contents: String,
    byte_size: usize,
    pointer_size: usize,
) -> Result<LowerResults, CompileError> {
    let parse_arena = Arena::new();
    let modules = parse_files(&parse_arena, module_name.to_string(), source_name, contents)?;

    let CompilationResults {
        modules,
        mut declarations,
    } = typecheck_module(&modules)?;

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
        module: Module::new(),
    };

    for (name, module) in modules {
        // TODO: execute imported statements?
        if name == "main" {
            statements.push(module.top_level_statements);
        }
        for function in module.functions {
            functions.push(linear_context.linearize_function(function));
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
    pub modules: HashMap<String, HirModule>,
    pub declarations: DeclarationContext,
}

pub fn check_types(source: &str) -> Result<CompilationResults, CompileError> {
    let parse_arena = Arena::new();
    let modules = parse_files(&parse_arena, "main".to_string(), "eval", source.to_string())?;

    typecheck_module(&modules)
}

pub fn typecheck_module<'a>(
    modules: &'a HashMap<String, Vec<AstNode<'a>>>,
) -> Result<CompilationResults, CompileError> {
    use rayon::prelude::*;

    let declarations = collect_declarations(modules)?;

    let module_results = modules
        .par_iter()
        .map(
            |(name, contents)| -> Result<(String, HirModule), TypecheckError> {
                let types = typecheck(contents.iter(), name, &declarations)?;
                let ir = lower_module(types, &declarations);
                Ok((name.clone(), ir))
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

pub fn typecheck_file(
    module_name: &str,
    source_name: &'static str,
    contents: String,
) -> Result<(HirModule, DeclarationContext), CompileError> {
    let parse_arena = Arena::new();
    let modules = parse_files(&parse_arena, module_name.to_string(), source_name, contents)?;

    let declarations = collect_declarations(&modules)?;

    let parsed_module = &modules[module_name];
    let types = typecheck(parsed_module.iter(), module_name, &declarations)?;
    let ir = lower_module(types, &declarations);

    Ok((ir, declarations))
}

fn collect_declarations(
    modules: &HashMap<String, Vec<AstNode>>,
) -> Result<DeclarationContext, TypecheckError> {
    let mut ctx = DeclarationContext::new();
    let mut result = Ok(());
    for (name, source) in modules.iter() {
        let name = name.clone();
        merge_results(&mut result, ctx.insert_file(name.leak(), source));
    }
    result?;
    ctx.propagate_viral_types();
    Ok(ctx)
}

struct ParseQueueEntry {
    name: String,
    source_name: &'static str,
    contents: String,
}

pub fn parse_files<'a>(
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
