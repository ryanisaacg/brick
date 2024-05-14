#![allow(clippy::result_large_err)]

use std::{
    collections::{HashMap, HashSet},
    fs, io,
};

use borrowck::BorrowError;
use hir::HirModule;
use interpreter::VM;
use linear_ir::{layout_types, linearize_function, LinearContext};
use thiserror::Error;
pub use typecheck::StaticDeclaration;
use typecheck::{resolve::resolve_module, typecheck};

mod borrowck;
mod hir;
mod interpreter;
mod linear_ir;
mod parser;
mod provenance;
mod runtime;
mod tokenizer;
mod typecheck;

use parser::{AstNode, AstNodeValue, ParseError};
use typed_arena::Arena;

use crate::{
    borrowck::borrow_check,
    hir::lower_module,
    id::TypeID,
    typecheck::{ModuleType, TypecheckError},
};

pub mod id;
pub use hir::HirNodeValue;
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
    #[error("borrow check errors: {0:?}")]
    BorrowcheckError(Vec<BorrowError>),
    #[error("typecheck errors: {0:?}")]
    TypecheckError(Vec<TypecheckError>),
}

pub fn eval(source: &str) -> Result<Vec<Value>, IntepreterError> {
    let (val, _) = interpret_code("eval", source.to_string(), HashMap::new())?;

    Ok(val)
}

pub fn eval_preserve_vm(source: &str) -> Result<(Vec<Value>, Vec<u8>), IntepreterError> {
    interpret_code("eval", source.to_string(), HashMap::new())
}

pub fn eval_with_bindings(
    source: &str,
    bindings: HashMap<String, ExternBinding>,
) -> Result<Vec<Value>, IntepreterError> {
    let (val, _) = interpret_code("eval", source.to_string(), bindings)?;

    Ok(val)
}

pub fn interpret_code(
    source_name: &'static str,
    contents: String,
    mut bindings: HashMap<String, ExternBinding>,
) -> Result<(Vec<Value>, Vec<u8>), IntepreterError> {
    // TODO: "main"?
    let CompilationResults {
        modules,
        declarations,
    } = typecheck_module("main", source_name, contents)?;

    let mut ty_declarations = HashMap::new();
    layout_types(&declarations, &mut ty_declarations);

    let mut statements = Vec::new();
    let mut functions = HashMap::new();

    let mut constant_data = Vec::new();

    for (name, module) in modules {
        // TODO: execute imported statements?
        if name == "main" {
            statements.push(module.top_level_statements);
        }
        for function in module.functions {
            let function = linearize_function(&mut constant_data, &ty_declarations, function);
            functions.insert(function.id, interpreter::Function::Ir(function));
        }
    }
    let module = StaticDeclaration::Module(ModuleType {
        id: TypeID::new(),
        exports: declarations,
    });
    module.visit(&mut |decl| {
        if let StaticDeclaration::Module(ModuleType { exports, .. }) = decl {
            for (name, decl) in exports.iter() {
                if let Some(implementation) = bindings.remove(name) {
                    let id = decl.unwrap_fn_id();
                    functions.insert(id, interpreter::Function::Extern(implementation));
                }
            }
        }
    });

    let statements =
        LinearContext::new(&ty_declarations, &mut constant_data).linearize_nodes(statements.into());

    let vm = VM::new(ty_declarations, &functions, constant_data);
    match vm.evaluate_top_level_statements(&statements[..]) {
        Ok(results) => Ok(results),
        Err(_) => Err(IntepreterError::Abort),
    }
}

pub struct CompilationResults {
    pub modules: HashMap<String, HirModule>,
    pub declarations: HashMap<String, StaticDeclaration>,
}

pub fn check_types(source: &str) -> Result<CompilationResults, CompileError> {
    typecheck_module("main", "eval", source.to_string())
}

pub fn typecheck_module(
    module_name: &str,
    source_name: &'static str,
    contents: String,
) -> Result<CompilationResults, CompileError> {
    use rayon::prelude::*;

    let parse_arena = Arena::new();
    let modules = collect_modules(&parse_arena, module_name.to_string(), source_name, contents)?;

    let declarations = modules
        .par_iter()
        .map(|(name, module)| {
            let name = name.clone();
            let module = StaticDeclaration::Module(ModuleType {
                id: TypeID::new(),
                exports: resolve_module(&module[..]),
            });
            (name, module)
        })
        .collect::<HashMap<_, _>>();
    let mut id_decls = HashMap::new();
    for decl in declarations.values() {
        decl.visit(&mut |decl: &StaticDeclaration| {
            id_decls.insert(decl.id(), decl);
        });
    }

    let mut modules = modules
        .par_iter()
        .map(|(name, contents)| {
            let types = typecheck(contents.iter(), name, &declarations)?;
            let ir = lower_module(types, &id_decls);
            Ok((name.clone(), ir))
        })
        .collect::<Result<HashMap<_, _>, Vec<TypecheckError>>>()
        .map_err(CompileError::TypecheckError)?;
    for module in modules.values_mut() {
        let errors = borrow_check(module, &id_decls);
        if !errors.is_empty() {
            return Err(CompileError::BorrowcheckError(errors));
        }
    }

    Ok(CompilationResults {
        modules,
        declarations,
    })
}

pub fn typecheck_file(
    module_name: &str,
    source_name: &'static str,
    contents: String,
) -> Result<(HirModule, HashMap<String, StaticDeclaration>), CompileError> {
    use rayon::prelude::*;

    let parse_arena = Arena::new();
    let modules = collect_modules(&parse_arena, module_name.to_string(), source_name, contents)?;

    let declarations = modules
        .par_iter()
        .map(|(name, module)| {
            let name = name.clone();
            let module = StaticDeclaration::Module(ModuleType {
                id: TypeID::new(),
                exports: resolve_module(&module[..]),
            });
            (name, module)
        })
        .collect::<HashMap<_, _>>();
    let mut id_decls = HashMap::new();
    for decl in declarations.values() {
        decl.visit(&mut |decl: &StaticDeclaration| {
            id_decls.insert(decl.id(), decl);
        });
    }

    let parsed_module = &modules[module_name];
    let types = typecheck(parsed_module.iter(), module_name, &declarations)
        .map_err(CompileError::TypecheckError)?;
    let ir = lower_module(types, &id_decls);

    Ok((ir, declarations))
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
