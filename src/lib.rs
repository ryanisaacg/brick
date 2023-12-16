#![allow(clippy::result_large_err)]

use std::{
    collections::{HashMap, HashSet},
    fs, io,
};

use hir::HirModule;
pub use interpreter::Value;
use interpreter::{evaluate_node, Context, Function};
use linear_interpreter::{evaluate_block, VM};
use linear_ir::{layout_types, linearize_function, linearize_nodes};
use thiserror::Error;
use typecheck::{resolve::resolve_module, typecheck, StaticDeclaration};

mod arena;
mod hir;
mod id;
mod interpreter;
mod linear_interpreter;
mod linear_ir;
mod parser;
mod provenance;
mod tokenizer;
mod typecheck;

use parser::{AstNode, AstNodeValue, ParseError};
use typed_arena::Arena;

use crate::{hir::lower_module, id::ID, typecheck::ModuleType};

#[derive(Debug, Error)]
pub enum CompileError {
    #[error("parse error: {0}")]
    ParseError(#[from] ParseError),
    #[error("filesystem error: {0} {1}")]
    FilesystemError(io::Error, String),
}

pub async fn eval(source: &str) -> Result<Vec<Value>, CompileError> {
    let val = linear_interpret_code("eval", source.to_string(), HashMap::new()).await?;

    Ok(val)
}

pub async fn eval_both(source: &str) -> Result<Vec<Value>, CompileError> {
    let val1 = interpret_code("eval", source.to_string(), HashMap::new()).await?;
    let val2 = linear_interpret_code("eval", source.to_string(), HashMap::new()).await?;

    if val1 != val2 {
        assert_eq!(val1, val2);
    }

    Ok(val1)
}

pub use interpreter::ExternBinding;

// TODO: move this to a separate crate
pub async fn interpret_code(
    source_name: &'static str,
    contents: String,
    mut bindings: HashMap<String, std::sync::Arc<ExternBinding>>,
) -> Result<Vec<Value>, CompileError> {
    // TODO: "main"?
    let CompilationResults {
        modules,
        declarations,
    } = typecheck_module("main", source_name, contents)?;
    let mut statements = Vec::new();
    let mut functions = HashMap::new();

    for (name, module) in modules {
        // TODO: execute imported statements?
        if name == "main" {
            statements.extend(module.top_level_statements);
        }
        for function in module.functions {
            functions.insert(function.id, Function::Ir(function));
        }
    }
    let module = StaticDeclaration::Module(ModuleType {
        id: ID::new(),
        exports: declarations,
    });
    module.visit(&mut |decl| {
        if let StaticDeclaration::Module(ModuleType { exports, .. }) = decl {
            for (name, decl) in exports.iter() {
                if let Some(implementation) = bindings.remove(name) {
                    let id = decl.id();
                    functions.insert(id, Function::Extern(implementation));
                }
            }
        }
    });

    let mut context = Context::new(vec![]);
    context.add_fns(&functions);
    for statement in statements {
        let _ = evaluate_node(&functions, &mut context, &statement).await;
    }

    Ok(context.values())
}

pub async fn linear_interpret_code(
    source_name: &'static str,
    contents: String,
    mut bindings: HashMap<String, std::sync::Arc<ExternBinding>>,
) -> Result<Vec<Value>, CompileError> {
    // TODO: "main"?
    let CompilationResults {
        modules,
        declarations,
    } = typecheck_module("main", source_name, contents)?;

    let mut ty_declarations = HashMap::new();
    layout_types(&declarations, &mut ty_declarations);

    let mut statements = Vec::new();
    let mut functions = HashMap::new();

    for (name, module) in modules {
        // TODO: execute imported statements?
        if name == "main" {
            statements.extend(module.top_level_statements);
        }
        for function in module.functions {
            let function = linearize_function(&ty_declarations, function);
            functions.insert(function.id, linear_interpreter::Function::Ir(function));
        }
    }
    /*let module = StaticDeclaration::Module(ModuleType {
        id: ID::new(),
        exports: declarations,
    });
    module.visit(&mut |decl| {
        if let StaticDeclaration::Module(ModuleType { exports, .. }) = decl {
            for (name, decl) in exports.iter() {
                if let Some(implementation) = bindings.remove(name) {
                    let id = decl.id();
                    functions.insert(id, linear_interpreter::Function::Extern(implementation));
                }
            }
        }
    });*/

    let mut stack_entries = HashMap::new();
    let mut stack_offset = std::mem::size_of::<usize>();
    let statements = linearize_nodes(
        &ty_declarations,
        &mut stack_entries,
        &mut stack_offset,
        statements.into(),
    );

    let mut vm = VM::new(ty_declarations);
    for statement in statements {
        let _ = evaluate_block(&functions, &mut [], &mut vm, &statement).await;
    }

    Ok(vm.op_stack)
}

pub struct CompilationResults {
    pub modules: HashMap<String, HirModule>,
    pub declarations: HashMap<String, StaticDeclaration>,
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
                id: ID::new(),
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

    let modules = modules
        .par_iter()
        .map(|(name, contents)| {
            let types = typecheck(contents.iter(), name, &declarations).unwrap();
            let ir = lower_module(types, &id_decls);
            (name.clone(), ir)
        })
        .collect();

    Ok(CompilationResults {
        modules,
        declarations,
    })
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
