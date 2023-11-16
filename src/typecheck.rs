use std::collections::HashMap;

use thiserror::Error;

use crate::{
    id::ID,
    parser::{
        AstNode, AstNodeValue, BinOp, CallableDeclaration, FunctionDeclarationValue,
        ParsedSourceFile, TypeDeclaration,
    },
};

use self::control_flow_graph::build_control_flow_graph;

mod control_flow_graph;
//mod resolve;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ExpressionType<'a> {
    Primitive(PrimitiveType),
    Func(&'a FuncType<'a>),
    Struct(&'a StructType<'a>),
}

pub enum ModuleDeclaration<'a> {
    Func(FuncType<'a>),
    Struct(StructType<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructType<'a> {
    pub fields: HashMap<String, ExpressionType<'a>>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncType<'a> {
    pub params: Vec<ExpressionType<'a>>,
    pub returns: ExpressionType<'a>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    // TODO: long int, long float
    Int,
    Float,
    Bool,
    Void,
}

// TODO: get the lifetimes out of these typecheck errors
#[derive(Debug, Error)]
pub enum TypecheckError<'a> {
    #[error("arithmetic")]
    ArithmeticMismatch(&'a AstNode<'a>), // TODO
    #[error("mismatched types: received {received:?}, expected {expected:?}")]
    TypeMismatch {
        // TODO: provenance
        expected: ExpressionType<'a>,
        received: ExpressionType<'a>,
    },
    #[error("declaration for {0:?} not found")]
    NameNotFound(&'a AstNode<'a>), // TODO
    #[error("can't call")]
    CantCall(&'a AstNode<'a>), // TODO
    #[error("wrong args count")]
    WrongArgsCount(&'a AstNode<'a>),
}

// TODO: pass import namespace in
pub fn typecheck<'a>(file: ParsedSourceFile<'a>) -> Result<(), TypecheckError<'a>> {
    // TODO: reject void params and fields

    //let module = resolve_module(file);
    let mut declarations = HashMap::new();
    for type_decl in file.types.values() {
        match type_decl {
            TypeDeclaration::Struct(decl) => {
                // TODO: create a type object to represent this struct
            }
        }
    }

    for (name, func_decl) in file.functions.iter() {
        let (params, returns) = match func_decl {
            CallableDeclaration::Function(func) => (&func.params, &func.returns),
            CallableDeclaration::External(func) => (&func.params, &func.returns),
        };
        // TODO: create a type object to represent this function
        let name = name.clone();
        let func = FuncType {
            params: params
                .iter()
                .map(|param| resolve_type_name(&declarations, &param.type_))
                .collect::<Result<Vec<_>, _>>()?,
            returns: returns
                .map(|returns| resolve_type_name(&declarations, returns))
                .unwrap_or(Ok(ExpressionType::Primitive(PrimitiveType::Void)))?,
        };
        declarations.insert(name, ModuleDeclaration::Func(func));
    }

    let types = declarations
        .iter()
        .map(|(name, value)| {
            (
                name.clone(),
                match value {
                    ModuleDeclaration::Func(inner) => ExpressionType::Func(inner),
                    ModuleDeclaration::Struct(inner) => ExpressionType::Struct(inner),
                },
            )
        })
        .collect();
    // TODO: handle imports and namespaces

    for function in file.functions.values() {
        match function {
            CallableDeclaration::Function(func) => {
                typecheck_function(func, &types).unwrap(); // TODO
            }
            CallableDeclaration::External(_) => {}
        }
    }

    Ok(())
}

fn resolve_type_name<'a>(
    types: &HashMap<String, ModuleDeclaration<'a>>,
    name: &'a AstNode<'a>,
) -> Result<ExpressionType<'a>, TypecheckError<'a>> {
    Ok(match &name.value {
        AstNodeValue::Name(name) => match name.as_str() {
            "bool" => ExpressionType::Primitive(PrimitiveType::Bool),
            "void" => ExpressionType::Primitive(PrimitiveType::Void),
            "i32" => ExpressionType::Primitive(PrimitiveType::Int),
            "f32" => ExpressionType::Primitive(PrimitiveType::Float),
            other => {
                unimplemented!("Resolving actual type names is not implemented");
            }
        },
        AstNodeValue::UniqueType(_) => todo!(),
        AstNodeValue::SharedType(_) => todo!(),
        AstNodeValue::ArrayType(_) => todo!(),
        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::Declaration(_, _)
        | AstNodeValue::Import(_)
        | AstNodeValue::Return(_)
        | AstNodeValue::Int(_)
        | AstNodeValue::Float(_)
        | AstNodeValue::Bool(_)
        | AstNodeValue::BinExpr(_, _, _)
        | AstNodeValue::If(_, _)
        | AstNodeValue::While(_, _)
        | AstNodeValue::Call(_, _)
        | AstNodeValue::TakeUnique(_)
        | AstNodeValue::TakeShared(_)
        | AstNodeValue::StructLiteral { .. }
        | AstNodeValue::ArrayLiteral(_)
        | AstNodeValue::ArrayLiteralLength(_, _)
        | AstNodeValue::Block(_) => {
            // TODO: report error
            panic!("Illegal in expression name");
        }
    })
}

// TODO: some sort of data structure to store the results?
fn typecheck_function<'a, 'b>(
    function: &'a FunctionDeclarationValue<'a>,
    module_types: &HashMap<String, ExpressionType<'a>>,
) -> Result<HashMap<ID, ExpressionType<'a>>, TypecheckError<'a>> {
    let cfg = build_control_flow_graph(&function.body);

    let Some(ExpressionType::Func(function_type)) = module_types.get(&function.name) else {
        panic!("expected function to be found in the module");
    };
    let parameters = function
        .params
        .iter()
        .zip(function_type.params.iter())
        .map(|(name, param)| (name.name.clone(), param.clone()))
        .collect();

    let mut expressions = HashMap::new();
    // TODO: outermost scope
    typecheck_expression(
        &function.body,
        &[module_types, &parameters],
        &mut HashMap::new(),
        &mut expressions,
    )?;
    // TODO: assign a type to every expression in the function body
    // TODO: ensure that the return type matches the function's declared return type

    Ok(expressions)
}

// TODO: allow ; to turn expressions into void
// TODO: if-else need to unify
fn typecheck_expression<'a, 'b>(
    node: &'a AstNode<'a>,
    outer_scopes: &[&HashMap<String, ExpressionType<'a>>],
    current_scope: &mut HashMap<String, ExpressionType<'a>>,
    expressions: &'b mut HashMap<ID, ExpressionType<'a>>,
) -> Result<ExpressionType<'a>, TypecheckError<'a>> {
    let ty = match &node.value {
        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::Import(_) => {
            unimplemented!("Can't do this inside a function");
        }
        AstNodeValue::UniqueType(_) | AstNodeValue::SharedType(_) | AstNodeValue::ArrayType(_) => {
            panic!("illegal type expression in function body");
        }
        AstNodeValue::Declaration(name, value) => {
            // TODO: do I want shadowing? currently this shadows
            let value = typecheck_expression(value, outer_scopes, current_scope, expressions)?;
            current_scope.insert(name.clone(), value);

            ExpressionType::Primitive(PrimitiveType::Void)
        }
        AstNodeValue::Return(returned) => {
            typecheck_expression(returned, outer_scopes, current_scope, expressions)?;

            ExpressionType::Primitive(PrimitiveType::Void)
        }
        AstNodeValue::Name(name) => {
            resolve(name, current_scope, outer_scopes).ok_or(TypecheckError::NameNotFound(node))?
        }
        AstNodeValue::Int(_) => ExpressionType::Primitive(PrimitiveType::Int),
        AstNodeValue::Float(_) => ExpressionType::Primitive(PrimitiveType::Float),
        AstNodeValue::Bool(_) => ExpressionType::Primitive(PrimitiveType::Bool),
        AstNodeValue::BinExpr(BinOp::Dot | BinOp::Index, _, _) => todo!(),
        AstNodeValue::BinExpr(
            BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide,
            left,
            right,
        ) => {
            let ExpressionType::Primitive(left) =
                typecheck_expression(left, outer_scopes, current_scope, expressions)?
            else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };
            let ExpressionType::Primitive(right) =
                typecheck_expression(right, outer_scopes, current_scope, expressions)?
            else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };
            if left == right {
                ExpressionType::Primitive(left)
            } else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            }
        }
        AstNodeValue::BinExpr(BinOp::LessThan | BinOp::GreaterThan, left, right) => {
            let ExpressionType::Primitive(_) =
                typecheck_expression(left, outer_scopes, current_scope, expressions)?
            else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };
            let ExpressionType::Primitive(_) =
                typecheck_expression(right, outer_scopes, current_scope, expressions)?
            else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };

            ExpressionType::Primitive(PrimitiveType::Bool)
        }
        AstNodeValue::BinExpr(BinOp::Assignment, left, right) => {
            // TODO: ensure left is a valid lvalue
            let left = typecheck_expression(left, outer_scopes, current_scope, expressions)?;
            let right = typecheck_expression(right, outer_scopes, current_scope, expressions)?;

            if !is_assignable_to(&left, &right) {
                return Err(TypecheckError::TypeMismatch {
                    received: right,
                    expected: left,
                });
            }

            ExpressionType::Primitive(PrimitiveType::Void)
        }
        AstNodeValue::If(condition, body) | AstNodeValue::While(condition, body) => {
            let condition =
                typecheck_expression(condition, outer_scopes, current_scope, expressions)?;
            if !matches!(condition, ExpressionType::Primitive(PrimitiveType::Bool)) {
                return Err(TypecheckError::TypeMismatch {
                    received: condition,
                    expected: ExpressionType::Primitive(PrimitiveType::Bool),
                });
            }

            typecheck_expression(body, outer_scopes, current_scope, expressions)?
        }
        AstNodeValue::Block(children) => {
            let mut scopes: Vec<&HashMap<_, _>> = Vec::with_capacity(outer_scopes.len() + 1);
            scopes.push(current_scope);
            scopes.extend_from_slice(outer_scopes);

            let mut child_scope = HashMap::new();
            let mut expr_ty = ExpressionType::Primitive(PrimitiveType::Void);
            for child in children.iter() {
                expr_ty = typecheck_expression(child, &scopes[..], &mut child_scope, expressions)?;
            }
            expr_ty
        }
        AstNodeValue::Call(func, args) => {
            let ExpressionType::Func(func) =
                typecheck_expression(func, outer_scopes, current_scope, expressions)?
            else {
                return Err(TypecheckError::CantCall(node));
            };

            if func.params.len() != args.len() {
                return Err(TypecheckError::WrongArgsCount(node));
            }

            for (arg, param) in args.iter().zip(func.params.iter()) {
                let arg = typecheck_expression(arg, outer_scopes, current_scope, expressions)?;
                if !is_assignable_to(&param, &arg) {
                    return Err(TypecheckError::TypeMismatch {
                        received: arg,
                        expected: *param,
                    });
                }
            }

            func.returns
        }
        AstNodeValue::TakeUnique(_) => todo!(),
        AstNodeValue::TakeShared(_) => todo!(),
        AstNodeValue::StructLiteral { .. } => todo!(),
        AstNodeValue::ArrayLiteral(_) => todo!(),
        AstNodeValue::ArrayLiteralLength(_, _) => todo!(),
    };

    expressions.insert(node.id, ty);

    Ok(*expressions.get(&node.id).unwrap())
}

fn resolve<'a>(
    name: &str,
    current_scope: &HashMap<String, ExpressionType<'a>>,
    outer_scopes: &[&HashMap<String, ExpressionType<'a>>],
) -> Option<ExpressionType<'a>> {
    current_scope
        .get(name)
        .or(outer_scopes.iter().find_map(|scope| scope.get(name)))
        .copied()
}

// TODO
fn is_assignable_to<'a>(left: &ExpressionType<'a>, right: &ExpressionType<'a>) -> bool {
    return left == right;
}
