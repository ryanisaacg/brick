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

#[derive(Debug)]
pub enum Type<'a> {
    Func(FuncType<'a>),
    Struct(StructType<'a>),
    Ref(TypeReference<'a>),
}

#[derive(Debug)]
pub struct StructType<'a> {
    pub fields: HashMap<String, TypeReference<'a>>,
}

#[derive(Debug)]
pub struct FuncType<'a> {
    pub params: Vec<TypeReference<'a>>,
    pub returns: TypeReference<'a>,
}

#[derive(Debug)]
pub enum TypeReference<'a> {
    Primitive(PrimitiveType),
    Resolved(&'a AstNode<'a>),
    Unresolved(String),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    Int,
    Float,
    Bool,
    Void,
}

#[derive(Debug)]
pub enum ExpressionType<'a> {
    Primitive(PrimitiveType),
    Reference(&'a Type<'a>),
}

#[derive(Debug, Error)]
pub enum TypecheckError<'a> {
    #[error("arithmetic")]
    ArithmeticMismatch(&'a AstNode<'a>), // TODO
    #[error("mismatched types: received {received:?}, expected {expected:?}")]
    TypeMismatch {
        expected: ExpressionType<'a>,
        received: ExpressionType<'a>,
    },
}

// TODO: pass import namespace in
pub fn typecheck<'a>(file: ParsedSourceFile<'a>) {
    //let module = resolve_module(file);
    let types = HashMap::new();
    for type_decl in file.types.values() {
        match type_decl {
            TypeDeclaration::Struct(decl) => {
                // TODO: create a type object to represent this struct
            }
        }
    }
    for func_decl in file.functions.values() {
        // TODO: create a type object to represent this function
    }
    // TODO: handle imports and namespaces

    for function in file.functions.values() {
        match function {
            CallableDeclaration::Function(func) => typecheck_function(func, &types),
            CallableDeclaration::External(_) => {}
        }
    }
}

// TODO: some sort of data structure to store the results?
fn typecheck_function<'a>(
    function: &'a FunctionDeclarationValue<'a>,
    types: &HashMap<String, Type<'a>>,
) {
    let cfg = build_control_flow_graph(&function.body);
    //let mut expressions = HashMap::new();
    typecheck_expression(&function.body, types).unwrap();
    // TODO: assign a type to every expression in the function body
    // TODO: ensure that the return type matches the function's declared return type
}

// TODO: allow ; to turn expressions into void
// TODO: if-else need to unify
fn typecheck_expression<'a>(
    node: &'a AstNode<'a>,
    types: &HashMap<String, Type<'a>>,
    //expressions: &mut HashMap<ID, &'a Type<'parser>>,
) -> Result<ExpressionType<'a>, TypecheckError<'a>> {
    Ok(match &node.value {
        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::Import(_) => {
            unimplemented!("Can't do this inside a function");
        }
        AstNodeValue::Declaration(_, _) => todo!(),
        AstNodeValue::Return(returned) => {
            typecheck_expression(returned, types)?;

            ExpressionType::Primitive(PrimitiveType::Void)
        }
        AstNodeValue::Name(_) => todo!(),
        AstNodeValue::Int(_) => ExpressionType::Primitive(PrimitiveType::Int),
        AstNodeValue::Float(_) => ExpressionType::Primitive(PrimitiveType::Float),
        AstNodeValue::Bool(_) => ExpressionType::Primitive(PrimitiveType::Bool),
        AstNodeValue::BinExpr(BinOp::Dot | BinOp::Index, _, _) => todo!(),
        AstNodeValue::BinExpr(
            BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide,
            left,
            right,
        ) => {
            let ExpressionType::Primitive(left) = typecheck_expression(left, types)? else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };
            let ExpressionType::Primitive(right) = typecheck_expression(right, types)? else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };
            if left == right {
                ExpressionType::Primitive(left)
            } else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            }
        }
        AstNodeValue::BinExpr(BinOp::LessThan | BinOp::GreaterThan, left, right) => {
            let ExpressionType::Primitive(_) = typecheck_expression(left, types)? else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };
            let ExpressionType::Primitive(_) = typecheck_expression(right, types)? else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };

            ExpressionType::Primitive(PrimitiveType::Bool)
        }
        AstNodeValue::BinExpr(BinOp::Assignment, _, _) => todo!(),
        AstNodeValue::If(condition, body) | AstNodeValue::While(condition, body) => {
            let condition = typecheck_expression(condition, types)?;
            if !matches!(condition, ExpressionType::Primitive(PrimitiveType::Bool)) {
                return Err(TypecheckError::TypeMismatch {
                    received: condition,
                    expected: ExpressionType::Primitive(PrimitiveType::Bool),
                });
            }

            typecheck_expression(body, types)?
        }
        AstNodeValue::Block(children) => {
            let mut expr_ty = ExpressionType::Primitive(PrimitiveType::Void);
            for child in children.iter() {
                expr_ty = typecheck_expression(child, types)?;
            }
            expr_ty
        }
        AstNodeValue::Call(_, _) => todo!(),
        AstNodeValue::TakeUnique(_) => todo!(),
        AstNodeValue::TakeShared(_) => todo!(),
        AstNodeValue::StructLiteral { .. } => todo!(),
        AstNodeValue::ArrayLiteral(_) => todo!(),
        AstNodeValue::ArrayLiteralLength(_, _) => todo!(),
        AstNodeValue::UniqueType(_) => todo!(),
        AstNodeValue::SharedType(_) => todo!(),
        AstNodeValue::ArrayType(_) => todo!(),
    })
}
