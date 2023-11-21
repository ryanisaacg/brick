use std::collections::HashMap;

use thiserror::Error;

use crate::{
    id::ID,
    parser::{AstNode, AstNodeValue, BinOp, FunctionDeclarationValue, IfDeclaration},
};

use self::control_flow_graph::build_control_flow_graph;

mod control_flow_graph;
pub mod resolve;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionType {
    Primitive(PrimitiveType),
    Named(ID),
    Pointer(PointerKind, Box<ExpressionType>),
    Array(Box<ExpressionType>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PointerKind {
    Shared,
    Unique,
}

pub enum ModuleDeclaration {
    Func(FuncType),
    Struct(StructType),
}

impl ModuleDeclaration {
    pub fn id(&self) -> ID {
        match self {
            ModuleDeclaration::Func(inner) => inner.id,
            ModuleDeclaration::Struct(inner) => inner.id,
        }
    }

    pub fn expr(&self) -> ExpressionType {
        match self {
            ModuleDeclaration::Func(inner) => ExpressionType::Named(inner.id),
            ModuleDeclaration::Struct(inner) => ExpressionType::Named(inner.id),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructType {
    pub id: ID,
    pub fields: HashMap<String, ExpressionType>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncType {
    pub id: ID,
    pub params: Vec<ExpressionType>,
    pub returns: ExpressionType,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    Int32,
    Float32,
    Int64,
    Float64,
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
        expected: ExpressionType,
        received: ExpressionType,
    },
    #[error("declaration for {0:?} not found")]
    NameNotFound(&'a AstNode<'a>), // TODO
    #[error("can't call")]
    CantCall(&'a AstNode<'a>), // TODO
    #[error("wrong args count")]
    WrongArgsCount(&'a AstNode<'a>),
    #[error("missing field")]
    MissingField(&'a AstNode<'a>),
}

struct Declarations<'a> {
    name_to_expr: HashMap<String, (ID, ExpressionType)>,
    id_to_decl: HashMap<ID, &'a ModuleDeclaration>,
}

impl<'a> Declarations<'a> {
    fn decl(&self, id: &ID) -> Option<&ModuleDeclaration> {
        self.id_to_decl.get(id).copied()
    }

    fn name_to_func(&self, name: &str) -> Option<&FuncType> {
        if let (_, ExpressionType::Named(id)) = self.name_to_expr.get(name)? {
            Some(self.id_to_func(&id))
        } else {
            None
        }
    }

    fn id_to_func(&self, id: &ID) -> &FuncType {
        let expr = self.decl(id).expect("function with ID should exist");
        match expr {
            ModuleDeclaration::Func(inner) => inner,
            _ => panic!("ID unexpectedly pointed to non-function"),
        }
    }

    fn id_to_struct(&self, id: &ID) -> &StructType {
        let expr = self.decl(id).expect("struct with ID should exist");
        match expr {
            ModuleDeclaration::Struct(inner) => inner,
            _ => panic!("ID unexpectedly pointed to non-struct"),
        }
    }
}

pub struct TypecheckedFile<'a> {
    pub expression_types: HashMap<ID, ExpressionType>,
    pub referenced_ids: HashMap<ID, ID>,
    pub functions: Vec<TypecheckedFunction<'a>>,
    pub top_level_statements: Vec<&'a AstNode<'a>>,
}

pub struct TypecheckedFunction<'a> {
    pub id: ID,
    pub name: String,
    pub func: &'a FunctionDeclarationValue<'a>,
}

// TODO: pass import namespace in
pub fn typecheck<'a>(
    file: impl Iterator<Item = &'a AstNode<'a>>,
    declarations: &HashMap<String, ModuleDeclaration>,
) -> Result<TypecheckedFile<'a>, TypecheckError<'a>> {
    // TODO: verify validity of type and function declarations

    let mut name_to_expr = HashMap::new();
    let mut id_to_decl = HashMap::new();

    for (name, value) in declarations {
        name_to_expr.insert(name.clone(), (value.id(), value.expr()));
        id_to_decl.insert(value.id(), value);
    }

    let context = Declarations {
        name_to_expr,
        id_to_decl,
    };

    // TODO: handle free-floating statements

    let mut functions = Vec::new();
    let mut top_level_statements = Vec::new();
    let mut expression_types = HashMap::new();
    let mut referenced_ids = HashMap::new();

    for statement in file {
        match &statement.value {
            AstNodeValue::FunctionDeclaration(func) => {
                // TODO: bubble errors
                typecheck_function(func, &context, &mut expression_types, &mut referenced_ids)
                    .unwrap();
                functions.push(TypecheckedFunction {
                    id: statement.id,
                    name: func.name.clone(),
                    func,
                });
            }
            // These nodes don't execute anything and therefore don't need to be typechecked
            AstNodeValue::Import(_)
            | AstNodeValue::StructDeclaration(_)
            | AstNodeValue::ExternFunctionBinding(_) => {}
            _ => {
                typecheck_expression(
                    statement,
                    &[&context.name_to_expr],
                    &mut HashMap::new(),
                    &mut expression_types,
                    &mut referenced_ids,
                    &context,
                )?;
                top_level_statements.push(statement);
            }
        }
    }

    Ok(TypecheckedFile {
        expression_types,
        referenced_ids,
        functions,
        top_level_statements,
    })
}

// TODO: some sort of data structure to store the results?
fn typecheck_function<'a, 'b>(
    function: &'a FunctionDeclarationValue<'a>,
    context: &Declarations,
    expressions: &'b mut HashMap<ID, ExpressionType>,
    referenced_id: &'b mut HashMap<ID, ID>,
) -> Result<(), TypecheckError<'a>> {
    let Some(function_type) = context.name_to_func(&function.name) else {
        panic!("expected function to be found in the module");
    };
    let parameters = function
        .params
        .iter()
        .zip(function_type.params.iter())
        .map(|(name, param)| (name.name.clone(), (name.id, param.clone())))
        .collect();

    typecheck_expression(
        &function.body,
        &[&context.name_to_expr, &parameters],
        &mut HashMap::new(),
        expressions,
        referenced_id,
        context,
    )?;
    // TODO: ensure that the return type matches the function's declared return type
    let _cfg = build_control_flow_graph(&function.body);

    Ok(())
}

// TODO: allow ; to turn expressions into void
// TODO: if-else need to unify
fn typecheck_expression<'a, 'b>(
    node: &'a AstNode<'a>,
    outer_scopes: &[&HashMap<String, (ID, ExpressionType)>],
    current_scope: &mut HashMap<String, (ID, ExpressionType)>,
    expressions: &'b mut HashMap<ID, ExpressionType>,
    referenced_id: &'b mut HashMap<ID, ID>,
    context: &Declarations,
) -> Result<ExpressionType, TypecheckError<'a>> {
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
        AstNodeValue::Statement(inner) => {
            typecheck_expression(
                inner,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            ExpressionType::Primitive(PrimitiveType::Void)
        }
        AstNodeValue::Declaration(name, value) => {
            // TODO: do I want shadowing? currently this shadows
            let value = typecheck_expression(
                value,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            current_scope.insert(name.clone(), (node.id, value));

            ExpressionType::Primitive(PrimitiveType::Void)
        }
        // TODO: You can only from within a function
        AstNodeValue::Return(returned) => {
            typecheck_expression(
                returned,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;

            ExpressionType::Primitive(PrimitiveType::Void)
        }
        AstNodeValue::Name(name) => {
            let (ref_id, expr) = resolve_name(name, current_scope, outer_scopes)
                .ok_or(TypecheckError::NameNotFound(node))?;
            referenced_id.insert(node.id, ref_id);
            expr
        }
        AstNodeValue::Int(constant) => {
            if i32::try_from(*constant).is_ok() {
                ExpressionType::Primitive(PrimitiveType::Int32)
            } else {
                ExpressionType::Primitive(PrimitiveType::Int64)
            }
        }
        AstNodeValue::Float(constant) => {
            if *constant as f32 as f64 == *constant {
                ExpressionType::Primitive(PrimitiveType::Float32)
            } else {
                ExpressionType::Primitive(PrimitiveType::Float64)
            }
        }
        AstNodeValue::Bool(_) => ExpressionType::Primitive(PrimitiveType::Bool),
        AstNodeValue::BinExpr(BinOp::Dot, left, right) => {
            let left = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            let ExpressionType::Named(id) = left else {
                panic!("TODO: left side of dot operator");
            };
            // TODO: fallible
            let struct_type = context.id_to_struct(&id);
            let AstNodeValue::Name(name) = &right.value else {
                panic!("TODO: right side of dot operator");
            };
            struct_type
                .fields
                .get(name)
                .expect("TODO: field is present")
                .clone()
        }
        AstNodeValue::BinExpr(BinOp::Index, _, _) => todo!(),
        AstNodeValue::BinExpr(
            BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide,
            left,
            right,
        ) => {
            let left = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            let ExpressionType::Primitive(left) = fully_dereference(&left) else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };
            let right = typecheck_expression(
                right,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            let ExpressionType::Primitive(right) = fully_dereference(&right) else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };
            if left == right {
                ExpressionType::Primitive(*left)
            } else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            }
        }
        AstNodeValue::BinExpr(BinOp::LessThan | BinOp::GreaterThan, left, right) => {
            let left = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            let ExpressionType::Primitive(_) = fully_dereference(&left) else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };
            let right = typecheck_expression(
                right,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            let ExpressionType::Primitive(_) = fully_dereference(&right) else {
                return Err(TypecheckError::ArithmeticMismatch(node));
            };

            ExpressionType::Primitive(PrimitiveType::Bool)
        }
        AstNodeValue::BinExpr(BinOp::Assignment, left, right) => {
            // TODO: ensure left is a valid lvalue
            let left = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            let right = typecheck_expression(
                right,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;

            if !is_assignable_to(&left, &right) {
                return Err(TypecheckError::TypeMismatch {
                    received: right,
                    expected: left,
                });
            }

            ExpressionType::Primitive(PrimitiveType::Void)
        }
        AstNodeValue::If(IfDeclaration {
            condition,
            if_branch: body,
            else_branch: None,
        })
        | AstNodeValue::While(condition, body) => {
            let condition = typecheck_expression(
                condition,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            let condition_deref = fully_dereference(&condition);
            if !matches!(
                condition_deref,
                ExpressionType::Primitive(PrimitiveType::Bool)
            ) {
                return Err(TypecheckError::TypeMismatch {
                    received: condition,
                    expected: ExpressionType::Primitive(PrimitiveType::Bool),
                });
            }

            typecheck_expression(
                body,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;

            ExpressionType::Primitive(PrimitiveType::Void)
        }
        AstNodeValue::If(IfDeclaration {
            condition,
            if_branch,
            else_branch: Some(else_branch),
        }) => {
            let condition = typecheck_expression(
                condition,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            let condition_deref = fully_dereference(&condition);
            if !matches!(
                condition_deref,
                ExpressionType::Primitive(PrimitiveType::Bool)
            ) {
                return Err(TypecheckError::TypeMismatch {
                    received: condition,
                    expected: ExpressionType::Primitive(PrimitiveType::Bool),
                });
            }

            let if_branch = typecheck_expression(
                if_branch,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            let else_branch = typecheck_expression(
                else_branch,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;

            if if_branch == else_branch {
                if_branch
            } else {
                // TODO: typecheck error here UNLESS there's a break/return
                panic!("if and else don't match");
            }
        }
        AstNodeValue::Block(children) => {
            let mut scopes: Vec<&HashMap<_, _>> = Vec::with_capacity(outer_scopes.len() + 1);
            scopes.push(current_scope);
            scopes.extend_from_slice(outer_scopes);

            let mut child_scope = HashMap::new();
            let mut expr_ty = ExpressionType::Primitive(PrimitiveType::Void);
            for (index, child) in children.iter().enumerate() {
                expr_ty = typecheck_expression(
                    child,
                    &scopes[..],
                    &mut child_scope,
                    expressions,
                    referenced_id,
                    context,
                )?;
                if index != children.len() - 1
                    && expr_ty != ExpressionType::Primitive(PrimitiveType::Void)
                {
                    return Err(TypecheckError::TypeMismatch {
                        expected: ExpressionType::Primitive(PrimitiveType::Void),
                        received: expr_ty,
                    });
                }
            }
            expr_ty
        }
        AstNodeValue::Call(func, args) => {
            let ExpressionType::Named(func) = typecheck_expression(
                func,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?
            else {
                return Err(TypecheckError::CantCall(node));
            };
            let func = context.id_to_func(&func);

            if func.params.len() != args.len() {
                return Err(TypecheckError::WrongArgsCount(node));
            }

            for (arg, param) in args.iter().zip(func.params.iter()) {
                let arg = typecheck_expression(
                    arg,
                    outer_scopes,
                    current_scope,
                    expressions,
                    referenced_id,
                    context,
                )?;
                if !is_assignable_to(&param, &arg) {
                    return Err(TypecheckError::TypeMismatch {
                        received: arg,
                        expected: param.clone(),
                    });
                }
            }

            func.returns.clone()
        }
        AstNodeValue::StructLiteral { name, fields } => {
            let (ref_id, ExpressionType::Named(struct_type_id)) =
                resolve_name(name, current_scope, outer_scopes)
                    .ok_or(TypecheckError::NameNotFound(node))?
            else {
                return Err(TypecheckError::CantCall(node));
            };
            referenced_id.insert(node.id, ref_id);
            let struct_type = context.id_to_struct(&struct_type_id);

            if struct_type.fields.len() != fields.len() {
                return Err(TypecheckError::WrongArgsCount(node));
            }

            for (name, param_field) in struct_type.fields.iter() {
                let Some(arg_field) = fields.get(name) else {
                    return Err(TypecheckError::MissingField(node));
                };
                let arg_field = typecheck_expression(
                    arg_field,
                    outer_scopes,
                    current_scope,
                    expressions,
                    referenced_id,
                    context,
                )?;
                if !is_assignable_to(param_field, &arg_field) {
                    return Err(TypecheckError::TypeMismatch {
                        received: arg_field,
                        expected: param_field.clone(),
                    });
                }
            }

            ExpressionType::Named(struct_type_id)
        }
        // TODO: assert that this is an lvalue
        AstNodeValue::TakeUnique(inner) => ExpressionType::Pointer(
            PointerKind::Unique,
            Box::new(typecheck_expression(
                inner,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?),
        ),
        AstNodeValue::TakeShared(inner) => ExpressionType::Pointer(
            PointerKind::Shared,
            Box::new(typecheck_expression(
                inner,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?),
        ),
        AstNodeValue::ArrayLiteral(_items) => {
            // TODO: how to determine what the intended array type is?
            todo!();
        }
        AstNodeValue::ArrayLiteralLength(value, _length) => {
            let inner = typecheck_expression(
                value,
                outer_scopes,
                current_scope,
                expressions,
                referenced_id,
                context,
            )?;
            ExpressionType::Array(Box::new(inner))
        }
    };

    expressions.insert(node.id, ty);

    Ok(expressions.get(&node.id).unwrap().clone())
}

fn resolve_name(
    name: &str,
    current_scope: &HashMap<String, (ID, ExpressionType)>,
    outer_scopes: &[&HashMap<String, (ID, ExpressionType)>],
) -> Option<(ID, ExpressionType)> {
    current_scope
        .get(name)
        .or(outer_scopes.iter().find_map(|scope| scope.get(name)))
        .cloned()
}

// TODO
fn is_assignable_to(left: &ExpressionType, right: &ExpressionType) -> bool {
    match (left, right) {
        (
            ExpressionType::Pointer(left_ty, left_inner),
            ExpressionType::Pointer(right_ty, right_inner),
        ) => left_ty == right_ty && is_assignable_to(left_inner, right_inner),
        (left, ExpressionType::Pointer(_, right_inner)) => is_assignable_to(left, right_inner),
        (ExpressionType::Pointer(_, _), _right) => false,
        (left, right) => left == right,
    }
}

fn fully_dereference(ty: &ExpressionType) -> &ExpressionType {
    if let ExpressionType::Pointer(_, inner) = ty {
        fully_dereference(inner)
    } else {
        ty
    }
}
