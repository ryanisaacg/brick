use std::collections::HashMap;

use thiserror::Error;

use crate::{
    id::{AnyID, FunctionID, NodeID, TypeID},
    parser::{
        AstNode, AstNodeValue, BinOp, FunctionDeclarationValue, IfDeclaration,
        InterfaceDeclarationValue, StructDeclarationValue,
    },
    provenance::SourceRange,
};

use self::{control_flow_graph::build_control_flow_graph, resolve::resolve_type_expr};

mod control_flow_graph;
pub mod resolve;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionType {
    Void,
    Primitive(PrimitiveType),
    InstanceOf(TypeID),
    ReferenceTo(TypeID),
    Pointer(PointerKind, Box<ExpressionType>),
    Collection(CollectionType),
    Null,
    Nullable(Box<ExpressionType>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CollectionType {
    Array(Box<ExpressionType>),
    Dict(Box<ExpressionType>, Box<ExpressionType>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PointerKind {
    Shared,
    Unique,
}

#[derive(Debug, PartialEq, Eq)]
pub enum StaticDeclaration {
    Func(FuncType),
    Struct(StructType),
    Interface(InterfaceType),
    Union(UnionType),
    Module(ModuleType),
}

impl StaticDeclaration {
    pub fn id(&self) -> TypeID {
        match self {
            StaticDeclaration::Func(inner) => inner.id,
            StaticDeclaration::Struct(inner) => inner.id,
            StaticDeclaration::Interface(inner) => inner.id,
            StaticDeclaration::Union(inner) => inner.id,
            StaticDeclaration::Module(inner) => inner.id,
        }
    }

    pub fn fn_id_or_type_id(&self) -> AnyID {
        match self {
            StaticDeclaration::Func(inner) => inner.func_id.into(),
            StaticDeclaration::Struct(inner) => inner.id.into(),
            StaticDeclaration::Interface(inner) => inner.id.into(),
            StaticDeclaration::Union(inner) => inner.id.into(),
            StaticDeclaration::Module(inner) => inner.id.into(),
        }
    }

    pub fn unwrap_fn_id(&self) -> FunctionID {
        match self {
            StaticDeclaration::Func(inner) => inner.func_id,
            _ => panic!("Not a function: {:?}", self),
        }
    }

    pub fn ref_to(&self) -> ExpressionType {
        ExpressionType::ReferenceTo(self.id())
    }

    pub fn visit<'a>(&'a self, visitor: &mut impl FnMut(&'a StaticDeclaration)) {
        visitor(self);
        match self {
            StaticDeclaration::Module(module) => {
                for child in module.exports.values() {
                    child.visit(visitor);
                }
            }
            StaticDeclaration::Struct(StructType {
                associated_functions,
                ..
            })
            | StaticDeclaration::Interface(InterfaceType {
                associated_functions,
                ..
            }) => {
                for child in associated_functions.values() {
                    child.visit(visitor);
                }
            }
            StaticDeclaration::Func(_) | StaticDeclaration::Union(_) => {}
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleType {
    pub id: TypeID,
    pub exports: HashMap<String, StaticDeclaration>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructType {
    pub id: TypeID,
    pub fields: HashMap<String, ExpressionType>,
    pub associated_functions: HashMap<String, StaticDeclaration>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncType {
    pub id: TypeID,
    pub func_id: FunctionID,
    pub params: Vec<ExpressionType>,
    pub returns: ExpressionType,
    pub is_associated: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnionType {
    pub id: TypeID,
    pub variants: HashMap<String, ExpressionType>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InterfaceType {
    pub id: TypeID,
    pub associated_functions: HashMap<String, StaticDeclaration>,
}

pub fn find_func<'a>(
    decls: &HashMap<TypeID, &'a StaticDeclaration>,
    id: FunctionID,
) -> Option<&'a FuncType> {
    decls.values().find_map(|decl| match decl {
        StaticDeclaration::Func(func) if func.func_id == id => Some(func),
        _ => None,
    })
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    Char,
    String, // TODO: should this be a non-primitive type?
    Int32,
    Float32,
    Int64,
    Float64,
    Bool,

    // TODO?
    PointerSize,
    FunctionID,
}

// TODO: get the lifetimes out of these typecheck errors
#[derive(Debug, Error)]
pub enum TypecheckError {
    #[error("arithmetic")]
    ArithmeticMismatch(SourceRange), // TODO
    #[error("mismatched types: received {received:?}, expected {expected:?}")]
    TypeMismatch {
        // TODO: provenance
        expected: ExpressionType,
        received: ExpressionType,
    },
    #[error("declaration for {0:?} not found")]
    NameNotFound(SourceRange), // TODO
    #[error("can't call")]
    CantCall(SourceRange), // TODO
    #[error("wrong args count")]
    WrongArgsCount(SourceRange),
    #[error("missing field")]
    MissingField(SourceRange),
}

struct Declarations<'a> {
    name_to_type_id: HashMap<&'a str, TypeID>,
    name_to_context_entry: HashMap<String, (AnyID, ExpressionType)>,
    id_to_decl: HashMap<TypeID, &'a StaticDeclaration>,
}

impl<'a> Declarations<'a> {
    fn decl(&self, id: &TypeID) -> Option<&StaticDeclaration> {
        self.id_to_decl.get(id).copied()
    }

    fn id_to_func(&self, id: &TypeID) -> &FuncType {
        let expr = self.decl(id).expect("function with ID should exist");
        match expr {
            StaticDeclaration::Func(inner) => inner,
            _ => panic!("ID unexpectedly pointed to non-function"),
        }
    }
}

pub struct TypecheckedFile<'a> {
    pub functions: Vec<TypecheckedFunction<'a>>,
    pub associated_functions: HashMap<NodeID, Vec<TypecheckedFunction<'a>>>,
    // TODO: convert this into a Sequence?
    pub top_level_statements: Vec<&'a AstNode<'a>>,
}

#[derive(Clone, Debug)]
pub struct TypecheckedFunction<'a> {
    pub id: FunctionID,
    pub name: String,
    pub func: &'a FunctionDeclarationValue<'a>,
}

// TODO: pass import namespace in
pub fn typecheck<'a>(
    file: impl Iterator<Item = &'a AstNode<'a>>,
    current_module_name: &str,
    declarations: &HashMap<String, StaticDeclaration>,
) -> Result<TypecheckedFile<'a>, TypecheckError> {
    // TODO: verify validity of type and function declarations

    let mut name_to_context_entry = HashMap::new();
    let mut name_to_type_id = HashMap::new();
    let mut id_to_decl = HashMap::new();

    for (name, value) in declarations {
        match value {
            StaticDeclaration::Module(module) if current_module_name == name => {
                for (name, value) in module.exports.iter() {
                    name_to_context_entry
                        .insert(name.clone(), (value.fn_id_or_type_id(), value.ref_to()));
                    name_to_type_id.insert(name.as_str(), value.id());
                }
            }
            _ => {
                name_to_context_entry
                    .insert(name.clone(), (value.fn_id_or_type_id(), value.ref_to()));
                name_to_type_id.insert(name.as_str(), value.id());
            }
        }
        value.visit(&mut |decl| {
            id_to_decl.insert(decl.id(), decl);
        });
    }

    let context = Declarations {
        name_to_context_entry,
        name_to_type_id,
        id_to_decl,
    };

    // TODO: handle free-floating statements

    let mut functions = Vec::new();
    let mut top_level_statements = Vec::new();
    let mut top_level_scope = HashMap::new();
    let mut associated_functions = HashMap::new();

    for statement in file {
        typecheck_node(
            statement,
            &context,
            &mut top_level_scope,
            &mut functions,
            &mut top_level_statements,
            &mut associated_functions,
        )?;
    }

    Ok(TypecheckedFile {
        functions,
        associated_functions,
        top_level_statements,
    })
}

fn typecheck_node<'ast>(
    statement: &'ast AstNode<'ast>,
    context: &Declarations,
    top_level_scope: &mut HashMap<String, (AnyID, ExpressionType)>,
    functions: &mut Vec<TypecheckedFunction<'ast>>,
    top_level_statements: &mut Vec<&AstNode<'ast>>,
    associated_functions_for_type: &mut HashMap<NodeID, Vec<TypecheckedFunction<'ast>>>,
) -> Result<(), TypecheckError> {
    match &statement.value {
        AstNodeValue::FunctionDeclaration(func) => {
            // TODO: bubble errors
            typecheck_function(func, context).unwrap();
            functions.push(TypecheckedFunction {
                id: func.id,
                name: func.name.clone(),
                func,
            });
        }
        AstNodeValue::StructDeclaration(StructDeclarationValue {
            associated_functions,
            ..
        })
        | AstNodeValue::InterfaceDeclaration(InterfaceDeclarationValue {
            associated_functions,
            ..
        }) => {
            let associated_functions = associated_functions
                .iter()
                .filter_map(|function| {
                    let AstNodeValue::FunctionDeclaration(func) = &function.value else {
                        return None;
                    };
                    typecheck_function(func, context).unwrap();
                    Some(TypecheckedFunction {
                        id: func.id,
                        name: func.name.clone(),
                        func,
                    })
                })
                .collect::<Vec<_>>();
            functions.extend(associated_functions.iter().cloned());
            associated_functions_for_type.insert(statement.id, associated_functions);
        }
        // These nodes don't execute anything and therefore don't need to be typechecked
        AstNodeValue::Import(_)
        | AstNodeValue::UnionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_) => {}
        _ => {
            typecheck_expression(
                statement,
                &[&context.name_to_context_entry],
                top_level_scope,
                context,
            )?;
            top_level_statements.push(statement);
        }
    }

    Ok(())
}

// TODO: some sort of data structure to store the results?
fn typecheck_function<'a>(
    function: &'a FunctionDeclarationValue<'a>,
    context: &Declarations,
) -> Result<(), TypecheckError> {
    let function_type = find_func(&context.id_to_decl, function.id).unwrap();
    let parameters = function
        .params
        .iter()
        .zip(function_type.params.iter())
        // TODO
        .map(|((id, name), param)| (name.name.clone(), ((*id).into(), param.clone())))
        .collect();

    let return_value = typecheck_expression(
        function.body,
        &[&context.name_to_context_entry, &parameters],
        &mut HashMap::new(),
        context,
    )?;
    let _cfg = build_control_flow_graph(function.body);
    // TODO: check all the return values for matching
    // TODO: check to see reachability

    if !is_assignable_to(&context.id_to_decl, &function_type.returns, return_value) {
        return Err(TypecheckError::TypeMismatch {
            expected: function_type.returns.clone(),
            received: return_value.clone(),
        });
    }

    Ok(())
}

// TODO: allow ; to turn expressions into void
// TODO: if-else need to unify
fn typecheck_expression<'a>(
    node: &'a AstNode<'a>,
    outer_scopes: &[&HashMap<String, (AnyID, ExpressionType)>],
    current_scope: &mut HashMap<String, (AnyID, ExpressionType)>,
    context: &Declarations,
) -> Result<&'a ExpressionType, TypecheckError> {
    let ty = match &node.value {
        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::RequiredFunction(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::UnionDeclaration(_)
        | AstNodeValue::InterfaceDeclaration(_)
        | AstNodeValue::Import(_) => {
            unimplemented!("Can't do this inside a function");
        }
        AstNodeValue::UniqueType(_)
        | AstNodeValue::SharedType(_)
        | AstNodeValue::ArrayType(_)
        | AstNodeValue::NullableType(_) => {
            panic!("illegal type expression in function body");
        }
        AstNodeValue::Statement(inner) => {
            typecheck_expression(inner, outer_scopes, current_scope, context)?;
            ExpressionType::Void
        }
        AstNodeValue::Declaration(name, type_hint, value) => {
            // TODO: do I want shadowing? currently this shadows
            let value = typecheck_expression(value, outer_scopes, current_scope, context)?;
            if let Some(type_hint) = type_hint {
                let ty = resolve_type_expr(&context.name_to_type_id, type_hint)?;
                // TODO: generate type error
                assert!(is_assignable_to(&context.id_to_decl, &ty, value));
                current_scope.insert(name.clone(), (node.id.into(), ty));
            } else {
                current_scope.insert(name.clone(), (node.id.into(), value.clone()));
            }

            ExpressionType::Void
        }
        // TODO: You can only from within a function
        AstNodeValue::Return(returned) => {
            typecheck_expression(returned, outer_scopes, current_scope, context)?;

            ExpressionType::Void
        }
        AstNodeValue::Name {
            value: name,
            referenced_id,
        } => {
            let (ref_id, expr) = resolve_name(name, current_scope, outer_scopes)
                .ok_or(TypecheckError::NameNotFound(node.provenance.clone()))?;
            referenced_id
                .set(ref_id)
                .expect("each node should be visited once");
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
        AstNodeValue::StringLiteral(_) => ExpressionType::Primitive(PrimitiveType::String),
        AstNodeValue::CharLiteral(_) => ExpressionType::Primitive(PrimitiveType::Char),
        AstNodeValue::Null => ExpressionType::Null,
        AstNodeValue::Bool(_) => ExpressionType::Primitive(PrimitiveType::Bool),
        AstNodeValue::BinExpr(BinOp::Dot, left, right) => {
            let left = typecheck_expression(left, outer_scopes, current_scope, context)?;
            let ExpressionType::InstanceOf(id) = fully_dereference(left) else {
                panic!("TODO: left side of dot operator");
            };
            let AstNodeValue::Name { value: name, .. } = &right.value else {
                panic!("TODO: right side of dot operator");
            };
            // TODO: fallible
            let lhs_type = context.decl(id);
            match lhs_type {
                Some(StaticDeclaration::Struct(StructType {
                    fields,
                    associated_functions,
                    ..
                })) => fields
                    .get(name)
                    .cloned()
                    .or_else(|| {
                        associated_functions
                            .get(name)
                            .map(|decl| ExpressionType::InstanceOf(decl.id()))
                    })
                    .expect("TODO: field is present"),
                Some(StaticDeclaration::Interface(InterfaceType {
                    associated_functions,
                    ..
                })) => associated_functions
                    .get(name)
                    .map(|decl| ExpressionType::InstanceOf(decl.id()))
                    .expect("TODO: field is present"),
                Some(StaticDeclaration::Union(lhs_type)) => ExpressionType::Nullable(Box::new(
                    lhs_type
                        .variants
                        .get(name)
                        .expect("TODO: variant is present")
                        .clone(),
                )),
                Some(StaticDeclaration::Module(lhs_type)) => lhs_type
                    .exports
                    .get(name)
                    .expect("TODO: export is present")
                    .ref_to(),
                _ => todo!("illegal lhs type"),
            }
        }
        AstNodeValue::BinExpr(BinOp::Index, collection, index) => {
            let collection_ty =
                typecheck_expression(collection, outer_scopes, current_scope, context)?;
            match collection_ty {
                ExpressionType::Collection(CollectionType::Array(item_ty)) => {
                    let index_ty =
                        typecheck_expression(index, outer_scopes, current_scope, context)?;
                    if !is_assignable_to(
                        &context.id_to_decl,
                        &ExpressionType::Primitive(PrimitiveType::PointerSize),
                        index_ty,
                    ) {
                        return Err(TypecheckError::TypeMismatch {
                            received: index_ty.clone(),
                            expected: ExpressionType::Primitive(PrimitiveType::Int32),
                        });
                    }

                    *item_ty.clone()
                }
                ExpressionType::Collection(CollectionType::Dict(key_ty, value_ty)) => {
                    let index_ty =
                        typecheck_expression(index, outer_scopes, current_scope, context)?;
                    if !is_assignable_to(&context.id_to_decl, index_ty, key_ty) {
                        return Err(TypecheckError::TypeMismatch {
                            received: index_ty.clone(),
                            expected: ExpressionType::Primitive(PrimitiveType::Int32),
                        });
                    }

                    *value_ty.clone()
                }
                _ => todo!("can't index that"),
            }
        }
        AstNodeValue::BinExpr(
            BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide,
            left,
            right,
        ) => {
            let left = typecheck_expression(left, outer_scopes, current_scope, context)?;
            let ExpressionType::Primitive(_) = fully_dereference(left) else {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            };
            let right = typecheck_expression(right, outer_scopes, current_scope, context)?;
            let ExpressionType::Primitive(_) = fully_dereference(right) else {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            };
            if is_assignable_to(&context.id_to_decl, left, right) {
                left.clone()
            } else if is_assignable_to(&context.id_to_decl, right, left) {
                right.clone()
            } else {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            }
        }
        // TODO: non-numeric equality
        AstNodeValue::BinExpr(
            BinOp::LessThan
            | BinOp::GreaterThan
            | BinOp::LessEqualThan
            | BinOp::GreaterEqualThan
            | BinOp::EqualTo
            | BinOp::NotEquals,
            left,
            right,
        ) => {
            let left = typecheck_expression(left, outer_scopes, current_scope, context)?;
            let ExpressionType::Primitive(_) = fully_dereference(left) else {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            };
            let right = typecheck_expression(right, outer_scopes, current_scope, context)?;
            let ExpressionType::Primitive(_) = fully_dereference(right) else {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            };

            ExpressionType::Primitive(PrimitiveType::Bool)
        }
        AstNodeValue::BinExpr(BinOp::Assignment, left, right) => {
            let left_ty = typecheck_expression(left, outer_scopes, current_scope, context)?;
            // TODO: actual type errors
            // TODO: validate legal type to assign
            assert!(validate_lvalue(left));
            let right = typecheck_expression(right, outer_scopes, current_scope, context)?;

            if !is_assignable_to(&context.id_to_decl, left_ty, right) {
                return Err(TypecheckError::TypeMismatch {
                    received: right.clone(),
                    expected: left_ty.clone(),
                });
            }

            ExpressionType::Void
        }
        AstNodeValue::BinExpr(
            BinOp::AddAssign | BinOp::SubtractAssign | BinOp::MultiplyAssign | BinOp::DivideAssign,
            left,
            right,
        ) => {
            let left_ty = typecheck_expression(left, outer_scopes, current_scope, context)?;
            assert!(validate_lvalue(left));
            let right = typecheck_expression(right, outer_scopes, current_scope, context)?;

            if !matches!(fully_dereference(left_ty), ExpressionType::Primitive(_))
                || !matches!(fully_dereference(right), ExpressionType::Primitive(_))
            {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            }

            if !is_assignable_to(&context.id_to_decl, left_ty, right) {
                return Err(TypecheckError::TypeMismatch {
                    received: right.clone(),
                    expected: left_ty.clone(),
                });
            }

            ExpressionType::Void
        }
        AstNodeValue::While(condition, body) => {
            let condition = typecheck_expression(condition, outer_scopes, current_scope, context)?;
            let condition_deref = fully_dereference(condition);
            if !matches!(
                condition_deref,
                ExpressionType::Primitive(PrimitiveType::Bool)
            ) {
                return Err(TypecheckError::TypeMismatch {
                    received: condition.clone(),
                    expected: ExpressionType::Primitive(PrimitiveType::Bool),
                });
            }

            typecheck_expression(body, outer_scopes, current_scope, context)?;

            ExpressionType::Void
        }
        AstNodeValue::If(IfDeclaration {
            condition,
            if_branch,
            else_branch,
        }) => {
            let condition = typecheck_expression(condition, outer_scopes, current_scope, context)?;
            let condition_deref = fully_dereference(condition);
            if !matches!(
                condition_deref,
                ExpressionType::Primitive(PrimitiveType::Bool)
            ) {
                return Err(TypecheckError::TypeMismatch {
                    received: condition.clone(),
                    expected: ExpressionType::Primitive(PrimitiveType::Bool),
                });
            }

            let if_branch = typecheck_expression(if_branch, outer_scopes, current_scope, context)?;
            let else_branch = else_branch
                .as_ref()
                .map(|else_branch| {
                    typecheck_expression(else_branch, outer_scopes, current_scope, context)
                })
                .transpose()?;

            match else_branch {
                Some(else_branch) => {
                    if if_branch == else_branch {
                        if_branch.clone()
                    } else {
                        // TODO: typecheck error here UNLESS there's a break/return
                        panic!("if and else don't match");
                    }
                }
                None => ExpressionType::Void,
            }
        }
        AstNodeValue::Block(children) => {
            let mut scopes: Vec<&HashMap<_, _>> = Vec::with_capacity(outer_scopes.len() + 1);
            scopes.push(current_scope);
            scopes.extend_from_slice(outer_scopes);

            let mut child_scope = HashMap::new();
            let mut expr_ty = ExpressionType::Void;
            for (index, child) in children.iter().enumerate() {
                expr_ty =
                    typecheck_expression(child, &scopes[..], &mut child_scope, context)?.clone();
                if index != children.len() - 1 && expr_ty != ExpressionType::Void {
                    return Err(TypecheckError::TypeMismatch {
                        expected: ExpressionType::Void,
                        received: expr_ty,
                    });
                }
            }
            expr_ty
        }
        AstNodeValue::Call(func, args) => {
            let (ExpressionType::InstanceOf(func) | ExpressionType::ReferenceTo(func)) =
                typecheck_expression(func, outer_scopes, current_scope, context)?
            else {
                return Err(TypecheckError::CantCall(node.provenance.clone()));
            };
            let func = context.id_to_func(func);

            let params = if func.is_associated {
                &func.params[1..]
            } else {
                &func.params[..]
            };

            if params.len() != args.len() {
                return Err(TypecheckError::WrongArgsCount(node.provenance.clone()));
            }

            for (arg, param) in args.iter().zip(params.iter()) {
                let arg = typecheck_expression(arg, outer_scopes, current_scope, context)?;
                if !is_assignable_to(&context.id_to_decl, param, arg) {
                    return Err(TypecheckError::TypeMismatch {
                        received: arg.clone(),
                        expected: param.clone(),
                    });
                }
            }

            func.returns.clone()
        }
        AstNodeValue::StructLiteral { name, fields } => {
            let ExpressionType::ReferenceTo(ty_id) =
                typecheck_expression(name, outer_scopes, current_scope, context)?
            else {
                return Err(TypecheckError::CantCall(node.provenance.clone()));
            };

            match context.decl(ty_id).unwrap() {
                StaticDeclaration::Struct(struct_type) => {
                    if struct_type.fields.len() != fields.len() {
                        return Err(TypecheckError::WrongArgsCount(node.provenance.clone()));
                    }

                    for (name, param_field) in struct_type.fields.iter() {
                        let Some(arg_field) = fields.get(name) else {
                            return Err(TypecheckError::MissingField(node.provenance.clone()));
                        };
                        let arg_field =
                            typecheck_expression(arg_field, outer_scopes, current_scope, context)?;
                        if !is_assignable_to(&context.id_to_decl, param_field, arg_field) {
                            return Err(TypecheckError::TypeMismatch {
                                received: arg_field.clone(),
                                expected: param_field.clone(),
                            });
                        }
                    }

                    ExpressionType::InstanceOf(*ty_id)
                }
                StaticDeclaration::Union(union_ty) => {
                    if fields.len() != 1 {
                        return Err(TypecheckError::WrongArgsCount(node.provenance.clone()));
                    }

                    let (name, arg) = fields.iter().next().unwrap();
                    let Some(param) = union_ty.variants.get(name) else {
                        return Err(TypecheckError::MissingField(node.provenance.clone()));
                    };
                    let arg = typecheck_expression(arg, outer_scopes, current_scope, context)?;
                    if !is_assignable_to(&context.id_to_decl, param, arg) {
                        return Err(TypecheckError::TypeMismatch {
                            received: param.clone(),
                            expected: arg.clone(),
                        });
                    }

                    ExpressionType::InstanceOf(*ty_id)
                }
                StaticDeclaration::Func(_) => todo!(),
                StaticDeclaration::Interface(_) => todo!(),
                StaticDeclaration::Module(_) => todo!(),
            }
        }
        AstNodeValue::DictLiteral(entries) => {
            let mut result_key_ty = None;
            let mut result_value_ty = None;
            for (key, value) in entries.iter() {
                let key_ty = typecheck_expression(key, outer_scopes, current_scope, context)?;
                if let Some(expected_key_ty) = result_key_ty {
                    if !is_assignable_to(&context.id_to_decl, key_ty, expected_key_ty) {
                        return Err(TypecheckError::TypeMismatch {
                            received: key_ty.clone(),
                            expected: expected_key_ty.clone(),
                        });
                    }
                } else {
                    result_key_ty = Some(key_ty);
                }
                let value_ty = typecheck_expression(value, outer_scopes, current_scope, context)?;
                if let Some(expected_value_ty) = result_value_ty {
                    if !is_assignable_to(&context.id_to_decl, value_ty, expected_value_ty) {
                        return Err(TypecheckError::TypeMismatch {
                            received: value_ty.clone(),
                            expected: expected_value_ty.clone(),
                        });
                    }
                } else {
                    result_value_ty = Some(value_ty);
                }
            }

            ExpressionType::Collection(CollectionType::Dict(
                Box::new(result_key_ty.unwrap().clone()),
                Box::new(result_value_ty.unwrap().clone()),
            ))
        }
        // TODO: assert that this is an lvalue
        AstNodeValue::TakeUnique(inner) => ExpressionType::Pointer(
            PointerKind::Unique,
            Box::new(typecheck_expression(inner, outer_scopes, current_scope, context)?.clone()),
        ),
        AstNodeValue::TakeRef(inner) => ExpressionType::Pointer(
            PointerKind::Shared,
            Box::new(typecheck_expression(inner, outer_scopes, current_scope, context)?.clone()),
        ),
        AstNodeValue::Deref(inner) => {
            let ty = typecheck_expression(inner, outer_scopes, current_scope, context)?;
            let ExpressionType::Pointer(_, ty) = ty else {
                todo!("compile error for illegal deref")
            };
            *ty.clone()
        }
        AstNodeValue::ArrayLiteral(items) => {
            if items.is_empty() {
                todo!("how to typecheck 0-length collections?");
            }
            let mut iter = items.iter();
            let ty =
                typecheck_expression(iter.next().unwrap(), outer_scopes, current_scope, context)?
                    .clone();
            for remaining in iter {
                let this_ty =
                    typecheck_expression(remaining, outer_scopes, current_scope, context)?;
                if &ty != this_ty {
                    return Err(TypecheckError::TypeMismatch {
                        received: this_ty.clone(),
                        expected: ty,
                    });
                }
            }
            ExpressionType::Collection(CollectionType::Array(Box::new(ty)))
        }
        AstNodeValue::ArrayLiteralLength(value, length) => {
            let inner = typecheck_expression(value, outer_scopes, current_scope, context)?;
            let length = typecheck_expression(length, outer_scopes, current_scope, context)?;
            // TODO: array index type
            if length != &ExpressionType::Primitive(PrimitiveType::Int32) {
                return Err(TypecheckError::TypeMismatch {
                    expected: ExpressionType::Primitive(PrimitiveType::Int32),
                    received: length.clone(),
                });
            }
            ExpressionType::Collection(CollectionType::Array(Box::new(inner.clone())))
        }
    };

    node.ty.set(ty).expect("each node should be visited once");

    Ok(node.ty.get().expect("just set"))
}

fn validate_lvalue(lvalue: &AstNode<'_>) -> bool {
    match &lvalue.value {
        // TODO: remove this if I add auto-deref later
        AstNodeValue::Name { .. } => true,
        AstNodeValue::Deref(inner) => {
            let Some(ExpressionType::Pointer(kind, _)) = inner.ty.get() else {
                unreachable!()
            };
            *kind == PointerKind::Unique && validate_lvalue(inner)
        }
        AstNodeValue::BinExpr(BinOp::Dot | BinOp::Index, lhs, _) => validate_lvalue(lhs),

        AstNodeValue::BinExpr(_, _, _)
        | AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::UnionDeclaration(_)
        | AstNodeValue::InterfaceDeclaration(_)
        | AstNodeValue::RequiredFunction(_)
        | AstNodeValue::Declaration(_, _, _)
        | AstNodeValue::Import(_)
        | AstNodeValue::Return(_)
        | AstNodeValue::Statement(_)
        | AstNodeValue::Int(_)
        | AstNodeValue::Float(_)
        | AstNodeValue::Bool(_)
        | AstNodeValue::CharLiteral(_)
        | AstNodeValue::StringLiteral(_)
        | AstNodeValue::Null
        | AstNodeValue::If(_)
        | AstNodeValue::While(_, _)
        | AstNodeValue::Call(_, _)
        | AstNodeValue::TakeUnique(_)
        | AstNodeValue::TakeRef(_)
        | AstNodeValue::StructLiteral { .. }
        | AstNodeValue::DictLiteral(_)
        | AstNodeValue::ArrayLiteral(_)
        | AstNodeValue::ArrayLiteralLength(_, _)
        | AstNodeValue::Block(_)
        | AstNodeValue::UniqueType(_)
        | AstNodeValue::SharedType(_)
        | AstNodeValue::ArrayType(_)
        | AstNodeValue::NullableType(_) => false,
    }
}

fn resolve_name(
    name: &str,
    current_scope: &HashMap<String, (AnyID, ExpressionType)>,
    outer_scopes: &[&HashMap<String, (AnyID, ExpressionType)>],
) -> Option<(AnyID, ExpressionType)> {
    current_scope
        .get(name)
        .or(outer_scopes.iter().find_map(|scope| scope.get(name)))
        .cloned()
}

pub fn is_assignable_to(
    id_to_decl: &HashMap<TypeID, &StaticDeclaration>,
    left: &ExpressionType,
    right: &ExpressionType,
) -> bool {
    use ExpressionType::*;
    use PrimitiveType::*;

    match (left, right) {
        // Kinda a special case, but a function that returns void should accept a void block
        (Void, Void) => true,
        // Void can never be assigned to or be assigned from
        // Null can never be assigned to
        (Void, _) | (_, Void) | (Null, _) => false,

        // Handle pointers and de-referencing
        (Pointer(left_ty, left_inner), Pointer(right_ty, right_inner)) => {
            (left_ty == right_ty
                || *left_ty == PointerKind::Shared && *right_ty == PointerKind::Unique)
                && is_assignable_to(id_to_decl, left_inner, right_inner)
        }
        // TODO: auto-dereference in the IR
        (left, Pointer(_, right_inner)) => is_assignable_to(id_to_decl, left, right_inner),
        // TODO: support auto-dereferencing on lhs
        (Pointer(_, _), _right) => false,

        // Nullability
        (Nullable(_), Null) => true,
        (_, Null) => false,
        (Nullable(left), Nullable(right)) => is_assignable_to(id_to_decl, left, right),
        (Nullable(left), right) => is_assignable_to(id_to_decl, left, right),
        (_, Nullable(_)) => false,

        (Primitive(Float32), Primitive(Int32))
        | (Primitive(Float64), Primitive(Int32))
        | (Primitive(Float64), Primitive(Int64))
        | (Primitive(Int64), Primitive(Int32))
        | (Primitive(Float64), Primitive(Float32))
        | (Primitive(PointerSize), Primitive(Int32)) => true,
        (Primitive(left), Primitive(right)) => left == right,
        (Primitive(_), _) => false,

        (InstanceOf(left), InstanceOf(right)) => {
            let left = id_to_decl[left];
            let right = id_to_decl[right];
            use StaticDeclaration::*;
            match (left, right) {
                // TODO: support function pointer types?
                (Func(left), Func(right)) => {
                    left.params
                        .iter()
                        .zip(right.params.iter())
                        .all(|(left, right)| left == right)
                        && left.returns == right.returns
                }
                (Func(_), _) => false,

                // TODO: support structural struct conversions?
                (Struct(_), Struct(_)) => left == right,
                (Struct(_), _) => false,

                // TODO: support structural union conversions?
                (Union(_), Union(_)) => left == right,
                (Union(_), _) => false,

                (
                    Interface(InterfaceType {
                        associated_functions: lhs_assoc,
                        ..
                    }),
                    Struct(StructType {
                        associated_functions: rhs_assoc,
                        ..
                    }),
                ) => lhs_assoc.iter().all(|(name, lhs_ty)| {
                    let Some(rhs_ty) = rhs_assoc.get(name) else {
                        return false;
                    };
                    let StaticDeclaration::Func(lhs) = lhs_ty else {
                        return false;
                    };
                    let StaticDeclaration::Func(rhs) = rhs_ty else {
                        return false;
                    };
                    // Ignore the first argument to both associated functions -
                    // the type will differ because it's a self param
                    lhs.params[1..]
                        .iter()
                        .zip(rhs.params[1..].iter())
                        .all(|(lhs, rhs)| lhs == rhs)
                        && lhs.returns == rhs.returns
                }),
                // TODO: support structural interface conversions?
                (Interface(_), Interface(_)) => left == right,
                (Interface(_), Func(_)) => todo!(),
                (Interface(_), Union(_)) => todo!(),

                // TODO: support module -> interface conversion
                (_, Module(_)) => false,
                // You can never assign to a module
                (Module(_), _) => false,
            }
        }

        // TODO: could these ever be valid?
        (InstanceOf(_), Primitive(_)) | (InstanceOf(_), Collection(_)) => false,

        // TODO: arrays
        (Collection(_), Primitive(_))
        | (Collection(_), InstanceOf(_))
        | (Collection(_), Collection(_)) => todo!(),

        // TODO: type references can't ever be in assignments right
        (ReferenceTo(_), _) | (_, ReferenceTo(_)) => todo!("{:?} = {:?}", left, right),
    }
}

pub fn fully_dereference(ty: &ExpressionType) -> &ExpressionType {
    if let ExpressionType::Pointer(_, inner) = ty {
        fully_dereference(inner)
    } else {
        ty
    }
}
