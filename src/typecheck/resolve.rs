use std::collections::HashMap;

use crate::{
    id::TypeID,
    parser::{
        AstNode, AstNodeValue, FunctionDeclarationValue, FunctionHeaderValue,
        InterfaceDeclarationValue, NameAndType, StructDeclarationValue, UnionDeclarationValue,
    },
};

use super::{
    CollectionType, ExpressionType, FuncType, InterfaceType, PointerKind, PrimitiveType,
    StaticDeclaration, StructType, TypecheckError, UnionType,
};

pub fn resolve_module(source: &[AstNode<'_>]) -> HashMap<String, StaticDeclaration> {
    let mut names_to_declarations = HashMap::new();
    for statement in source.iter() {
        match &statement.value {
            AstNodeValue::StructDeclaration(StructDeclarationValue { name, .. })
            | AstNodeValue::UnionDeclaration(UnionDeclarationValue { name, .. })
            | AstNodeValue::InterfaceDeclaration(InterfaceDeclarationValue { name, .. })
            | AstNodeValue::FunctionDeclaration(FunctionDeclarationValue { name, .. })
            | AstNodeValue::ExternFunctionBinding(FunctionHeaderValue { name, .. }) => {
                names_to_declarations.insert(name.clone(), statement);
            }
            _ => {}
        }
    }

    resolve_top_level_declarations(&names_to_declarations).unwrap()
}

// TODO
pub fn resolve_top_level_declarations(
    names_to_declarations: &HashMap<String, &AstNode<'_>>,
) -> Result<HashMap<String, StaticDeclaration>, TypecheckError> {
    let name_to_type_id = names_to_declarations
        .iter()
        .map(|(name, _)| (name.as_str(), TypeID::new()))
        .collect();
    names_to_declarations
        .iter()
        .map(|(name, node)| {
            Ok((
                name.clone(),
                resolve_declaration(&name_to_type_id, node, false)?,
            ))
        })
        .collect::<Result<HashMap<_, _>, _>>()
}

fn resolve_declaration(
    names_to_type_id: &HashMap<&str, TypeID>,
    node: &AstNode<'_>,
    is_associated: bool,
) -> Result<StaticDeclaration, TypecheckError> {
    Ok(match &node.value {
        AstNodeValue::StructDeclaration(StructDeclarationValue {
            fields,
            associated_functions,
            name,
        }) => {
            let fields = fields.iter().map(|NameAndType { id: _, name, ty }| {
                Ok((name.clone(), resolve_type_expr(names_to_type_id, ty)?))
            });
            let associated_functions = associated_functions
                .iter()
                .map(|node| {
                    Ok(match &node.value {
                        AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                            name,
                            ..
                        }) => (
                            name.clone(),
                            resolve_declaration(names_to_type_id, node, true)?,
                        ),
                        _ => panic!(
                            "Associated function should not be anything but function declaration"
                        ),
                    })
                })
                .collect::<Result<HashMap<_, _>, _>>()?;
            let fields = fields.collect::<Result<_, _>>()?;

            StaticDeclaration::Struct(StructType {
                id: names_to_type_id[name.as_str()],
                fields,
                associated_functions,
            })
        }
        AstNodeValue::InterfaceDeclaration(InterfaceDeclarationValue {
            associated_functions,
            name,
            ..
        }) => {
            let associated_functions = associated_functions
                .iter()
                .map(|node| {
                    Ok(match &node.value {
                        AstNodeValue::RequiredFunction(FunctionHeaderValue { name, .. }) => (
                            name.clone(),
                            resolve_declaration(names_to_type_id, node, true)?,
                        ),
                        AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                            name,
                            ..
                        }) => (
                            name.clone(),
                            resolve_declaration(names_to_type_id, node, true)?,
                        ),
                        _ => panic!(
                            "Associated function should not be anything but function declaration"
                        ),
                    })
                })
                .collect::<Result<HashMap<_, _>, _>>()?;

            StaticDeclaration::Interface(InterfaceType {
                id: names_to_type_id[name.as_str()],
                associated_functions,
            })
        }
        AstNodeValue::UnionDeclaration(UnionDeclarationValue { variants, name, .. }) => {
            StaticDeclaration::Union(UnionType {
                id: names_to_type_id[name.as_str()],
                variants: variants
                    .iter()
                    .map(
                        |NameAndType {
                             id: _,
                             name,
                             ty: type_,
                         }| {
                            Ok((name.clone(), resolve_type_expr(names_to_type_id, type_)?))
                        },
                    )
                    .collect::<Result<HashMap<_, _>, _>>()?,
            })
        }
        AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
            params,
            returns,
            id,
            name,
            ..
        }) => StaticDeclaration::Func(FuncType {
            id: names_to_type_id
                .get(name.as_str())
                .copied()
                .unwrap_or_else(TypeID::new),
            func_id: *id,
            params: params
                .iter()
                .map(|(_, NameAndType { ty: type_, .. })| {
                    resolve_type_expr(names_to_type_id, type_)
                })
                .collect::<Result<Vec<_>, _>>()?,
            returns: returns
                .as_ref()
                .map(|returns| resolve_type_expr(names_to_type_id, returns))
                .unwrap_or(Ok(ExpressionType::Void))?,
            is_associated,
        }),
        AstNodeValue::RequiredFunction(FunctionHeaderValue {
            params,
            returns,
            id,
            name,
        })
        | AstNodeValue::ExternFunctionBinding(FunctionHeaderValue {
            params,
            returns,
            id,
            name,
        }) => StaticDeclaration::Func(FuncType {
            id: names_to_type_id
                .get(name.as_str())
                .copied()
                .unwrap_or_else(TypeID::new),
            func_id: *id,
            params: params
                .iter()
                .map(|NameAndType { ty: type_, .. }| resolve_type_expr(names_to_type_id, type_))
                .collect::<Result<Vec<_>, _>>()?,
            returns: returns
                .as_ref()
                .map(|returns| resolve_type_expr(names_to_type_id, returns))
                .unwrap_or(Ok(ExpressionType::Void))?,
            is_associated,
        }),
        _ => panic!("internal compiler error: unexpected decl node"),
    })
}

pub fn resolve_type_expr(
    name_to_type_id: &HashMap<&str, TypeID>,
    node: &AstNode<'_>,
) -> Result<ExpressionType, TypecheckError> {
    Ok(match &node.value {
        AstNodeValue::Name { value: name, .. } => match name.as_str() {
            "bool" => ExpressionType::Primitive(PrimitiveType::Bool),
            "void" => ExpressionType::Void,
            "i32" => ExpressionType::Primitive(PrimitiveType::Int32),
            "f32" => ExpressionType::Primitive(PrimitiveType::Float32),
            "i64" => ExpressionType::Primitive(PrimitiveType::Int64),
            "f64" => ExpressionType::Primitive(PrimitiveType::Float64),
            "char" => ExpressionType::Primitive(PrimitiveType::Char),
            "string" => ExpressionType::Primitive(PrimitiveType::String),
            other => ExpressionType::DeclaredType(
                *name_to_type_id
                    .get(other)
                    .ok_or(TypecheckError::NameNotFound(node.provenance.clone()))?,
            ),
        },
        AstNodeValue::UniqueType(inner) => ExpressionType::Pointer(
            PointerKind::Unique,
            Box::new(resolve_type_expr(name_to_type_id, inner)?),
        ),
        AstNodeValue::SharedType(inner) => ExpressionType::Pointer(
            PointerKind::Shared,
            Box::new(resolve_type_expr(name_to_type_id, inner)?),
        ),
        AstNodeValue::ArrayType(inner) => ExpressionType::Collection(CollectionType::Array(
            Box::new(resolve_type_expr(name_to_type_id, inner)?),
        )),
        AstNodeValue::NullableType(inner) => {
            ExpressionType::Nullable(Box::new(resolve_type_expr(name_to_type_id, inner)?))
        }
        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::RequiredFunction(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::UnionDeclaration(_)
        | AstNodeValue::InterfaceDeclaration(_)
        | AstNodeValue::Declaration(_, _, _)
        | AstNodeValue::Import(_)
        | AstNodeValue::Return(_)
        | AstNodeValue::Statement(_)
        | AstNodeValue::Deref(_)
        | AstNodeValue::Int(_)
        | AstNodeValue::Null
        | AstNodeValue::Float(_)
        | AstNodeValue::Bool(_)
        | AstNodeValue::BinExpr(_, _, _)
        | AstNodeValue::If(_)
        | AstNodeValue::While(_, _)
        | AstNodeValue::Call(_, _)
        | AstNodeValue::TakeUnique(_)
        | AstNodeValue::TakeRef(_)
        | AstNodeValue::StructLiteral { .. }
        | AstNodeValue::ArrayLiteral(_)
        | AstNodeValue::ArrayLiteralLength(_, _)
        | AstNodeValue::Block(_)
        | AstNodeValue::StringLiteral(_)
        | AstNodeValue::CharLiteral(_)
        | AstNodeValue::DictLiteral(_) => {
            // TODO: report error
            panic!("Illegal in type name");
        }
    })
}
