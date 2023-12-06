use std::collections::HashMap;

use crate::parser::{
    AstNode, AstNodeValue, ExternFunctionBindingValue, FunctionDeclarationValue, NameAndType,
    StructDeclarationValue, UnionDeclarationValue,
};

use super::{
    ExpressionType, FuncType, PointerKind, PrimitiveType, StaticDeclaration, StructType,
    TypecheckError, UnionType,
};

pub fn resolve_module(source: &[AstNode<'_>]) -> HashMap<String, StaticDeclaration> {
    let mut names_to_declarations = HashMap::new();
    for statement in source.iter() {
        match &statement.value {
            AstNodeValue::StructDeclaration(StructDeclarationValue { name, .. })
            | AstNodeValue::UnionDeclaration(UnionDeclarationValue { name, .. })
            | AstNodeValue::FunctionDeclaration(FunctionDeclarationValue { name, .. })
            | AstNodeValue::ExternFunctionBinding(ExternFunctionBindingValue { name, .. }) => {
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
    names_to_declarations
        .iter()
        .map(|(name, node)| {
            Ok((
                name.clone(),
                match &node.value {
                    AstNodeValue::StructDeclaration(StructDeclarationValue { fields, .. }) => {
                        StaticDeclaration::Struct(StructType {
                            id: node.id,
                            fields: fields
                                .iter()
                                .map(|NameAndType { id: _, name, type_ }| {
                                    Ok((
                                        name.clone(),
                                        resolve_type_name(&names_to_declarations, type_)?,
                                    ))
                                })
                                .collect::<Result<HashMap<_, _>, _>>()?,
                        })
                    }
                    AstNodeValue::UnionDeclaration(UnionDeclarationValue { variants, .. }) => {
                        StaticDeclaration::Union(UnionType {
                            id: node.id,
                            variants: variants
                                .iter()
                                .map(|NameAndType { id: _, name, type_ }| {
                                    Ok((
                                        name.clone(),
                                        resolve_type_name(&names_to_declarations, type_)?,
                                    ))
                                })
                                .collect::<Result<HashMap<_, _>, _>>()?,
                        })
                    }
                    // TODO: union
                    AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                        params,
                        returns,
                        ..
                    })
                    | AstNodeValue::ExternFunctionBinding(ExternFunctionBindingValue {
                        params,
                        returns,
                        ..
                    }) => StaticDeclaration::Func(FuncType {
                        id: node.id,
                        params: params
                            .iter()
                            .map(|NameAndType { type_, .. }| {
                                resolve_type_name(&names_to_declarations, type_)
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                        returns: returns
                            .as_ref()
                            .map(|returns| resolve_type_name(&names_to_declarations, returns))
                            .unwrap_or(Ok(ExpressionType::Primitive(PrimitiveType::Void)))?,
                    }),
                    _ => panic!("internal compiler error: unexpected decl node"),
                },
            ))
        })
        .collect::<Result<HashMap<_, _>, _>>()
}

pub fn resolve_type_name(
    types: &HashMap<String, &AstNode<'_>>,
    node: &AstNode<'_>,
) -> Result<ExpressionType, TypecheckError> {
    Ok(match &node.value {
        AstNodeValue::Name(name) => match name.as_str() {
            "bool" => ExpressionType::Primitive(PrimitiveType::Bool),
            "void" => ExpressionType::Primitive(PrimitiveType::Void),
            "i32" => ExpressionType::Primitive(PrimitiveType::Int32),
            "f32" => ExpressionType::Primitive(PrimitiveType::Float32),
            "i64" => ExpressionType::Primitive(PrimitiveType::Int64),
            "f64" => ExpressionType::Primitive(PrimitiveType::Float64),
            "char" => ExpressionType::Primitive(PrimitiveType::Char),
            "string" => ExpressionType::Primitive(PrimitiveType::String),
            other => ExpressionType::DeclaredType(
                types
                    .get(other)
                    .ok_or(TypecheckError::NameNotFound(node.provenance.clone()))?
                    .id,
            ),
        },
        AstNodeValue::UniqueType(inner) => ExpressionType::Pointer(
            PointerKind::Unique,
            Box::new(resolve_type_name(types, inner)?),
        ),
        AstNodeValue::SharedType(inner) => ExpressionType::Pointer(
            PointerKind::Shared,
            Box::new(resolve_type_name(types, inner)?),
        ),
        AstNodeValue::ArrayType(inner) => {
            ExpressionType::Array(Box::new(resolve_type_name(types, inner)?))
        }
        AstNodeValue::NullableType(inner) => {
            ExpressionType::Nullable(Box::new(resolve_type_name(types, inner)?))
        }
        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::UnionDeclaration(_)
        | AstNodeValue::Declaration(_, _)
        | AstNodeValue::Import(_)
        | AstNodeValue::Return(_)
        | AstNodeValue::Statement(_)
        | AstNodeValue::Int(_)
        | AstNodeValue::Null
        | AstNodeValue::Float(_)
        | AstNodeValue::Bool(_)
        | AstNodeValue::BinExpr(_, _, _)
        | AstNodeValue::If(_)
        | AstNodeValue::While(_, _)
        | AstNodeValue::Call(_, _)
        | AstNodeValue::TakeUnique(_)
        | AstNodeValue::TakeShared(_)
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
