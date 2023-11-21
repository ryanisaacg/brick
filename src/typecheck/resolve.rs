use std::collections::HashMap;

use crate::parser::{
    AstNode, AstNodeValue, ExternFunctionBindingValue, FunctionDeclarationValue, NameAndType,
    StructDeclarationValue,
};

use super::{
    ExpressionType, FuncType, ModuleDeclaration, PointerKind, PrimitiveType, StructType,
    TypecheckError,
};

pub fn name_to_declaration<'a>(
    source: impl Iterator<Item = &'a AstNode<'a>>,
) -> HashMap<String, &'a AstNode<'a>> {
    let mut nodes = HashMap::new();
    for statement in source {
        match &statement.value {
            AstNodeValue::StructDeclaration(StructDeclarationValue { name, .. })
            | AstNodeValue::FunctionDeclaration(FunctionDeclarationValue { name, .. })
            | AstNodeValue::ExternFunctionBinding(ExternFunctionBindingValue { name, .. }) => {
                nodes.insert(name.clone(), statement);
            }
            _ => {}
        }
    }

    nodes
}

// TODO
pub fn resolve_top_level_declarations<'a>(
    names_to_declarations: &HashMap<String, &'a AstNode<'a>>,
) -> Result<HashMap<String, ModuleDeclaration>, TypecheckError<'a>> {
    names_to_declarations
        .iter()
        .map(|(name, node)| {
            Ok((
                name.clone(),
                match &node.value {
                    AstNodeValue::StructDeclaration(StructDeclarationValue { fields, .. }) => {
                        ModuleDeclaration::Struct(StructType {
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
                    AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                        params,
                        returns,
                        ..
                    })
                    | AstNodeValue::ExternFunctionBinding(ExternFunctionBindingValue {
                        params,
                        returns,
                        ..
                    }) => ModuleDeclaration::Func(FuncType {
                        id: node.id,
                        params: params
                            .iter()
                            .map(|NameAndType { type_, .. }| {
                                resolve_type_name(&names_to_declarations, type_)
                            })
                            .collect::<Result<Vec<_>, _>>()?,
                        returns: returns
                            .map(|returns| resolve_type_name(&names_to_declarations, returns))
                            .unwrap_or(Ok(ExpressionType::Primitive(PrimitiveType::Void)))?,
                    }),
                    _ => panic!("internal compiler error: unexpected decl node"),
                },
            ))
        })
        .collect::<Result<HashMap<_, _>, _>>()
}

pub fn resolve_type_name<'a>(
    types: &HashMap<String, &'a AstNode<'a>>,
    node: &'a AstNode<'a>,
) -> Result<ExpressionType, TypecheckError<'a>> {
    Ok(match &node.value {
        AstNodeValue::Name(name) => match name.as_str() {
            "bool" => ExpressionType::Primitive(PrimitiveType::Bool),
            "void" => ExpressionType::Primitive(PrimitiveType::Void),
            "i32" => ExpressionType::Primitive(PrimitiveType::Int32),
            "f32" => ExpressionType::Primitive(PrimitiveType::Float32),
            "i64" => ExpressionType::Primitive(PrimitiveType::Int64),
            "f64" => ExpressionType::Primitive(PrimitiveType::Float64),
            other => ExpressionType::Named(
                types
                    .get(other)
                    .ok_or(TypecheckError::NameNotFound(node))?
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
        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::Declaration(_, _)
        | AstNodeValue::Import(_)
        | AstNodeValue::Return(_)
        | AstNodeValue::Statement(_)
        | AstNodeValue::Int(_)
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
        | AstNodeValue::Block(_) => {
            // TODO: report error
            panic!("Illegal in expression name");
        }
    })
}
