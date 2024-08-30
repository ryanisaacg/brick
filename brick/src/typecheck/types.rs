use std::collections::HashMap;

use crate::{
    id::{AnyID, FunctionID},
    SourceRange, TypeID, TypecheckError,
};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum ExpressionType {
    Void,
    Unreachable,
    Primitive(PrimitiveType),
    InstanceOf(TypeID),
    ReferenceToType(TypeID),
    ReferenceToFunction(FunctionID),
    Pointer(PointerKind, Box<ExpressionType>),
    Collection(CollectionType),
    Null,
    Nullable(Box<ExpressionType>),
    TypeParameterReference(usize),
    Generator {
        yield_ty: Box<ExpressionType>,
        param_ty: Box<ExpressionType>,
    },
    FunctionReference {
        parameters: Vec<ExpressionType>,
        returns: Box<ExpressionType>,
    },
}

impl ExpressionType {
    pub fn type_id(&self) -> Option<&TypeID> {
        match self {
            ExpressionType::InstanceOf(id) | ExpressionType::ReferenceToType(id) => Some(id),
            ExpressionType::Void
            | ExpressionType::ReferenceToFunction(_)
            | ExpressionType::Unreachable
            | ExpressionType::Primitive(_)
            | ExpressionType::Pointer(_, _)
            | ExpressionType::Collection(_)
            | ExpressionType::Null
            | ExpressionType::Nullable(_)
            | ExpressionType::TypeParameterReference(_)
            | ExpressionType::Generator { .. }
            | ExpressionType::FunctionReference { .. } => None,
        }
    }

    pub fn is_affine(&self, declarations: &HashMap<TypeID, TypeDeclaration>) -> bool {
        match self {
            ExpressionType::Nullable(inner) => inner.is_affine(declarations),
            ExpressionType::Void
                | ExpressionType::Unreachable
                | ExpressionType::Null
                // Only if references truly aren't allowed in re-assignments
                | ExpressionType::Pointer(_, _)
                | ExpressionType::Primitive(_) => false,
            ExpressionType::InstanceOf(id) => declarations[id].is_affine(),
            ExpressionType::ReferenceToType(_)
            | ExpressionType::TypeParameterReference(_)
            | ExpressionType::Collection(_)
            | ExpressionType::Generator { .. }
            | ExpressionType::ReferenceToFunction(_)
            | ExpressionType::FunctionReference { .. } => true,
        }
    }

    pub fn is_reference(&self) -> bool {
        match self {
            ExpressionType::Collection(_)
            | ExpressionType::Null
            | ExpressionType::Void
            | ExpressionType::Unreachable
            | ExpressionType::Primitive(_)
            | ExpressionType::InstanceOf(_)
            | ExpressionType::Generator { .. }
            | ExpressionType::FunctionReference { .. }
            | ExpressionType::ReferenceToType(_)
            | ExpressionType::ReferenceToFunction(_)
            | ExpressionType::TypeParameterReference(_) => false,
            ExpressionType::Pointer(_, _) => true,
            ExpressionType::Nullable(inner) => inner.is_reference(),
        }
    }

    pub(super) fn resolve_generics(&mut self, bindings: &[ExpressionType]) {
        match self {
            ExpressionType::Void
            | ExpressionType::Unreachable
            | ExpressionType::Primitive(_)
            | ExpressionType::InstanceOf(_)
            | ExpressionType::ReferenceToType(_)
            | ExpressionType::ReferenceToFunction(_)
            | ExpressionType::Null
            | ExpressionType::Collection(CollectionType::String) => {}
            ExpressionType::Nullable(child)
            | ExpressionType::Pointer(_, child)
            | ExpressionType::Collection(
                CollectionType::Array(child)
                | CollectionType::ReferenceCounter(child)
                | CollectionType::Cell(child),
            ) => {
                child.resolve_generics(bindings);
            }
            ExpressionType::Collection(CollectionType::Dict(key, value)) => {
                key.resolve_generics(bindings);
                value.resolve_generics(bindings);
            }
            ExpressionType::TypeParameterReference(idx) => {
                *self = bindings[*idx].clone();
            }
            ExpressionType::Generator { yield_ty, param_ty } => {
                yield_ty.resolve_generics(bindings);
                param_ty.resolve_generics(bindings);
            }
            ExpressionType::FunctionReference {
                parameters,
                returns,
            } => {
                for param in parameters.iter_mut() {
                    param.resolve_generics(bindings);
                }
                returns.resolve_generics(bindings);
            }
        }
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum CollectionType {
    Array(Box<ExpressionType>),
    Dict(Box<ExpressionType>, Box<ExpressionType>),
    ReferenceCounter(Box<ExpressionType>),
    Cell(Box<ExpressionType>),
    String,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum PointerKind {
    Shared,
    Unique,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TypeDeclaration {
    Struct(StructType),
    Interface(InterfaceType),
    Union(UnionType),
    Module(ModuleType),
}

impl TypeDeclaration {
    pub fn id(&self) -> TypeID {
        match self {
            TypeDeclaration::Struct(inner) => inner.id,
            TypeDeclaration::Interface(inner) => inner.id,
            TypeDeclaration::Union(inner) => inner.id,
            TypeDeclaration::Module(inner) => inner.id,
        }
    }

    pub fn provenance(&self) -> Option<&SourceRange> {
        match self {
            TypeDeclaration::Struct(StructType { provenance, .. }) => provenance.as_ref(),
            TypeDeclaration::Interface(InterfaceType { provenance, .. }) => provenance.as_ref(),
            TypeDeclaration::Union(UnionType { provenance, .. }) => provenance.as_ref(),
            TypeDeclaration::Module(ModuleType { provenance, .. }) => provenance.as_ref(),
        }
    }

    pub fn fn_id_or_type_id(&self) -> AnyID {
        match self {
            TypeDeclaration::Struct(inner) => inner.id.into(),
            TypeDeclaration::Interface(inner) => inner.id.into(),
            TypeDeclaration::Union(inner) => inner.id.into(),
            TypeDeclaration::Module(inner) => inner.id.into(),
        }
    }

    pub fn is_affine(&self) -> bool {
        match self {
            TypeDeclaration::Struct(decl) => decl.is_affine,
            TypeDeclaration::Interface(_) => false,
            TypeDeclaration::Union(decl) => decl.is_affine,
            TypeDeclaration::Module(_) => false,
        }
    }

    pub fn ref_to(&self) -> ExpressionType {
        ExpressionType::ReferenceToType(self.id())
    }

    pub fn field_access(
        &self,
        field: &str,
        provenance: &SourceRange,
    ) -> Result<ExpressionType, TypecheckError> {
        match self {
            TypeDeclaration::Struct(StructType {
                fields,
                associated_functions,
                ..
            }) => fields
                .get(field)
                .cloned()
                .or_else(|| {
                    associated_functions
                        .get(field)
                        .map(|decl| ExpressionType::ReferenceToFunction(*decl))
                })
                .ok_or_else(|| {
                    TypecheckError::FieldNotPresent(field.to_string(), provenance.clone())
                }),
            TypeDeclaration::Interface(InterfaceType {
                associated_functions,
                ..
            }) => associated_functions
                .get(field)
                .map(|decl| ExpressionType::ReferenceToFunction(*decl))
                .ok_or_else(|| {
                    TypecheckError::FieldNotPresent(field.to_string(), provenance.clone())
                }),
            TypeDeclaration::Union(lhs_type) => Ok(ExpressionType::Nullable(Box::new(
                lhs_type
                    .variants
                    .get(field)
                    .ok_or_else(|| {
                        TypecheckError::FieldNotPresent(field.to_string(), provenance.clone())
                    })?
                    .clone()
                    .unwrap_or(ExpressionType::Void),
            ))),
            TypeDeclaration::Module(lhs_type) => {
                lhs_type.exports.get(field).cloned().ok_or_else(|| {
                    TypecheckError::FieldNotPresent(field.to_string(), provenance.clone())
                })
            }
        }
    }

    pub fn as_module(&self) -> Option<&ModuleType> {
        match self {
            TypeDeclaration::Struct(_)
            | TypeDeclaration::Interface(_)
            | TypeDeclaration::Union(_) => None,
            TypeDeclaration::Module(module) => Some(module),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ModuleType {
    pub id: TypeID,
    pub exports: HashMap<String, ExpressionType>,
    pub provenance: Option<SourceRange>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct StructType {
    pub id: TypeID,
    pub fields: HashMap<String, ExpressionType>,
    pub associated_functions: HashMap<String, FunctionID>,
    pub is_affine: bool,
    pub provenance: Option<SourceRange>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncType {
    pub id: FunctionID,
    pub type_param_count: usize,
    pub params: Vec<ExpressionType>,
    pub returns: ExpressionType,
    pub is_associated: bool,
    pub is_coroutine: bool,
    pub provenance: Option<SourceRange>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnionType {
    pub id: TypeID,
    pub variant_order: Vec<String>,
    pub variants: HashMap<String, Option<ExpressionType>>,
    pub is_affine: bool,
    pub provenance: Option<SourceRange>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct InterfaceType {
    pub id: TypeID,
    pub associated_functions: HashMap<String, FunctionID>,
    pub provenance: Option<SourceRange>,
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum PrimitiveType {
    Char,
    Int32,
    Float32,
    Int64,
    Float64,
    Bool,
    PointerSize,
}
