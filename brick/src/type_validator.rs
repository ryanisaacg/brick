use std::collections::HashSet;

use rayon::prelude::*;
use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContents, DiagnosticMarker},
    multi_error::{merge_results, print_multi_errors, MultiError},
    typecheck::{InterfaceType, PointerKind, StructType, UnionType},
    DeclarationContext, ExpressionType, FuncType, SourceRange, TypeDeclaration, TypeID,
};

#[derive(Debug, Error)]
pub enum TypeValidationError {
    #[error("{}", print_multi_errors(&.0[..]))]
    MultiError(Vec<TypeValidationError>),
    #[error("illegal return of a reference from a function: {0}")]
    ReferenceReturn(SourceRange),
    #[error("drop may not be defined on normal types: {0}")]
    DropOnNormalType(SourceRange),
    #[error("drop may not be defined on interfaces: {0}")]
    DropOnInterface(SourceRange),
    #[error("drop must return void: {0}")]
    NonVoidDrop(SourceRange),
    #[error("drop must have one parameter: {0}")]
    WrongDropParamCount(SourceRange),
    #[error("parameter to drop must be a mutable self pointer: {0}")]
    IllegalDropParam(SourceRange),
    #[error("recursive type: {0}")]
    RecursiveType(SourceRange),
    #[error("non-affine type may not contain non-affine types: {0}")]
    IllegalAffineInNonAffine(SourceRange),
}

impl Diagnostic for TypeValidationError {
    fn contents(&self) -> DiagnosticContents {
        DiagnosticContents::Scalar(match self {
            TypeValidationError::MultiError(children) => {
                let mut contents = Vec::new();
                for child in children.iter() {
                    match child.contents() {
                        DiagnosticContents::Scalar(value) => contents.push(value),
                        DiagnosticContents::Vector(values) => contents.extend(values),
                    }
                }
                return DiagnosticContents::Vector(contents);
            }
            TypeValidationError::ReferenceReturn(range) => DiagnosticMarker::error(
                range.clone(),
                "illegal return of a reference from a function",
            ),
            TypeValidationError::DropOnNormalType(range) => DiagnosticMarker::error(
                range.clone(),
                "drop may not be defined on normal types (they must be affine)",
            ),
            TypeValidationError::DropOnInterface(range) => {
                DiagnosticMarker::error(range.clone(), "drop may not be defined on interfaces")
            }
            TypeValidationError::NonVoidDrop(range) => {
                DiagnosticMarker::error(range.clone(), "drop must return void")
            }
            TypeValidationError::WrongDropParamCount(range) => {
                DiagnosticMarker::error(range.clone(), "drop must have exactly one parameter")
            }
            TypeValidationError::IllegalDropParam(range) => DiagnosticMarker::error(
                range.clone(),
                "parameter to drop must be a mutable self pointer",
            ),
            TypeValidationError::RecursiveType(range) => DiagnosticMarker::error(
                range.clone(),
                "type is recursive without indirection, which would require infinite size",
            ),
            TypeValidationError::IllegalAffineInNonAffine(range) => DiagnosticMarker::error(
                range.clone(),
                "non-resource type may not have resource fields",
            ),
        })
    }
}

impl MultiError for TypeValidationError {
    fn from_error_list(list: Vec<Self>) -> Self {
        Self::MultiError(list)
    }

    fn as_error_list(&mut self) -> Option<&mut Vec<Self>> {
        match self {
            Self::MultiError(errs) => Some(errs),
            _ => None,
        }
    }
}

pub fn validate_types(decls: &DeclarationContext) -> Result<(), TypeValidationError> {
    let mut validate_results = Ok(());

    acc(
        &mut validate_results,
        decls
            .id_to_decl
            .par_iter()
            .map(|(_, decl)| validate_decl(decls, decl)),
    );
    acc(
        &mut validate_results,
        decls
            .id_to_func
            .par_iter()
            .map(|(_, decl)| validate_fn(decl)),
    );
    acc(
        &mut validate_results,
        decls
            .id_to_decl
            .par_iter()
            .map(|(_, decl)| validate_nonrecursive(decls, decl)),
    );
    acc(
        &mut validate_results,
        decls
            .id_to_decl
            .par_iter()
            .map(|(_, decl)| validate_affine_fields(decls, decl)),
    );

    validate_results
}

fn acc(
    validate_results: &mut Result<(), TypeValidationError>,
    iter: impl ParallelIterator<Item = Result<(), TypeValidationError>>,
) {
    merge_results(
        validate_results,
        iter.reduce(
            || Ok(()),
            |mut acc, result| {
                merge_results(&mut acc, result);
                acc
            },
        ),
    );
}

fn validate_decl(
    decls: &DeclarationContext,
    ty: &TypeDeclaration,
) -> Result<(), TypeValidationError> {
    let mut results = Ok(());

    merge_results(&mut results, validate_drop(decls, ty));

    results
}

fn validate_drop(
    decls: &DeclarationContext,
    ty: &TypeDeclaration,
) -> Result<(), TypeValidationError> {
    match ty {
        TypeDeclaration::Module(_) | TypeDeclaration::Union(_) => {}
        TypeDeclaration::Struct(StructType {
            associated_functions,
            ..
        }) => {
            if let Some((_, fn_id)) = associated_functions
                .iter()
                .find(|(name, _)| name.as_str() == "drop")
            {
                let fn_ty = &decls.id_to_func[fn_id];
                let provenance = fn_ty
                    .provenance
                    .clone()
                    .expect("all non-intrinsic functions have a provenance");

                let mut results = Ok(());

                if !ty.is_affine() {
                    merge_results(
                        &mut results,
                        Err(TypeValidationError::DropOnNormalType(provenance.clone())),
                    );
                }

                if fn_ty.returns != ExpressionType::Void {
                    merge_results(
                        &mut results,
                        Err(TypeValidationError::NonVoidDrop(provenance.clone())),
                    );
                }

                if fn_ty.params.len() != 1 {
                    merge_results(
                        &mut results,
                        Err(TypeValidationError::WrongDropParamCount(provenance.clone())),
                    );
                } else {
                    let is_self_unique_ptr =
                        if let ExpressionType::Pointer(PointerKind::Unique, inner) =
                            &fn_ty.params[0]
                        {
                            match inner.as_ref() {
                                ExpressionType::InstanceOf(ty_id) => *ty_id == ty.id(),
                                _ => false,
                            }
                        } else {
                            false
                        };
                    if !is_self_unique_ptr {
                        merge_results(
                            &mut results,
                            Err(TypeValidationError::IllegalDropParam(provenance.clone())),
                        );
                    }
                }

                return results;
            }
        }
        TypeDeclaration::Interface(InterfaceType {
            associated_functions,
            ..
        }) => {
            if let Some((_, fn_id)) = associated_functions
                .iter()
                .find(|(name, _)| name.as_str() == "drop")
            {
                let fn_ty = &decls.id_to_func[fn_id];
                return Err(TypeValidationError::DropOnInterface(
                    fn_ty
                        .provenance
                        .clone()
                        .expect("all non-intrinsic functions have a provenance"),
                ));
            }
        }
    }

    Ok(())
}

fn validate_fn(fn_ty: &FuncType) -> Result<(), TypeValidationError> {
    let mut result = Ok(());

    if matches!(&fn_ty.returns, ExpressionType::Pointer(_, _)) {
        merge_results(
            &mut result,
            Err(TypeValidationError::ReferenceReturn(
                fn_ty
                    .provenance
                    .clone()
                    .expect("all non-intrinsic functions have a provenance"),
            )),
        );
    }

    result
}

fn validate_nonrecursive(
    decls: &DeclarationContext,
    ty: &TypeDeclaration,
) -> Result<(), TypeValidationError> {
    if is_recursive(decls, ty.id(), &HashSet::new()) {
        Err(TypeValidationError::RecursiveType(
            ty.provenance()
                .expect("ICE: generated types must not be recursive")
                .clone(),
        ))
    } else {
        Ok(())
    }
}

fn is_recursive(decls: &DeclarationContext, current: TypeID, visited: &HashSet<TypeID>) -> bool {
    if visited.contains(&current) {
        return true;
    }
    let mut visited = visited.clone();
    visited.insert(current);

    match &decls.id_to_decl[&current] {
        TypeDeclaration::Struct(StructType { fields, .. }) => fields
            .values()
            .filter_map(|field| field.type_id())
            .any(|ty_id| is_recursive(decls, *ty_id, &visited)),
        TypeDeclaration::Union(UnionType { variants, .. }) => variants
            .values()
            .filter_map(|field| field.as_ref().and_then(|field| field.type_id()))
            .any(|ty_id| is_recursive(decls, *ty_id, &visited)),
        TypeDeclaration::Interface(_) | TypeDeclaration::Module(_) => false,
    }
}

fn validate_affine_fields(
    decls: &DeclarationContext,
    ty: &TypeDeclaration,
) -> Result<(), TypeValidationError> {
    match ty {
        TypeDeclaration::Struct(StructType {
            is_affine,
            fields,
            provenance,
            ..
        }) => {
            let non_affine_with_affine_children = !is_affine
                && fields
                    .values()
                    .any(|expr| expr.is_affine(&decls.id_to_decl));
            if non_affine_with_affine_children {
                Err(TypeValidationError::IllegalAffineInNonAffine(
                    provenance
                        .as_ref()
                        .expect("ICE: generated types must not have affinity errors")
                        .clone(),
                ))
            } else {
                Ok(())
            }
        }
        TypeDeclaration::Union(UnionType {
            is_affine,
            variants,
            provenance,
            ..
        }) => {
            let non_affine_with_affine_children = !is_affine
                && variants
                    .values()
                    .filter_map(|v| v.as_ref())
                    .any(|expr| expr.is_affine(&decls.id_to_decl));
            if non_affine_with_affine_children {
                Err(TypeValidationError::IllegalAffineInNonAffine(
                    provenance
                        .as_ref()
                        .expect("ICE: generated types must not have affinity errors")
                        .clone(),
                ))
            } else {
                Ok(())
            }
        }
        TypeDeclaration::Module(_) | TypeDeclaration::Interface(_) => Ok(()),
    }
}
