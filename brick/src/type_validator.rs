use rayon::prelude::*;
use thiserror::Error;

use crate::{
    diagnostic::{Diagnostic, DiagnosticContents, DiagnosticMarker},
    multi_error::{merge_results, print_multi_errors, MultiError},
    typecheck::{InterfaceType, PointerKind, StructType},
    DeclarationContext, ExpressionType, FuncType, SourceRange, TypeDeclaration,
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
    merge_results(
        &mut validate_results,
        decls
            .id_to_decl
            .par_iter()
            .map(|(_, decl)| validate_decl(decls, decl))
            .reduce(
                || Ok(()),
                |mut acc, result| {
                    merge_results(&mut acc, result);
                    acc
                },
            ),
    );
    merge_results(
        &mut validate_results,
        decls
            .id_to_func
            .par_iter()
            .map(|(_, decl)| validate_fn(decl))
            .reduce(
                || Ok(()),
                |mut acc, result| {
                    merge_results(&mut acc, result);
                    acc
                },
            ),
    );

    validate_results
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
