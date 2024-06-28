use rayon::prelude::*;
use thiserror::Error;

use crate::{
    multi_error::{merge_results, print_multi_errors, MultiError},
    DeclarationContext, ExpressionType, FuncType, SourceRange, TypeDeclaration,
};

#[derive(Debug, Error)]
pub enum TypeValidationError {
    #[error("{}", print_multi_errors(&.0[..]))]
    MultiError(Vec<TypeValidationError>),
    #[error("illegal return of a reference from a function: {0}")]
    IllegalReferenceReturn(SourceRange),
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
            .map(|(_, decl)| validate_decl(decl))
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

fn validate_decl(_ty: &TypeDeclaration) -> Result<(), TypeValidationError> {
    Ok(())
}

fn validate_fn(fn_ty: &FuncType) -> Result<(), TypeValidationError> {
    let mut result = Ok(());

    if matches!(&fn_ty.returns, ExpressionType::Pointer(_, _)) {
        merge_results(
            &mut result,
            Err(TypeValidationError::IllegalReferenceReturn(
                fn_ty
                    .provenance
                    .clone()
                    .expect("all non-intrinsic functions have a provenance"),
            )),
        );
    }

    result
}
