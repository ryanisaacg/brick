use std::{error::Error, fmt::Display};

use crate::{
    diagnostic::{Diagnostic, DiagnosticContents, DiagnosticMarker},
    multi_error::MultiError,
    ExpressionType, SourceRange,
};

#[derive(Debug)]
pub enum TypecheckError {
    MultiError(Vec<TypecheckError>),
    ArithmeticMismatch(SourceRange),
    TypeMismatch {
        provenance: SourceRange,
        expected: ExpressionType,
        received: ExpressionType,
    },
    NameNotFound(SourceRange, String),
    CantCall(SourceRange),
    WrongArgsCount(SourceRange),
    MissingField(SourceRange),
    NoNullDeclarations(SourceRange),
    ExpectedNullableLHS(SourceRange),
    CannotYield(SourceRange),
    IllegalAssignmentLHS(SourceRange),
    IllegalDotLHS(SourceRange),
    MustReturnGenerator(SourceRange),
    CaseStatementRequiresUnion(SourceRange),
    IllegalDotRHS(SourceRange),
    BindingCountDoesntMatch(SourceRange),
    BindingNameDoesntMatch(SourceRange),
    DereferenceNonPointer(SourceRange),
    NonExhaustiveCase(SourceRange),
    IllegalFirstClassReference(SourceRange),
    IllegalNonRefBorrow(SourceRange),
    IllegalNonLvalueBorrow(SourceRange),
    IllegalReferenceInsideDataType(SourceRange),
    UnknownProperty(String, SourceRange),
    FieldNotPresent(String, SourceRange),
    NonStructDeclStructLiteral(SourceRange),
    CantAssignToReference(SourceRange),
    IllegalSharedRefMutation(SourceRange),
    IllegalImport(SourceRange),
    ImportPathMustBeModule(SourceRange),
    FileNotFound(SourceRange, String),
    NonConstantInConst(SourceRange),
    SelfParameterInNonAssociatedFunc(SourceRange),
}

impl Error for TypecheckError {}

impl Display for TypecheckError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.contents().fmt(f)
    }
}

impl Diagnostic for TypecheckError {
    fn contents(&self) -> DiagnosticContents {
        use TypecheckError::*;
        DiagnosticContents::Scalar(match self {
            MultiError(children) => {
                let mut contents = Vec::new();
                for child in children.iter() {
                    match child.contents() {
                        DiagnosticContents::Scalar(value) => contents.push(value),
                        DiagnosticContents::Vector(values) => contents.extend(values),
                    }
                }
                return DiagnosticContents::Vector(contents);
            }
            TypeMismatch { provenance, .. } => {
                DiagnosticMarker::error(provenance.clone(), "type mismatch")
            }
            ArithmeticMismatch(range) => DiagnosticMarker::error(range.clone(), "arithmetic"),
            NameNotFound(range, name) => {
                DiagnosticMarker::error_context(range.clone(), "name not found", name.clone())
            }
            CantCall(range) => DiagnosticMarker::error(range.clone(), "can't call"),
            WrongArgsCount(range) => DiagnosticMarker::error(range.clone(), "wrong args count"),
            MissingField(range) => DiagnosticMarker::error(range.clone(), "missing field"),
            NoNullDeclarations(range) => DiagnosticMarker::error(
                range.clone(),
                "insufficient type info: null variables must have a type annotation",
            ),
            ExpectedNullableLHS(range) => DiagnosticMarker::error(
                range.clone(),
                "expected nullable left-hand-side to ?? operator",
            ),
            CannotYield(range) => {
                DiagnosticMarker::error(range.clone(), "cannot yield outside of a generator")
            }
            IllegalAssignmentLHS(range) => {
                DiagnosticMarker::error(range.clone(), "illegal left hand side of assignment")
            }
            IllegalDotLHS(range) => {
                DiagnosticMarker::error(range.clone(), "illegal lhs of dot operator")
            }
            MustReturnGenerator(range) => {
                DiagnosticMarker::error(range.clone(), "must return a generator")
            }
            CaseStatementRequiresUnion(range) => {
                DiagnosticMarker::error(range.clone(), "argument to case statement must be a union")
            }
            IllegalDotRHS(range) => {
                DiagnosticMarker::error(range.clone(), "right side of dot operator must be a name")
            }
            BindingCountDoesntMatch(range) => DiagnosticMarker::error(
                range.clone(),
                "variant doesn't match previous count of bindings",
            ),
            BindingNameDoesntMatch(range) => {
                DiagnosticMarker::error(range.clone(), "variant doesn't match binding name")
            }
            DereferenceNonPointer(range) => {
                DiagnosticMarker::error(range.clone(), "attempted to dereference a non-ptr value")
            }
            NonExhaustiveCase(range) => {
                DiagnosticMarker::error(range.clone(), "non-exhaustive case statement")
            }
            IllegalFirstClassReference(range) => DiagnosticMarker::error(
                range.clone(),
                "references may not be assigned to variables, use 'borrow' instead of 'let'",
            ),
            IllegalNonRefBorrow(range) => DiagnosticMarker::error(
                range.clone(),
                "right hand side of 'borrow' statement must be a reference",
            ),
            IllegalNonLvalueBorrow(range) => DiagnosticMarker::error(
                range.clone(),
                "right hand side of 'borrow' statement must be a valid lvalue",
            ),
            IllegalReferenceInsideDataType(range) => {
                DiagnosticMarker::error(range.clone(), "illegal reference inside data type")
            }
            UnknownProperty(property_name, range) => DiagnosticMarker::error_context(
                range.clone(),
                "unknown property",
                property_name.to_string(),
            ),
            FieldNotPresent(property_name, range) => DiagnosticMarker::error_context(
                range.clone(),
                "accessing unknown field",
                property_name.to_string(),
            ),
            NonStructDeclStructLiteral(range) => {
                DiagnosticMarker::error(range.clone(), "non-struct declaration in struct literal")
            }
            CantAssignToReference(range) => {
                DiagnosticMarker::error(range.clone(), "can't assign new value to reference")
            }
            IllegalSharedRefMutation(range) => {
                DiagnosticMarker::error(range.clone(), "illegal assignment to a shared reference")
            }
            IllegalImport(range) => DiagnosticMarker::error(range.clone(), "illegal import"),
            ImportPathMustBeModule(range) => {
                DiagnosticMarker::error(range.clone(), "import path items must be modules")
            }
            FileNotFound(range, file_name) => DiagnosticMarker::error_context(
                range.clone(),
                "file not found",
                file_name.to_string(),
            ),
            NonConstantInConst(range) => {
                DiagnosticMarker::error(range.clone(), "non-constant value in const")
            }
            SelfParameterInNonAssociatedFunc(range) => {
                DiagnosticMarker::error(range.clone(), "self parameter in non-associated function")
            }
        })
    }
}

impl MultiError for TypecheckError {
    fn from_error_list(list: Vec<Self>) -> Self {
        TypecheckError::MultiError(list)
    }

    fn as_error_list(&mut self) -> Option<&mut Vec<Self>> {
        match self {
            TypecheckError::MultiError(list) => Some(list),
            _ => None,
        }
    }
}
