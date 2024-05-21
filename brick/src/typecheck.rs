use std::collections::{HashMap, HashSet};

use thiserror::Error;

use crate::{
    id::{AnyID, FunctionID, TypeID},
    multi_error::{merge_results, print_multi_errors, MultiError},
    parser::{
        AstNode, AstNodeValue, BinOp, FunctionDeclarationValue, IfDeclaration,
        InterfaceDeclarationValue, MatchDeclaration, StructDeclarationValue, UnaryOp,
    },
    provenance::SourceRange,
    runtime::{add_runtime_functions, array_runtime_functions, dictionary_runtime_functions},
};

use self::resolve::resolve_type_expr;

pub mod resolve;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionType {
    Void,
    Unreachable,
    Primitive(PrimitiveType),
    InstanceOf(TypeID),
    ReferenceTo(TypeID),
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
    pub fn id(&self) -> Option<&TypeID> {
        match self {
            ExpressionType::InstanceOf(id) | ExpressionType::ReferenceTo(id) => Some(id),
            ExpressionType::Void
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

    pub fn is_affine(&self, declarations: &HashMap<TypeID, &StaticDeclaration>) -> bool {
        match self {
            ExpressionType::Nullable(inner) => inner.is_affine(declarations),
            ExpressionType::Void
                | ExpressionType::Unreachable
                | ExpressionType::Null
                // Only if references truly aren't allowed in re-assignments
                | ExpressionType::Pointer(_, _)
                | ExpressionType::Primitive(_) => false,
            ExpressionType::InstanceOf(id) => declarations[id].is_affine(),
            ExpressionType::ReferenceTo(_)
                | ExpressionType::TypeParameterReference(_)
                | ExpressionType::Collection(_)
                | ExpressionType::Generator { .. }
            | ExpressionType::FunctionReference { .. } => true,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum CollectionType {
    Array(Box<ExpressionType>),
    Dict(Box<ExpressionType>, Box<ExpressionType>),
    String,
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

    pub fn is_affine(&self) -> bool {
        match self {
            StaticDeclaration::Func(_) => false,
            StaticDeclaration::Struct(decl) => decl.is_affine,
            StaticDeclaration::Interface(_) => true,
            StaticDeclaration::Union(decl) => decl.is_affine,
            StaticDeclaration::Module(_) => false,
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

    pub fn field_access(
        &self,
        field: &str,
        provenance: &SourceRange,
    ) -> Result<ExpressionType, TypecheckError> {
        match self {
            StaticDeclaration::Struct(StructType {
                fields,
                associated_functions,
                ..
            }) => fields
                .get(field)
                .cloned()
                .or_else(|| {
                    associated_functions
                        .get(field)
                        .map(|decl| ExpressionType::InstanceOf(decl.id()))
                })
                .ok_or_else(|| {
                    TypecheckError::FieldNotPresent(field.to_string(), provenance.clone())
                }),
            StaticDeclaration::Interface(InterfaceType {
                associated_functions,
                ..
            }) => associated_functions
                .get(field)
                .map(|decl| ExpressionType::InstanceOf(decl.id()))
                .ok_or_else(|| {
                    TypecheckError::FieldNotPresent(field.to_string(), provenance.clone())
                }),
            StaticDeclaration::Union(lhs_type) => Ok(ExpressionType::Nullable(Box::new(
                lhs_type
                    .variants
                    .get(field)
                    .ok_or_else(|| {
                        TypecheckError::FieldNotPresent(field.to_string(), provenance.clone())
                    })?
                    .clone()
                    .unwrap_or(ExpressionType::Void),
            ))),
            StaticDeclaration::Module(lhs_type) => lhs_type
                .exports
                .get(field)
                .map(|decl| decl.ref_to())
                .ok_or_else(|| {
                    TypecheckError::FieldNotPresent(field.to_string(), provenance.clone())
                }),
            StaticDeclaration::Func(_) => Err(TypecheckError::IllegalDotLHS(provenance.clone())),
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
    pub is_affine: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FuncType {
    pub id: TypeID,
    pub func_id: FunctionID,
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
    Int32,
    Float32,
    Int64,
    Float64,
    Bool,
    PointerSize,
}

#[derive(Debug, Error)]
pub enum TypecheckError {
    #[error("{}", print_multi_errors(&.0[..]))]
    MultiError(Vec<TypecheckError>),
    #[error("arithmetic")]
    ArithmeticMismatch(SourceRange),
    #[error("mismatched types: received {received:?}, expected {expected:?}")]
    TypeMismatch {
        expected: ExpressionType,
        received: ExpressionType,
    },
    #[error("declaration for {0:?} not found")]
    NameNotFound(SourceRange),
    #[error("can't call")]
    CantCall(SourceRange),
    #[error("wrong args count")]
    WrongArgsCount(SourceRange),
    #[error("missing field")]
    MissingField(SourceRange),
    #[error("insufficient type info: null variables must have a type annotation")]
    NoNullDeclarations(SourceRange),
    #[error("expected nullable left-hand-side to ?? operator")]
    ExpectedNullableLHS(SourceRange),
    #[error("cannot yield outside of a generator: {0}")]
    CannotYield(SourceRange),
    #[error("illegal lvalue: {0}")]
    IllegalLvalue(SourceRange),
    #[error("illegal lhs of dot operator: {0}")]
    IllegalDotLHS(SourceRange),
    #[error("must return a generator: {0}")]
    MustReturnGenerator(SourceRange),
    #[error("argument to case statement must be a union: {0}")]
    CaseStatementRequiresUnion(SourceRange),
    #[error("right side of dot operator must be a name: {0}")]
    IllegalDotRHS(SourceRange),
    #[error("variant doesn't match previous count of bindings: {0}")]
    BindingCountDoesntMatch(SourceRange),
    #[error("variant doesn't match binding name: {0}")]
    BindingNameDoesntMatch(SourceRange),
    #[error("attempted to dereference a non-ptr value: {0}")]
    DereferenceNonPointer(SourceRange),
    #[error("non-exhaustive case statement: {0}")]
    NonExhaustiveCase(SourceRange),
    #[error("references may not be assigned to variables, use 'borrow' instead of 'let': {0}")]
    IllegalFirstClassReference(SourceRange),
    #[error("right hand side of 'borrow' statement must be a reference: {0}")]
    IllegalNonRefBorrow(SourceRange),
    #[error("right hand side of 'borrow' statement must be a valid lvalue: {0}")]
    IllegalNonLvalueBorrow(SourceRange),
    #[error("illegal reference inside data type: {0}")]
    IllegalReferenceInsideDataType(SourceRange),
    #[error("unknown property {0}: {1}")]
    UnknownProperty(String, SourceRange),
    #[error("unknown property {0}: {1}")]
    FieldNotPresent(String, SourceRange),
    #[error("non-struct declaration in struct literal: {0}")]
    NonStructDeclStructLiteral(SourceRange),
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
    let mut name_to_context_entry = HashMap::new();
    let mut name_to_type_id = HashMap::new();
    let mut id_to_decl = HashMap::new();
    add_runtime_functions(&mut id_to_decl);

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

    let mut functions = Vec::new();
    let mut top_level_statements = Vec::new();
    let mut top_level_scope = HashMap::new();
    let mut results = Ok(());

    for statement in file {
        merge_results(
            &mut results,
            typecheck_node(
                statement,
                &context,
                &mut top_level_scope,
                &mut functions,
                &mut top_level_statements,
            ),
        );
    }
    results?;

    Ok(TypecheckedFile {
        functions,
        top_level_statements,
    })
}

fn typecheck_node<'ast>(
    statement: &'ast AstNode<'ast>,
    context: &Declarations,
    top_level_scope: &mut HashMap<String, (AnyID, ExpressionType)>,
    functions: &mut Vec<TypecheckedFunction<'ast>>,
    top_level_statements: &mut Vec<&AstNode<'ast>>,
) -> Result<(), TypecheckError> {
    match &statement.value {
        AstNodeValue::FunctionDeclaration(func) => {
            typecheck_function(func, context)?;
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
                    if let Err(err) = typecheck_function(func, context) {
                        Some(Err(err))
                    } else {
                        Some(Ok(TypecheckedFunction {
                            id: func.id,
                            name: func.name.clone(),
                            func,
                        }))
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            functions.extend(associated_functions.iter().cloned());
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
                None,
            )?;
            top_level_statements.push(statement);
        }
    }

    Ok(())
}

fn typecheck_function<'a>(
    function: &'a FunctionDeclarationValue<'a>,
    context: &Declarations,
) -> Result<(), TypecheckError> {
    let function_type = find_func(&context.id_to_decl, function.id).unwrap();
    let parameters = function
        .params
        .iter()
        .zip(function_type.params.iter())
        .map(|((id, name), param)| (name.name.clone(), ((*id).into(), param.clone())))
        .collect();

    if function.is_coroutine {
        let ExpressionType::Generator { param_ty, .. } = &function_type.returns else {
            return Err(TypecheckError::MustReturnGenerator(
                function_type.provenance.clone().unwrap(),
            ));
        };
        let return_ty = typecheck_expression(
            function.body,
            &[&context.name_to_context_entry, &parameters],
            &mut HashMap::new(),
            context,
            Some(param_ty),
        )?;

        if return_ty != &ExpressionType::Void && return_ty != &ExpressionType::Unreachable {
            return Err(TypecheckError::TypeMismatch {
                expected: ExpressionType::Void,
                received: return_ty.clone(),
            });
        }
    } else {
        let return_ty = typecheck_expression(
            function.body,
            &[&context.name_to_context_entry, &parameters],
            &mut HashMap::new(),
            context,
            None,
        )?;
        assert_assignable_to(&context.id_to_decl, &function_type.returns, return_ty)?;

        typecheck_returns(context, &function_type.returns, function.body)?;
    }

    Ok(())
}

fn typecheck_expression<'a>(
    node: &'a AstNode<'a>,
    outer_scopes: &[&HashMap<String, (AnyID, ExpressionType)>],
    current_scope: &mut HashMap<String, (AnyID, ExpressionType)>,
    context: &Declarations,
    generator_input_ty: Option<&ExpressionType>,
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
        | AstNodeValue::VoidType
        | AstNodeValue::SharedType(_)
        | AstNodeValue::ArrayType(_)
        | AstNodeValue::DictType(_, _)
        | AstNodeValue::NullableType(_)
        | AstNodeValue::GeneratorType { .. } => {
            panic!("illegal type expression in function body");
        }
        AstNodeValue::Statement(inner) => {
            typecheck_expression(
                inner,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            ExpressionType::Void
        }
        AstNodeValue::Declaration(name, type_hint, value, variable_id) => {
            let value_ty = typecheck_expression(
                value,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let mut result = Ok(());
            if matches!(value_ty, ExpressionType::Pointer(_, _)) {
                merge_results(
                    &mut result,
                    Err(TypecheckError::IllegalFirstClassReference(
                        node.provenance.clone(),
                    )),
                );
            }

            if let Some(type_hint) = type_hint {
                let hint_ty = resolve_type_expr(&context.name_to_type_id, type_hint)?;
                if matches!(hint_ty, ExpressionType::Pointer(_, _)) {
                    merge_results(
                        &mut result,
                        Err(TypecheckError::IllegalFirstClassReference(
                            node.provenance.clone(),
                        )),
                    );
                }

                merge_results(
                    &mut result,
                    assert_assignable_to(&context.id_to_decl, &hint_ty, value_ty),
                );
                type_hint.ty.set(hint_ty.clone()).unwrap();
                current_scope.insert(name.clone(), ((*variable_id).into(), hint_ty));
            } else {
                if value_ty == &ExpressionType::Null
                    || value_ty == &ExpressionType::Unreachable
                    || value_ty == &ExpressionType::Void
                {
                    return Err(TypecheckError::NoNullDeclarations(node.provenance.clone()));
                }
                current_scope.insert(name.clone(), ((*variable_id).into(), value_ty.clone()));
            }

            result?;

            ExpressionType::Void
        }
        AstNodeValue::BorrowDeclaration(name, value, variable_id) => {
            let value_ty = typecheck_expression(
                value,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            current_scope.insert(name.clone(), ((*variable_id).into(), value_ty.clone()));

            let mut results = Ok(());
            if let AstNodeValue::TakeRef(child) | AstNodeValue::TakeUnique(child) = &value.value {
                if !validate_lvalue(child) {
                    merge_results(
                        &mut results,
                        Err(TypecheckError::IllegalNonLvalueBorrow(
                            node.provenance.clone(),
                        )),
                    );
                }
            } else {
                merge_results(
                    &mut results,
                    Err(TypecheckError::IllegalNonRefBorrow(node.provenance.clone())),
                );
            };
            results?;

            ExpressionType::Void
        }
        AstNodeValue::Return(returned) => {
            if let Some(returned) = returned {
                typecheck_expression(
                    returned,
                    outer_scopes,
                    current_scope,
                    context,
                    generator_input_ty,
                )?;
            }

            ExpressionType::Unreachable
        }
        AstNodeValue::Yield(yielded) => {
            let Some(yield_ctx_ty) = generator_input_ty else {
                return Err(TypecheckError::CannotYield(node.provenance.clone()));
            };
            if let Some(yielded) = yielded {
                typecheck_expression(
                    yielded,
                    outer_scopes,
                    current_scope,
                    context,
                    Some(yield_ctx_ty),
                )?;
            }

            yield_ctx_ty.clone()
        }
        AstNodeValue::Name {
            value: name,
            referenced_id,
        } => {
            let (ref_id, expr) = resolve_name(name, current_scope, outer_scopes)
                .ok_or_else(|| TypecheckError::NameNotFound(node.provenance.clone()))?;
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
        AstNodeValue::StringLiteral(_) => ExpressionType::Collection(CollectionType::String),
        AstNodeValue::CharLiteral(_) => ExpressionType::Primitive(PrimitiveType::Char),
        AstNodeValue::Null => ExpressionType::Null,
        AstNodeValue::Bool(_) => ExpressionType::Primitive(PrimitiveType::Bool),
        AstNodeValue::BinExpr(BinOp::Dot, left, right) => {
            let left_ty = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let AstNodeValue::Name { value: name, .. } = &right.value else {
                return Err(TypecheckError::IllegalDotRHS(right.provenance.clone()));
            };
            match fully_dereference(left_ty) {
                ExpressionType::InstanceOf(id) => context
                    .decl(id)
                    .unwrap()
                    .field_access(name, &right.provenance)?,
                ExpressionType::ReferenceTo(id) => match context.decl(id).unwrap() {
                    StaticDeclaration::Union(union_ty) => {
                        let variant_ty = union_ty.variants.get(name).ok_or_else(|| {
                            TypecheckError::FieldNotPresent(name.clone(), node.provenance.clone())
                        })?;

                        if let Some(variant_ty) = variant_ty {
                            ExpressionType::FunctionReference {
                                parameters: vec![variant_ty.clone()],
                                returns: Box::new(ExpressionType::InstanceOf(*id)),
                            }
                        } else {
                            ExpressionType::InstanceOf(*id)
                        }
                    }
                    // TODO: module dot operator
                    // TODO: static functions on structs?
                    StaticDeclaration::Struct(_)
                    | StaticDeclaration::Module(_)
                    | StaticDeclaration::Func(_)
                    | StaticDeclaration::Interface(_) => {
                        return Err(TypecheckError::IllegalDotLHS(left.provenance.clone()));
                    }
                },
                ExpressionType::Collection(CollectionType::Array(_item_ty)) => {
                    let runtime = array_runtime_functions();
                    if let Some(ty) = runtime.get(name) {
                        ExpressionType::ReferenceTo(ty.decl.id())
                    } else {
                        todo!("array methods")
                    }
                }
                ExpressionType::Collection(CollectionType::Dict(_key_ty, _value_ty)) => {
                    let runtime = dictionary_runtime_functions();
                    if let Some(ty) = runtime.get(name) {
                        ExpressionType::ReferenceTo(ty.decl.id())
                    } else {
                        todo!("dict methods")
                    }
                }
                _ => {
                    return Err(TypecheckError::IllegalDotLHS(left.provenance.clone()));
                }
            }
        }
        AstNodeValue::BinExpr(BinOp::Concat, left, right) => {
            let mut results = Ok(());

            merge_results(
                &mut results,
                typecheck_expression(
                    left,
                    outer_scopes,
                    current_scope,
                    context,
                    generator_input_ty,
                )
                .and_then(|left_ty| {
                    assert_assignable_to(
                        &context.id_to_decl,
                        &ExpressionType::Collection(CollectionType::String),
                        left_ty,
                    )
                }),
            );
            merge_results(
                &mut results,
                typecheck_expression(
                    right,
                    outer_scopes,
                    current_scope,
                    context,
                    generator_input_ty,
                )
                .and_then(|right_ty| {
                    assert_assignable_to(
                        &context.id_to_decl,
                        &ExpressionType::Collection(CollectionType::String),
                        right_ty,
                    )
                }),
            );
            results?;

            ExpressionType::Collection(CollectionType::String)
        }
        AstNodeValue::BinExpr(BinOp::NullChaining, left, right) => {
            let left = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let ExpressionType::Nullable(ty) = fully_dereference(left) else {
                panic!("TODO: left side of nullable dot operator");
            };
            let mut field_ty = Ok(*ty.clone());
            traverse_dots(right, |name, provenance| {
                let Ok(ty) = &field_ty else {
                    return;
                };
                let ExpressionType::InstanceOf(id) = ty else {
                    unreachable!()
                };
                field_ty = context.decl(id).unwrap().field_access(name, provenance);
            });
            ExpressionType::Nullable(Box::new(field_ty?))
        }
        AstNodeValue::BinExpr(BinOp::Index, collection, index) => {
            let collection_ty = typecheck_expression(
                collection,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            match collection_ty {
                ExpressionType::Collection(CollectionType::Array(item_ty)) => {
                    let index_ty = typecheck_expression(
                        index,
                        outer_scopes,
                        current_scope,
                        context,
                        generator_input_ty,
                    )?;
                    assert_assignable_to(
                        &context.id_to_decl,
                        &ExpressionType::Primitive(PrimitiveType::PointerSize),
                        index_ty,
                    )?;

                    *item_ty.clone()
                }
                ExpressionType::Collection(CollectionType::Dict(key_ty, value_ty)) => {
                    let index_ty = typecheck_expression(
                        index,
                        outer_scopes,
                        current_scope,
                        context,
                        generator_input_ty,
                    )?;
                    assert_assignable_to(&context.id_to_decl, index_ty, key_ty)?;

                    *value_ty.clone()
                }
                ty => todo!("can't index {:?}", ty),
            }
        }
        AstNodeValue::BinExpr(
            BinOp::Add | BinOp::Subtract | BinOp::Multiply | BinOp::Divide,
            left,
            right,
        ) => {
            let left = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let ExpressionType::Primitive(_) = fully_dereference(left) else {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            };
            let right = typecheck_expression(
                right,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let ExpressionType::Primitive(_) = fully_dereference(right) else {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            };
            if is_assignable_to(&context.id_to_decl, None, left, right) {
                left.clone()
            } else if is_assignable_to(&context.id_to_decl, None, right, left) {
                right.clone()
            } else {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            }
        }
        AstNodeValue::BinExpr(BinOp::BooleanAnd | BinOp::BooleanOr, left, right) => {
            let left = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let right = typecheck_expression(
                right,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;

            let mut results = Ok(());
            merge_results(
                &mut results,
                assert_assignable_to(
                    &context.id_to_decl,
                    &ExpressionType::Primitive(PrimitiveType::Bool),
                    left,
                ),
            );
            merge_results(
                &mut results,
                assert_assignable_to(
                    &context.id_to_decl,
                    &ExpressionType::Primitive(PrimitiveType::Bool),
                    right,
                ),
            );
            results?;

            ExpressionType::Primitive(PrimitiveType::Bool)
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
            let left = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let ExpressionType::Primitive(_) = fully_dereference(left) else {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            };
            let right = typecheck_expression(
                right,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let ExpressionType::Primitive(_) = fully_dereference(right) else {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            };

            ExpressionType::Primitive(PrimitiveType::Bool)
        }
        AstNodeValue::BinExpr(BinOp::Assignment, left, right) => {
            let left_ty = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            // TODO: validate legal type to assign
            if !validate_lvalue(left) {
                return Err(TypecheckError::IllegalLvalue(left.provenance.clone()));
            }
            let right = typecheck_expression(
                right,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;

            assert_assignable_to(&context.id_to_decl, left_ty, right)?;

            ExpressionType::Void
        }
        AstNodeValue::BinExpr(
            BinOp::AddAssign | BinOp::SubtractAssign | BinOp::MultiplyAssign | BinOp::DivideAssign,
            left,
            right,
        ) => {
            let left_ty = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            if !validate_lvalue(left) {
                return Err(TypecheckError::IllegalLvalue(left.provenance.clone()));
            }
            let right = typecheck_expression(
                right,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;

            if !matches!(fully_dereference(left_ty), ExpressionType::Primitive(_))
                || !matches!(fully_dereference(right), ExpressionType::Primitive(_))
            {
                return Err(TypecheckError::ArithmeticMismatch(node.provenance.clone()));
            }

            assert_assignable_to(&context.id_to_decl, left_ty, right)?;

            ExpressionType::Void
        }
        AstNodeValue::BinExpr(BinOp::NullCoalesce, left, right) => {
            let left_ty = typecheck_expression(
                left,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let right_ty = typecheck_expression(
                right,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let ExpressionType::Nullable(ty) = left_ty else {
                return Err(TypecheckError::ExpectedNullableLHS(left.provenance.clone()));
            };
            let ty = *ty.clone();

            assert_assignable_to(&context.id_to_decl, &ty, right_ty)?;

            ty.clone()
        }
        AstNodeValue::While(condition, body) => {
            let condition = typecheck_expression(
                condition,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
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

            typecheck_expression(
                body,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;

            ExpressionType::Void
        }
        AstNodeValue::Loop(body) => {
            typecheck_expression(
                body,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            ExpressionType::Unreachable
        }
        AstNodeValue::If(IfDeclaration {
            condition,
            if_branch,
            else_branch,
        }) => {
            let condition = typecheck_expression(
                condition,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
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

            let if_branch = typecheck_expression(
                if_branch,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let else_branch = else_branch
                .as_ref()
                .map(|else_branch| {
                    typecheck_expression(
                        else_branch,
                        outer_scopes,
                        current_scope,
                        context,
                        generator_input_ty,
                    )
                })
                .transpose()?;

            match else_branch {
                Some(else_branch) => {
                    if is_assignable_to(&context.id_to_decl, None, if_branch, else_branch) {
                        if_branch.clone()
                    } else {
                        assert_assignable_to(&context.id_to_decl, else_branch, if_branch)?;
                        else_branch.clone()
                    }
                }
                None => ExpressionType::Void,
            }
        }
        AstNodeValue::Match(MatchDeclaration { value, cases }) => {
            let input_ty = typecheck_expression(
                value,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let Some(StaticDeclaration::Union(union_ty)) = shallow_dereference(input_ty)
                .id()
                .map(|ty_id| context.id_to_decl[ty_id])
            else {
                return Err(TypecheckError::CaseStatementRequiresUnion(
                    value.provenance.clone(),
                ));
            };
            let mut return_type = None;
            let mut results = Ok(());
            let mut variants_matched_against = HashSet::new();
            for case in cases.iter() {
                let mut binding = BindingState::Uninit;
                for variant in case.variants.iter() {
                    variants_matched_against.insert(&variant.name);
                    let mut variant_ty = union_ty.variants[&variant.name].as_ref();
                    if !variant.bindings.is_empty() && variant.bindings[0] == "_" {
                        variant_ty = None;
                    }
                    variant.ty.set(variant_ty.cloned()).unwrap();
                    match &binding {
                        BindingState::Uninit => {
                            binding = match variant_ty {
                                Some(variant_ty) => {
                                    BindingState::Binding(&variant.bindings[0], variant_ty)
                                }
                                _ => BindingState::NoBinding,
                            };
                        }
                        BindingState::NoBinding => {
                            if variant_ty.is_some() {
                                merge_results(
                                    &mut results,
                                    Err(TypecheckError::BindingCountDoesntMatch(
                                        variant.provenance.clone(),
                                    )),
                                );
                            }
                        }
                        BindingState::Binding(name, binding_ty) => {
                            if let Some(variant_ty) = variant_ty {
                                merge_results(
                                    &mut results,
                                    assert_assignable_to(
                                        &context.id_to_decl,
                                        binding_ty,
                                        variant_ty,
                                    ),
                                );
                                if *name != &variant.bindings[0] {
                                    merge_results(
                                        &mut results,
                                        Err(TypecheckError::BindingNameDoesntMatch(
                                            variant.provenance.clone(),
                                        )),
                                    );
                                }
                            } else {
                                merge_results(
                                    &mut results,
                                    Err(TypecheckError::BindingCountDoesntMatch(
                                        variant.provenance.clone(),
                                    )),
                                );
                            }
                        }
                    }
                }
                let mut scopes: Vec<&HashMap<_, _>> = Vec::with_capacity(outer_scopes.len() + 1);
                scopes.push(current_scope);
                scopes.extend_from_slice(outer_scopes);
                let mut child_scope = HashMap::new();
                if let BindingState::Binding(binding_name, binding_ty) = binding {
                    let binding_ty = if let ExpressionType::Pointer(ptr_ty, _) = input_ty {
                        ExpressionType::Pointer(*ptr_ty, Box::new(binding_ty.clone()))
                    } else {
                        binding_ty.clone()
                    };
                    child_scope.insert(binding_name.clone(), (case.var_id.into(), binding_ty));
                }

                let body_ty = typecheck_expression(
                    &case.body,
                    &scopes,
                    &mut child_scope,
                    context,
                    generator_input_ty,
                )?;
                if let Some(return_type) = &return_type {
                    merge_results(
                        &mut results,
                        assert_assignable_to(&context.id_to_decl, return_type, body_ty),
                    );
                } else {
                    return_type = Some(body_ty.clone());
                }
            }
            if !union_ty
                .variants
                .keys()
                .all(|variant_name| variants_matched_against.contains(variant_name))
            {
                merge_results(
                    &mut results,
                    Err(TypecheckError::NonExhaustiveCase(node.provenance.clone())),
                );
            }

            results?;

            return_type.unwrap_or(ExpressionType::Void)
        }
        AstNodeValue::Block(children) => {
            let mut scopes: Vec<&HashMap<_, _>> = Vec::with_capacity(outer_scopes.len() + 1);
            scopes.push(current_scope);
            scopes.extend_from_slice(outer_scopes);

            let mut child_scope = HashMap::new();
            let mut expr_ty = ExpressionType::Void;
            for (index, child) in children.iter().enumerate() {
                let new_ty = typecheck_expression(
                    child,
                    &scopes[..],
                    &mut child_scope,
                    context,
                    generator_input_ty,
                )?;

                if expr_ty != ExpressionType::Unreachable {
                    expr_ty = new_ty.clone();
                }
                if index != children.len() - 1
                    && !(expr_ty == ExpressionType::Void || expr_ty == ExpressionType::Unreachable)
                {
                    return Err(TypecheckError::TypeMismatch {
                        expected: ExpressionType::Void,
                        received: expr_ty,
                    });
                }
            }
            expr_ty
        }
        AstNodeValue::Call(func, args) => {
            match fully_dereference(typecheck_expression(
                func,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?) {
                ExpressionType::InstanceOf(func) | ExpressionType::ReferenceTo(func) => {
                    let func = context.id_to_func(func);
                    let params = if func.is_associated {
                        &func.params[1..]
                    } else {
                        &func.params[..]
                    };
                    if params.len() != args.len() {
                        return Err(TypecheckError::WrongArgsCount(node.provenance.clone()));
                    }
                    let mut generic_args = vec![ExpressionType::Unreachable; func.type_param_count];

                    for (arg, param) in args.iter().zip(params.iter()) {
                        let arg = typecheck_expression(
                            arg,
                            outer_scopes,
                            current_scope,
                            context,
                            generator_input_ty,
                        )?;
                        find_generic_bindings(&mut generic_args[..], param, arg);
                        if !is_assignable_to(&context.id_to_decl, Some(&generic_args), param, arg) {
                            return Err(TypecheckError::TypeMismatch {
                                received: arg.clone(),
                                expected: param.clone(),
                            });
                        }
                    }

                    func.returns.clone()
                }
                ExpressionType::Generator { yield_ty, param_ty } => {
                    // TODO: allow more than one parameter
                    if param_ty.as_ref() == &ExpressionType::Void {
                        if !args.is_empty() {
                            return Err(TypecheckError::WrongArgsCount(node.provenance.clone()));
                        }
                    } else {
                        if args.len() != 1 {
                            return Err(TypecheckError::WrongArgsCount(node.provenance.clone()));
                        }
                        let arg = args.last().unwrap();
                        let arg = typecheck_expression(
                            arg,
                            outer_scopes,
                            current_scope,
                            context,
                            generator_input_ty,
                        )?;
                        // TODO: generic generators?
                        assert_assignable_to(&context.id_to_decl, param_ty, arg)?;
                    }
                    *yield_ty.clone()
                }
                ExpressionType::FunctionReference {
                    parameters,
                    returns,
                } => {
                    let mut results = Ok(());
                    for (arg, param) in args.iter().zip(parameters.iter()) {
                        let arg = typecheck_expression(
                            arg,
                            outer_scopes,
                            current_scope,
                            context,
                            generator_input_ty,
                        )?;
                        merge_results(
                            &mut results,
                            assert_assignable_to(&context.id_to_decl, param, arg),
                        );
                    }

                    results?;

                    returns.as_ref().clone()
                }
                _ => return Err(TypecheckError::CantCall(node.provenance.clone())),
            }
        }
        AstNodeValue::RecordLiteral { name, fields } => {
            let ExpressionType::ReferenceTo(ty_id) = typecheck_expression(
                name,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?
            else {
                return Err(TypecheckError::CantCall(node.provenance.clone()));
            };

            match context.decl(ty_id).unwrap() {
                StaticDeclaration::Struct(struct_type) => {
                    if struct_type.fields.len() != fields.len() {
                        return Err(TypecheckError::WrongArgsCount(node.provenance.clone()));
                    }

                    let mut results = Ok(());
                    for (name, param_field) in struct_type.fields.iter() {
                        let Some(arg_field) = fields.get(name) else {
                            return Err(TypecheckError::MissingField(node.provenance.clone()));
                        };
                        match typecheck_expression(
                            arg_field,
                            outer_scopes,
                            current_scope,
                            context,
                            generator_input_ty,
                        ) {
                            Ok(arg_field) => {
                                merge_results(
                                    &mut results,
                                    assert_assignable_to(
                                        &context.id_to_decl,
                                        param_field,
                                        arg_field,
                                    ),
                                );
                            }
                            err => {
                                merge_results(&mut results, err.map(|_| {}));
                            }
                        }
                    }

                    results?;

                    ExpressionType::InstanceOf(*ty_id)
                }
                StaticDeclaration::Union(_)
                | StaticDeclaration::Func(_)
                | StaticDeclaration::Interface(_)
                | StaticDeclaration::Module(_) => {
                    return Err(TypecheckError::NonStructDeclStructLiteral(
                        node.provenance.clone(),
                    ));
                }
            }
        }
        AstNodeValue::DictLiteral(entries) => {
            let mut result_key_ty = None;
            let mut result_value_ty = None;
            for (key, value) in entries.iter() {
                let key_ty = typecheck_expression(
                    key,
                    outer_scopes,
                    current_scope,
                    context,
                    generator_input_ty,
                )?;
                if let Some(expected_key_ty) = result_key_ty {
                    assert_assignable_to(&context.id_to_decl, key_ty, expected_key_ty)?;
                } else {
                    result_key_ty = Some(key_ty);
                }
                let value_ty = typecheck_expression(
                    value,
                    outer_scopes,
                    current_scope,
                    context,
                    generator_input_ty,
                )?;
                if let Some(expected_value_ty) = result_value_ty {
                    assert_assignable_to(&context.id_to_decl, value_ty, expected_value_ty)?;
                } else {
                    result_value_ty = Some(value_ty);
                }
            }

            ExpressionType::Collection(CollectionType::Dict(
                Box::new(result_key_ty.unwrap().clone()),
                Box::new(result_value_ty.unwrap().clone()),
            ))
        }
        AstNodeValue::TakeUnique(inner) => ExpressionType::Pointer(
            PointerKind::Unique,
            Box::new(
                typecheck_expression(
                    inner,
                    outer_scopes,
                    current_scope,
                    context,
                    generator_input_ty,
                )?
                .clone(),
            ),
        ),
        AstNodeValue::TakeRef(inner) => ExpressionType::Pointer(
            PointerKind::Shared,
            Box::new(
                typecheck_expression(
                    inner,
                    outer_scopes,
                    current_scope,
                    context,
                    generator_input_ty,
                )?
                .clone(),
            ),
        ),
        AstNodeValue::Deref(inner) => {
            let ty = typecheck_expression(
                inner,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let ExpressionType::Pointer(_, ty) = ty else {
                return Err(TypecheckError::DereferenceNonPointer(
                    inner.provenance.clone(),
                ));
            };
            *ty.clone()
        }
        AstNodeValue::ArrayLiteral(items) => {
            if items.is_empty() {
                todo!("how to typecheck 0-length collections?");
            }
            let mut iter = items.iter();
            let ty = typecheck_expression(
                iter.next().unwrap(),
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?
            .clone();
            for remaining in iter {
                let this_ty = typecheck_expression(
                    remaining,
                    outer_scopes,
                    current_scope,
                    context,
                    generator_input_ty,
                )?;
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
            let inner = typecheck_expression(
                value,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            let length = typecheck_expression(
                length,
                outer_scopes,
                current_scope,
                context,
                generator_input_ty,
            )?;
            assert_assignable_to(
                &context.id_to_decl,
                &ExpressionType::Primitive(PrimitiveType::PointerSize),
                length,
            )?;
            ExpressionType::Collection(CollectionType::Array(Box::new(inner.clone())))
        }
        AstNodeValue::UnaryExpr(op, child) => match op {
            UnaryOp::BooleanNot => {
                let child_ty = typecheck_expression(
                    child,
                    outer_scopes,
                    current_scope,
                    context,
                    generator_input_ty,
                )?;
                assert_assignable_to(
                    &context.id_to_decl,
                    &ExpressionType::Primitive(PrimitiveType::Bool),
                    child_ty,
                )?;
                ExpressionType::Primitive(PrimitiveType::Bool)
            }
        },
    };

    node.ty.set(ty).expect("each node should be visited once");

    Ok(node.ty.get().expect("just set"))
}

enum BindingState<'a> {
    Uninit,
    NoBinding,
    Binding(&'a String, &'a ExpressionType),
}

pub fn traverse_dots(node: &AstNode, mut callback: impl FnMut(&str, &SourceRange)) {
    traverse_dots_recursive(node, &mut callback);
}

fn traverse_dots_recursive(node: &AstNode, callback: &mut impl FnMut(&str, &SourceRange)) {
    match &node.value {
        AstNodeValue::BinExpr(BinOp::Dot, lhs, rhs) => {
            traverse_dots_recursive(lhs, callback);
            traverse_dots_recursive(rhs, callback);
        }
        AstNodeValue::Name { value, .. } => {
            callback(value, &node.provenance);
        }
        _ => {}
    }
}

fn typecheck_returns<'a>(
    context: &Declarations,
    expected_ty: &ExpressionType,
    current: &'a AstNode<'a>,
) -> Result<(), TypecheckError> {
    let mut results = Ok(());
    if let AstNodeValue::Return(child) = &current.value {
        let return_ty = child
            .as_ref()
            .map(|child| child.ty.get().unwrap())
            .unwrap_or(&ExpressionType::Void);
        if !is_assignable_to(&context.id_to_decl, None, expected_ty, return_ty) {
            merge_results(
                &mut results,
                Err(TypecheckError::TypeMismatch {
                    expected: expected_ty.clone(),
                    received: return_ty.clone(),
                }),
            );
        }
    }
    current.children(|child| {
        merge_results(&mut results, typecheck_returns(context, expected_ty, child))
    });

    results
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
        | AstNodeValue::Declaration(_, _, _, _)
        | AstNodeValue::Import(_)
        | AstNodeValue::Return(_)
        | AstNodeValue::Yield(_)
        | AstNodeValue::Statement(_)
        | AstNodeValue::Int(_)
        | AstNodeValue::Float(_)
        | AstNodeValue::Bool(_)
        | AstNodeValue::CharLiteral(_)
        | AstNodeValue::StringLiteral(_)
        | AstNodeValue::Null
        | AstNodeValue::If(_)
        | AstNodeValue::While(_, _)
        | AstNodeValue::Match(_)
        | AstNodeValue::Loop(_)
        | AstNodeValue::Call(_, _)
        | AstNodeValue::TakeUnique(_)
        | AstNodeValue::TakeRef(_)
        | AstNodeValue::RecordLiteral { .. }
        | AstNodeValue::DictLiteral(_)
        | AstNodeValue::ArrayLiteral(_)
        | AstNodeValue::ArrayLiteralLength(_, _)
        | AstNodeValue::Block(_)
        | AstNodeValue::UniqueType(_)
        | AstNodeValue::VoidType
        | AstNodeValue::SharedType(_)
        | AstNodeValue::ArrayType(_)
        | AstNodeValue::DictType(_, _)
        | AstNodeValue::UnaryExpr(_, _)
        | AstNodeValue::NullableType(_)
        | AstNodeValue::GeneratorType { .. }
        | AstNodeValue::BorrowDeclaration(..) => false,
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

fn find_generic_bindings(
    generic_args: &mut [ExpressionType],
    left: &ExpressionType,
    right: &ExpressionType,
) {
    match (left, right) {
        // All scalar expression types cannot contain a generic binding. They may not match
        // the right side of the expression, but we're not checking that in this method
        (
            ExpressionType::Void
            | ExpressionType::Unreachable
            | ExpressionType::Primitive(_)
            | ExpressionType::InstanceOf(_)
            | ExpressionType::ReferenceTo(_)
            | ExpressionType::Null
            | ExpressionType::Collection(CollectionType::String),
            _,
        ) => {}
        (
            ExpressionType::Pointer(PointerKind::Shared, left),
            ExpressionType::Pointer(PointerKind::Shared | PointerKind::Unique, right),
        )
        | (
            ExpressionType::Pointer(PointerKind::Unique, left),
            ExpressionType::Pointer(PointerKind::Unique, right),
        )
        | (
            ExpressionType::Collection(CollectionType::Array(left)),
            ExpressionType::Collection(CollectionType::Array(right)),
        )
        | (ExpressionType::Nullable(left), ExpressionType::Nullable(right)) => {
            find_generic_bindings(generic_args, left, right);
        }
        (ExpressionType::Nullable(left), _) => find_generic_bindings(generic_args, left, right),
        (
            ExpressionType::Collection(CollectionType::Dict(left_key, left_value)),
            ExpressionType::Collection(CollectionType::Dict(right_key, right_value)),
        ) => {
            find_generic_bindings(generic_args, left_key, right_key);
            find_generic_bindings(generic_args, left_value, right_value);
        }
        (ExpressionType::TypeParameterReference(idx), _) => {
            if generic_args[*idx] == ExpressionType::Unreachable {
                generic_args[*idx] = right.clone();
            }
        }
        (
            ExpressionType::Generator {
                yield_ty: left_yield_ty,
                param_ty: left_param_ty,
            },
            ExpressionType::Generator {
                yield_ty: right_yield_ty,
                param_ty: right_param_ty,
            },
        ) => {
            find_generic_bindings(generic_args, left_yield_ty, right_yield_ty);
            find_generic_bindings(generic_args, left_param_ty, right_param_ty);
        }
        (ExpressionType::FunctionReference { .. }, _)
        | (_, ExpressionType::FunctionReference { .. }) => {
            todo!("function references only exist within the compiler")
        }
        (ExpressionType::Collection(CollectionType::Dict(..) | CollectionType::Array(_)), _)
        | (ExpressionType::Pointer(_, _), _)
        | (ExpressionType::Generator { .. }, _) => {}
    }
}

pub fn is_assignable_to(
    id_to_decl: &HashMap<TypeID, &StaticDeclaration>,
    generic_args: Option<&[ExpressionType]>,
    left: &ExpressionType,
    right: &ExpressionType,
) -> bool {
    use ExpressionType::*;
    use PrimitiveType::*;

    match (left, right) {
        (TypeParameterReference(idx), _) => is_assignable_to(
            id_to_decl,
            generic_args,
            &generic_args.unwrap()[*idx],
            right,
        ),
        (_, TypeParameterReference(_)) => {
            todo!("can you ever end up here?")
        }
        (Unreachable, Unreachable) => true,
        (Unreachable, _) => false,
        (_, Unreachable) => true,
        // Kinda a special case, but a function that returns void should accept a void block
        (Void, Void) => true,
        // Void can never be assigned to or be assigned from
        // Null can never be assigned to
        (Void, _) | (_, Void) | (Null, _) => false,

        // Handle pointers and de-referencing
        (Pointer(left_ty, left_inner), Pointer(right_ty, right_inner)) => {
            (left_ty == right_ty
                || *left_ty == PointerKind::Shared && *right_ty == PointerKind::Unique)
                && is_assignable_to(id_to_decl, generic_args, left_inner, right_inner)
        }
        // TODO: auto-dereference in the IR
        (left, Pointer(_, right_inner)) => {
            is_assignable_to(id_to_decl, generic_args, left, right_inner)
        }
        // TODO: support auto-dereferencing on lhs
        (Pointer(_, _), _right) => false,

        // Nullability
        (Nullable(_), Null) => true,
        (_, Null) => false,
        (Nullable(left), Nullable(right)) => {
            is_assignable_to(id_to_decl, generic_args, left, right)
        }
        (Nullable(left), right) => is_assignable_to(id_to_decl, generic_args, left, right),
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

                (Struct(_), Struct(_)) => left == right,
                (Struct(_), _) => false,

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
                (Interface(_), Interface(_)) => left == right,
                (Interface(_), Func(_)) => todo!(),
                (Interface(_), Union(_)) => todo!(),

                (_, Module(_)) => false,
                // You can never assign to a module
                (Module(_), _) => false,
            }
        }
        (
            Generator {
                yield_ty: left_yield_ty,
                param_ty: left_param_ty,
            },
            Generator {
                yield_ty: right_yield_ty,
                param_ty: right_param_ty,
            },
        ) => left_yield_ty == right_yield_ty && left_param_ty == right_param_ty,
        (Generator { .. }, _) | (_, Generator { .. }) => false,

        (InstanceOf(_), Primitive(_))
        | (InstanceOf(_), Collection(_))
        | (Collection(_), Primitive(_))
        | (Collection(_), InstanceOf(_)) => false,

        (Collection(CollectionType::Array(left)), Collection(CollectionType::Array(right))) => {
            left == right
        }
        (
            Collection(CollectionType::Dict(left_key, left_value)),
            Collection(CollectionType::Dict(right_key, right_value)),
        ) => left_key == right_key && left_value == right_value,
        (Collection(CollectionType::String), Collection(CollectionType::String)) => true,
        (Collection(_), Collection(_)) => false,

        (ExpressionType::FunctionReference { .. }, _)
        | (_, ExpressionType::FunctionReference { .. }) => {
            todo!("function references only exist within the compiler")
        }

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

pub fn shallow_dereference(ty: &ExpressionType) -> &ExpressionType {
    if let ExpressionType::Pointer(_, inner) = ty {
        inner
    } else {
        ty
    }
}

fn assert_assignable_to(
    id_to_decl: &HashMap<TypeID, &StaticDeclaration>,
    lhs: &ExpressionType,
    rhs: &ExpressionType,
) -> Result<(), TypecheckError> {
    if is_assignable_to(id_to_decl, None, lhs, rhs) {
        Ok(())
    } else {
        Err(TypecheckError::TypeMismatch {
            received: lhs.clone(),
            expected: rhs.clone(),
        })
    }
}
