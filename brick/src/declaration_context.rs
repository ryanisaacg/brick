use bytemuck::{Pod, Zeroable};

use crate::{
    multi_error::{merge_result_list, merge_results_or_value},
    parser::{
        AstNode, AstNodeValue, FunctionDeclarationValue, FunctionHeaderValue,
        InterfaceDeclarationValue, NameAndType, StructDeclarationValue, UnionDeclarationValue,
        UnionDeclarationVariant,
    },
    typecheck::{
        CollectionType, FuncType, InterfaceType, ModuleType, PointerKind, PrimitiveType,
        StructType, TypecheckError, UnionType,
    },
    ExpressionType, TypeDeclaration,
};
use std::{
    collections::HashMap,
    sync::atomic::{AtomicU32, Ordering},
};

pub struct DeclarationContext {
    pub intrinsic_module: FileDeclarations,
    pub files: HashMap<&'static str, FileDeclarations>,
    pub id_to_decl: HashMap<TypeID, TypeDeclaration>,
    pub id_to_func: HashMap<FunctionID, FuncType>,
    pub intrinsic_to_id: HashMap<IntrinsicFunction, FunctionID>,
    pub array_intrinsics: HashMap<&'static str, CollectionIntrinsic>,
    pub dict_intrinsics: HashMap<&'static str, CollectionIntrinsic>,
    pub rc_intrinsics: HashMap<&'static str, CollectionIntrinsic>,
    pub cell_intrinsics: HashMap<&'static str, CollectionIntrinsic>,
    pub extern_function_bindings: Vec<(String, FunctionID)>,
    pub extern_function_exports: Vec<(String, FunctionID)>,
}

impl Default for DeclarationContext {
    fn default() -> Self {
        Self::new()
    }
}

impl DeclarationContext {
    pub fn new() -> DeclarationContext {
        let mut ctx = DeclarationContext {
            intrinsic_module: FileDeclarations::new(),
            files: HashMap::new(),
            id_to_decl: HashMap::new(),
            id_to_func: HashMap::new(),
            intrinsic_to_id: HashMap::new(),
            array_intrinsics: HashMap::new(),
            dict_intrinsics: HashMap::new(),
            rc_intrinsics: HashMap::new(),
            cell_intrinsics: HashMap::new(),
            extern_function_bindings: Vec::new(),
            extern_function_exports: Vec::new(),
        };
        add_intrinsics(&mut ctx);
        ctx
    }

    // at some point this interface may have to evolve to support replacing existing contents
    pub fn insert_file(
        &mut self,
        module_name: &'static str,
        source: &[AstNode<'_>],
    ) -> Result<(), TypecheckError> {
        let module = match self.files.get_mut(module_name) {
            Some(module) => module,
            None => {
                let file = FileDeclarations::new();
                self.id_to_decl.insert(
                    file.module_id,
                    TypeDeclaration::Module(ModuleType {
                        id: file.module_id,
                        exports: HashMap::new(),
                    }),
                );
                self.files.insert(module_name, file);
                self.files.get_mut(module_name).unwrap()
            }
        };
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

        resolve_top_level_declarations(
            module,
            names_to_declarations,
            &mut self.id_to_decl,
            &mut self.id_to_func,
            &mut self.extern_function_bindings,
            &mut self.extern_function_exports,
        )
    }

    pub fn propagate_viral_types(&mut self) {
        let mut affinity = HashMap::new();
        for decl in self.id_to_decl.values() {
            is_decl_affine(&self.id_to_decl, decl, &mut affinity);
        }
        for decl in self.id_to_decl.values_mut() {
            match decl {
                TypeDeclaration::Struct(decl) => decl.is_affine = affinity[&decl.id],
                TypeDeclaration::Union(decl) => decl.is_affine = affinity[&decl.id],
                TypeDeclaration::Interface(_) | TypeDeclaration::Module(_) => {}
            }
        }
    }
}

pub struct FileDeclarations {
    id: FileID,
    pub module_id: TypeID,
    type_id_counter: AtomicU32,
    func_id_counter: AtomicU32,
}

impl FileDeclarations {
    pub(crate) fn new() -> FileDeclarations {
        let id = FileID::new();

        FileDeclarations {
            id,
            module_id: TypeID(id, 1),
            type_id_counter: AtomicU32::new(2),
            func_id_counter: AtomicU32::new(1),
        }
    }

    pub fn new_type_id(&self) -> TypeID {
        let type_id = self.type_id_counter.fetch_add(1, Ordering::Relaxed);
        TypeID(self.id, type_id)
    }

    pub fn new_func_id(&self) -> FunctionID {
        let func_id = self.func_id_counter.fetch_add(1, Ordering::Relaxed);
        FunctionID(self.id, func_id)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileID(u32);

impl FileID {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        static PACKAGE_COUNTER: AtomicU32 = AtomicU32::new(1);
        Self(PACKAGE_COUNTER.fetch_add(1, Ordering::Relaxed))
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeID(FileID, u32);

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FunctionID(FileID, u32);

unsafe impl Zeroable for FunctionID {}

unsafe impl Pod for FunctionID {}

fn is_decl_affine(
    id_to_decl: &HashMap<TypeID, TypeDeclaration>,
    decl: &TypeDeclaration,
    affine_types: &mut HashMap<TypeID, bool>,
) -> bool {
    let id = decl.id();
    if let Some(affinity) = affine_types.get(&id) {
        return *affinity;
    }
    if decl.is_affine() {
        affine_types.insert(id, true);
        return true;
    }
    let is_children_affine = match decl {
        TypeDeclaration::Struct(decl) => decl
            .fields
            .values()
            .filter_map(|expr| expr.type_id())
            .any(|id| is_decl_affine(id_to_decl, &id_to_decl[id], affine_types)),
        TypeDeclaration::Union(decl) => decl
            .variants
            .values()
            .filter_map(|expr| expr.as_ref()?.type_id())
            .any(|id| is_decl_affine(id_to_decl, &id_to_decl[id], affine_types)),
        TypeDeclaration::Interface(_) | TypeDeclaration::Module(_) => false,
    };

    affine_types.insert(id, is_children_affine);
    is_children_affine
}

fn resolve_top_level_declarations(
    module: &mut FileDeclarations,
    names_to_declarations: HashMap<String, &AstNode<'_>>,
    id_to_decl: &mut HashMap<TypeID, TypeDeclaration>,
    id_to_func: &mut HashMap<FunctionID, FuncType>,
    extern_function_bindings: &mut Vec<(String, FunctionID)>,
    extern_function_exports: &mut Vec<(String, FunctionID)>,
) -> Result<(), TypecheckError> {
    let name_to_type_id = names_to_declarations
        .keys()
        .map(|name| (name.as_str(), module.new_type_id()))
        .collect();
    let mut decl_result = Ok(());

    for (name, node) in names_to_declarations.iter() {
        if is_node_function(node) {
            let fn_id = merge_results_or_value(
                &mut decl_result,
                resolve_function(module, &name_to_type_id, node, false, id_to_func),
            );
            if let Some(fn_id) = fn_id {
                let TypeDeclaration::Module(module_decl) =
                    id_to_decl.get_mut(&module.module_id).unwrap()
                else {
                    unreachable!()
                };
                module_decl
                    .exports
                    .insert(name.clone(), ExpressionType::ReferenceToFunction(fn_id));
                if matches!(&node.value, AstNodeValue::ExternFunctionBinding(_)) {
                    extern_function_bindings.push((name.clone(), fn_id));
                }
                if matches!(
                    &node.value,
                    AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                        is_extern: true,
                        ..
                    })
                ) {
                    extern_function_exports.push((name.clone(), fn_id));
                }
            }
        } else {
            let decl = merge_results_or_value(
                &mut decl_result,
                resolve_declaration(module, &name_to_type_id, node, id_to_func),
            );
            if let Some(declaration) = decl {
                let TypeDeclaration::Module(module_decl) =
                    id_to_decl.get_mut(&module.module_id).unwrap()
                else {
                    unreachable!()
                };
                module_decl.exports.insert(
                    name.clone(),
                    ExpressionType::ReferenceToType(declaration.id()),
                );
                id_to_decl.insert(declaration.id(), declaration);
            }
        }
    }

    decl_result
}

fn is_node_function(node: &AstNode<'_>) -> bool {
    matches!(
        &node.value,
        AstNodeValue::FunctionDeclaration(_)
            | AstNodeValue::RequiredFunction(_)
            | AstNodeValue::ExternFunctionBinding(_)
    )
}

fn resolve_function(
    module: &FileDeclarations,
    names_to_type_id: &HashMap<&str, TypeID>,
    node: &AstNode<'_>,
    is_associated: bool,
    id_to_func: &mut HashMap<FunctionID, FuncType>,
) -> Result<FunctionID, TypecheckError> {
    let func_type = match &node.value {
        AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
            params,
            returns,
            is_coroutine,
            ..
        }) => FuncType {
            id: module.new_func_id(),
            type_param_count: 0,
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
            is_coroutine: *is_coroutine,
            provenance: Some(node.provenance.clone()),
        },
        AstNodeValue::RequiredFunction(FunctionHeaderValue {
            params, returns, ..
        })
        | AstNodeValue::ExternFunctionBinding(FunctionHeaderValue {
            params, returns, ..
        }) => FuncType {
            id: module.new_func_id(),
            type_param_count: 0,
            params: params
                .iter()
                .map(|NameAndType { ty: type_, .. }| resolve_type_expr(names_to_type_id, type_))
                .collect::<Result<Vec<_>, _>>()?,
            returns: returns
                .as_ref()
                .map(|returns| resolve_type_expr(names_to_type_id, returns))
                .unwrap_or(Ok(ExpressionType::Void))?,
            is_associated,
            is_coroutine: false,
            provenance: Some(node.provenance.clone()),
        },
        other => panic!("ICE: illegal non-function decl {other:?}"),
    };
    let id = func_type.id;
    id_to_func.insert(func_type.id, func_type);

    Ok(id)
}

fn resolve_declaration(
    module: &FileDeclarations,
    names_to_type_id: &HashMap<&str, TypeID>,
    node: &AstNode<'_>,
    id_to_func: &mut HashMap<FunctionID, FuncType>,
) -> Result<TypeDeclaration, TypecheckError> {
    Ok(match &node.value {
        AstNodeValue::StructDeclaration(StructDeclarationValue {
            fields,
            associated_functions,
            name,
            properties,
        }) => {
            let fields: HashMap<_, _> = merge_result_list(fields.iter().map(
                |NameAndType {
                     name,
                     ty,
                     provenance,
                 }| {
                    let ty = resolve_type_expr(names_to_type_id, ty)?;
                    if matches!(ty, ExpressionType::Pointer(_, _)) {
                        return Err(TypecheckError::IllegalReferenceInsideDataType(
                            provenance.clone(),
                        ));
                    }
                    Ok((name.clone(), ty))
                },
            ))?;
            let associated_functions = associated_functions
                .iter()
                .map(|node| {
                    Ok(match &node.value {
                        AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                            name,
                            ..
                        }) => (
                            name.clone(),
                            resolve_function(module, names_to_type_id, node, true, id_to_func)?,
                        ),
                        _ => panic!(
                            "Associated function should not be anything but function declaration"
                        ),
                    })
                })
                .collect::<Result<HashMap<_, _>, _>>()?;
            let mut is_affine = false;
            for property in properties.iter() {
                match property.as_str() {
                    "Move" => is_affine = true,
                    _ => {
                        return Err(TypecheckError::UnknownProperty(
                            property.clone(),
                            node.provenance.clone(),
                        ))
                    }
                }
            }

            TypeDeclaration::Struct(StructType {
                id: names_to_type_id[name.as_str()],
                fields,
                associated_functions,
                is_affine,
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
                            resolve_function(module, names_to_type_id, node, true, id_to_func)?,
                        ),
                        AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                            name,
                            ..
                        }) => (
                            name.clone(),
                            resolve_function(module, names_to_type_id, node, true, id_to_func)?,
                        ),
                        _ => panic!(
                            "Associated function should not be anything but function declaration"
                        ),
                    })
                })
                .collect::<Result<HashMap<_, _>, _>>()?;

            TypeDeclaration::Interface(InterfaceType {
                id: names_to_type_id[name.as_str()],
                associated_functions,
            })
        }
        AstNodeValue::UnionDeclaration(UnionDeclarationValue {
            variants: variant_ast,
            name,
            properties,
        }) => {
            let variants = merge_result_list(variant_ast.iter().map(|variant| {
                Ok(match variant {
                    UnionDeclarationVariant::WithValue(NameAndType {
                        name,
                        ty,
                        provenance,
                    }) => {
                        let ty = resolve_type_expr(names_to_type_id, ty)?;
                        if matches!(ty, ExpressionType::Pointer(_, _)) {
                            return Err(TypecheckError::IllegalReferenceInsideDataType(
                                provenance.clone(),
                            ));
                        }
                        (name.clone(), Some(ty))
                    }
                    UnionDeclarationVariant::WithoutValue(name) => (name.clone(), None),
                })
            }))?;

            let mut is_affine = false;
            for property in properties.iter() {
                match property.as_str() {
                    "Move" => is_affine = true,
                    _ => {
                        return Err(TypecheckError::UnknownProperty(
                            property.clone(),
                            node.provenance.clone(),
                        ))
                    }
                }
            }

            TypeDeclaration::Union(UnionType {
                id: names_to_type_id[name.as_str()],
                variant_order: variant_ast
                    .iter()
                    .map(|variant| match variant {
                        UnionDeclarationVariant::WithValue(name_and_type) => {
                            name_and_type.name.clone()
                        }
                        UnionDeclarationVariant::WithoutValue(name) => name.clone(),
                    })
                    .collect(),
                variants,
                is_affine,
            })
        }
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
            "i32" => ExpressionType::Primitive(PrimitiveType::Int32),
            "f32" => ExpressionType::Primitive(PrimitiveType::Float32),
            "i64" => ExpressionType::Primitive(PrimitiveType::Int64),
            "f64" => ExpressionType::Primitive(PrimitiveType::Float64),
            "char" => ExpressionType::Primitive(PrimitiveType::Char),
            "string" => ExpressionType::Collection(CollectionType::String),
            "size" => ExpressionType::Primitive(PrimitiveType::PointerSize),
            other => ExpressionType::InstanceOf(
                *name_to_type_id
                    .get(other)
                    .ok_or(TypecheckError::NameNotFound(node.provenance.clone()))?,
            ),
        },
        AstNodeValue::VoidType => ExpressionType::Void,
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
        AstNodeValue::RcType(inner) => ExpressionType::Collection(
            CollectionType::ReferenceCounter(Box::new(resolve_type_expr(name_to_type_id, inner)?)),
        ),
        AstNodeValue::DictType(key, value) => ExpressionType::Collection(CollectionType::Dict(
            Box::new(resolve_type_expr(name_to_type_id, key)?),
            Box::new(resolve_type_expr(name_to_type_id, value)?),
        )),
        AstNodeValue::NullableType(inner) => {
            ExpressionType::Nullable(Box::new(resolve_type_expr(name_to_type_id, inner)?))
        }
        AstNodeValue::GeneratorType { yield_ty, param_ty } => ExpressionType::Generator {
            yield_ty: Box::new(resolve_type_expr(name_to_type_id, yield_ty)?),
            param_ty: Box::new(resolve_type_expr(name_to_type_id, param_ty)?),
        },
        AstNodeValue::CellType(inner_ty) => ExpressionType::Collection(CollectionType::Cell(
            Box::new(resolve_type_expr(name_to_type_id, inner_ty)?),
        )),
        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::RequiredFunction(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::UnionDeclaration(_)
        | AstNodeValue::InterfaceDeclaration(_)
        | AstNodeValue::Declaration(_, _, _, _)
        | AstNodeValue::Import(_)
        | AstNodeValue::Return(_)
        | AstNodeValue::Yield(_)
        | AstNodeValue::Statement(_)
        | AstNodeValue::Deref(_)
        | AstNodeValue::Int(_)
        | AstNodeValue::Null
        | AstNodeValue::Float(_)
        | AstNodeValue::Bool(_)
        | AstNodeValue::BinExpr(_, _, _)
        | AstNodeValue::If(_)
        | AstNodeValue::While(_, _)
        | AstNodeValue::Loop(_)
        | AstNodeValue::Call(_, _)
        | AstNodeValue::TakeUnique(_)
        | AstNodeValue::TakeRef(_)
        | AstNodeValue::RecordLiteral { .. }
        | AstNodeValue::ArrayLiteral(_)
        | AstNodeValue::ArrayLiteralLength(_, _)
        | AstNodeValue::Block(_)
        | AstNodeValue::StringLiteral(_)
        | AstNodeValue::CharLiteral(_)
        | AstNodeValue::UnaryExpr(_, _)
        | AstNodeValue::DictLiteral(_)
        | AstNodeValue::Match(_)
        | AstNodeValue::BorrowDeclaration(..)
        | AstNodeValue::ReferenceCountLiteral(_)
        | AstNodeValue::CellLiteral(_) => {
            panic!("ICE: Illegal node in type name: {:?}", node.value);
        }
    })
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntrinsicFunction {
    ArrayLength,
    ArrayPush,
    ArrayFree,

    DictionaryInsert,
    DictionaryContains,

    RcClone,
    RcDecrement,
    RcFree,

    CellGet,
    CellSet,
}

pub struct CollectionIntrinsic {
    pub intrinsic_fn: IntrinsicFunction,
    pub fn_id: FunctionID,
    pub ptr_ty: PointerKind,
}

fn add_intrinsics(ctx: &mut DeclarationContext) {
    let mut array_intrinsics = HashMap::new();
    add_intrinsic(
        ctx,
        &mut array_intrinsics,
        "len",
        IntrinsicFunction::ArrayLength,
        1,
        vec![ExpressionType::Pointer(
            PointerKind::Shared,
            Box::new(ExpressionType::TypeParameterReference(0)),
        )],
        ExpressionType::Primitive(PrimitiveType::PointerSize),
        PointerKind::Shared,
    );
    add_intrinsic(
        ctx,
        &mut array_intrinsics,
        "push",
        IntrinsicFunction::ArrayPush,
        1,
        vec![
            ExpressionType::Pointer(
                PointerKind::Unique,
                Box::new(ExpressionType::Collection(CollectionType::Array(Box::new(
                    ExpressionType::TypeParameterReference(0),
                )))),
            ),
            ExpressionType::TypeParameterReference(0),
        ],
        ExpressionType::Void,
        PointerKind::Unique,
    );
    ctx.array_intrinsics = array_intrinsics;

    let mut dict_intrinsics = HashMap::new();
    add_intrinsic(
        ctx,
        &mut dict_intrinsics,
        "contains_key",
        IntrinsicFunction::DictionaryContains,
        2,
        vec![
            ExpressionType::Pointer(
                PointerKind::Shared,
                Box::new(ExpressionType::Collection(CollectionType::Dict(
                    Box::new(ExpressionType::TypeParameterReference(0)),
                    Box::new(ExpressionType::TypeParameterReference(1)),
                ))),
            ),
            ExpressionType::Pointer(
                PointerKind::Shared,
                Box::new(ExpressionType::TypeParameterReference(0)),
            ),
        ],
        ExpressionType::Primitive(PrimitiveType::Bool),
        PointerKind::Shared,
    );
    add_intrinsic(
        ctx,
        &mut dict_intrinsics,
        "insert",
        IntrinsicFunction::DictionaryInsert,
        2,
        vec![
            ExpressionType::Pointer(
                PointerKind::Unique,
                Box::new(ExpressionType::Collection(CollectionType::Dict(
                    Box::new(ExpressionType::TypeParameterReference(0)),
                    Box::new(ExpressionType::TypeParameterReference(1)),
                ))),
            ),
            ExpressionType::TypeParameterReference(0),
            ExpressionType::TypeParameterReference(1),
        ],
        ExpressionType::Void,
        PointerKind::Unique,
    );
    ctx.dict_intrinsics = dict_intrinsics;

    let mut rc_intrinsics = HashMap::new();
    add_intrinsic(
        ctx,
        &mut rc_intrinsics,
        "clone",
        IntrinsicFunction::RcClone,
        1,
        vec![ExpressionType::Pointer(
            PointerKind::Shared,
            Box::new(ExpressionType::Collection(
                CollectionType::ReferenceCounter(Box::new(ExpressionType::TypeParameterReference(
                    0,
                ))),
            )),
        )],
        ExpressionType::Collection(CollectionType::ReferenceCounter(Box::new(
            ExpressionType::TypeParameterReference(0),
        ))),
        PointerKind::Shared,
    );
    ctx.rc_intrinsics = rc_intrinsics;

    let mut cell_intrinsics = HashMap::new();
    add_intrinsic(
        ctx,
        &mut cell_intrinsics,
        "get",
        IntrinsicFunction::CellGet,
        1,
        vec![
            ExpressionType::Pointer(
                PointerKind::Shared,
                Box::new(ExpressionType::Collection(CollectionType::Cell(Box::new(
                    ExpressionType::TypeParameterReference(0),
                )))),
            ),
            ExpressionType::Pointer(
                PointerKind::Unique,
                Box::new(ExpressionType::TypeParameterReference(0)),
            ),
        ],
        ExpressionType::Void,
        PointerKind::Shared,
    );
    add_intrinsic(
        ctx,
        &mut cell_intrinsics,
        "set",
        IntrinsicFunction::CellSet,
        1,
        vec![
            ExpressionType::Pointer(
                PointerKind::Shared,
                Box::new(ExpressionType::Collection(CollectionType::Cell(Box::new(
                    ExpressionType::TypeParameterReference(0),
                )))),
            ),
            ExpressionType::TypeParameterReference(0),
        ],
        ExpressionType::Void,
        PointerKind::Shared,
    );
    ctx.cell_intrinsics = cell_intrinsics;
}

#[allow(clippy::too_many_arguments)]
fn add_intrinsic(
    ctx: &mut DeclarationContext,
    collection_fns: &mut HashMap<&'static str, CollectionIntrinsic>,
    name: &'static str,
    intrinsic_fn: IntrinsicFunction,
    type_param_count: usize,
    params: Vec<ExpressionType>,
    returns: ExpressionType,
    ptr_ty: PointerKind,
) {
    let fn_id = ctx.intrinsic_module.new_func_id();
    collection_fns.insert(
        name,
        CollectionIntrinsic {
            intrinsic_fn,
            fn_id,
            ptr_ty,
        },
    );
    ctx.id_to_func.insert(
        fn_id,
        FuncType {
            id: fn_id,
            is_associated: true,
            type_param_count,
            params,
            returns,
            is_coroutine: false,
            provenance: None,
        },
    );
    ctx.intrinsic_to_id.insert(intrinsic_fn, fn_id);
}
