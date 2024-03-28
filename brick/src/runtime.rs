use std::{
    collections::HashMap,
    sync::{OnceLock, RwLock},
};

use crate::{
    id::{FunctionID, TypeID},
    typecheck::{
        CollectionType, ExpressionType, FuncType, PointerKind, PrimitiveType, StaticDeclaration,
    },
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeFunction {
    ArrayLength,
    ArrayPush,
    DictionaryInsert,
    DictionaryContains,
}

pub struct RuntimeFunctionOnType {
    pub func: RuntimeFunction,
    pub decl: StaticDeclaration,
    pub ptr_ty: PointerKind,
}

static ARRAY_FUNCTIONS: OnceLock<HashMap<String, RuntimeFunctionOnType>> = OnceLock::new();
static DICT_FUNCTIONS: OnceLock<HashMap<String, RuntimeFunctionOnType>> = OnceLock::new();
static ALL_FUNCTIONS: OnceLock<RwLock<HashMap<RuntimeFunction, &RuntimeFunctionOnType>>> =
    OnceLock::new();

pub fn add_runtime_functions(declarations: &mut HashMap<TypeID, &StaticDeclaration>) {
    let array_functions = ARRAY_FUNCTIONS.get_or_init(|| {
        let mut map = HashMap::new();
        map.insert(
            "len".to_string(),
            RuntimeFunctionOnType {
                func: RuntimeFunction::ArrayLength,
                decl: StaticDeclaration::Func(FuncType {
                    id: TypeID::new(),
                    func_id: FunctionID::new(),
                    is_associated: true,
                    type_param_count: 1,
                    params: vec![ExpressionType::Pointer(
                        PointerKind::Shared,
                        Box::new(ExpressionType::TypeParameterReference(0)),
                    )],
                    returns: ExpressionType::Primitive(PrimitiveType::PointerSize),
                    is_coroutine: false,
                    provenance: None,
                }),
                ptr_ty: PointerKind::Shared,
            },
        );
        map.insert(
            "push".to_string(),
            RuntimeFunctionOnType {
                func: RuntimeFunction::ArrayPush,
                decl: StaticDeclaration::Func(FuncType {
                    id: TypeID::new(),
                    func_id: FunctionID::new(),
                    is_associated: true,
                    type_param_count: 1,
                    params: vec![
                        ExpressionType::Pointer(
                            PointerKind::Unique,
                            Box::new(ExpressionType::Collection(CollectionType::Array(Box::new(
                                ExpressionType::TypeParameterReference(0),
                            )))),
                        ),
                        ExpressionType::TypeParameterReference(0),
                    ],
                    returns: ExpressionType::Void,
                    is_coroutine: false,
                    provenance: None,
                }),
                ptr_ty: PointerKind::Unique,
            },
        );

        map
    });
    let dict_functions = DICT_FUNCTIONS.get_or_init(|| {
        let mut map = HashMap::new();
        map.insert(
            "contains_key".to_string(),
            RuntimeFunctionOnType {
                func: RuntimeFunction::DictionaryContains,
                decl: StaticDeclaration::Func(FuncType {
                    id: TypeID::new(),
                    func_id: FunctionID::new(),
                    is_associated: true,
                    type_param_count: 2,
                    params: vec![
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
                    returns: ExpressionType::Primitive(PrimitiveType::Bool),
                    is_coroutine: false,
                    provenance: None,
                }),
                ptr_ty: PointerKind::Shared,
            },
        );
        map.insert(
            "insert".to_string(),
            RuntimeFunctionOnType {
                func: RuntimeFunction::DictionaryInsert,
                decl: StaticDeclaration::Func(FuncType {
                    id: TypeID::new(),
                    func_id: FunctionID::new(),
                    is_associated: true,
                    type_param_count: 2,
                    params: vec![
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
                    is_coroutine: false,
                    returns: ExpressionType::Void,
                    provenance: None,
                }),
                ptr_ty: PointerKind::Unique,
            },
        );

        map
    });

    let all_funcs = array_functions.iter().chain(dict_functions.iter());

    for (_key, decl) in all_funcs {
        declarations.insert(decl.decl.id(), &decl.decl);
    }
}

pub fn array_runtime_functions() -> &'static HashMap<String, RuntimeFunctionOnType> {
    ARRAY_FUNCTIONS
        .get()
        .expect("add_runtime_functions must be called first")
}

pub fn dictionary_runtime_functions() -> &'static HashMap<String, RuntimeFunctionOnType> {
    DICT_FUNCTIONS
        .get()
        .expect("add_runtime_functions must be called first")
}

pub fn info_for_function(fn_id: &RuntimeFunction) -> &'static RuntimeFunctionOnType {
    let all_functions = ALL_FUNCTIONS.get_or_init(|| {
        let array_functions = array_runtime_functions();
        let dict_functions = dictionary_runtime_functions();
        let combination: HashMap<_, _> = array_functions
            .iter()
            .chain(dict_functions.iter())
            .map(|(_, runtime_fn)| (runtime_fn.func, runtime_fn))
            .collect();
        RwLock::new(combination)
    });
    all_functions.read().unwrap().get(fn_id).unwrap()
}
