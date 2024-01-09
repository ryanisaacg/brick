use std::{
    collections::HashMap,
    sync::{OnceLock, RwLock},
};

use crate::{
    id::{FunctionID, TypeID},
    typecheck::{ExpressionType, FuncType, PointerKind, PrimitiveType, StaticDeclaration},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RuntimeFunction {
    ArrayLength,
}

pub struct RuntimeFunctionOnType {
    pub func: RuntimeFunction,
    pub decl: StaticDeclaration,
}

static ARRAY_FUNCTIONS: OnceLock<HashMap<String, RuntimeFunctionOnType>> = OnceLock::new();
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
                    // TODO: generic types
                    params: vec![ExpressionType::Pointer(
                        PointerKind::Shared,
                        Box::new(ExpressionType::Void),
                    )],
                    returns: ExpressionType::Primitive(PrimitiveType::PointerSize),
                }),
            },
        );
        map
    });

    for (_key, decl) in array_functions.iter() {
        declarations.insert(decl.decl.id(), &array_functions.get("len").unwrap().decl);
    }
}

pub fn array_runtime_functions() -> &'static HashMap<String, RuntimeFunctionOnType> {
    ARRAY_FUNCTIONS
        .get()
        .expect("add_runtime_functions must be called first")
}

pub fn info_for_function(fn_id: &RuntimeFunction) -> &'static RuntimeFunctionOnType {
    let all_functions = ALL_FUNCTIONS.get_or_init(|| {
        let array_functions = ARRAY_FUNCTIONS
            .get()
            .expect("add_runtime_functions must be called first");
        let combination: HashMap<_, _> = array_functions
            .iter()
            .map(|(_, runtime_fn)| (runtime_fn.func, runtime_fn))
            .collect();
        RwLock::new(combination)
    });
    all_functions.read().unwrap().get(fn_id).unwrap()
}
