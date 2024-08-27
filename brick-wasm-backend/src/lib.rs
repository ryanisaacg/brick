use std::collections::HashMap;

use brick::{
    expr_ty_to_physical, lower_code, CompileError, ExpressionType, LinearFunction, LowerResults,
    SourceFile,
};
use function_bodies::{walk_vals_write_order, FunctionEncoder};
use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, DataSegment, DataSegmentMode, ElementSection, Elements,
    EntityType, ExportKind, ExportSection, FunctionSection, GlobalType, ImportSection, MemoryType,
    Module, RefType, StartSection, TableSection, TableType, TypeSection, ValType,
};

mod function_bodies;
mod function_headers;
mod indirect_function_table;
mod runtime;

/**
 * Note: currently in WASM, there is only a 0-memory. However, the spec is forwards-compatible with
 * more
 */
const MAIN_MEMORY: u32 = 0;
/**
 * Defined by the spec
 */
const WASM_PAGE_SIZE: i32 = 65536;

const WASM_BOOL_SIZE: usize = 4;
const WASM_USIZE: usize = 4;

pub struct BackendOptions<'a> {
    pub include_start_marker: bool,
    pub top_level_name: &'a str,
}

impl Default for BackendOptions<'_> {
    fn default() -> Self {
        Self {
            include_start_marker: true,
            top_level_name: "main",
        }
    }
}

pub fn compile(
    sources: Vec<SourceFile>,
    BackendOptions {
        include_start_marker,
        top_level_name,
    }: BackendOptions,
) -> Result<Module, CompileError> {
    let LowerResults {
        statements,
        statements_ty,
        mut functions,
        declarations,
        type_layouts,
        constant_data,
    } = lower_code(sources, WASM_BOOL_SIZE, WASM_USIZE)?;

    let mut function_return_types = HashMap::new();
    for func in declarations.id_to_func.values() {
        function_return_types.insert(
            func.id,
            if func.returns == ExpressionType::Void || func.returns == ExpressionType::Unreachable {
                None
            } else {
                Some(expr_ty_to_physical(&func.returns))
            },
        );
    }

    let mut main_fn_results = Vec::new();
    if let Some(return_ty) = &statements_ty {
        walk_vals_write_order(&type_layouts, return_ty, 0, &mut |p, _| {
            main_fn_results.push(p)
        });
    }
    let main = LinearFunction {
        id: declarations.intrinsic_module.new_func_id(),
        body: statements,
        params: Vec::new(),
        returns: statements_ty,
    };
    functions.insert(0, main);

    let mut module = Module::new();

    let mut ty_section = TypeSection::new();
    let mut import_section = ImportSection::new();
    let mut fn_section = FunctionSection::new();
    let mut table_section = TableSection::new();
    let mut exports = ExportSection::new();
    let mut elem_section = ElementSection::new();
    let mut codes = CodeSection::new();
    let mut data_section = DataSection::new();

    let constant_data_start = 4;
    data_section.segment(DataSegment {
        mode: DataSegmentMode::Active {
            memory_index: 0,
            offset: &ConstExpr::i32_const(constant_data_start),
        },
        data: constant_data,
    });

    let mut function_id_to_fn_idx = HashMap::new();
    let mut function_id_to_ty_idx = HashMap::new();
    // Imports
    let linear_function_to_id = runtime::add_runtime_imports(&mut import_section, &mut ty_section);
    for (name, fn_id) in declarations.extern_function_bindings.iter() {
        function_id_to_fn_idx.insert(*fn_id, fn_section.len() + import_section.len());
        function_id_to_ty_idx.insert(*fn_id, ty_section.len());
        import_section.import("bindings", name, EntityType::Function(ty_section.len()));
        let function = &declarations.id_to_func[fn_id];
        function_headers::encode_func_ty(&type_layouts, function, &mut ty_section);
    }
    let runtime_init_idx = fn_section.len() + import_section.len();
    runtime::add_init_import(&mut import_section, &mut ty_section);
    let imported_functions = import_section.len();

    import_section.import(
        "brick-runtime",
        "memory",
        EntityType::Memory(MemoryType {
            minimum: 16,
            maximum: None,
            memory64: false,
            shared: false,
            page_size_log2: None,
        }),
    );

    let stack_pointer = 0;
    import_section.import(
        "brick-runtime",
        "__stack_pointer",
        EntityType::Global(GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        }),
    );
    let heap_base_pointer = 1;
    import_section.import(
        "brick-runtime",
        "__heap_base",
        EntityType::Global(GlobalType {
            val_type: ValType::I32,
            mutable: false,
            shared: false,
        }),
    );

    // Function headers
    let main_index = ty_section.len();
    for function in functions.iter() {
        if function_id_to_ty_idx.contains_key(&function.id) {
            continue;
        }
        function_id_to_fn_idx.insert(function.id, fn_section.len() + imported_functions);
        fn_section.function(ty_section.len());
        function_id_to_ty_idx.insert(function.id, ty_section.len());
        function_headers::encode_linear(&type_layouts, function, &mut ty_section);
    }
    for function in declarations.id_to_func.values() {
        // Skip intrinsics with generics
        if function.type_param_count > 0 || function_id_to_ty_idx.contains_key(&function.id) {
            continue;
        }
        function_id_to_ty_idx.insert(function.id, ty_section.len());
        function_headers::encode_func_ty(&type_layouts, function, &mut ty_section);
    }
    let mut indirect_function_id_to_table = HashMap::new();
    let mut indirect_functions_for_table = Vec::new();
    for function in functions.iter() {
        indirect_function_table::encode(
            &mut indirect_function_id_to_table,
            &mut indirect_functions_for_table,
            &function_id_to_ty_idx,
            function,
        );
    }

    // Function pointers
    let indirect_call_table = table_section.len();
    table_section.table(TableType {
        element_type: RefType::FUNCREF,
        minimum: indirect_functions_for_table.len() as u32,
        maximum: Some(indirect_functions_for_table.len() as u32),
    });
    elem_section.active(
        Some(indirect_call_table),
        &ConstExpr::i32_const(0),
        Elements::Functions(&indirect_functions_for_table[..]),
    );

    // Function bodies
    let context = FunctionEncoder {
        function_id_to_fn_idx: &function_id_to_fn_idx,
        function_id_to_ty_idx: &function_id_to_ty_idx,
        type_layouts: &type_layouts,
        function_return_types: &function_return_types,
        stackptr_global_idx: stack_pointer,
        allocptr_global_idx: heap_base_pointer,
        linear_function_to_id: &linear_function_to_id,
        constant_data_start,
        indirect_call_table,
        indirect_function_id_to_table: &indirect_function_id_to_table,
    };
    for function in functions.iter() {
        codes.function(&context.encode(function));
    }

    // Start section
    let start_index = imported_functions + fn_section.len();
    fn_section.function(ty_section.len());
    ty_section.function([], main_fn_results);
    runtime::add_start(&mut codes, runtime_init_idx, main_index, heap_base_pointer);

    exports.export("memory", ExportKind::Memory, MAIN_MEMORY);
    exports.export(top_level_name, ExportKind::Func, start_index);
    for (name, func_id) in declarations.extern_function_exports.iter() {
        let fn_idx = function_id_to_fn_idx[func_id];
        exports.export(name, ExportKind::Func, fn_idx);
    }

    module.section(&ty_section);
    module.section(&import_section);
    module.section(&fn_section);
    module.section(&table_section);
    module.section(&exports);
    if include_start_marker {
        module.section(&StartSection {
            function_index: start_index,
        });
    }
    module.section(&elem_section);
    module.section(&codes);
    module.section(&data_section);

    Ok(module)
}
