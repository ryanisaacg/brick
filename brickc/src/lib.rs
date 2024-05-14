use std::collections::HashMap;

use brick::{
    id::FunctionID, lower_code, typecheck_module, CompileError, LinearFunction, LowerResults,
    PhysicalPrimitive, PhysicalType,
};
use wasm_encoder::{
    CodeSection, ConstExpr, ExportKind, ExportSection, FunctionSection, GlobalSection, GlobalType,
    ImportSection, MemorySection, MemoryType, Module, StartSection, TypeSection, ValType,
};

mod function_bodies;
mod function_headers;
mod runtime;

/**
 * Note: currently in WASM, there is only a 0-memory. However, the spec is forwards-compatible with
 * more
 */
const MAIN_MEMORY: u32 = 0;
const STACK_PAGES: u64 = 16;
const HEAP_MINIMUM_PAGES: u64 = 48;
const MEMORY_MINIMUM_PAGES: u64 = STACK_PAGES + HEAP_MINIMUM_PAGES;
const MAXIMUM_MEMORY: u64 = 16_384;
const WASM_PAGE_SIZE: u64 = 65_536;

pub fn compile(
    module_name: &str,
    source_name: &'static str,
    contents: String,
    is_start_function: bool,
) -> Result<Module, CompileError> {
    let LowerResults {
        statements,
        statements_ty,
        mut functions,
        declarations: _,
        ty_declarations,
        constant_data: _,
    } = lower_code(module_name, source_name, contents)?;
    let main = LinearFunction {
        id: FunctionID::new(),
        body: statements,
        params: Vec::new(),
        returns: statements_ty,
    };
    functions.insert(0, main);

    let mut module = Module::new();

    let mut ty_section = TypeSection::new();
    let mut import_section = ImportSection::new();
    let mut fn_section = FunctionSection::new();
    let mut codes = CodeSection::new();
    let mut globals = GlobalSection::new();
    let mut exports = ExportSection::new();
    let mut memories = MemorySection::new();

    let stack_pointer = 0;
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        // TODO
        &ConstExpr::i32_const(2048),
    );
    let alloc_pointer = 1;
    globals.global(
        GlobalType {
            val_type: ValType::I32,
            mutable: true,
            shared: false,
        },
        // TODO
        &ConstExpr::i32_const(4),
    );

    let mut function_id_to_idx = HashMap::new();
    let mut type_index = 0;
    let linear_function_to_id =
        runtime::add_runtime_imports(&mut import_section, &mut ty_section, &mut type_index);
    let main_index = type_index;
    for function in functions.iter() {
        function_headers::encode(
            &ty_declarations,
            type_index,
            function,
            &mut ty_section,
            &mut fn_section,
        );
        function_id_to_idx.insert(function.id, type_index);
        type_index += 1;
    }
    for function in functions.iter() {
        codes.function(&function_bodies::encode(
            &function_id_to_idx,
            &ty_declarations,
            stack_pointer,
            alloc_pointer,
            &linear_function_to_id,
            &function,
        ));
    }

    memories.memory(MemoryType {
        minimum: MEMORY_MINIMUM_PAGES,
        maximum: Some(MAXIMUM_MEMORY),
        memory64: false,
        shared: false,
        page_size_log2: None,
    });
    exports.export("memory", ExportKind::Memory, MAIN_MEMORY);
    exports.export("main", ExportKind::Func, main_index);

    module.section(&ty_section);
    module.section(&import_section);
    module.section(&fn_section);
    module.section(&memories);
    module.section(&globals);
    module.section(&exports);
    if is_start_function {
        module.section(&StartSection { function_index: 0 });
    }
    module.section(&codes);

    Ok(module)
}
