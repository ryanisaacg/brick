use std::collections::HashMap;

use brick::{
    expr_ty_to_physical, id::FunctionID, lower_code, CompileError, ExpressionType, LinearFunction,
    LowerResults, StaticDeclaration,
};
use wasm_encoder::{
    CodeSection, ConstExpr, DataSection, DataSegment, DataSegmentMode, ExportKind, ExportSection,
    FunctionSection, GlobalSection, GlobalType, ImportSection, MemorySection, MemoryType, Module,
    StartSection, TypeSection, ValType,
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

const WASM_BOOL_SIZE: usize = 4;
const WASM_USIZE: usize = 4;

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
        declarations,
        ty_declarations,
        constant_data,
    } = lower_code(
        module_name,
        source_name,
        contents,
        WASM_BOOL_SIZE,
        WASM_USIZE,
    )?;

    let mut function_return_types = HashMap::new();
    for decl in declarations.values() {
        decl.visit(&mut |decl| {
            let StaticDeclaration::Func(func) = decl else {
                return;
            };
            function_return_types.insert(
                func.func_id,
                if func.returns == ExpressionType::Void
                    || func.returns == ExpressionType::Unreachable
                {
                    None
                } else {
                    Some(expr_ty_to_physical(&func.returns))
                },
            );
        })
    }

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
    let mut data_section = DataSection::new();

    let constant_data_start = 0;
    let constant_data_offset = constant_data.len() as i32;
    data_section.segment(DataSegment {
        mode: DataSegmentMode::Active {
            memory_index: 0,
            offset: &ConstExpr::i32_const(constant_data_start),
        },
        data: constant_data,
    });

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
        &ConstExpr::i32_const(constant_data_offset),
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
            &function_return_types,
            stack_pointer,
            alloc_pointer,
            &linear_function_to_id,
            constant_data_start,
            function,
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
    module.section(&data_section);

    Ok(module)
}
