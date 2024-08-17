use std::collections::HashMap;

use brick::RuntimeFunction;
use wasm_encoder::{
    CodeSection, EntityType, Function, ImportSection, Instruction, TypeSection, ValType,
};

use crate::{MAIN_MEMORY, WASM_PAGE_SIZE};

pub fn add_runtime_imports(
    imports: &mut ImportSection,
    ty_section: &mut TypeSection,
) -> HashMap<RuntimeFunction, u32> {
    let mut indices = HashMap::new();

    let functions = [
        (
            RuntimeFunction::Memcpy,
            "brick_memcpy",
            vec![ValType::I32, ValType::I32, ValType::I32],
            vec![],
        ),
        (
            RuntimeFunction::Alloc { alignment: 0 },
            "brick_runtime_alloc",
            vec![ValType::I32, ValType::I32, ValType::I32],
            vec![ValType::I32],
        ),
        (
            RuntimeFunction::Realloc,
            "brick_runtime_realloc",
            vec![ValType::I32, ValType::I32, ValType::I32],
            vec![ValType::I32],
        ),
        (
            RuntimeFunction::Dealloc,
            "brick_runtime_dealloc",
            vec![ValType::I32, ValType::I32],
            vec![],
        ),
        (
            RuntimeFunction::StringConcat,
            "brick_string_concat",
            vec![
                // allocator
                ValType::I32,
                // string a
                ValType::I32,
                ValType::I32,
                // string b
                ValType::I32,
                ValType::I32,
                // pointers for string c
                ValType::I32,
                ValType::I32,
            ],
            vec![],
        ),
    ];
    for (linear_id, name, params, returns) in functions {
        imports.import(
            "brick-runtime",
            name,
            EntityType::Function(ty_section.len()),
        );
        indices.insert(linear_id, ty_section.len());
        ty_section.function(params.iter().cloned(), returns.iter().cloned());
    }

    indices
}

pub fn add_init_import(imports: &mut ImportSection, ty_section: &mut TypeSection) {
    imports.import(
        "brick-runtime",
        "brick_runtime_init",
        EntityType::Function(ty_section.len()),
    );
    ty_section.function([ValType::I32, ValType::I32], [ValType::I32]);
}

pub fn add_start(
    codes_section: &mut CodeSection,
    runtime_initialize_index: u32,
    main_index: u32,
    heap_base_pointer_idx: u32,
) {
    let mut start = Function::new([]);
    // Call the init function (heap base, heap size)
    start.instruction(&Instruction::GlobalGet(heap_base_pointer_idx));
    // calculate the total availabile memory
    start.instruction(&Instruction::MemorySize(MAIN_MEMORY));
    start.instruction(&Instruction::I32Const(WASM_PAGE_SIZE));
    start.instruction(&Instruction::I32Mul);
    // Only include the heap portion
    start.instruction(&Instruction::GlobalGet(heap_base_pointer_idx));
    start.instruction(&Instruction::I32Sub);
    start.instruction(&Instruction::Call(runtime_initialize_index));
    start.instruction(&Instruction::Drop);

    start.instruction(&Instruction::Call(main_index));
    start.instruction(&Instruction::End);
    codes_section.function(&start);
}
