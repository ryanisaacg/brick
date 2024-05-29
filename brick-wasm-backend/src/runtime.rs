use std::collections::HashMap;

use brick::RuntimeFunction;
use wasm_encoder::{
    CodeSection, EntityType, Function, ImportSection, Instruction, TypeSection, ValType,
};

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
            RuntimeFunction::Alloc,
            "brick_runtime_alloc",
            vec![ValType::I32, ValType::I32],
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
                ValType::I32,
                ValType::I32,
                ValType::I32,
                ValType::I32,
                ValType::I32,
            ],
            vec![ValType::I32, ValType::I32],
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
    init_index: u32,
    main_index: u32,
    alloc_pointer_idx: u32,
    heap_size: i32,
) {
    let mut start = Function::new([]);
    start.instruction(&Instruction::GlobalGet(alloc_pointer_idx));
    start.instruction(&Instruction::I32Const(heap_size));
    start.instruction(&Instruction::Call(init_index));
    start.instruction(&Instruction::GlobalSet(alloc_pointer_idx));
    start.instruction(&Instruction::Call(main_index));
    start.instruction(&Instruction::End);
    codes_section.function(&start);
}
