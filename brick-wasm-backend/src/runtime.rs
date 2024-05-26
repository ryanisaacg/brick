use std::collections::HashMap;

use brick::RuntimeFunction;
use wasm_encoder::{
    CodeSection, EntityType, Function, FunctionSection, ImportSection, Instruction, TypeSection,
    ValType,
};

pub fn add_runtime_imports(
    imports: &mut ImportSection,
    ty_section: &mut TypeSection,
    type_index: &mut u32,
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
        ty_section.function(params.iter().cloned(), returns.iter().cloned());
        imports.import("brick-runtime", name, EntityType::Function(*type_index));
        indices.insert(linear_id, *type_index);
        *type_index += 1;
    }

    indices
}

pub fn add_init_import(
    imports: &mut ImportSection,
    ty_section: &mut TypeSection,
    type_index: &mut u32,
) -> u32 {
    ty_section.function([ValType::I32, ValType::I32], [ValType::I32]);
    imports.import(
        "brick-runtime",
        "brick_runtime_init",
        EntityType::Function(*type_index),
    );
    let init_idx = *type_index;
    *type_index += 1;

    init_idx
}

#[allow(clippy::too_many_arguments)]
pub fn add_start(
    ty_section: &mut TypeSection,
    fn_section: &mut FunctionSection,
    codes_section: &mut CodeSection,
    type_index: &mut u32,
    init_index: u32,
    main_index: u32,
    main_return: Vec<ValType>,
    alloc_pointer_idx: u32,
    heap_size: i32,
) -> u32 {
    ty_section.function([], main_return);
    fn_section.function(*type_index);
    let mut start = Function::new([]);
    start.instruction(&Instruction::GlobalGet(alloc_pointer_idx));
    start.instruction(&Instruction::I32Const(heap_size));
    start.instruction(&Instruction::Call(init_index));
    start.instruction(&Instruction::GlobalSet(alloc_pointer_idx));
    start.instruction(&Instruction::Call(main_index));
    start.instruction(&Instruction::End);
    codes_section.function(&start);

    let start_idx = *type_index;
    *type_index += 1;

    start_idx
}
