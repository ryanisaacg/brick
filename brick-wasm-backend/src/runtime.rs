use std::collections::HashMap;

use brick::RuntimeFunction;
use wasm_encoder::{
    BlockType, CodeSection, EntityType, Function, ImportSection, Instruction, TypeSection, ValType,
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
    init_index: u32,
    main_index: u32,
    alloc_pointer_idx: u32,
    heap_size: i32,
    intended_pages: i32,
) {
    let mut start = Function::new([(1, ValType::I32)]);
    // Allocate the right size of linear memory
    start.instruction(&Instruction::I32Const(intended_pages));
    start.instruction(&Instruction::MemorySize(0));
    start.instruction(&Instruction::I32Sub);
    // Store intended - actual memory
    start.instruction(&Instruction::LocalSet(0));
    start.instruction(&Instruction::LocalGet(0));
    start.instruction(&Instruction::I32Const(0));
    // If intended - actual > 0, grow memory by that amount
    start.instruction(&Instruction::I32GtS);
    start.instruction(&Instruction::If(BlockType::Empty));
    start.instruction(&Instruction::LocalGet(0));
    start.instruction(&Instruction::MemoryGrow(0));
    start.instruction(&Instruction::Drop);
    start.instruction(&Instruction::End);

    start.instruction(&Instruction::GlobalGet(alloc_pointer_idx));
    start.instruction(&Instruction::I32Const(heap_size));
    start.instruction(&Instruction::Call(init_index));
    start.instruction(&Instruction::GlobalSet(alloc_pointer_idx));
    start.instruction(&Instruction::Call(main_index));
    start.instruction(&Instruction::End);
    codes_section.function(&start);
}
