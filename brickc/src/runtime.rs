use std::collections::HashMap;

use brick::LinearRuntimeFunction;
use wasm_encoder::{EntityType, FunctionSection, ImportSection, TypeSection, ValType};

pub fn add_runtime_imports(
    imports: &mut ImportSection,
    ty_section: &mut TypeSection,
    type_index: &mut u32,
) -> HashMap<LinearRuntimeFunction, u32> {
    let mut indices = HashMap::new();

    let functions = [(
        LinearRuntimeFunction::Memcpy,
        "brick_memcpy",
        [ValType::I32, ValType::I32, ValType::I32],
        [],
    )];
    for (linear_id, name, params, returns) in functions {
        ty_section.function(params.iter().cloned(), returns.iter().cloned());
        imports.import("brick-runtime", name, EntityType::Function(*type_index));
        indices.insert(linear_id, *type_index);
        *type_index += 1;
    }

    indices
}
