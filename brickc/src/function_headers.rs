use brick::LinearFunction;
use wasm_encoder::{FunctionSection, TypeSection, ValType};

pub fn encode(
    fn_index: u32,
    func: &LinearFunction,
    ty_section: &mut TypeSection,
    fn_section: &mut FunctionSection,
) {
    // TODO
    let params = vec![];
    let results = vec![ValType::I32];
    ty_section.function(params, results);
    fn_section.function(fn_index);
}
