use brick::{LinearFunction, PhysicalPrimitive, PhysicalType};
use wasm_encoder::{FunctionSection, TypeSection, ValType};

pub fn encode(
    fn_index: u32,
    func: &LinearFunction,
    ty_section: &mut TypeSection,
    fn_section: &mut FunctionSection,
) {
    let params = vec![];
    let results = match func.returns {
        None => vec![],
        Some(PhysicalType::Primitive(prim)) => vec![match prim {
            PhysicalPrimitive::Byte | PhysicalPrimitive::Int32 | PhysicalPrimitive::PointerSize => {
                ValType::I32
            }
            PhysicalPrimitive::Float32 => ValType::F32,
            PhysicalPrimitive::Int64 => ValType::I64,
            PhysicalPrimitive::Float64 => ValType::F64,
        }],
        // TODO
        _ => vec![],
    };
    ty_section.function(params, results);
    fn_section.function(fn_index);
}
