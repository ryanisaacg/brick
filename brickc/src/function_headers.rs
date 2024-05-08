use brick::{LinearFunction, PhysicalPrimitive, PhysicalType};
use wasm_encoder::{FunctionSection, TypeSection, ValType};

pub fn encode(
    fn_index: u32,
    func: &LinearFunction,
    ty_section: &mut TypeSection,
    fn_section: &mut FunctionSection,
) {
    let params = func.params.iter().map(|p| match p {
        PhysicalType::Primitive(p) => prim_to_val(*p),
        // TODO
        _ => ValType::I32,
    });
    let results = match func.returns {
        None => vec![],
        Some(PhysicalType::Primitive(prim)) => vec![prim_to_val(prim)],
        // TODO
        _ => vec![],
    };
    ty_section.function(params, results);
    fn_section.function(fn_index);
}

fn prim_to_val(prim: PhysicalPrimitive) -> ValType {
    match prim {
        PhysicalPrimitive::Byte | PhysicalPrimitive::Int32 | PhysicalPrimitive::PointerSize => {
            ValType::I32
        }
        PhysicalPrimitive::Float32 => ValType::F32,
        PhysicalPrimitive::Int64 => ValType::I64,
        PhysicalPrimitive::Float64 => ValType::F64,
    }
}
