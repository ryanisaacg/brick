use std::collections::HashMap;

use brick::{id::TypeID, DeclaredTypeLayout, LinearFunction, PhysicalPrimitive, PhysicalType};
use wasm_encoder::{FunctionSection, TypeSection, ValType};

use crate::function_bodies::walk_vals_read_order;

pub fn encode(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    fn_index: u32,
    func: &LinearFunction,
    ty_section: &mut TypeSection,
    fn_section: &mut FunctionSection,
) {
    let mut params = Vec::new();
    for param in func.params.iter() {
        walk_vals_read_order(declarations, param, 0, &mut |p, _| params.push(p));
    }
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
