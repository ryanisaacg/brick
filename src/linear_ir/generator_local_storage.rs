use std::collections::HashMap;

use crate::{
    id::{TypeID, VariableID},
    linear_ir::POINTER_SIZE,
};

use super::{DeclaredTypeLayout, LinearNode, LinearNodeValue, PhysicalPrimitive, PhysicalType};

pub fn generator_local_storage(
    generator_id: VariableID,
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    body: &mut [LinearNode],
) {
    let mut variable_offsets = HashMap::new();
    let mut generator_size = 0;
    for node in body.iter_mut() {
        node.visit_mut(|node| match &mut node.value {
            LinearNodeValue::VariableInit(var_id, ty) if *var_id != generator_id => {
                let size = ty.size(declarations);
                variable_offsets.insert(*var_id, generator_size);
                generator_size += size;
                node.value = LinearNodeValue::Sequence(Vec::new());
            }
            LinearNodeValue::VariableLocation(var_id) if *var_id != generator_id => {
                let offset = *variable_offsets.get(var_id).unwrap();
                *node = LinearNode::ptr_arithmetic(
                    crate::hir::ArithmeticOp::Add,
                    LinearNode::read_memory(
                        LinearNode::read_memory(
                            LinearNode::new(LinearNodeValue::VariableLocation(generator_id)),
                            0,
                            PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                        ),
                        POINTER_SIZE * 2,
                        PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                    ),
                    LinearNode::size(offset),
                );
            }
            LinearNodeValue::VariableDestroy(var_id) if *var_id != generator_id => {
                node.value = LinearNodeValue::Sequence(Vec::new());
            }
            _ => {}
        });
    }
}