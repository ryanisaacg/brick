use std::collections::HashMap;

use crate::id::VariableID;

use super::{LinearContext, LinearNode, LinearNodeValue, PhysicalPrimitive, PhysicalType};

pub fn generator_local_storage(
    ctx: &LinearContext,
    generator_id: VariableID,
    param_var_id: Option<VariableID>,
    body: &mut [LinearNode],
) {
    let mut variable_offsets = HashMap::new();
    let mut generator_size = 0;
    for node in body.iter_mut() {
        node.visit_mut(|node| match &mut node.value {
            LinearNodeValue::VariableInit(var_id, ty)
                if !is_special(generator_id, param_var_id, *var_id) =>
            {
                let size = ty.size(ctx);
                variable_offsets.insert(*var_id, generator_size);
                generator_size += size;
                node.value = LinearNodeValue::Sequence(Vec::new());
            }
            LinearNodeValue::VariableLocation(var_id)
                if !is_special(generator_id, param_var_id, *var_id) =>
            {
                let offset = *variable_offsets.get(var_id).unwrap();
                *node = LinearNode::ptr_arithmetic(
                    crate::hir::ArithmeticOp::Add,
                    LinearNode::read_memory(
                        LinearNode::read_memory(
                            LinearNode::new(LinearNodeValue::VariableLocation(generator_id)),
                            0,
                            PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                        ),
                        ctx.pointer_size * 2,
                        PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
                    ),
                    LinearNode::size(offset),
                );
            }
            LinearNodeValue::VariableDestroy(var_id)
                if !is_special(generator_id, param_var_id, *var_id) =>
            {
                node.value = LinearNodeValue::Sequence(Vec::new());
            }
            _ => {}
        });
    }
}

fn is_special(
    generator_id: VariableID,
    param_var_id: Option<VariableID>,
    var_id: VariableID,
) -> bool {
    generator_id == var_id || param_var_id.map_or(false, |param_var_id| param_var_id == var_id)
}
