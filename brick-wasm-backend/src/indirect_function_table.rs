use std::collections::HashMap;

use brick::{id::FunctionID, LinearFunction, LinearNodeValue};

pub fn encode(
    indirect_function_id_to_table: &mut HashMap<FunctionID, u32>,
    functions_for_table: &mut Vec<u32>,
    function_id_to_idx: &HashMap<FunctionID, u32>,
    func: &LinearFunction,
) {
    for node in func.body.iter() {
        node.visit(|node| {
            let LinearNodeValue::FunctionID(func_id) = &node.value else {
                return;
            };
            if indirect_function_id_to_table.contains_key(func_id) {
                return;
            }
            indirect_function_id_to_table.insert(*func_id, functions_for_table.len() as u32);
            functions_for_table.push(function_id_to_idx[func_id]);
        });
    }
}
