use std::collections::HashMap;

use crate::{
    id::{AnyID, ConstantID},
    parser::{AstNode, AstNodeValue},
    typecheck::TypecheckedFile,
    DeclarationContext, HirNodeValue,
};

use super::{lower::lower_node, HirModule, HirNode};

pub fn extract_constant_values(
    ast: &TypecheckedFile,
    declarations: &DeclarationContext,
) -> HashMap<ConstantID, HirNode> {
    let mut map = HashMap::new();
    for statement in ast.top_level_statements.iter() {
        extract_constant_value(statement, declarations, &mut map);
    }
    for func in ast.functions.iter() {
        extract_constant_value(func.func.body, declarations, &mut map);
    }

    map
}

fn extract_constant_value<'a>(
    node: &'a AstNode<'a>,
    decls: &DeclarationContext,
    map: &mut HashMap<ConstantID, HirNode>,
) {
    if let AstNodeValue::ConstDeclaration {
        value, variable_id, ..
    } = &node.value
    {
        let value = lower_node(decls, value);
        map.insert(*variable_id, value);
    }
    node.children(|child| {
        extract_constant_value(child, decls, map);
    });
}

pub fn inline_constants(module: &mut HirModule, constant_values: HashMap<ConstantID, HirNode>) {
    module.par_visit_mut(|node| {
        let HirNodeValue::VariableReference(id) = &node.value else {
            return;
        };
        let AnyID::Constant(const_id) = id else {
            return;
        };
        *node = constant_values[const_id].clone();
    })
}
