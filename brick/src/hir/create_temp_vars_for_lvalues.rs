use crate::{id::VariableID, typecheck::ExpressionType, HirNodeValue};

use super::{HirModule, HirNode};

pub fn create_temp_vars_for_lvalues(module: &mut HirModule) {
    module.visit_mut(|node| {
        let Some(lvalue) = node.value.lvalue_mut() else {
            return;
        };
        // Nothing to do if this lvalue is already good
        if is_valid_lvalue(lvalue) {
            return;
        }
        let temp_var_id = VariableID::new();
        let mut temp_var_value = HirNode::autogenerated(
            HirNodeValue::VariableReference(temp_var_id.into()),
            lvalue.ty.clone(),
        );
        std::mem::swap(lvalue, &mut temp_var_value);
        let node_value = std::mem::take(&mut node.value);
        node.value = HirNodeValue::Sequence(vec![
            HirNode::autogenerated(
                HirNodeValue::Declaration(temp_var_id),
                temp_var_value.ty.clone(),
            ),
            HirNode::autogenerated(
                HirNodeValue::Assignment(
                    Box::new(HirNode::autogenerated(
                        HirNodeValue::VariableReference(temp_var_id.into()),
                        temp_var_value.ty.clone(),
                    )),
                    Box::new(temp_var_value),
                ),
                ExpressionType::Void,
            ),
            HirNode::autogenerated(node_value, node.ty.clone()),
        ])
    });
}

fn is_valid_lvalue(lvalue: &HirNode) -> bool {
    match &lvalue.value {
        HirNodeValue::VariableReference(_) => true,
        HirNodeValue::Access(lhs, _) => is_valid_lvalue(lhs),
        HirNodeValue::Dereference(lhs) => is_valid_lvalue(lhs),
        HirNodeValue::ArrayIndex(arr, _) => is_valid_lvalue(arr),
        HirNodeValue::DictIndex(dict, _) => is_valid_lvalue(dict),
        HirNodeValue::UnionVariant(union, _) => is_valid_lvalue(union),
        _ => false,
    }
}
