use std::collections::HashMap;

use crate::{id::TypeID, typecheck::ExpressionType, HirNodeValue, StaticDeclaration};

use super::HirModule;

pub fn convert_calls_to_union_literals(
    module: &mut HirModule,
    declarations: &HashMap<TypeID, &StaticDeclaration>,
) {
    module.visit_mut(|node| {
        let HirNodeValue::Call(func, _args) = &node.value else {
            return;
        };
        // Determine if the "function call" is actually a union variant
        let HirNodeValue::Access(lhs, _) = &func.value else {
            return;
        };
        let ExpressionType::ReferenceTo(ty_id) = &lhs.ty else {
            return;
        };
        if !matches!(declarations.get(ty_id), Some(StaticDeclaration::Union(_))) {
            return;
        }
        let ty_id = *ty_id;
        // Rewrite the node - it's a union variant
        let node_value = std::mem::take(&mut node.value);
        let HirNodeValue::Call(func, mut args) = node_value else {
            unreachable!()
        };
        let HirNodeValue::Access(_, variant) = func.value else {
            unreachable!()
        };
        node.value = HirNodeValue::UnionLiteral(ty_id, variant, Box::new(args.remove(0)));
    });
}
