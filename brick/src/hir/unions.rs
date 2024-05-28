use crate::{typecheck::ExpressionType, DeclarationContext, HirNodeValue, TypeDeclaration};

use super::HirModule;

pub fn convert_calls_to_union_literals(module: &mut HirModule, declarations: &DeclarationContext) {
    // Rewrite union variants with values
    module.visit_mut(|node| {
        let HirNodeValue::Call(func, _args) = &node.value else {
            return;
        };
        // Determine if the "function call" is actually a union variant
        let HirNodeValue::Access(lhs, _) = &func.value else {
            return;
        };
        let ExpressionType::ReferenceToType(ty_id) = &lhs.ty else {
            return;
        };
        if !matches!(
            declarations.id_to_decl.get(ty_id),
            Some(TypeDeclaration::Union(_))
        ) {
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
        node.value = HirNodeValue::UnionLiteral(ty_id, variant, Some(Box::new(args.remove(0))));
    });
    // Rewrite union variants without values
    module.visit_mut(|node| {
        // Determine if the "access" is actually a union variant
        let HirNodeValue::Access(lhs, _) = &node.value else {
            return;
        };
        let ExpressionType::ReferenceToType(ty_id) = &lhs.ty else {
            return;
        };
        if !matches!(
            declarations.id_to_decl.get(ty_id),
            Some(TypeDeclaration::Union(_))
        ) {
            return;
        }
        let ty_id = *ty_id;
        // Rewrite the node - it's a union variant
        let node_value = std::mem::take(&mut node.value);
        let HirNodeValue::Access(_, variant) = node_value else {
            unreachable!()
        };
        node.value = HirNodeValue::UnionLiteral(ty_id, variant, None);
    });
}
