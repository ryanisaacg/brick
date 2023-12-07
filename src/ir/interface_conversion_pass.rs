use std::collections::HashMap;

use crate::{
    id::ID,
    typecheck::{ExpressionType, StaticDeclaration},
};

use super::{IrModule, IrNode, IrNodeValue};

pub fn rewrite<'ast>(module: &mut IrModule, declarations: &HashMap<ID, &'ast StaticDeclaration>) {
    // Find all places where a parent expects a child to be a struct
    let mut struct_expectations = HashMap::new();
    module.visit(|parent, child| {
        let Some(parent) = parent else { return };
        let ExpressionType::DeclaredType(parent_ty_id) = &parent.ty else {
            return;
        };
        let ExpressionType::DeclaredType(child_ty_id) = &child.ty else {
            return;
        };
        let Some(StaticDeclaration::Interface(parent_ty)) = declarations.get(parent_ty_id) else {
            return;
        };
        let Some(StaticDeclaration::Struct(_)) = declarations.get(child_ty_id) else {
            return;
        };
        struct_expectations.insert(child.id, parent_ty);
    });
    module.visit_mut(|node| {
        let Some(expected_ty) = struct_expectations.get(&node.id) else {
            return;
        };
        let mut temp = IrNode::dummy();
        std::mem::swap(&mut temp, node);
        *node = IrNode {
            id: ID::new(),
            value: IrNodeValue::StructToInterface(Box::new(temp)),
            ty: ExpressionType::DeclaredType(expected_ty.id),
        };
    });
}
