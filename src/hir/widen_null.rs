use std::collections::HashMap;

use crate::{
    id::TypeID,
    typecheck::{ExpressionType, StaticDeclaration},
};

use super::{HirModule, HirNode, HirNodeValue};

pub fn widen_null(module: &mut HirModule, declarations: &HashMap<TypeID, &StaticDeclaration>) {
    module.visit_mut(|node| {
        node.walk_expected_types_for_children_mut(declarations, |ty, child| {
            if !matches!(&ty, ExpressionType::Nullable(_)) {
                return;
            }
            if matches!(&child.value, HirNodeValue::Null) {
                child.ty = ty.clone();
            } else if !matches!(&child.ty, ExpressionType::Nullable(_)) {
                let mut temp = HirNode::dummy();
                std::mem::swap(&mut temp, child);
                let mut widened =
                    HirNode::autogenerated(HirNodeValue::MakeNullable(Box::new(temp)), ty.clone());
                std::mem::swap(&mut widened, child);
            }
        });
    });
}
