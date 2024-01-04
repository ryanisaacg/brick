use std::collections::HashMap;

use crate::{id::TypeID, typecheck::StaticDeclaration};

use super::{HirModule, HirNodeValue};

pub fn widen_null(module: &mut HirModule, declarations: &HashMap<TypeID, &StaticDeclaration>) {
    module.visit_mut(|node| {
        node.walk_expected_types_for_children_mut(declarations, |ty, child| {
            if !matches!(&child.value, HirNodeValue::Null) {
                return;
            }
            child.ty = ty.clone();
        });
    });
}
