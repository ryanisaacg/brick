use std::collections::HashMap;

use brick::{
    expr_ty_to_physical, DeclaredTypeLayout, ExpressionType, FuncType, LinearFunction, TypeID,
};
use wasm_encoder::TypeSection;

use crate::function_bodies::{walk_vals_read_order, walk_vals_write_order};

pub fn encode_func_ty(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    func: &FuncType,
    ty_section: &mut TypeSection,
) {
    let mut params = Vec::new();
    for param in func.params.iter() {
        let param = expr_ty_to_physical(param);
        walk_vals_read_order(declarations, &param, 0, &mut |p, _| params.push(p));
    }
    let mut results = Vec::new();
    if !matches!(
        &func.returns,
        ExpressionType::Void | ExpressionType::Unreachable,
    ) {
        let return_ty = expr_ty_to_physical(&func.returns);
        walk_vals_write_order(declarations, &return_ty, 0, &mut |p, _| results.push(p));
    }
    ty_section.function(params, results);
}

pub fn encode_linear(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    func: &LinearFunction,
    ty_section: &mut TypeSection,
) {
    let mut params = Vec::new();
    for param in func.params.iter() {
        walk_vals_read_order(declarations, param, 0, &mut |p, _| params.push(p));
    }
    let mut results = Vec::new();
    if let Some(return_ty) = &func.returns {
        walk_vals_write_order(declarations, return_ty, 0, &mut |p, _| results.push(p));
    }
    ty_section.function(params, results);
}
