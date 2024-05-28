use std::collections::HashMap;

use brick::{DeclaredTypeLayout, LinearFunction, TypeID};
use wasm_encoder::{FunctionSection, TypeSection};

use crate::function_bodies::{walk_vals_read_order, walk_vals_write_order};

pub fn encode(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    fn_index: u32,
    func: &LinearFunction,
    ty_section: &mut TypeSection,
    fn_section: &mut FunctionSection,
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
    fn_section.function(fn_index);
}
