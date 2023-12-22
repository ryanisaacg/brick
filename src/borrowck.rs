use std::collections::HashMap;

use thiserror::Error;

use crate::{hir::HirModule, id::ID, typecheck::StaticDeclaration};

mod control_flow_graph;
mod detect_moves;
mod drop_points;

use self::control_flow_graph::build_control_flow_graph;

#[derive(Debug, Error)]
pub enum BorrowError {
    // TODO: where
    #[error("use after move")]
    UseAfterMove,
}

pub fn borrow_check(
    module: &mut HirModule,
    declarations: &HashMap<ID, &StaticDeclaration>,
) -> Vec<BorrowError> {
    // TODO: borrow chekck top level statements
    let mut errors = Vec::new();
    for func in module.functions.iter() {
        let mut cfg = build_control_flow_graph(&func.body);
        detect_moves::detect_moves(&mut cfg, &mut errors, declarations);
        let _drop_points = drop_points::find_drop_points(&cfg);
    }

    errors
}
