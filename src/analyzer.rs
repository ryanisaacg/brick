use crate::{
    id::IDMap,
    parser::{AstNode, ParsedSourceFile},
};

use self::{
    control_flow_graph::build_control_flow_graph,
    resolve::{resolve_module, ResolvedCallable, ResolvedFunction},
};

mod control_flow_graph;
mod resolve;

pub fn typecheck(file: ParsedSourceFile) {
    let module = resolve_module(file);

    for function in module.functions.values() {
        match function {
            ResolvedCallable::Function(func) => typecheck_function(func, &module.nodes),
            ResolvedCallable::Extern(_ex) => {}
        }
    }
}

fn typecheck_function(function: &ResolvedFunction, ast_nodes: &IDMap<AstNode>) {
    let _cfg = build_control_flow_graph(function, ast_nodes);
}
