use crate::parser::AstNode;

use self::resolve::resolve_module;

mod resolve;

pub fn typecheck(top_level_nodes: Vec<usize>, nodes: Vec<AstNode>) {
    let (_module, _errors) = resolve_module(top_level_nodes, nodes);
}
