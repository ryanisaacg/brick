use crate::{
    id::{IDMap, ID},
    parser::AstNode,
};

use super::resolve::ResolvedFunction;

pub fn build_control_flow_graph(
    function: &ResolvedFunction,
    ast_nodes: &IDMap<AstNode>,
) -> ControlFlowGraph {
    let mut cfg_nodes = IDMap::new();
    let root = create_graph_for_node(function.body, ast_nodes, &mut cfg_nodes);

    ControlFlowGraph {
        root,
        nodes: cfg_nodes,
    }
}

fn create_graph_for_node(
    root_ast: ID,
    ast_nodes: &IDMap<AstNode>,
    cfg_nodes: &mut IDMap<ControlFlowNode>,
) -> ID {
    let cfg_node = ControlFlowNode {
        statements: Vec::new(),
        branches: Vec::new(),
    };

    let id = ID::new();
    cfg_nodes.insert(id, cfg_node);

    id
}

pub struct ControlFlowGraph {
    pub root: ID,
    pub nodes: IDMap<ControlFlowNode>,
}

pub struct ControlFlowNode {
    statements: Vec<ID>,
    // TODO: track branch conditions legibly?
    branches: Vec<ID>,
}

pub struct ControlFlowBranch {
    target_node: ID,
    branch_type: ControlFlowBranchType,
}

pub enum ControlFlowBranchType {
    If(ID),
    // TODO: else (ID)
}
