use crate::{
    arena::ArenaNode,
    id::{IDMap, ID},
    parser::{AstNode, AstNodeValue},
};

use super::resolve::ResolvedFunction;

pub fn build_control_flow_graph(
    function: &ResolvedFunction,
    ast_nodes: &IDMap<AstNode>,
) -> ControlFlowGraph {
    let mut cfg_nodes = IDMap::new();
    let end_node_id = ID::new();
    // TODO: enum member for end node?
    cfg_nodes.insert(end_node_id, ControlFlowNode::default());
    let root = create_graph_for_node(function.body, ast_nodes, &mut cfg_nodes, end_node_id);

    ControlFlowGraph {
        root,
        nodes: cfg_nodes,
    }
}

fn create_graph_for_node(
    root_ast: ID,
    ast_nodes: &IDMap<AstNode>,
    cfg_nodes: &mut IDMap<ControlFlowNode>,
    // Not guaranteed to be initialized!
    // TODO: should this be a stack? it probably should so that you can `break` and `return`
    end_node_id: ID,
) -> ID {
    let mut cfg_node = ControlFlowNode::default();
    let mut id = ID::new();

    let mut ast_node_children = Vec::new();
    ast_nodes[&root_ast].write_children(&mut ast_node_children);
    let node_children_len = ast_node_children.len();

    for (idx, child) in ast_node_children.into_iter().enumerate() {
        let is_last = idx == node_children_len - 1;
        match &ast_nodes[&child].value {
            AstNodeValue::Block(_) => {
                if is_last {
                    let child_cfg = create_graph_for_node(child, ast_nodes, cfg_nodes, end_node_id);
                    cfg_node.unconditional_branch(child_cfg);
                } else {
                    let new_node_id = ID::new();

                    let child_cfg = create_graph_for_node(child, ast_nodes, cfg_nodes, new_node_id);
                    cfg_node.unconditional_branch(child_cfg);
                    cfg_nodes.insert(id, cfg_node);

                    id = new_node_id;
                    cfg_node = ControlFlowNode::default();
                }
            }
            AstNodeValue::If(predicate, body) => {
                cfg_node.statements.push(*predicate);
                if is_last {
                    let child_cfg = create_graph_for_node(*body, ast_nodes, cfg_nodes, end_node_id);
                    cfg_node.if_branch(*predicate, child_cfg);
                    cfg_node.else_branch(*predicate, end_node_id);
                } else {
                    let new_node_id = ID::new();

                    let child_cfg = create_graph_for_node(*body, ast_nodes, cfg_nodes, new_node_id);
                    cfg_node.if_branch(*predicate, child_cfg);
                    cfg_node.else_branch(*predicate, new_node_id);
                    cfg_nodes.insert(id, cfg_node);

                    id = new_node_id;
                    cfg_node = ControlFlowNode::default();
                }
            }
            AstNodeValue::While(predicate, body) => {
                // Pass current node onto loop condition
                let predicate_cfg_id = ID::new();
                cfg_node.unconditional_branch(predicate_cfg_id);
                cfg_nodes.insert(id, cfg_node);
                cfg_node = ControlFlowNode::default();

                let child_cfg =
                    create_graph_for_node(*body, ast_nodes, cfg_nodes, predicate_cfg_id);

                // Loop points either to its own body or the next statement
                let mut predicate_cfg = ControlFlowNode::default();
                predicate_cfg.statements.push(*predicate);
                predicate_cfg.if_branch(*predicate, child_cfg);
                if is_last {
                    predicate_cfg.else_branch(*predicate, end_node_id);
                } else {
                    id = ID::new();
                    predicate_cfg.else_branch(*predicate, id);
                }

                cfg_nodes.insert(predicate_cfg_id, predicate_cfg);
            }
            _ => {
                cfg_node.statements.push(child);
            }
        }
    }

    cfg_nodes.insert(id, cfg_node);

    id
}

#[derive(Debug)]
pub struct ControlFlowGraph {
    pub root: ID,
    pub nodes: IDMap<ControlFlowNode>,
}

#[derive(Debug, Default)]
pub struct ControlFlowNode {
    statements: Vec<ID>,
    // TODO: track branch conditions legibly?
    branches: Vec<ControlFlowBranch>,
}

impl ControlFlowNode {
    fn unconditional_branch(&mut self, target_node: ID) {
        self.branches.push(ControlFlowBranch {
            target_node,
            branch_type: ControlFlowBranchType::Unconditional,
        });
    }

    fn if_branch(&mut self, predicate_node: ID, target_node: ID) {
        self.branches.push(ControlFlowBranch {
            target_node,
            branch_type: ControlFlowBranchType::If(predicate_node),
        });
    }

    fn else_branch(&mut self, predicate_node: ID, target_node: ID) {
        self.branches.push(ControlFlowBranch {
            target_node,
            branch_type: ControlFlowBranchType::Else(predicate_node),
        });
    }
}

#[derive(Debug)]
pub struct ControlFlowBranch {
    target_node: ID,
    branch_type: ControlFlowBranchType,
}

#[derive(Debug)]
pub enum ControlFlowBranchType {
    Unconditional,
    If(ID),
    Else(ID),
    // TODO: else (ID)
}
