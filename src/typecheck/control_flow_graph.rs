use std::collections::HashMap;

use crate::{
    arena::ArenaNode,
    parser::{AstNode, AstNodeValue, IfDeclaration},
};

// Based on https://ics.uci.edu/~lopes/teaching/inf212W12/readings/rep-analysis-soft.pdf

use petgraph::stable_graph::{NodeIndex, StableGraph};

pub type ControlFlowGraph<'a> = StableGraph<CfgNode<'a>, CfgEdge>;

type IntermediateCFG<'a> = StableGraph<IntermediateNode<'a>, CfgEdge>;

pub fn build_control_flow_graph<'a>(body: &'a AstNode<'a>) -> ControlFlowGraph<'a> {
    let mut intermediate_cfg = StableGraph::new();
    let intermediate_entrance = intermediate_cfg.add_node(IntermediateNode::Entrance);
    let exit = intermediate_cfg.add_node(IntermediateNode::Exit);

    let (start_body, end_body) = create_graph_for_node(body, &mut intermediate_cfg, exit);
    intermediate_cfg.add_edge(intermediate_entrance, start_body, CfgEdge::Flow);
    intermediate_cfg.add_edge(end_body, exit, CfgEdge::Flow);

    let mut cfg = StableGraph::new();

    let mut leaders = HashMap::new();
    leaders.insert(
        intermediate_entrance,
        cfg.add_node(CfgNode::Block(Vec::new())),
    );
    // Calculate leaders
    for edge_idx in intermediate_cfg.edge_indices() {
        if !intermediate_cfg
            .edge_weight(edge_idx)
            .expect("edge to exist")
            .is_goto()
        {
            continue;
        }

        let (start, end) = intermediate_cfg
            .edge_endpoints(edge_idx)
            .expect("endpoints to exist");
        // Nodes pointed to by gotos are leaders
        leaders.insert(end, cfg.add_node(CfgNode::Block(Vec::new())));
        // All nodes following a goto are also leaders
        let mut neighbors = intermediate_cfg.neighbors(start).detach();
        while let Some((edge, endpoint)) = neighbors.next(&intermediate_cfg) {
            if intermediate_cfg
                .edge_weight(edge)
                .expect("edge to exist")
                .is_goto()
            {
                continue;
            }
            leaders.insert(endpoint, cfg.add_node(CfgNode::Block(Vec::new())));
        }
    }
    let cfg_end = cfg.add_node(CfgNode::Exit);
    // Create blocks
    for (node_idx, leader_idx) in leaders.iter() {
        collect_blocks_to_add(
            &mut cfg,
            &mut intermediate_cfg,
            &leaders,
            *leader_idx,
            cfg_end,
            *node_idx,
        );
    }

    cfg
}

fn collect_blocks_to_add<'a>(
    cfg: &mut ControlFlowGraph<'a>,
    intermediate_cfg: &mut IntermediateCFG<'a>,
    leaders: &HashMap<NodeIndex, NodeIndex>,
    leader: NodeIndex,
    cfg_end: NodeIndex,
    current: NodeIndex,
) {
    match intermediate_cfg.node_weight(current) {
        Some(IntermediateNode::Expression(expr)) => {
            let Some(CfgNode::Block(block)) = cfg.node_weight_mut(leader) else {
                panic!("expected block");
            };
            block.push(expr);
        }
        _ => {}
    }
    let mut edges = intermediate_cfg.neighbors(current).detach();
    while let Some((edge, neighbor)) = edges.next(&intermediate_cfg) {
        let edge = intermediate_cfg.edge_weight(edge).expect("edge exists");
        if let Some(IntermediateNode::Exit) = intermediate_cfg.node_weight(neighbor) {
            cfg.add_edge(leader, cfg_end, *edge);
        } else if let Some(leader_neighbor) = leaders.get(&neighbor) {
            cfg.add_edge(leader, *leader_neighbor, *edge);
        } else {
            collect_blocks_to_add(cfg, intermediate_cfg, leaders, leader, cfg_end, neighbor);
        }
    }
}

#[derive(Debug)]
pub enum CfgNode<'a> {
    Block(Vec<&'a AstNode<'a>>),
    Exit,
}

#[derive(Debug)]
pub enum IntermediateNode<'a> {
    Expression(&'a AstNode<'a>),
    Empty,
    Entrance,
    Exit,
}

#[derive(Copy, Clone, Debug)]
pub enum CfgEdge {
    Flow,
    Goto,
    If,
    Else,
}

impl CfgEdge {
    fn is_goto(&self) -> bool {
        match self {
            CfgEdge::Goto | CfgEdge::If | CfgEdge::Else => true,
            CfgEdge::Flow => false,
        }
    }
}

fn create_graph_for_node<'a>(
    current: &'a AstNode<'a>,
    graph: &mut IntermediateCFG<'a>,
    exit: NodeIndex,
) -> (NodeIndex, NodeIndex) {
    use AstNodeValue::*;

    match &current.value {
        FunctionDeclaration(_)
        | ExternFunctionBinding(_)
        | StructDeclaration(_)
        | Import(_)
        | UnionDeclaration(_) => {
            panic!("TODO: handle class of top level declaration inside statement?")
        }
        UniqueType(_) | SharedType(_) | ArrayType(_) | NullableType(_) => {
            panic!("Can't handle type nodes inside statement")
        }
        Return(expr) => {
            let node = graph.add_node(IntermediateNode::Expression(current));
            let (start_inner, end_inner) = create_graph_for_node(expr, graph, exit);
            graph.add_edge(node, start_inner, CfgEdge::Flow);
            graph.add_edge(end_inner, exit, CfgEdge::Goto);

            // TODO: what should the exit be?
            (start_inner, end_inner)
        }
        Name(_) | Int(_) | Float(_) | Bool(_) | Null | CharLiteral(_) | StringLiteral(_) => {
            let node = graph.add_node(IntermediateNode::Expression(current));
            (node, node)
        }
        BinExpr(_, _, _)
        | Statement(_)
        | Call(_, _)
        | TakeUnique(_)
        | TakeShared(_)
        | StructLiteral { .. }
        | ArrayLiteral(_)
        | ArrayLiteralLength(_, _)
        | Declaration(_, _)
        | Block(_)
        | DictLiteral(_) => {
            let start = graph.add_node(IntermediateNode::Expression(current));
            let mut current_node = start;
            let mut children = Vec::new();
            current.write_children(&mut children);

            for child in children {
                let (start_child, end_child) = create_graph_for_node(child, graph, exit);
                graph.add_edge(current_node, start_child, CfgEdge::Flow);
                current_node = end_child;
            }

            (start, current_node)
        }

        If(IfDeclaration {
            condition,
            if_branch,
            ..
        }) => {
            // TODO: else branch
            let (start_condition, end_condition) = create_graph_for_node(condition, graph, exit);
            let (start_body, end_body) = create_graph_for_node(if_branch, graph, exit);
            graph.add_edge(end_condition, start_body, CfgEdge::If);

            let virtual_next = graph.add_node(IntermediateNode::Empty);
            graph.add_edge(end_condition, virtual_next, CfgEdge::Else);
            graph.add_edge(end_body, virtual_next, CfgEdge::Flow);

            (start_condition, virtual_next)
        }
        While(condition, body) => {
            let (start_condition, end_condition) = create_graph_for_node(condition, graph, exit);
            let (start_body, end_body) = create_graph_for_node(body, graph, exit);
            graph.add_edge(end_condition, start_body, CfgEdge::If);

            let virtual_next = graph.add_node(IntermediateNode::Empty);
            graph.add_edge(end_condition, virtual_next, CfgEdge::Else);
            graph.add_edge(end_body, start_condition, CfgEdge::Flow);

            (start_condition, virtual_next)
        }
    }
}
