use std::collections::{HashMap, HashSet};

use crate::{
    hir::{BinaryLogicalOp, HirNode, HirNodeValue},
    id::{NodeID, VariableID},
};

// Based on https://ics.uci.edu/~lopes/teaching/inf212W12/readings/rep-analysis-soft.pdf

use petgraph::stable_graph::{NodeIndex, StableGraph};

pub type ControlFlowGraph<'a> = StableGraph<CfgNode<'a>, CfgEdge>;

type IntermediateCFG<'a> = StableGraph<IntermediateNode<'a>, CfgEdge>;

#[derive(Debug)]
pub struct FunctionCFG<'a> {
    pub cfg: ControlFlowGraph<'a>,
    pub start: NodeIndex,
    pub end: NodeIndex,
}

pub fn build_control_flow_graph(body: &HirNode) -> FunctionCFG {
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
        cfg.add_node(CfgNode::Block {
            expressions: Vec::new(),
            liveness: HashMap::new(),
        }),
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
        leaders.insert(
            end,
            cfg.add_node(CfgNode::Block {
                expressions: Vec::new(),
                liveness: HashMap::new(),
            }),
        );
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
            leaders.insert(
                endpoint,
                cfg.add_node(CfgNode::Block {
                    expressions: Vec::new(),
                    liveness: HashMap::new(),
                }),
            );
        }
    }
    let cfg_end = cfg.add_node(CfgNode::Exit {
        liveness: HashMap::new(),
    });
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

    FunctionCFG {
        cfg,
        start: leaders[&intermediate_entrance],
        end: cfg_end,
    }
}

fn collect_blocks_to_add<'a>(
    cfg: &mut ControlFlowGraph<'a>,
    intermediate_cfg: &mut IntermediateCFG<'a>,
    leaders: &HashMap<NodeIndex, NodeIndex>,
    leader: NodeIndex,
    cfg_end: NodeIndex,
    current: NodeIndex,
) {
    if let Some(IntermediateNode::Expression(expr)) = intermediate_cfg.node_weight(current) {
        let Some(CfgNode::Block {
            expressions: block, ..
        }) = cfg.node_weight_mut(leader)
        else {
            panic!("expected block");
        };
        block.push(expr);
    }
    let mut edges = intermediate_cfg.neighbors(current).detach();
    while let Some((edge, neighbor)) = edges.next(intermediate_cfg) {
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
    Block {
        expressions: Vec<&'a HirNode>,
        liveness: HashMap<VariableID, Liveness>,
    },
    Exit {
        liveness: HashMap<VariableID, Liveness>,
    },
}

impl CfgNode<'_> {
    pub fn liveness(&self) -> &HashMap<VariableID, Liveness> {
        match self {
            CfgNode::Block { liveness, .. } => liveness,
            CfgNode::Exit { liveness } => liveness,
        }
    }

    pub fn liveness_mut(&mut self) -> &mut HashMap<VariableID, Liveness> {
        match self {
            CfgNode::Block { liveness, .. } => liveness,
            CfgNode::Exit { liveness } => liveness,
        }
    }
}

#[derive(Debug)]
pub enum Liveness {
    Moved(NodeID),
    MovedInParents(HashSet<NodeIndex>),

    Referenced(NodeID),
    ParentReferenced,
}

#[derive(Debug)]
pub enum IntermediateNode<'a> {
    Expression(&'a HirNode),
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
    current: &'a HirNode,
    graph: &mut IntermediateCFG<'a>,
    function_exit: NodeIndex,
) -> (NodeIndex, NodeIndex) {
    use HirNodeValue::*;

    match &current.value {
        VariableReference(_)
        | Int(_)
        | Float(_)
        | Bool(_)
        | Null
        | CharLiteral(_)
        | StringLiteral(_)
        | Declaration(_)
        | UnaryLogical(_, _)
        | Arithmetic(_, _, _)
        | Comparison(_, _, _)
        | Dereference(_)
        | Call(_, _)
        | VtableCall(_, _, _)
        | RuntimeCall(_, _)
        | TakeUnique(_)
        | TakeShared(_)
        | StructLiteral { .. }
        | ArrayLiteral(_)
        | ArrayLiteralLength(_, _)
        | Assignment(_, _)
        | StructToInterface { .. }
        | NumericCast { .. }
        | Parameter(_, _)
        | Access(_, _)
        | NullableTraverse(_, _)
        | ArrayIndex(_, _)
        | DictIndex(_, _)
        | DictLiteral(_)
        | MakeNullable(_)
        | UnionLiteral(_, _, _)
        | InterfaceAddress(_) => {
            let start = graph.add_node(IntermediateNode::Expression(current));

            (start, start)
        }
        Sequence(children) => {
            let start = graph.add_node(IntermediateNode::Empty);
            let mut current_node = start;

            for child in children.iter() {
                let (start_child, end_child) = create_graph_for_node(child, graph, function_exit);
                graph.add_edge(current_node, start_child, CfgEdge::Flow);
                current_node = end_child;
            }

            (start, current_node)
        }
        Return(_) => {
            let node = graph.add_node(IntermediateNode::Expression(current));
            graph.add_edge(node, function_exit, CfgEdge::Goto);

            // TODO: what should the exit be?
            (node, node)
        }
        BinaryLogical(BinaryLogicalOp::BooleanAnd, lhs, rhs) => {
            let (start_lhs, end_lhs) = create_graph_for_node(lhs, graph, function_exit);
            let (start_rhs, end_rhs) = create_graph_for_node(rhs, graph, function_exit);

            let virtual_next = graph.add_node(IntermediateNode::Empty);
            graph.add_edge(end_lhs, virtual_next, CfgEdge::Else);
            graph.add_edge(end_lhs, start_rhs, CfgEdge::If);
            graph.add_edge(end_rhs, virtual_next, CfgEdge::Flow);

            (start_lhs, virtual_next)
        }
        BinaryLogical(BinaryLogicalOp::BooleanOr, lhs, rhs) | NullCoalesce(lhs, rhs) => {
            let (start_lhs, end_lhs) = create_graph_for_node(lhs, graph, function_exit);
            let (start_rhs, end_rhs) = create_graph_for_node(rhs, graph, function_exit);

            let virtual_next = graph.add_node(IntermediateNode::Empty);
            graph.add_edge(end_lhs, virtual_next, CfgEdge::If);
            graph.add_edge(end_lhs, start_rhs, CfgEdge::Else);
            graph.add_edge(end_rhs, virtual_next, CfgEdge::Flow);

            (start_lhs, virtual_next)
        }
        If(condition, if_branch, else_branch) => {
            let (start_condition, end_condition) =
                create_graph_for_node(condition, graph, function_exit);
            let (start_body, end_body) = create_graph_for_node(if_branch, graph, function_exit);
            graph.add_edge(end_condition, start_body, CfgEdge::If);

            let virtual_next = graph.add_node(IntermediateNode::Empty);
            if let Some(else_branch) = else_branch {
                let (start_else, end_else) =
                    create_graph_for_node(else_branch, graph, function_exit);
                graph.add_edge(end_condition, start_else, CfgEdge::Else);
                graph.add_edge(end_else, virtual_next, CfgEdge::Flow);
            } else {
                graph.add_edge(end_condition, virtual_next, CfgEdge::Else);
                graph.add_edge(end_body, virtual_next, CfgEdge::Flow);
            }

            (start_condition, virtual_next)
        }
        While(condition, body) => {
            let (start_condition, end_condition) =
                create_graph_for_node(condition, graph, function_exit);
            let (start_body, end_body) = create_graph_for_node(body, graph, function_exit);
            graph.add_edge(end_condition, start_body, CfgEdge::If);

            let virtual_next = graph.add_node(IntermediateNode::Empty);
            graph.add_edge(end_condition, virtual_next, CfgEdge::Else);
            graph.add_edge(end_body, start_condition, CfgEdge::Flow);

            (start_condition, virtual_next)
        }
    }
}
