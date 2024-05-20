use std::collections::HashMap;

use crate::{hir::HirNode, HirNodeValue};

// Based on https://ics.uci.edu/~lopes/teaching/inf212W12/readings/rep-analysis-soft.pdf

use petgraph::stable_graph::{NodeIndex, StableGraph};

use super::BlockLiveness;

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
    if let Some(end_body) = end_body {
        intermediate_cfg.add_edge(end_body, exit, CfgEdge::Flow);
    }

    let mut cfg = StableGraph::new();

    let mut leaders = HashMap::new();
    leaders.insert(
        intermediate_entrance,
        cfg.add_node(CfgNode::Block {
            expressions: Vec::new(),
            life_state: BlockLiveness::new(),
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
                life_state: BlockLiveness::new(),
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
                    life_state: BlockLiveness::new(),
                }),
            );
        }
    }
    let cfg_end = cfg.add_node(CfgNode::Exit {
        life_state: BlockLiveness::new(),
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
    if let Some(IntermediateNode::Node(expr)) = intermediate_cfg.node_weight(current) {
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
        life_state: BlockLiveness,
    },
    Exit {
        life_state: BlockLiveness,
    },
}

impl CfgNode<'_> {
    pub fn life_state(&self) -> &BlockLiveness {
        match self {
            CfgNode::Exit { life_state } | CfgNode::Block { life_state, .. } => life_state,
        }
    }

    pub fn life_state_mut(&mut self) -> &mut BlockLiveness {
        match self {
            CfgNode::Exit { life_state } | CfgNode::Block { life_state, .. } => life_state,
        }
    }
}

#[derive(Debug)]
pub enum IntermediateNode<'a> {
    Node(&'a HirNode),
    Empty,
    Entrance,
    Exit,
}

#[derive(Copy, Clone, Debug)]
pub enum CfgEdge {
    Flow,
    Goto,
    Loop,
    If,
    // TODO: is else required?
    Else,
}

impl CfgEdge {
    fn is_goto(&self) -> bool {
        match self {
            CfgEdge::Goto | CfgEdge::If | CfgEdge::Else | CfgEdge::Loop => true,
            CfgEdge::Flow => false,
        }
    }
}

fn create_graph_for_node<'a>(
    current: &'a HirNode,
    graph: &mut IntermediateCFG<'a>,
    function_exit: NodeIndex,
) -> (NodeIndex, Option<NodeIndex>) {
    use HirNodeValue::*;

    match &current.value {
        Sequence(children) => {
            let start = graph.add_node(IntermediateNode::Empty);
            let mut current_node = start;

            for child in children.iter() {
                let (start_child, end_child) = create_graph_for_node(child, graph, function_exit);
                graph.add_edge(current_node, start_child, CfgEdge::Flow);
                if let Some(end_child) = end_child {
                    current_node = end_child;
                } else {
                    // TODO: warn about dead code?
                    return (start, None);
                }
            }

            (start, Some(current_node))
        }
        Yield(_) | Return(_) => {
            let node = graph.add_node(IntermediateNode::Node(current));
            graph.add_edge(node, function_exit, CfgEdge::Goto);

            // TODO: what should the exit be?
            (node, None)
        }
        // TODO: short-circuiting boolean logic
        If(condition, if_branch, else_branch) => {
            let (start_condition, end_condition) =
                create_graph_for_node(condition, graph, function_exit);
            let Some(end_condition) = end_condition else {
                return (start_condition, None);
            };
            let (start_body, end_body) = create_graph_for_node(if_branch, graph, function_exit);
            graph.add_edge(end_condition, start_body, CfgEdge::If);

            let rejoin_node = graph.add_node(IntermediateNode::Empty);
            if let Some(end_body) = end_body {
                graph.add_edge(end_body, rejoin_node, CfgEdge::Flow);
            }

            let else_destination = if let Some(else_branch) = else_branch {
                let (start_else, end_else) =
                    create_graph_for_node(else_branch, graph, function_exit);

                if let Some(end_else) = end_else {
                    graph.add_edge(end_else, rejoin_node, CfgEdge::Flow);
                }

                start_else
            } else {
                rejoin_node
            };
            graph.add_edge(end_condition, else_destination, CfgEdge::Else);

            (start_condition, Some(rejoin_node))
        }
        Switch { value, cases } => {
            let (start_value, end_value) = create_graph_for_node(value, graph, function_exit);
            let Some(end_value) = end_value else {
                return (start_value, None);
            };

            let rejoin_node = graph.add_node(IntermediateNode::Empty);
            for case in cases.iter() {
                let (start_case, end_case) = create_graph_for_node(case, graph, function_exit);
                graph.add_edge(end_value, start_case, CfgEdge::If);
                if let Some(end_case) = end_case {
                    graph.add_edge(end_case, rejoin_node, CfgEdge::Flow);
                }
            }

            (start_value, Some(rejoin_node))
        }
        While(condition, body) => {
            let (start_condition, end_condition) =
                create_graph_for_node(condition, graph, function_exit);
            let Some(end_condition) = end_condition else {
                return (start_condition, None);
            };
            let (start_body, end_body) = create_graph_for_node(body, graph, function_exit);
            graph.add_edge(end_condition, start_body, CfgEdge::If);

            let after_loop = graph.add_node(IntermediateNode::Empty);
            graph.add_edge(end_condition, after_loop, CfgEdge::Else);
            if let Some(end_body) = end_body {
                graph.add_edge(end_body, start_condition, CfgEdge::Loop);
            }

            (start_condition, Some(after_loop))
        }
        Loop(body) => {
            let (start_child, end_child) = create_graph_for_node(body, graph, function_exit);
            if let Some(end_child) = end_child {
                graph.add_edge(end_child, start_child, CfgEdge::Loop);
                (start_child, Some(end_child))
            } else {
                (start_child, None)
            }
        }
        _ => {
            let node = graph.add_node(IntermediateNode::Node(current));

            (node, Some(node))
        }
    }
}
