use crate::{
    arena::ArenaNode,
    parser::{AstNode, AstNodeValue},
};

use petgraph::stable_graph::{NodeIndex, StableGraph};

pub type ControlFlowGraph<'a> = StableGraph<CfgNode<'a>, CfgEdge>;

pub fn build_control_flow_graph<'a>(body: &'a AstNode<'a>) -> ControlFlowGraph<'a> {
    let mut cfg = StableGraph::new();
    let entrance = cfg.add_node(CfgNode::Entrance);
    let exit = cfg.add_node(CfgNode::Exit);

    let (start_body, end_body) = create_graph_for_node(body, &mut cfg, exit);
    cfg.add_edge(entrance, start_body, CfgEdge::Flow);
    cfg.add_edge(end_body, exit, CfgEdge::Flow);

    let mut edges_to_remove = Vec::new();

    for node in cfg.node_indices() {
        let does_goto = cfg
            .edges(node)
            .any(|edge| matches!(edge.weight(), CfgEdge::Goto));
        if does_goto {
            // TODO: will this break because of mutating the edges while iterating
            let mut edges = cfg.neighbors(node).detach();
            while let Some((edge, _)) = edges.next(&cfg) {
                if !matches!(cfg.edge_weight(edge), Some(CfgEdge::Goto)) {
                    edges_to_remove.push(edge);
                }
            }
        }
    }
    for edge in edges_to_remove {
        cfg.remove_edge(edge);
    }

    // TODO: Promote to leaders
    // TODO: Collect basics into leaders

    cfg
}

#[derive(Debug)]
pub enum CfgNode<'a> {
    //LeaderBlock(Vec<&'a AstNode<'a>>),
    BasicBlock(&'a AstNode<'a>),
    EmptyBasicBlock,
    Entrance,
    Exit,
}

#[derive(Debug)]
pub enum CfgEdge {
    Flow,
    Goto,
    If,
    Else,
}

fn create_graph_for_node<'a>(
    current: &'a AstNode<'a>,
    graph: &mut ControlFlowGraph<'a>,
    exit: NodeIndex,
) -> (NodeIndex, NodeIndex) {
    use AstNodeValue::*;

    match &current.value {
        FunctionDeclaration(_) | ExternFunctionBinding(_) | StructDeclaration(_) | Import(_) => {
            panic!("TODO: handle class of top level declaration inside statement?")
        }
        UniqueType(_) | SharedType(_) | ArrayType(_) => {
            panic!("Can't handle type nodes inside statement")
        }
        Declaration(_, _) => todo!(),
        Return(expr) => {
            let node = graph.add_node(CfgNode::BasicBlock(current));
            let (start_inner, end_inner) = create_graph_for_node(expr, graph, exit);
            graph.add_edge(node, start_inner, CfgEdge::Flow);
            graph.add_edge(end_inner, exit, CfgEdge::Goto);

            // TODO: what should the exit be?
            (start_inner, end_inner)
        }
        Name(_) | Int(_) | Float(_) | Bool(_) => {
            let node = graph.add_node(CfgNode::BasicBlock(current));
            (node, node)
        }
        BinExpr(_, _, _)
        | Call(_, _)
        | TakeUnique(_)
        | TakeShared(_)
        | StructLiteral { .. }
        | ArrayLiteral(_)
        | ArrayLiteralLength(_, _)
        | Block(_) => {
            let start = graph.add_node(CfgNode::BasicBlock(current));
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

        If(condition, body) => {
            let (start_condition, end_condition) = create_graph_for_node(condition, graph, exit);
            let (start_body, end_body) = create_graph_for_node(body, graph, exit);
            graph.add_edge(end_condition, start_body, CfgEdge::If);

            let virtual_next = graph.add_node(CfgNode::EmptyBasicBlock);
            graph.add_edge(end_condition, virtual_next, CfgEdge::Else);
            graph.add_edge(end_body, virtual_next, CfgEdge::Flow);

            (start_condition, virtual_next)
        }
        While(condition, body) => {
            let (start_condition, end_condition) = create_graph_for_node(condition, graph, exit);
            let (start_body, end_body) = create_graph_for_node(body, graph, exit);
            graph.add_edge(end_condition, start_body, CfgEdge::If);

            let virtual_next = graph.add_node(CfgNode::EmptyBasicBlock);
            graph.add_edge(end_condition, virtual_next, CfgEdge::Else);
            graph.add_edge(end_body, start_condition, CfgEdge::Flow);

            (start_condition, virtual_next)
        }
    }
}
