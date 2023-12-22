use std::collections::{HashMap, HashSet};

use petgraph::{stable_graph::NodeIndex, Direction};
use thiserror::Error;

use crate::hir::{HirModule, HirNode, HirNodeValue};

mod control_flow_graph;

use control_flow_graph::{CfgNode, ControlFlowGraph, FunctionCFG, Move};

use self::control_flow_graph::build_control_flow_graph;

#[derive(Debug, Error)]
pub enum BorrowError {
    // TODO: where
    #[error("use after move")]
    UseAfterMove,
}

pub fn borrow_check(module: &mut HirModule) -> Vec<BorrowError> {
    // TODO: borrow chekck top level statements
    let mut errors = Vec::new();
    for func in module.functions.iter() {
        let mut cfg = build_control_flow_graph(&func.body);
        dbg!(&cfg);
        detect_moves(&mut cfg, &mut errors);
    }

    errors
}

fn detect_moves(cfg: &mut FunctionCFG, errors: &mut Vec<BorrowError>) {
    detect_moves_node(&mut cfg.cfg, cfg.end, errors);
}

fn detect_moves_node(cfg: &mut ControlFlowGraph, node: NodeIndex, errors: &mut Vec<BorrowError>) {
    if let CfgNode::Block { moves, .. } = cfg.node_weight(node).unwrap() {
        // Moves already detected here?
        if !moves.is_empty() {
            return;
        }
    }

    let mut moves = HashMap::new();
    let mut incoming_connections = cfg.neighbors_directed(node, Direction::Incoming).detach();
    let mut edge_count = 0;
    while let Some((_edge, connector)) = incoming_connections.next(&cfg) {
        detect_moves_node(cfg, connector, errors);
        let Some(CfgNode::Block {
            moves: incoming_moves,
            ..
        }) = cfg.node_weight(connector)
        else {
            unreachable!()
        };
        for var_id in incoming_moves.keys() {
            match moves.get_mut(var_id) {
                Some(Move::MaybeMoved(count)) => {
                    *count += 1;
                }
                Some(Move::Moved) => unreachable!(),
                None => {
                    moves.insert(*var_id, Move::MaybeMoved(1));
                }
            }
        }
        edge_count += 1;
    }

    for mv in moves.values_mut() {
        match mv {
            Move::MaybeMoved(count) if *count == edge_count => {
                *mv = Move::Moved;
            }
            Move::MaybeMoved(_) => {}
            Move::Moved => unreachable!(),
        }
    }

    let CfgNode::Block { expressions, moves } = cfg.node_weight_mut(node).unwrap() else {
        return;
    };
    for expr in expressions.iter() {
        // TODO: don't repeatedly visit the same node over and over
        expr.children(|child| match &child.value {
            HirNodeValue::VariableReference(var_id) => {
                dbg!(expr);
                if moves.contains_key(var_id) {
                    errors.push(BorrowError::UseAfterMove);
                }
                if !matches!(
                    &expr.value,
                    HirNodeValue::TakeUnique(_) | HirNodeValue::TakeShared(_),
                ) {
                    moves.insert(*var_id, Move::Moved);
                }
                // TODO: borrow rules
            }
            HirNodeValue::Dereference(_) => todo!(),
            HirNodeValue::Access(_, _) => todo!(),
            HirNodeValue::ArrayIndex(_, _) => todo!(),
            HirNodeValue::Parameter(_, _)
            | HirNodeValue::Declaration(_)
            | HirNodeValue::Call(_, _)
            | HirNodeValue::Assignment(_, _)
            | HirNodeValue::BinOp(_, _, _)
            | HirNodeValue::Return(_)
            | HirNodeValue::Int(_)
            | HirNodeValue::Float(_)
            | HirNodeValue::Bool(_)
            | HirNodeValue::Null
            | HirNodeValue::CharLiteral(_)
            | HirNodeValue::StringLiteral(_)
            | HirNodeValue::NumericCast { .. }
            | HirNodeValue::TakeUnique(_)
            | HirNodeValue::TakeShared(_)
            | HirNodeValue::Sequence(_)
            | HirNodeValue::If(_, _, _)
            | HirNodeValue::While(_, _)
            | HirNodeValue::StructLiteral(_, _)
            | HirNodeValue::ArrayLiteral(_)
            | HirNodeValue::ArrayLiteralLength(_, _)
            | HirNodeValue::VtableCall(_, _, _)
            | HirNodeValue::InterfaceAddress(_)
            | HirNodeValue::StructToInterface { .. } => {}
        });
    }
}
