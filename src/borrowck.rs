use std::collections::HashMap;

use petgraph::{stable_graph::NodeIndex, Direction};
use thiserror::Error;

use crate::{
    hir::{HirModule, HirNode, HirNodeValue},
    id::ID,
    typecheck::{ExpressionType, PointerKind, StaticDeclaration},
};

mod control_flow_graph;

use control_flow_graph::{CfgNode, ControlFlowGraph, FunctionCFG, Move};

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
        detect_moves(&mut cfg, &mut errors, declarations);
    }

    errors
}

fn detect_moves(
    cfg: &mut FunctionCFG,
    errors: &mut Vec<BorrowError>,
    declarations: &HashMap<ID, &StaticDeclaration>,
) {
    detect_moves_node(&mut cfg.cfg, cfg.end, errors, declarations);
}

fn detect_moves_node(
    cfg: &mut ControlFlowGraph,
    node: NodeIndex,
    errors: &mut Vec<BorrowError>,
    declarations: &HashMap<ID, &StaticDeclaration>,
) {
    if let CfgNode::Block { moves, .. } = cfg.node_weight(node).unwrap() {
        // Moves already detected here?
        if !moves.is_empty() {
            return;
        }
    }

    let mut parent_moves = HashMap::new();
    let mut incoming_connections = cfg.neighbors_directed(node, Direction::Incoming).detach();
    let mut edge_count = 0;
    while let Some((_edge, connector)) = incoming_connections.next(cfg) {
        detect_moves_node(cfg, connector, errors, declarations);
        let Some(CfgNode::Block {
            moves: incoming_moves,
            ..
        }) = cfg.node_weight(connector)
        else {
            unreachable!()
        };
        for var_id in incoming_moves.keys() {
            match parent_moves.get_mut(var_id) {
                Some(Move::MaybeMoved(count)) => {
                    *count += 1;
                }
                Some(Move::Moved) => unreachable!(),
                None => {
                    parent_moves.insert(*var_id, Move::MaybeMoved(1));
                }
            }
        }
        edge_count += 1;
    }

    for mv in parent_moves.values_mut() {
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
    *moves = parent_moves;
    for expr in expressions.iter() {
        if matches!(
            &expr.value,
            HirNodeValue::TakeUnique(_) | HirNodeValue::TakeShared(_)
        ) {
            continue;
        }
        move_all(moves, errors, declarations, expr);
    }
}

fn move_all(
    moves: &mut HashMap<ID, Move>,
    errors: &mut Vec<BorrowError>,
    declarations: &HashMap<ID, &StaticDeclaration>,
    expr: &HirNode,
) {
    match &expr.value {
        HirNodeValue::VariableReference(id) => {
            if is_affine(declarations, &expr.ty) {
                if moves.contains_key(id) {
                    errors.push(BorrowError::UseAfterMove);
                } else {
                    moves.insert(*id, Move::Moved);
                }
            }
        }
        HirNodeValue::Parameter(_, _)
        | HirNodeValue::Declaration(_)
        | HirNodeValue::Float(_)
        | HirNodeValue::Bool(_)
        | HirNodeValue::Null
        | HirNodeValue::CharLiteral(_)
        | HirNodeValue::StringLiteral(_)
        | HirNodeValue::Int(_) => {}
        HirNodeValue::TakeUnique(_) | HirNodeValue::TakeShared(_) => {}
        HirNodeValue::Sequence(vals) => {
            for val in vals.iter() {
                move_all(moves, errors, declarations, val);
            }
        }
        HirNodeValue::VtableCall(_, _, params)
        | HirNodeValue::Call(_, params)
        | HirNodeValue::ArrayLiteral(params) => {
            for param in params.iter() {
                move_all(moves, errors, declarations, param);
            }
        }
        HirNodeValue::NumericCast { value, .. } => {
            move_all(moves, errors, declarations, value);
        }
        HirNodeValue::ArrayLiteralLength(lhs, rhs) | HirNodeValue::BinOp(_, lhs, rhs) => {
            move_all(moves, errors, declarations, lhs);
            move_all(moves, errors, declarations, rhs);
        }
        HirNodeValue::ArrayIndex(arr, idx) => {
            if is_affine(declarations, &expr.ty) {
                move_all(moves, errors, declarations, arr);
                move_all(moves, errors, declarations, idx);
            }
        }
        HirNodeValue::Access(val, _) | HirNodeValue::Dereference(val) => {
            if is_affine(declarations, &expr.ty) {
                move_all(moves, errors, declarations, val);
            }
        }
        HirNodeValue::Return(val) => {
            move_all(moves, errors, declarations, val);
        }
        HirNodeValue::Assignment(_, rhs) => {
            move_all(moves, errors, declarations, rhs);
        }
        HirNodeValue::If(cond, _, _) | HirNodeValue::While(cond, _) => {
            move_all(moves, errors, declarations, cond);
        }
        HirNodeValue::StructLiteral(_, fields) => {
            for field in fields.values() {
                move_all(moves, errors, declarations, field);
            }
        }
        // TODO
        HirNodeValue::InterfaceAddress(_) => {}
        HirNodeValue::StructToInterface { .. } => {}
    }
}

// TODO
fn is_affine(declarations: &HashMap<ID, &StaticDeclaration>, ty: &ExpressionType) -> bool {
    match ty {
        ExpressionType::Void => unreachable!(),
        ExpressionType::Primitive(_) => false,
        ExpressionType::DeclaredType(ty) => match declarations[ty] {
            StaticDeclaration::Func(_) => false,
            StaticDeclaration::Struct(_) => true,
            StaticDeclaration::Interface(_) => false,
            StaticDeclaration::Union(_) => true,
            StaticDeclaration::Module(_) => false,
        },
        ExpressionType::Pointer(kind, _) => match kind {
            PointerKind::Shared => false,
            PointerKind::Unique => true,
        },
        ExpressionType::Array(_) => true,
        ExpressionType::Null => false,
        ExpressionType::Nullable(inner) => is_affine(declarations, inner),
    }
}
