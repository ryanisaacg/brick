use std::collections::HashMap;

use petgraph::{stable_graph::NodeIndex, Direction};

use crate::{
    hir::{HirNode, HirNodeValue},
    id::ID,
    typecheck::{ExpressionType, PointerKind, StaticDeclaration},
};

use super::{
    control_flow_graph::{CfgNode, ControlFlowGraph, FunctionCFG, Liveness},
    BorrowError,
};

pub fn detect_moves(
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
    if let CfgNode::Block { liveness, .. } = cfg.node_weight(node).unwrap() {
        // Moves already detected here?
        if !liveness.is_empty() {
            return;
        }
    }

    let mut parent_liveness = HashMap::new();
    let mut incoming_connections = cfg.neighbors_directed(node, Direction::Incoming).detach();
    let mut edge_count = 0;
    while let Some((_edge, connector)) = incoming_connections.next(cfg) {
        detect_moves_node(cfg, connector, errors, declarations);
        let Some(
            CfgNode::Block {
                liveness: incoming_liveness,
                ..
            }
            | CfgNode::Exit {
                liveness: incoming_liveness,
            },
        ) = cfg.node_weight(connector)
        else {
            unreachable!()
        };
        for (var_id, liveness) in incoming_liveness.iter() {
            match liveness {
                Liveness::Moved | Liveness::ParentConditionalMoved(_) => {
                    match parent_liveness.get_mut(var_id) {
                        Some(Liveness::ParentConditionalMoved(count)) => {
                            *count += 1;
                        }
                        Some(Liveness::Moved) => unreachable!(),
                        _ => {
                            parent_liveness.insert(*var_id, Liveness::ParentConditionalMoved(1));
                        }
                    }
                }
                Liveness::Referenced(_) | Liveness::ParentReferenced => {
                    if !parent_liveness.contains_key(var_id) {
                        parent_liveness.insert(*var_id, Liveness::ParentReferenced);
                    }
                }
            }
        }
        edge_count += 1;
    }

    for mv in parent_liveness.values_mut() {
        match mv {
            Liveness::ParentConditionalMoved(count) if *count == edge_count => {
                *mv = Liveness::Moved;
            }
            Liveness::ParentConditionalMoved(_)
            | Liveness::ParentReferenced
            | Liveness::Referenced(_) => {}
            Liveness::Moved => unreachable!(),
        }
    }

    let CfgNode::Block {
        expressions,
        liveness: moves,
    } = cfg.node_weight_mut(node).unwrap()
    else {
        return;
    };
    *moves = parent_liveness;
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
    liveness: &mut HashMap<ID, Liveness>,
    errors: &mut Vec<BorrowError>,
    declarations: &HashMap<ID, &StaticDeclaration>,
    expr: &HirNode,
) {
    match &expr.value {
        HirNodeValue::VariableReference(id) => {
            if is_affine(declarations, &expr.ty) {
                match liveness.get(id) {
                    Some(Liveness::Moved | Liveness::ParentConditionalMoved(_)) => {
                        errors.push(BorrowError::UseAfterMove);
                    }
                    Some(Liveness::Referenced(_) | Liveness::ParentReferenced) | None => {
                        liveness.insert(*id, Liveness::Moved);
                    }
                }
            }
        }
        HirNodeValue::Declaration(_) => {
            liveness.insert(expr.id, Liveness::Referenced(expr.id));
        }
        HirNodeValue::Parameter(_, _)
        | HirNodeValue::Float(_)
        | HirNodeValue::Bool(_)
        | HirNodeValue::Null
        | HirNodeValue::CharLiteral(_)
        | HirNodeValue::StringLiteral(_)
        | HirNodeValue::Int(_) => {}
        HirNodeValue::TakeUnique(val) | HirNodeValue::TakeShared(val) => {
            reference_all(liveness, errors, declarations, val);
        }
        HirNodeValue::Sequence(vals) => {
            for val in vals.iter() {
                move_all(liveness, errors, declarations, val);
            }
        }
        HirNodeValue::VtableCall(_, _, params)
        | HirNodeValue::Call(_, params)
        | HirNodeValue::ArrayLiteral(params) => {
            for param in params.iter() {
                move_all(liveness, errors, declarations, param);
            }
        }
        HirNodeValue::NumericCast { value, .. } => {
            move_all(liveness, errors, declarations, value);
        }
        HirNodeValue::ArrayLiteralLength(lhs, rhs) | HirNodeValue::BinOp(_, lhs, rhs) => {
            move_all(liveness, errors, declarations, lhs);
            move_all(liveness, errors, declarations, rhs);
        }
        HirNodeValue::ArrayIndex(arr, idx) => {
            if is_affine(declarations, &expr.ty) {
                move_all(liveness, errors, declarations, arr);
            } else {
                reference_all(liveness, errors, declarations, arr);
            }
            move_all(liveness, errors, declarations, idx);
        }
        HirNodeValue::Access(val, _) | HirNodeValue::Dereference(val) => {
            if is_affine(declarations, &expr.ty) {
                move_all(liveness, errors, declarations, val);
            } else {
                reference_all(liveness, errors, declarations, val);
            }
        }
        HirNodeValue::Return(val) => {
            move_all(liveness, errors, declarations, val);
        }
        HirNodeValue::Assignment(_, rhs) => {
            move_all(liveness, errors, declarations, rhs);
        }
        HirNodeValue::If(cond, _, _) | HirNodeValue::While(cond, _) => {
            move_all(liveness, errors, declarations, cond);
        }
        HirNodeValue::StructLiteral(_, fields) => {
            for field in fields.values() {
                move_all(liveness, errors, declarations, field);
            }
        }
        // TODO
        HirNodeValue::InterfaceAddress(_) => {}
        HirNodeValue::StructToInterface { .. } => {}
    }
}

fn reference_all(
    liveness: &mut HashMap<ID, Liveness>,
    errors: &mut Vec<BorrowError>,
    declarations: &HashMap<ID, &StaticDeclaration>,
    expr: &HirNode,
) {
    expr.visit(|_, expr| {
        if let HirNodeValue::VariableReference(id) = &expr.value {
            if is_affine(declarations, &expr.ty) {
                match liveness.get(id) {
                    Some(Liveness::Moved | Liveness::ParentConditionalMoved(_)) => {
                        errors.push(BorrowError::UseAfterMove);
                    }
                    Some(Liveness::Referenced(_) | Liveness::ParentReferenced) | None => {
                        liveness.insert(*id, Liveness::Referenced(expr.id));
                    }
                }
            }
        }
    });
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
