use std::collections::{HashMap, HashSet};

use petgraph::Direction;

use crate::{
    hir::{HirNode, HirNodeValue},
    id::{TypeID, VariableID},
    typecheck::{CollectionType, ExpressionType, PointerKind, StaticDeclaration},
};

use super::{
    control_flow_graph::{CfgNode, FunctionCFG, Liveness},
    BorrowError,
};

/*
 * The liveness detection algorithm walks the control flow graph repeatedly to find the liveness
 * states of each variable at each point in the program. It starts with a list of all directed
 * edges in the graph, and examines
 */

pub fn detect_moves(
    cfg: &mut FunctionCFG,
    errors: &mut Vec<BorrowError>,
    declarations: &HashMap<TypeID, &StaticDeclaration>,
) {
    for cfg_node in cfg.cfg.node_weights_mut() {
        calculate_liveness_of_expressions_in_node(errors, declarations, cfg_node);
    }

    let mut stack: Vec<_> = cfg.cfg.node_indices().collect();

    let mut changed_nodes = Vec::new();

    while let Some(parent) = stack.pop() {
        let parent_liveness = cfg.cfg.node_weight(parent).unwrap().liveness();
        let mut outgoing_neighbors = cfg
            .cfg
            .neighbors_directed(parent, Direction::Outgoing)
            .detach();

        while let Some((_edge, child)) = outgoing_neighbors.next(&cfg.cfg) {
            let mut changes_for_child = Vec::new();
            let child_weight = cfg.cfg.node_weight(child).unwrap();
            let child_liveness = child_weight.liveness();

            for (var_id, parent_liveness) in parent_liveness.iter() {
                if let Some(child_liveness) = child_liveness.get(var_id) {
                    match (parent_liveness, child_liveness) {
                        (
                            Liveness::Moved(_) | Liveness::MovedInParents(_),
                            Liveness::Referenced(node_id) | Liveness::Moved(node_id),
                        ) => {
                            let CfgNode::Block { expressions, .. } = child_weight else {
                                unreachable!()
                            };
                            let provenance = expressions.iter().find_map(|expr| {
                                let mut provenenace = None;
                                expr.children(|child| {
                                    if child.id == *node_id {
                                        provenenace = expr.provenance.clone();
                                    }
                                });
                                provenenace
                            });
                            errors.push(BorrowError::UseAfterMove(provenance));
                        }
                        (
                            Liveness::Moved(_) | Liveness::MovedInParents(_),
                            Liveness::ParentReferenced,
                        ) => {
                            let mut parents = HashSet::new();
                            parents.insert(parent);
                            changes_for_child.push((*var_id, Liveness::MovedInParents(parents)));
                        }
                        (Liveness::Referenced(_) | Liveness::ParentReferenced, _) => {}
                        (
                            Liveness::Moved(_) | Liveness::MovedInParents(_),
                            Liveness::MovedInParents(parents),
                        ) => {
                            if !parents.contains(&parent) {
                                let mut parents = parents.clone();
                                parents.insert(parent);
                                changes_for_child
                                    .push((*var_id, Liveness::MovedInParents(parents)));
                            }
                        }
                    }
                } else {
                    match parent_liveness {
                        Liveness::Moved(_) | Liveness::MovedInParents(_) => {
                            let mut parents = HashSet::new();
                            parents.insert(parent);
                            changes_for_child.push((*var_id, Liveness::MovedInParents(parents)));
                        }
                        Liveness::Referenced(_) | Liveness::ParentReferenced => {
                            changes_for_child.push((*var_id, Liveness::ParentReferenced));
                        }
                    }
                }
            }
            if !changes_for_child.is_empty() {
                changed_nodes.push((child, changes_for_child));
            }
        }

        for (child, changes_for_child) in changed_nodes.drain(..) {
            let child_liveness = cfg.cfg.node_weight_mut(child).unwrap().liveness_mut();
            for (var_id, liveness) in changes_for_child {
                child_liveness.insert(var_id, liveness);
            }
            if !stack.contains(&child) {
                stack.push(child);
            }
        }
    }
}

fn calculate_liveness_of_expressions_in_node(
    errors: &mut Vec<BorrowError>,
    declarations: &HashMap<TypeID, &StaticDeclaration>,
    cfg_node: &mut CfgNode,
) {
    let CfgNode::Block {
        expressions,
        liveness: moves,
    } = cfg_node
    else {
        return;
    };
    for expr in expressions.iter() {
        if matches!(
            &expr.value,
            HirNodeValue::TakeUnique(_) | HirNodeValue::TakeShared(_)
        ) {
            continue;
        }
        find_moves_in_node(moves, errors, declarations, expr);
    }
}

fn find_moves_in_node(
    liveness: &mut HashMap<VariableID, Liveness>,
    errors: &mut Vec<BorrowError>,
    declarations: &HashMap<TypeID, &StaticDeclaration>,
    expr: &HirNode,
) {
    match &expr.value {
        HirNodeValue::VariableReference(id) => {
            if is_affine(declarations, &expr.ty) {
                let id = id.as_var();
                match liveness.get(&id) {
                    Some(Liveness::Moved(_) | Liveness::MovedInParents(_)) => {
                        errors.push(BorrowError::UseAfterMove(expr.provenance.clone()));
                    }
                    Some(Liveness::Referenced(_) | Liveness::ParentReferenced) | None => {
                        liveness.insert(id, Liveness::Moved(expr.id));
                    }
                }
            }
        }
        HirNodeValue::Declaration(id) => {
            liveness.insert(*id, Liveness::Referenced(expr.id));
        }
        HirNodeValue::Parameter(_, _)
        | HirNodeValue::Float(_)
        | HirNodeValue::Bool(_)
        | HirNodeValue::Null
        | HirNodeValue::CharLiteral(_)
        | HirNodeValue::StringLiteral(_)
        | HirNodeValue::Int(_)
        | HirNodeValue::Yield(None)
        | HirNodeValue::Return(None) => {}
        HirNodeValue::TakeUnique(val) | HirNodeValue::TakeShared(val) => {
            find_references_in_node(liveness, errors, declarations, val);
        }
        HirNodeValue::Sequence(vals) => {
            for val in vals.iter() {
                find_moves_in_node(liveness, errors, declarations, val);
            }
        }
        HirNodeValue::VtableCall(_, _, params)
        | HirNodeValue::Call(_, params)
        | HirNodeValue::RuntimeCall(_, params)
        | HirNodeValue::ArrayLiteral(params) => {
            for param in params.iter() {
                find_moves_in_node(liveness, errors, declarations, param);
            }
        }
        HirNodeValue::DictLiteral(entries) => {
            for (key, value) in entries.iter() {
                find_moves_in_node(liveness, errors, declarations, key);
                find_moves_in_node(liveness, errors, declarations, value);
            }
        }
        HirNodeValue::UnaryLogical(_, value)
        | HirNodeValue::NumericCast { value, .. }
        | HirNodeValue::MakeNullable(value) => {
            find_moves_in_node(liveness, errors, declarations, value);
        }
        HirNodeValue::ArrayLiteralLength(lhs, rhs)
        | HirNodeValue::Arithmetic(_, lhs, rhs)
        | HirNodeValue::Comparison(_, lhs, rhs)
        | HirNodeValue::BinaryLogical(_, lhs, rhs)
        | HirNodeValue::NullCoalesce(lhs, rhs) => {
            find_moves_in_node(liveness, errors, declarations, lhs);
            find_moves_in_node(liveness, errors, declarations, rhs);
        }
        HirNodeValue::ArrayIndex(arr, idx) => {
            if is_affine(declarations, &expr.ty) {
                find_moves_in_node(liveness, errors, declarations, arr);
            } else {
                find_references_in_node(liveness, errors, declarations, arr);
            }
            find_moves_in_node(liveness, errors, declarations, idx);
        }
        HirNodeValue::DictIndex(dict, idx) => {
            if is_affine(declarations, &expr.ty) {
                find_moves_in_node(liveness, errors, declarations, dict);
            } else {
                find_references_in_node(liveness, errors, declarations, dict);
            }
            find_moves_in_node(liveness, errors, declarations, idx);
        }
        HirNodeValue::NullableTraverse(val, _)
        | HirNodeValue::Access(val, _)
        | HirNodeValue::Dereference(val) => {
            if is_affine(declarations, &expr.ty) {
                find_moves_in_node(liveness, errors, declarations, val);
            } else {
                find_references_in_node(liveness, errors, declarations, val);
            }
        }
        HirNodeValue::UnionLiteral(_, _, child)
        | HirNodeValue::Return(Some(child))
        | HirNodeValue::Yield(Some(child))
        | HirNodeValue::Assignment(_, child) => {
            find_moves_in_node(liveness, errors, declarations, child);
        }
        HirNodeValue::If(cond, _, _) | HirNodeValue::While(cond, _) => {
            find_moves_in_node(liveness, errors, declarations, cond);
        }
        HirNodeValue::StructLiteral(_, fields) => {
            for field in fields.values() {
                find_moves_in_node(liveness, errors, declarations, field);
            }
        }
        // TODO
        HirNodeValue::Loop(_) => {}
        HirNodeValue::InterfaceAddress(_) => {}
        HirNodeValue::StructToInterface { .. } => {}
        HirNodeValue::PointerSize(_) => todo!(),
        HirNodeValue::GotoLabel(_) => {}
        HirNodeValue::GeneratorResume(_) => {}
        HirNodeValue::GeneratorSuspend(_, _) => {}
        HirNodeValue::GeneratorCreate { .. } => {}
        HirNodeValue::StringConcat(_, _) => {}
    }
}

fn find_references_in_node(
    liveness: &mut HashMap<VariableID, Liveness>,
    errors: &mut Vec<BorrowError>,
    declarations: &HashMap<TypeID, &StaticDeclaration>,
    expr: &HirNode,
) {
    expr.visit(|_, expr| {
        if let HirNodeValue::VariableReference(id) = &expr.value {
            let id = id.as_var();
            if is_affine(declarations, &expr.ty) {
                match liveness.get(&id) {
                    Some(Liveness::Moved(_) | Liveness::MovedInParents(_)) => {
                        errors.push(BorrowError::UseAfterMove(expr.provenance.clone()));
                    }
                    Some(Liveness::Referenced(_) | Liveness::ParentReferenced) | None => {
                        liveness.insert(id, Liveness::Referenced(expr.id));
                    }
                }
            }
        }
    });
}

// TODO
fn is_affine(declarations: &HashMap<TypeID, &StaticDeclaration>, ty: &ExpressionType) -> bool {
    match ty {
        ExpressionType::Void | ExpressionType::Unreachable => unreachable!(),
        ExpressionType::Primitive(_) => false,
        ExpressionType::InstanceOf(ty) => match declarations[ty] {
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
        ExpressionType::Collection(CollectionType::Array(_)) => true,
        ExpressionType::Collection(CollectionType::Dict(_, _)) => true,
        ExpressionType::Null => false,
        ExpressionType::Nullable(inner) => is_affine(declarations, inner),
        ExpressionType::ReferenceTo(_) => todo!(),
        ExpressionType::TypeParameterReference(_) => todo!(),
        ExpressionType::Generator { .. } => true,
        // TODO: is this correct
        ExpressionType::FunctionReference { .. } => false,
    }
}
