use std::collections::{HashMap, HashSet};

use petgraph::{stable_graph::NodeIndex, Direction};

use crate::{
    generate_destructors::drop_variable,
    hir::HirNode,
    id::{NodeID, VariableID},
    typecheck::PointerKind,
    DeclarationContext, ExpressionType, HirNodeValue,
};

use super::{
    control_flow_graph::{CfgNode, ControlFlowGraph},
    VariableLifeState, VariableState,
};

#[derive(Debug)]
pub struct DropPoint {
    var_id: VariableID,
    ty: ExpressionType,
    timing: DropTiming,
}

#[derive(Debug)]
pub enum DropTiming {
    BeforeAssignment(NodeID),
    AfterLastUse,
}

pub fn calculate_drop_points(
    decls: &DeclarationContext,
    cfg: &ControlFlowGraph<'_>,
    root: &HirNode,
    exit: NodeIndex,
) -> HashMap<NodeID, Vec<DropPoint>> {
    let mut drop_points = calculate_non_move_drops(cfg, exit);
    calculate_move_drops(cfg, &mut drop_points);
    calculate_reassignment_drop_points(decls, root, &mut drop_points);

    drop_points
}

fn calculate_non_move_drops(
    cfg: &ControlFlowGraph<'_>,
    exit: NodeIndex,
) -> HashMap<NodeID, Vec<DropPoint>> {
    let Some(CfgNode::Exit { life_state, .. }) = cfg.node_weight(exit) else {
        unreachable!()
    };
    let mut drop_points: HashMap<NodeID, Vec<DropPoint>> = HashMap::new();
    for (var_id, variable) in life_state.var_state.iter() {
        match variable.state {
            VariableLifeState::Used(last_use, _) => {
                insert_drop(
                    &mut drop_points,
                    last_use,
                    DropPoint {
                        var_id: *var_id,
                        ty: variable.ty.clone(),
                        timing: DropTiming::AfterLastUse,
                    },
                );
            }
            VariableLifeState::Moved(_, _) => {}
        }
    }

    drop_points
}

fn calculate_move_drops(
    cfg: &ControlFlowGraph<'_>,
    drop_points: &mut HashMap<NodeID, Vec<DropPoint>>,
) {
    for node_id in cfg.node_indices() {
        let node = cfg.node_weight(node_id).unwrap();
        let node_states = node.life_state();
        for (var_id, variable) in node_states.var_state.iter() {
            let var_id = *var_id;
            let any_parents_move = parent_states_for_var(cfg, node_id, var_id)
                .any(|parent_var| matches!(parent_var.state, VariableLifeState::Moved(_, _)));
            let any_parents_dont_move = parent_states_for_var(cfg, node_id, var_id)
                .any(|parent_var| matches!(parent_var.state, VariableLifeState::Used(_, _)));
            if any_parents_move && any_parents_dont_move {
                for parent_var_state in parent_states_for_var(cfg, node_id, var_id) {
                    let VariableLifeState::Used(last_used, _) = &parent_var_state.state else {
                        continue;
                    };
                    insert_drop(
                        drop_points,
                        *last_used,
                        DropPoint {
                            var_id,
                            ty: variable.ty.clone(),
                            timing: DropTiming::AfterLastUse,
                        },
                    );
                }
            }
        }
    }
}

fn calculate_reassignment_drop_points(
    decls: &DeclarationContext,
    node: &HirNode,
    drop_points: &mut HashMap<NodeID, Vec<DropPoint>>,
) {
    let mut initialized = HashSet::new();

    node.visit(|_, child| {
        let HirNodeValue::Assignment(lhs, _) = &child.value else {
            return;
        };
        if !lhs.ty.is_affine(&decls.id_to_decl) {
            return;
        }
        let HirNodeValue::VariableReference(var_id) = &lhs.value else {
            todo!("assigning to affine fields rather than variables not yet supported");
        };
        if !initialized.contains(var_id) {
            initialized.insert(*var_id);
        } else {
            insert_drop(
                drop_points,
                child.id,
                DropPoint {
                    var_id: var_id.as_var(),
                    ty: lhs.ty.clone(),
                    timing: DropTiming::BeforeAssignment(child.id),
                },
            );
        }
    });
}

fn insert_drop(
    drop_points: &mut HashMap<NodeID, Vec<DropPoint>>,
    target: NodeID,
    drop_point: DropPoint,
) {
    if let Some(points) = drop_points.get_mut(&target) {
        points.push(drop_point);
    } else {
        drop_points.insert(target, vec![drop_point]);
    }
}

fn parent_states_for_var<'a>(
    cfg: &'a ControlFlowGraph<'a>,
    node_id: NodeIndex,
    var_id: VariableID,
) -> impl Iterator<Item = &'a VariableState> {
    cfg.neighbors_directed(node_id, Direction::Incoming)
        .filter_map(move |parent| {
            cfg.node_weight(parent)
                .unwrap()
                .life_state()
                .var_state
                .get(&var_id)
        })
}

pub fn insert_drops(
    decls: &DeclarationContext,
    node: &mut HirNode,
    mut drops: HashMap<NodeID, Vec<DropPoint>>,
) {
    node.visit_mut(|parent| {
        if !matches!(parent.value, HirNodeValue::Sequence(_)) {
            return;
        }
        let mut drops_to_insert = Vec::new();
        parent.children(|child| find_drops_within_block(child, &mut drops_to_insert, &mut drops));
        let HirNodeValue::Sequence(children) = &mut parent.value else {
            unreachable!()
        };
        for drop in drops_to_insert {
            match drop.timing {
                DropTiming::BeforeAssignment(assignment_id) => {
                    let assignment_node = children
                        .iter_mut()
                        .find(|child| child.id == assignment_id)
                        .expect("ICE: child expression must be found when creating drop");
                    let HirNodeValue::Assignment(lhs, rhs) =
                        std::mem::take(&mut assignment_node.value)
                    else {
                        unreachable!()
                    };
                    let ty = lhs.ty.clone();
                    let temp_var_id = VariableID::new();
                    let mut seq = vec![
                        HirNode::new(
                            HirNodeValue::Declaration(temp_var_id),
                            ty.clone(),
                            lhs.provenance.clone(),
                        ),
                        HirNode::new(
                            HirNodeValue::Assignment(
                                Box::new(HirNode::new(
                                    HirNodeValue::VariableReference(temp_var_id.into()),
                                    ty.clone(),
                                    lhs.provenance.clone(),
                                )),
                                rhs,
                            ),
                            ty.clone(),
                            lhs.provenance.clone(),
                        ),
                    ];
                    drop_variable(
                        decls,
                        &mut seq,
                        HirNode::autogenerated(
                            HirNodeValue::TakeUnique(Box::new(HirNode::autogenerated(
                                HirNodeValue::VariableReference(drop.var_id.into()),
                                drop.ty.clone(),
                            ))),
                            ExpressionType::Pointer(PointerKind::Unique, Box::new(drop.ty.clone())),
                        ),
                        &drop.ty,
                    );
                    let provenance = lhs.provenance.clone();
                    seq.push(HirNode::new(
                        HirNodeValue::Assignment(
                            lhs,
                            Box::new(HirNode::new(
                                HirNodeValue::VariableReference(temp_var_id.into()),
                                ty.clone(),
                                provenance.clone(),
                            )),
                        ),
                        ty.clone(),
                        provenance.clone(),
                    ));
                    assignment_node.value = HirNodeValue::Sequence(seq);
                }
                DropTiming::AfterLastUse => {
                    drop_variable(
                        decls,
                        children,
                        HirNode::autogenerated(
                            HirNodeValue::TakeUnique(Box::new(HirNode::autogenerated(
                                HirNodeValue::VariableReference(drop.var_id.into()),
                                drop.ty.clone(),
                            ))),
                            ExpressionType::Pointer(PointerKind::Unique, Box::new(drop.ty.clone())),
                        ),
                        &drop.ty,
                    );
                }
            }
        }
    });
}

fn find_drops_within_block(
    node: &HirNode,
    drops_to_insert: &mut Vec<DropPoint>,
    drops: &mut HashMap<NodeID, Vec<DropPoint>>,
) {
    if let Some(drop_points) = drops.remove(&node.id) {
        drops_to_insert.extend(drop_points);
    }
    node.children(|child| {
        if !matches!(child.value, HirNodeValue::Sequence(_)) {
            find_drops_within_block(child, drops_to_insert, drops);
        }
    });
}
