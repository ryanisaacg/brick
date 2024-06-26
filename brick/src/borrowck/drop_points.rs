use std::collections::HashMap;

use petgraph::{stable_graph::NodeIndex, Direction};

use crate::{
    declaration_context::IntrinsicFunction,
    hir::HirNode,
    id::{NodeID, VariableID},
    typecheck::{CollectionType, PointerKind, PrimitiveType, StructType},
    DeclarationContext, ExpressionType, HirNodeValue, TypeDeclaration,
};

use super::{
    control_flow_graph::{CfgNode, ControlFlowGraph},
    VariableLifeState, VariableState,
};

#[derive(Debug)]
pub struct DropPoint {
    var_id: VariableID,
    ty: ExpressionType,
}

pub fn calculate_drop_points(
    cfg: &ControlFlowGraph<'_>,
    exit: NodeIndex,
) -> HashMap<NodeID, Vec<DropPoint>> {
    let mut drop_points = calculate_non_move_drops(cfg, exit);
    calculate_move_drops(cfg, &mut drop_points);

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
                let drop_point = DropPoint {
                    var_id: *var_id,
                    ty: variable.ty.clone(),
                };
                if let Some(points) = drop_points.get_mut(&last_use) {
                    points.push(drop_point);
                } else {
                    drop_points.insert(last_use, vec![drop_point]);
                }
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
                    let drop_point = DropPoint {
                        var_id,
                        ty: variable.ty.clone(),
                    };
                    if let Some(points) = drop_points.get_mut(last_used) {
                        points.push(drop_point);
                    } else {
                        drop_points.insert(*last_used, vec![drop_point]);
                    }
                }
            }
        }
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
    });
}

fn drop_variable(
    decls: &DeclarationContext,
    children: &mut Vec<HirNode>,
    value: HirNode,
    ty: &ExpressionType,
) {
    match ty {
        ExpressionType::Void
        | ExpressionType::Unreachable
        | ExpressionType::Primitive(_)
        | ExpressionType::ReferenceToFunction(_)
        | ExpressionType::ReferenceToType(_)
        | ExpressionType::Pointer(_, _)
        | ExpressionType::Null
        | ExpressionType::FunctionReference { .. } => {}
        ExpressionType::Collection(CollectionType::Array(_)) => {
            children.push(HirNode::new_void(HirNodeValue::IntrinsicCall(
                IntrinsicFunction::ArrayFree,
                vec![value],
            )));
        }
        ExpressionType::Collection(CollectionType::Dict(_, _)) => {
            // TODO
        }
        ExpressionType::Collection(CollectionType::String) => {
            // TODO
        }
        ExpressionType::Collection(CollectionType::Cell(inner)) => {
            drop_variable(decls, children, value, inner.as_ref());
        }
        ExpressionType::Collection(CollectionType::ReferenceCounter(inner_ty)) => {
            let value =
                HirNode::autogenerated(HirNodeValue::Dereference(Box::new(value)), ty.clone());
            let mut drop_body = Vec::new();
            drop_variable(
                decls,
                &mut drop_body,
                HirNode::autogenerated(
                    HirNodeValue::TakeUnique(Box::new(HirNode::autogenerated(
                        HirNodeValue::Dereference(Box::new(value.clone())),
                        inner_ty.as_ref().clone(),
                    ))),
                    ExpressionType::Pointer(PointerKind::Unique, inner_ty.clone()),
                ),
                inner_ty.as_ref(),
            );
            drop_body.push(HirNode::new_void(HirNodeValue::IntrinsicCall(
                IntrinsicFunction::RcFree,
                vec![value.clone()],
            )));
            children.push(HirNode::new_void(HirNodeValue::If(
                Box::new(HirNode::autogenerated(
                    HirNodeValue::IntrinsicCall(IntrinsicFunction::RcDecrement, vec![value]),
                    ExpressionType::Primitive(PrimitiveType::Bool),
                )),
                Box::new(HirNode::new_void(HirNodeValue::Sequence(drop_body))),
                None,
            )));
        }
        ExpressionType::Generator { .. } => {
            // TODO
        }
        ExpressionType::InstanceOf(ty_id) => {
            // TODO: drop unions
            // TODO: should interfaces drop? I think no?
            if let TypeDeclaration::Struct(StructType {
                associated_functions,
                ..
            }) = &decls.id_to_decl[ty_id]
            {
                if let Some(drop_fn_id) = associated_functions.get("drop") {
                    children.push(HirNode::new_void(HirNodeValue::Call(
                        Box::new(HirNode::autogenerated(
                            HirNodeValue::VariableReference((*drop_fn_id).into()),
                            ExpressionType::ReferenceToFunction(*drop_fn_id),
                        )),
                        vec![value],
                    )));
                }
            }
        }
        ExpressionType::Nullable(_) => {
            // TODO
        }
        ExpressionType::TypeParameterReference(_) => todo!(),
    }
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
