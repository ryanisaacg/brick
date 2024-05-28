use std::collections::HashMap;

use petgraph::stable_graph::NodeIndex;

use crate::{
    declaration_context::IntrinsicFunction,
    hir::HirNode,
    id::{NodeID, VariableID},
    typecheck::CollectionType,
    ExpressionType, HirNodeValue,
};

use super::{
    control_flow_graph::{CfgNode, ControlFlowGraph},
    VariableLifeState,
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
    let Some(CfgNode::Exit { life_state, .. }) = cfg.node_weight(exit) else {
        unreachable!()
    };
    let mut drop_points: HashMap<NodeID, Vec<DropPoint>> = HashMap::new();
    for (var_id, variable) in life_state.var_state.iter() {
        // obviously when we have real drop suport, this will look different
        if !matches!(
            variable.ty,
            ExpressionType::Collection(CollectionType::Array(..))
        ) {
            continue;
        }
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

pub fn insert_drops(node: &mut HirNode, mut drops: HashMap<NodeID, Vec<DropPoint>>) {
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
            // super not real drop to actually insert
            children.push(HirNode::autogenerated(
                HirNodeValue::IntrinsicCall(
                    IntrinsicFunction::ArrayFree,
                    vec![HirNode::autogenerated(
                        HirNodeValue::VariableReference(drop.var_id.into()),
                        drop.ty,
                    )],
                ),
                ExpressionType::Void,
            ));
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