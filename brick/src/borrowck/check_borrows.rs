use std::collections::{HashMap, HashSet};

use petgraph::{stable_graph::NodeIndex, Direction};

use super::{
    control_flow_graph::{CfgNode, ControlFlowGraph, FunctionCFG},
    BorrowError,
};
use crate::{
    hir::{HirNode, HirNodeValue},
    id::{NodeID, VariableID},
};

#[derive(Clone, Debug)]
enum Borrow {
    Unique,
    Shared(u32),
}

pub fn check_borrows(
    cfg: &FunctionCFG,
    drops: &HashMap<NodeID, Vec<VariableID>>,
    errors: &mut Vec<BorrowError>,
) {
    check_borrow_cfg_node(
        &cfg.cfg,
        drops,
        &mut HashSet::new(),
        cfg.start,
        HashMap::new(),
        HashMap::new(),
        errors,
    );
}

fn check_borrow_cfg_node(
    cfg: &ControlFlowGraph,
    drops: &HashMap<NodeID, Vec<VariableID>>,
    visited: &mut HashSet<NodeIndex>,
    node: NodeIndex,
    mut existing_borrows: HashMap<VariableID, Borrow>,
    mut borrow_to_variable: HashMap<VariableID, VariableID>,
    errors: &mut Vec<BorrowError>,
) {
    let Some(CfgNode::Block { expressions, .. }) = cfg.node_weight(node) else {
        return;
    };
    // TODO: is this legal? what if you do need to visit a node more than once
    if visited.contains(&node) {
        return;
    }
    visited.insert(node);

    for expr in expressions.iter() {
        check_borrow_expr(
            drops,
            expr,
            &mut existing_borrows,
            &mut borrow_to_variable,
            errors,
        );
    }

    for neighbor in cfg.neighbors_directed(node, Direction::Outgoing) {
        check_borrow_cfg_node(
            cfg,
            drops,
            visited,
            neighbor,
            existing_borrows.clone(),
            borrow_to_variable.clone(),
            errors,
        );
    }
}

fn check_borrow_expr(
    drops: &HashMap<NodeID, Vec<VariableID>>,
    expr: &HirNode,
    borrowed_variables: &mut HashMap<VariableID, Borrow>,
    borrow_to_variable: &mut HashMap<VariableID, VariableID>,
    errors: &mut Vec<BorrowError>,
) {
    // TODO: check function calls for disjoint borrows
    if let HirNodeValue::Assignment(lhs, rhs) = &expr.value {
        // TODO: create borrow
        match &rhs.value {
            HirNodeValue::TakeShared(inner) => {
                let HirNodeValue::VariableReference(borrow_var_id) = &lhs.value else {
                    todo!("reference lvalue that aren't variables: {:?}", lhs)
                };
                let HirNodeValue::VariableReference(var_id) = &inner.value else {
                    todo!("references rvalue that aren't variables: {:?}", inner)
                };
                let borrow_var_id = borrow_var_id.as_var();
                let var_id = var_id.as_var();
                match borrowed_variables.get_mut(&var_id) {
                    Some(Borrow::Unique) => {
                        errors.push(BorrowError::AlreadyBorrowed);
                    }
                    Some(Borrow::Shared(borrow_count)) => {
                        *borrow_count += 1;
                    }
                    None => {
                        borrowed_variables.insert(var_id, Borrow::Shared(1));
                    }
                }
                borrow_to_variable.insert(borrow_var_id, var_id);
            }
            HirNodeValue::TakeUnique(inner) => {
                let HirNodeValue::VariableReference(borrow_var_id) = &lhs.value else {
                    todo!("reference lvalue that aren't variables: {:?}", lhs)
                };
                let HirNodeValue::VariableReference(var_id) = &inner.value else {
                    todo!("references rvalue that aren't variables: {:?}", inner)
                };
                let borrow_var_id = borrow_var_id.as_var();
                let var_id = var_id.as_var();
                if let std::collections::hash_map::Entry::Vacant(e) =
                    borrowed_variables.entry(var_id)
                {
                    e.insert(Borrow::Unique);
                    borrow_to_variable.insert(borrow_var_id, var_id);
                } else {
                    errors.push(BorrowError::AlreadyBorrowed);
                }
            }
            _ => {}
        }
    }
    if let Some(dropped_var_ids) = drops.get(&expr.id) {
        for dropped_var_id in dropped_var_ids.iter() {
            if let Some(borrowed_var_id) = borrow_to_variable.remove(dropped_var_id) {
                match borrowed_variables.get_mut(&borrowed_var_id).unwrap() {
                    Borrow::Unique => {
                        borrowed_variables.remove(&borrowed_var_id);
                    }
                    Borrow::Shared(borrowers) => {
                        *borrowers -= 1;
                        if *borrowers == 0 {
                            borrowed_variables.remove(&borrowed_var_id);
                        }
                    }
                }
            }
        }
    }
}
