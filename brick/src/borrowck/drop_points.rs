use std::collections::{HashMap, HashSet};

use petgraph::{stable_graph::NodeIndex, Direction};

use crate::id::{NodeID, VariableID};

use super::control_flow_graph::{CfgNode, ControlFlowGraph, FunctionCFG, Liveness};

/// Node ID -> Vars to drop
pub fn find_drop_points(cfg: &FunctionCFG) -> HashMap<NodeID, Vec<VariableID>> {
    let mut map = HashMap::new();
    let CfgNode::Exit { liveness } = cfg.cfg.node_weight(cfg.end).unwrap() else {
        unreachable!()
    };
    let reachability = find_reachability(cfg);
    for var_id in liveness.keys() {
        let mut visited = HashSet::new();
        find_drop_points_node(
            &cfg.cfg,
            &mut map,
            &reachability,
            var_id,
            &mut visited,
            cfg.end,
        );
    }
    map
}

fn find_reachability(cfg: &FunctionCFG) -> HashMap<NodeIndex, HashSet<NodeIndex>> {
    let mut map: HashMap<NodeIndex, HashSet<NodeIndex>> = HashMap::new();
    map.insert(cfg.end, HashSet::new());

    let mut stack = vec![cfg.end];
    while let Some(current) = stack.pop() {
        for parent in cfg.cfg.neighbors_directed(current, Direction::Incoming) {
            if map.contains_key(&parent) {
                let child_reachability = &map[&current];
                let parent_reachability = &map[&parent];
                let new_nodes: HashSet<_> = child_reachability
                    .iter()
                    .copied()
                    .filter(|item| !parent_reachability.contains(item))
                    .collect();
                if !new_nodes.is_empty() {
                    map.get_mut(&parent).unwrap().extend(new_nodes);
                    stack.push(parent);
                }
            } else {
                let mut reachable = map[&current].clone();
                reachable.insert(current);
                map.insert(parent, reachable);
                stack.push(parent);
            }
        }
    }

    map
}

/**
 * Recurse upwards through the graph from the exit, stopping when we find the last reference or
 * move in a given code path
 */
fn find_drop_points_node(
    cfg: &ControlFlowGraph,
    map: &mut HashMap<NodeID, Vec<VariableID>>,
    reachability: &HashMap<NodeIndex, HashSet<NodeIndex>>,
    variable: &VariableID,
    visited: &mut HashSet<NodeIndex>,
    node: NodeIndex,
) {
    if visited.contains(&node) {
        return;
    }
    visited.insert(node);
    let liveness = cfg.node_weight(node).unwrap().liveness();
    match liveness.get(variable) {
        // If the variable is moved in this branch OR isn't known in this branch, don't drop
        Some(Liveness::Moved(_)) | None => {}
        // If the variable is moved or referenced in some branches, recurse up and drop in those only
        Some(Liveness::MovedInParents(_) | Liveness::ParentReferenced) => {
            let do_any_require_late_drop = cfg
                .neighbors_directed(node, Direction::Incoming)
                .flat_map(|parent| reachability[&parent].iter())
                .any(|reachable| {
                    matches!(
                        cfg.node_weight(*reachable)
                            .unwrap()
                            .liveness()
                            .get(variable),
                        Some(Liveness::Referenced(_))
                    )
                });
            if do_any_require_late_drop {
                // TODO: can this ever happen to the exit node?
                // TODO: can blocks end up empty?
                let CfgNode::Block { expressions, .. } = cfg.node_weight(node).unwrap() else {
                    return;
                    //todo!("{:?}", cfg.node_weight(node))
                };
                map.entry(expressions[0].id).or_default().push(*variable);
            } else {
                for parent in cfg.neighbors_directed(node, Direction::Incoming) {
                    find_drop_points_node(cfg, map, reachability, variable, visited, parent);
                }
            }
        }
        // The variable is referenced here. Drop after this
        Some(Liveness::Referenced(id)) => {
            map.entry(*id).or_default().push(*variable);
        }
    }
}
