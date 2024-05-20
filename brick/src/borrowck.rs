use std::collections::{HashMap, HashSet, VecDeque};

use petgraph::{
    data::DataMap,
    stable_graph::{EdgeIndex, NodeIndex},
    visit::EdgeRef,
    Direction,
};
use thiserror::Error;

use crate::{
    hir::{HirModule, HirNode},
    id::{AnyID, NodeID, TypeID, VariableID},
    typecheck::{ExpressionType, PointerKind},
    HirNodeValue, SourceRange, StaticDeclaration,
};

use self::control_flow_graph::{CfgEdge, CfgNode, ControlFlowGraph, FunctionCFG};

mod control_flow_graph;

#[derive(Debug, Error)]
pub enum LifetimeError {
    #[error("use after move{}; moved{}", maybe_range(.use_point), maybe_range(.move_point))]
    UseAfterMove {
        use_point: Option<SourceRange>,
        move_point: Option<SourceRange>,
    },
    #[error("{}",  print_multi_errors(&.0[..]))]
    MultiError(Vec<LifetimeError>),
    #[error("borrow used after move{}; moved{}", maybe_range(.use_point), maybe_range(.move_point))]
    BorrowUseAfterMove {
        use_point: Option<SourceRange>,
        move_point: Option<SourceRange>,
    },
    #[error("borrow used after reassignment{}; assigned{}", maybe_range(.use_point), maybe_range(.reassign_point))]
    BorrowUseAfterReassignment {
        use_point: Option<SourceRange>,
        reassign_point: Option<SourceRange>,
    },
    #[error("borrow used after unique reference taken{}; reference taken{}", maybe_range(.use_point), maybe_range(.ref_point))]
    BorrowUseAfterMutableRefTaken {
        use_point: Option<SourceRange>,
        ref_point: Option<SourceRange>,
    },
}

fn maybe_range(range: &Option<SourceRange>) -> String {
    if let Some(range) = range {
        format!(": {range}")
    } else {
        " in generated code, this is probably an ICE".to_string()
    }
}

fn print_multi_errors(errors: &[LifetimeError]) -> String {
    let mut string = String::new();
    for error in errors.iter() {
        string.push_str(&format!("{error}\n"));
    }
    string
}

pub fn borrow_check(
    module: &mut HirModule,
    declarations: &HashMap<TypeID, &StaticDeclaration>,
) -> Result<(), LifetimeError> {
    let mut result = Ok(());

    for func in module.functions.iter() {
        let func_result = borrow_check_body(declarations, &func.body);
        merge_results(&mut result, func_result);
    }

    let top_level_result = borrow_check_body(declarations, &module.top_level_statements);
    merge_results(&mut result, top_level_result);

    result
}

fn borrow_check_body(
    declarations: &HashMap<TypeID, &StaticDeclaration>,
    node: &HirNode,
) -> Result<(), LifetimeError> {
    let FunctionCFG {
        mut cfg,
        start,
        end: _,
    } = control_flow_graph::build_control_flow_graph(node);
    let mut block_queue = VecDeque::new();
    block_queue.push_front(start);
    let mut context = Context {
        declarations,
        block_queue,
        loops_visited: HashSet::new(),
    };

    let mut result = Ok(());

    while let Some(block_idx) = context.block_queue.pop_front() {
        merge_results(
            &mut result,
            borrow_check_block(&mut context, &mut cfg, block_idx),
        );
    }

    result
}

#[derive(Clone, Debug, Default)]
struct BlockLiveness {
    var_state: HashMap<VariableID, VariableState>,
    borrow_state: HashMap<VariableID, BorrowState>,
}

impl BlockLiveness {
    fn new() -> BlockLiveness {
        BlockLiveness::default()
    }
}

#[derive(Clone, Debug)]
struct VariableState {
    state: VariableLifeState,
    borrows: Vec<VariableID>,
}

#[derive(Clone, Debug)]
enum VariableLifeState {
    #[allow(dead_code)] // for future drop insertion
    Used(NodeID, Option<SourceRange>),
    Moved(NodeID, Option<SourceRange>),
    //MovedInParents(HashSet<NodeIndex>),
    //ParentReferenced,
}

#[derive(Clone, Debug)]
struct BorrowState {
    state: BorrowLifeState,
}

#[derive(Clone, Debug)]
enum BorrowLifeState {
    Used(NodeID, Option<SourceRange>),
    ValueMoved(NodeID, Option<SourceRange>),
    ValueReassigned(NodeID, Option<SourceRange>),
    MutableRefTaken(NodeID, Option<SourceRange>),
}

struct Context<'a> {
    declarations: &'a HashMap<TypeID, &'a StaticDeclaration>,
    block_queue: VecDeque<NodeIndex>,
    loops_visited: HashSet<EdgeIndex>,
}

fn borrow_check_block<'a>(
    ctx: &mut Context<'a>,
    cfg: &mut ControlFlowGraph<'a>,
    block_idx: NodeIndex,
) -> Result<(), LifetimeError> {
    // Merge parent states into a new state
    let mut var_state = HashMap::new();
    for incoming_edge in cfg.edges_directed(block_idx, Direction::Incoming) {
        let parent = cfg
            .node_weight(incoming_edge.source())
            .unwrap()
            .life_state();
        for (var_id, parent_state) in parent.var_state.iter() {
            var_state
                .entry(*var_id)
                .and_modify(|child_state| merge_var_states(parent_state, child_state))
                .or_insert(parent_state.clone());
        }
    }
    let block = cfg.node_weight_mut(block_idx).unwrap();
    block.life_state_mut().var_state = var_state;
    // Analyze contained blocks
    let result = match block {
        CfgNode::Block {
            expressions,
            life_state,
        } => {
            let mut result = Ok(());
            for node in expressions.iter() {
                merge_results(
                    &mut result,
                    borrow_check_node(
                        ctx,
                        &mut life_state.var_state,
                        &mut life_state.borrow_state,
                        node,
                    ),
                );
            }
            result
        }
        CfgNode::Exit { .. } => Ok(()),
    };
    // Push outgoing blocks to the stack
    for outgoing_edge in cfg.edges_directed(block_idx, Direction::Outgoing) {
        if matches!(outgoing_edge.weight(), CfgEdge::Loop) {
            let edge_id = outgoing_edge.id();
            if ctx.loops_visited.contains(&edge_id) {
                continue;
            } else {
                ctx.loops_visited.insert(edge_id);
            }
        }
        ctx.block_queue.push_back(outgoing_edge.target());
    }
    result
}

fn merge_var_states(parent: &VariableState, child: &mut VariableState) {
    match (&parent.state, &mut child.state) {
        (VariableLifeState::Used(_, _), _) => {
            // which 'used' the child keeps track of doesn't seem super important
        }
        (VariableLifeState::Moved(_, _), VariableLifeState::Used(_, _)) => {
            *child = parent.clone();
        }
        (VariableLifeState::Moved(_, _), VariableLifeState::Moved(_, _)) => {
            // not sure but I think we can safely leave this alone
        }
    }
}

fn borrow_check_node(
    ctx: &mut Context<'_>,
    variable_state: &mut HashMap<VariableID, VariableState>,
    borrow_state: &mut HashMap<VariableID, BorrowState>,
    node: &HirNode,
) -> Result<(), LifetimeError> {
    let mut results = Ok(());
    match &node.value {
        // If we've reached a variable reference without something shunting us
        // into borrow-mode, then this value is being used directly
        HirNodeValue::VariableReference(id) => {
            // Type or function IDs don't matter for this purpose
            let AnyID::Variable(var_id) = id else {
                return Ok(());
            };
            if let Some(existing_borrow_state) = borrow_state.get_mut(var_id) {
                match &mut existing_borrow_state.state {
                    BorrowLifeState::Used(id, provenance) => {
                        *id = node.id;
                        *provenance = node.provenance.clone();
                    }
                    BorrowLifeState::ValueMoved(_id, move_point) => {
                        merge_results(
                            &mut results,
                            Err(LifetimeError::BorrowUseAfterMove {
                                use_point: node.provenance.clone(),
                                move_point: move_point.clone(),
                            }),
                        );
                    }
                    BorrowLifeState::ValueReassigned(_id, move_point) => {
                        merge_results(
                            &mut results,
                            Err(LifetimeError::BorrowUseAfterReassignment {
                                use_point: node.provenance.clone(),
                                reassign_point: move_point.clone(),
                            }),
                        );
                    }
                    BorrowLifeState::MutableRefTaken(_id, ref_point) => {
                        merge_results(
                            &mut results,
                            Err(LifetimeError::BorrowUseAfterMutableRefTaken {
                                use_point: node.provenance.clone(),
                                ref_point: ref_point.clone(),
                            }),
                        );
                    }
                }
            } else {
                // We can ignore copy types
                if is_copy(ctx.declarations, &node.ty) {
                    return Ok(());
                }

                let existing_state = variable_state.get_mut(var_id).unwrap();
                if let VariableLifeState::Moved(_id, range) = &existing_state.state {
                    merge_results(
                        &mut results,
                        Err(LifetimeError::UseAfterMove {
                            move_point: range.clone(),
                            use_point: node.provenance.clone(),
                        }),
                    );
                }

                existing_state.state = VariableLifeState::Moved(node.id, node.provenance.clone());
                invalidate_borrowers(
                    existing_state,
                    borrow_state,
                    BorrowLifeState::ValueMoved(node.id, node.provenance.clone()),
                );
            }
        }
        HirNodeValue::Parameter(_, id) => {
            variable_state.insert(
                *id,
                VariableState {
                    state: VariableLifeState::Used(node.id, node.provenance.clone()),
                    borrows: Vec::new(),
                },
            );
        }
        HirNodeValue::Declaration(id) => {
            if let ExpressionType::Pointer(_, _) = &node.ty {
                borrow_state.insert(
                    *id,
                    BorrowState {
                        state: BorrowLifeState::Used(node.id, node.provenance.clone()),
                    },
                );
            } else {
                variable_state.insert(
                    *id,
                    VariableState {
                        state: VariableLifeState::Used(node.id, node.provenance.clone()),
                        borrows: Vec::new(),
                    },
                );
            }
        }
        HirNodeValue::Assignment(lhs, rhs) => {
            merge_results(
                &mut results,
                borrow_check_node(ctx, variable_state, borrow_state, rhs),
            );
            let id = find_id_for_lvalue(lhs);
            if let AnyID::Variable(var_id) = id {
                if let Some(existing_state) = variable_state.get_mut(var_id) {
                    existing_state.state =
                        VariableLifeState::Used(node.id, node.provenance.clone());
                    invalidate_borrowers(
                        existing_state,
                        borrow_state,
                        BorrowLifeState::ValueReassigned(node.id, node.provenance.clone()),
                    );
                } else if let ExpressionType::Pointer(ref_ty, _) = &lhs.ty {
                    let lender_id = find_id_for_lvalue(rhs).as_var();
                    let var_state = variable_state.get_mut(&lender_id).unwrap();
                    if *ref_ty == PointerKind::Unique {
                        invalidate_borrowers(
                            var_state,
                            borrow_state,
                            BorrowLifeState::MutableRefTaken(node.id, node.provenance.clone()),
                        );
                    }

                    var_state.borrows.push(*var_id);
                }
                // If assignment is to a reference but not to its reference value directly,
                // it's just updating a path within its owner
            }
        }
        // TODO: path search for moves
        HirNodeValue::Access(_, _) => {}
        HirNodeValue::NullableTraverse(_, _) => {}
        HirNodeValue::ArrayIndex(_, _) => {}
        HirNodeValue::DictIndex(_, _) => {}
        HirNodeValue::UnionTag(_) => {}
        HirNodeValue::UnionVariant(_, _) => {}

        // TODO: check borrows are disjoint
        HirNodeValue::Call(_, params)
        | HirNodeValue::VtableCall(_, _, params)
        | HirNodeValue::RuntimeCall(_, params) => {
            for param in params.iter() {
                merge_results(
                    &mut results,
                    borrow_check_node(ctx, variable_state, borrow_state, param),
                );
            }
        }

        // TODO: children are borrowed, not moved
        HirNodeValue::TakeUnique(_) => {}
        HirNodeValue::TakeShared(_) => {}

        _ => {
            node.children(|child| {
                merge_results(
                    &mut results,
                    borrow_check_node(ctx, variable_state, borrow_state, child),
                );
            });
        }
    }

    results
}

fn invalidate_borrowers(
    variable_state: &VariableState,
    borrow_state: &mut HashMap<VariableID, BorrowState>,
    new_state: BorrowLifeState,
) {
    for borrow_id in variable_state.borrows.iter() {
        let existing_borrow_state = borrow_state.get_mut(borrow_id).unwrap();
        existing_borrow_state.state = new_state.clone();
    }
}

// MASSIVE TODO: what determines if a type is copy or not?
#[allow(clippy::only_used_in_recursion)]
fn is_copy(declarations: &HashMap<TypeID, &StaticDeclaration>, ty: &ExpressionType) -> bool {
    match ty {
        ExpressionType::Nullable(inner) => is_copy(declarations, inner.as_ref()),
        ExpressionType::Void
        | ExpressionType::Unreachable
        | ExpressionType::Null
        // Only if references truly aren't allowed in re-assignments
        | ExpressionType::Pointer(_, _)
        | ExpressionType::Primitive(_) => true,
        ExpressionType::InstanceOf(_)
        | ExpressionType::ReferenceTo(_)
        | ExpressionType::TypeParameterReference(_)
        | ExpressionType::Collection(_)
        | ExpressionType::Generator { .. }
        | ExpressionType::FunctionReference { .. } => false,
    }
}

fn find_id_for_lvalue(lvalue: &HirNode) -> &AnyID {
    match &lvalue.value {
        HirNodeValue::VariableReference(id) => id,
        HirNodeValue::NullableTraverse(child, _)
        | HirNodeValue::Access(child, _)
        | HirNodeValue::ArrayIndex(child, _)
        | HirNodeValue::DictIndex(child, _)
        | HirNodeValue::Dereference(child)
        | HirNodeValue::TakeUnique(child)
        | HirNodeValue::TakeShared(child) => find_id_for_lvalue(child),
        other => panic!("ICE: illegal lvalue: {other:?}"),
    }
}

pub fn merge_results(current: &mut Result<(), LifetimeError>, new: Result<(), LifetimeError>) {
    match (current.as_mut(), new) {
        (Ok(_), Ok(_)) | (Err(_), Ok(_)) => {}
        (Ok(_), new @ Err(_)) => {
            *current = new;
        }
        (Err(old), Err(new)) => match (old, new) {
            (LifetimeError::MultiError(old), LifetimeError::MultiError(new)) => {
                old.extend(new);
            }
            (LifetimeError::MultiError(old), new_error) => {
                old.push(new_error);
            }
            (_, LifetimeError::MultiError(mut list)) => {
                let mut temp = Ok(());
                std::mem::swap(&mut temp, current);
                list.push(temp.unwrap_err());
                *current = Err(LifetimeError::MultiError(list));
            }
            (_, b) => {
                let mut temp = Ok(());
                std::mem::swap(&mut temp, current);
                *current = Err(LifetimeError::MultiError(vec![temp.unwrap_err(), b]));
            }
        },
    }
}
