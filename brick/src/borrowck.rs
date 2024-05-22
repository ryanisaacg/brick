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
    multi_error::{merge_results, print_multi_errors, MultiError},
    typecheck::{ExpressionType, PointerKind},
    HirNodeValue, SourceRange, StaticDeclaration,
};

use self::control_flow_graph::{CfgEdge, CfgNode, ControlFlowGraph, FunctionCFG};

mod control_flow_graph;

#[derive(Debug, Error)]
pub enum LifetimeError {
    #[error("{}",  print_multi_errors(&.0[..]))]
    MultiError(Vec<LifetimeError>),
    #[error("use after move{}; moved{}", maybe_range(.use_point), maybe_range(.move_point))]
    UseAfterMove {
        use_point: Option<SourceRange>,
        move_point: Option<SourceRange>,
    },
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
    #[error("unique borrow used after shared reference taken{}; reference taken{}", maybe_range(.use_point), maybe_range(.ref_point))]
    UniqueBorrowUseAfterSharedRefTaken {
        use_point: Option<SourceRange>,
        ref_point: Option<SourceRange>,
    },
}

impl MultiError for LifetimeError {
    fn from_error_list(list: Vec<Self>) -> Self {
        LifetimeError::MultiError(list)
    }

    fn as_error_list(&mut self) -> Option<&mut Vec<Self>> {
        match self {
            LifetimeError::MultiError(list) => Some(list),
            _ => None,
        }
    }
}

fn maybe_range(range: &Option<SourceRange>) -> String {
    if let Some(range) = range {
        format!(": {range}")
    } else {
        " in generated code, this is probably an ICE".to_string()
    }
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
    Initialized,
    Assigned(VariableID, NodeID, Option<SourceRange>),
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
                    BorrowLifeState::Initialized => {}
                    BorrowLifeState::Assigned(_var_id, id, provenance) => {
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
                        state: BorrowLifeState::Initialized,
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
                    borrow_state.get_mut(var_id).unwrap().state =
                        BorrowLifeState::Assigned(lender_id, node.id, node.provenance.clone());

                    if *ref_ty == PointerKind::Unique {
                        invalidate_borrowers(
                            var_state,
                            borrow_state,
                            BorrowLifeState::MutableRefTaken(node.id, node.provenance.clone()),
                        );
                    }
                    // TODO: invalidate mutable references

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
        HirNodeValue::UnionVariant(_, _) => {}

        HirNodeValue::Call(_, params)
        | HirNodeValue::VtableCall(_, _, params)
        | HirNodeValue::RuntimeCall(_, params) => {
            let mut unique_params = HashMap::new();
            let mut shared_params: HashMap<AnyID, Option<SourceRange>> = HashMap::new();
            for param in params.iter() {
                merge_results(
                    &mut results,
                    borrow_check_node(ctx, variable_state, borrow_state, param),
                );
                match &param.value {
                    HirNodeValue::TakeUnique(inner) => {
                        let id = *find_id_for_lvalue(inner);
                        let previous_mut_borrow =
                            unique_params.insert(id, param.provenance.clone());
                        if let Some(previous_mut_borrow) = previous_mut_borrow {
                            merge_results(
                                &mut results,
                                Err(LifetimeError::BorrowUseAfterMutableRefTaken {
                                    use_point: param.provenance.clone(),
                                    ref_point: previous_mut_borrow.clone(),
                                }),
                            );
                        }
                        if let Some(previous_shared_borrow) = shared_params.get(&id) {
                            merge_results(
                                &mut results,
                                Err(LifetimeError::UniqueBorrowUseAfterSharedRefTaken {
                                    use_point: param.provenance.clone(),
                                    ref_point: previous_shared_borrow.clone(),
                                }),
                            );
                        }
                        invalidate_borrowers(
                            &variable_state[&id.as_var()],
                            borrow_state,
                            BorrowLifeState::MutableRefTaken(node.id, node.provenance.clone()),
                        );
                    }
                    HirNodeValue::TakeShared(inner) => {
                        let id = *find_id_for_lvalue(inner);
                        shared_params.insert(id, param.provenance.clone());
                        if let Some(previous_mut_borrow) = unique_params.get(&id) {
                            merge_results(
                                &mut results,
                                Err(LifetimeError::BorrowUseAfterMutableRefTaken {
                                    use_point: param.provenance.clone(),
                                    ref_point: previous_mut_borrow.clone(),
                                }),
                            );
                        }
                        // TODO: invalidate mutable borrows
                    }
                    HirNodeValue::VariableReference(AnyID::Variable(var_id)) => {
                        if let Some(BorrowState {
                            state: BorrowLifeState::Assigned(lender_id, _, _),
                        }) = borrow_state.get(var_id)
                        {
                            let lender_id = (*lender_id).into();
                            let previous_mut_borrow =
                                unique_params.insert(lender_id, param.provenance.clone());
                            if let Some(previous_mut_borrow) = previous_mut_borrow {
                                merge_results(
                                    &mut results,
                                    Err(LifetimeError::BorrowUseAfterMutableRefTaken {
                                        use_point: param.provenance.clone(),
                                        ref_point: previous_mut_borrow.clone(),
                                    }),
                                );
                            }
                            if let Some(previous_shared_borrow) = shared_params.get(&lender_id) {
                                merge_results(
                                    &mut results,
                                    Err(LifetimeError::UniqueBorrowUseAfterSharedRefTaken {
                                        use_point: param.provenance.clone(),
                                        ref_point: previous_shared_borrow.clone(),
                                    }),
                                );
                            }
                        }
                    }
                    _ => {}
                }
            }
        }

        // TODO: children are borrowed, not moved
        HirNodeValue::TakeUnique(_) => {}
        HirNodeValue::TakeShared(_) => {}

        // Union tags are always safe to retrieve
        HirNodeValue::UnionTag(_) => {}

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

fn is_copy(declarations: &HashMap<TypeID, &StaticDeclaration>, ty: &ExpressionType) -> bool {
    !ty.is_affine(declarations)
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
