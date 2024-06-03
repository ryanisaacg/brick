use std::collections::{HashMap, HashSet, VecDeque};

use petgraph::{
    data::DataMap,
    stable_graph::{EdgeIndex, NodeIndex},
    visit::EdgeRef,
    Direction,
};
use thiserror::Error;

use crate::{
    declaration_context::TypeID,
    hir::{HirModule, HirNode},
    id::{AnyID, NodeID, VariableID},
    multi_error::{merge_results, print_multi_errors, MultiError},
    typecheck::{ExpressionType, PointerKind},
    DeclarationContext, HirNodeValue, SourceRange, TypeDeclaration,
};

use self::control_flow_graph::{CfgEdge, CfgNode, ControlFlowGraph, FunctionCFG};

mod control_flow_graph;
mod drop_points;

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
    declarations: &DeclarationContext,
    module: &mut HirModule,
) -> Result<(), LifetimeError> {
    let mut result = Ok(());

    for func in module.functions.iter_mut() {
        let func_result = borrow_check_body(declarations, &mut func.body);
        merge_results(&mut result, func_result);
    }

    let top_level_result = borrow_check_body(declarations, &mut module.top_level_statements);
    merge_results(&mut result, top_level_result);

    result
}

fn borrow_check_body(
    declarations: &DeclarationContext,
    node: &mut HirNode,
) -> Result<(), LifetimeError> {
    let FunctionCFG {
        mut cfg,
        start,
        end,
    } = control_flow_graph::build_control_flow_graph(node);
    let mut block_queue = VecDeque::new();
    block_queue.push_front(start);
    let mut context = Context {
        declarations: &declarations.id_to_decl,
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

    let drop_points = drop_points::calculate_drop_points(&cfg, end);
    drop_points::insert_drops(declarations, node, drop_points);

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
    borrows: Vec<(PointerKind, VariableID)>,
    ty: ExpressionType,
}

#[derive(Clone, Debug)]
enum VariableLifeState {
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
    declarations: &'a HashMap<TypeID, TypeDeclaration>,
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
    let first_node_in_child = cfg.node_weight(block_idx).unwrap().first_node();
    for incoming_edge in cfg.edges_directed(block_idx, Direction::Incoming) {
        let parent = cfg
            .node_weight(incoming_edge.source())
            .unwrap()
            .life_state();
        for (var_id, parent_state) in parent.var_state.iter() {
            var_state
                .entry(*var_id)
                .and_modify(|child_state| {
                    merge_var_states(parent_state, child_state, first_node_in_child)
                })
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

fn merge_var_states(
    parent: &VariableState,
    child: &mut VariableState,
    first_node: Option<&HirNode>,
) {
    match (&parent.state, &mut child.state) {
        (_, VariableLifeState::Moved(_, _)) => {
            // If already moved in child, then ignore all parent beliefs
        }
        (VariableLifeState::Used(_, _), VariableLifeState::Used(node_id, provenance)) => {
            // if more than one parent has used a node, then mark it used at this merge point
            if let Some(first_node) = first_node {
                *node_id = first_node.id;
                *provenance = first_node.provenance.clone();
            }
        }
        (VariableLifeState::Moved(_, _), VariableLifeState::Used(_, _)) => {
            *child = parent.clone();
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
                merge_results(
                    &mut results,
                    update_borrow_state(existing_borrow_state, node),
                );
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
                    InvalidateType::AllBorrows,
                );
            }
        }
        HirNodeValue::Parameter(_, id) => {
            variable_state.insert(
                *id,
                VariableState {
                    state: VariableLifeState::Used(node.id, node.provenance.clone()),
                    borrows: Vec::new(),
                    ty: node.ty.clone(),
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
                        ty: node.ty.clone(),
                    },
                );
            }
        }
        HirNodeValue::Assignment(lhs, rhs) => {
            merge_results(
                &mut results,
                borrow_check_node(ctx, variable_state, borrow_state, rhs),
            );
            let id = find_variable_for_lvalue(lhs);
            if let AnyID::Variable(var_id) = id {
                if let Some(existing_state) = variable_state.get_mut(var_id) {
                    existing_state.state =
                        VariableLifeState::Used(node.id, node.provenance.clone());
                    invalidate_borrowers(
                        existing_state,
                        borrow_state,
                        BorrowLifeState::ValueReassigned(node.id, node.provenance.clone()),
                        InvalidateType::AllBorrows,
                    );
                } else if let ExpressionType::Pointer(ref_ty, _) = &lhs.ty {
                    let lender_id = find_variable_for_lvalue(rhs).as_var();
                    let var_state = variable_state.get_mut(&lender_id).unwrap();
                    borrow_state.get_mut(var_id).unwrap().state =
                        BorrowLifeState::Assigned(lender_id, node.id, node.provenance.clone());

                    invalidate_borrowers(
                        var_state,
                        borrow_state,
                        BorrowLifeState::MutableRefTaken(node.id, node.provenance.clone()),
                        if *ref_ty == PointerKind::Unique {
                            InvalidateType::AllBorrows
                        } else {
                            InvalidateType::MutableBorrows
                        },
                    );

                    var_state.borrows.push((*ref_ty, *var_id));
                }
                // If assignment is to a reference but not to its reference value directly,
                // it's just updating a path within its owner
            }
        }

        HirNodeValue::ArrayIndex(lhs, rhs) | HirNodeValue::DictIndex(lhs, rhs) => {
            merge_results(
                &mut results,
                borrow_check_node(ctx, variable_state, borrow_state, rhs),
            );
            if node.ty.is_affine(ctx.declarations) {
                merge_results(
                    &mut results,
                    borrow_check_node(ctx, variable_state, borrow_state, lhs),
                );
            } else {
                merge_results(
                    &mut results,
                    mark_node_used(variable_state, borrow_state, lhs),
                );
            }
        }
        HirNodeValue::UnionVariant(lhs, _)
        | HirNodeValue::Access(lhs, _)
        | HirNodeValue::NullableTraverse(lhs, _) => {
            if node.ty.is_affine(ctx.declarations) {
                merge_results(
                    &mut results,
                    borrow_check_node(ctx, variable_state, borrow_state, lhs),
                );
            } else {
                merge_results(
                    &mut results,
                    mark_node_used(variable_state, borrow_state, lhs),
                );
            }
        }
        HirNodeValue::Call(_, params)
        | HirNodeValue::VtableCall(_, _, params)
        | HirNodeValue::IntrinsicCall(_, params) => {
            let mut unique_params = HashMap::new();
            let mut shared_params: HashMap<AnyID, Option<SourceRange>> = HashMap::new();
            for param in params.iter() {
                merge_results(
                    &mut results,
                    borrow_check_node(ctx, variable_state, borrow_state, param),
                );
                match &param.value {
                    HirNodeValue::TakeUnique(inner) => {
                        let id = *find_variable_for_lvalue(inner);
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
                            InvalidateType::AllBorrows,
                        );
                    }
                    HirNodeValue::TakeShared(inner) => {
                        let id = *find_variable_for_lvalue(inner);
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
                        invalidate_borrowers(
                            &variable_state[&id.as_var()],
                            borrow_state,
                            BorrowLifeState::MutableRefTaken(node.id, node.provenance.clone()),
                            InvalidateType::MutableBorrows,
                        );
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

        // Only mark as used, don't mark as moved
        HirNodeValue::TakeUnique(inner) | HirNodeValue::TakeShared(inner) => {
            merge_results(
                &mut results,
                mark_node_used(variable_state, borrow_state, inner),
            );
        }

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

fn mark_node_used(
    variable_state: &mut HashMap<VariableID, VariableState>,
    borrow_state: &mut HashMap<VariableID, BorrowState>,
    node: &HirNode,
) -> Result<(), LifetimeError> {
    let mut results = Ok(());
    match &node.value {
        HirNodeValue::VariableReference(var_id) => {
            let AnyID::Variable(var_id) = var_id else {
                return Ok(());
            };
            if let Some(var_state) = variable_state.get_mut(var_id) {
                match &mut var_state.state {
                    VariableLifeState::Used(node_id, provenance) => {
                        *node_id = node.id;
                        *provenance = node.provenance.clone();
                    }
                    VariableLifeState::Moved(_, range) => {
                        merge_results(
                            &mut results,
                            Err(LifetimeError::UseAfterMove {
                                move_point: range.clone(),
                                use_point: node.provenance.clone(),
                            }),
                        );
                    }
                }
            } else if let Some(borrow_state) = borrow_state.get_mut(var_id) {
                merge_results(&mut results, update_borrow_state(borrow_state, node));
            }
        }
        _ => {
            node.children(|child| {
                merge_results(
                    &mut results,
                    mark_node_used(variable_state, borrow_state, child),
                )
            });
        }
    }

    results
}

fn update_borrow_state(
    borrow_state: &mut BorrowState,
    node: &HirNode,
) -> Result<(), LifetimeError> {
    match &mut borrow_state.state {
        BorrowLifeState::Initialized => Ok(()),
        BorrowLifeState::Assigned(_var_id, id, provenance) => {
            *id = node.id;
            *provenance = node.provenance.clone();
            Ok(())
        }
        BorrowLifeState::ValueMoved(_id, move_point) => Err(LifetimeError::BorrowUseAfterMove {
            use_point: node.provenance.clone(),
            move_point: move_point.clone(),
        }),
        BorrowLifeState::ValueReassigned(_id, move_point) => {
            Err(LifetimeError::BorrowUseAfterReassignment {
                use_point: node.provenance.clone(),
                reassign_point: move_point.clone(),
            })
        }
        BorrowLifeState::MutableRefTaken(_id, ref_point) => {
            Err(LifetimeError::BorrowUseAfterMutableRefTaken {
                use_point: node.provenance.clone(),
                ref_point: ref_point.clone(),
            })
        }
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum InvalidateType {
    AllBorrows,
    MutableBorrows,
}

fn invalidate_borrowers(
    variable_state: &VariableState,
    borrow_state: &mut HashMap<VariableID, BorrowState>,
    new_state: BorrowLifeState,
    invalidate_type: InvalidateType,
) {
    for (ref_ty, borrow_id) in variable_state.borrows.iter() {
        if invalidate_type == InvalidateType::MutableBorrows && *ref_ty == PointerKind::Shared {
            continue;
        }
        let existing_borrow_state = borrow_state.get_mut(borrow_id).unwrap();
        existing_borrow_state.state = new_state.clone();
    }
}

fn is_copy(declarations: &HashMap<TypeID, TypeDeclaration>, ty: &ExpressionType) -> bool {
    !ty.is_affine(declarations)
}

fn find_variable_for_lvalue(lvalue: &HirNode) -> &AnyID {
    match &lvalue.value {
        HirNodeValue::VariableReference(id) => id,
        HirNodeValue::NullableTraverse(child, _)
        | HirNodeValue::Access(child, _)
        | HirNodeValue::ArrayIndex(child, _)
        | HirNodeValue::DictIndex(child, _)
        | HirNodeValue::Dereference(child)
        | HirNodeValue::TakeUnique(child)
        | HirNodeValue::TakeShared(child) => find_variable_for_lvalue(child),
        other => panic!("ICE: illegal lvalue: {other:?}"),
    }
}
