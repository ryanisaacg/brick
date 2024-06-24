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
    #[error("borrow used before initialization. this is almost certainly an ICE. {}", maybe_range(.0))]
    UsedUninitBorrow(Option<SourceRange>),
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
    lender_id: VariableID,
    path: Vec<PathSegment>,
    borrows: Vec<(PointerKind, VariableID)>,
}

#[derive(Clone, Debug)]
enum BorrowLifeState {
    Initialized,
    Assigned {
        node_id: NodeID,
        provenance: Option<SourceRange>,
    },
    ValueMoved(Option<SourceRange>),
    ValueReassigned(Option<SourceRange>),
    MutableRefTaken(Option<SourceRange>),
}

#[derive(Clone, PartialEq, Debug)]
enum PathSegment {
    Access(String),
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
    let mut borrow_state = HashMap::new();
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
        for (var_id, parent_state) in parent.borrow_state.iter() {
            borrow_state
                .entry(*var_id)
                .and_modify(|child_state| merge_borrow_states(parent_state, child_state))
                .or_insert(parent_state.clone());
        }
    }
    let block = cfg.node_weight_mut(block_idx).unwrap();
    block.life_state_mut().var_state = var_state;
    block.life_state_mut().borrow_state = borrow_state;
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

fn merge_borrow_states(parent: &BorrowState, child: &mut BorrowState) {
    match (&parent.state, &mut child.state) {
        (_, BorrowLifeState::ValueMoved(_) | BorrowLifeState::ValueReassigned(_)) => {
            // already invalidated
        }
        (
            BorrowLifeState::ValueMoved(_)
            | BorrowLifeState::ValueReassigned(_)
            | BorrowLifeState::MutableRefTaken(_),
            _,
        ) => {
            *child = parent.clone();
        }
        (BorrowLifeState::Initialized, _) | (_, BorrowLifeState::Initialized) => unreachable!(),
        (
            BorrowLifeState::Assigned { .. },
            BorrowLifeState::Assigned { .. } | BorrowLifeState::MutableRefTaken(_),
        ) => {}
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
            if borrow_state.contains_key(var_id) {
                merge_results(
                    &mut results,
                    update_borrow_state(*var_id, variable_state, borrow_state, node),
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
                    *var_id,
                    variable_state,
                    borrow_state,
                    &[],
                    BorrowLifeState::ValueMoved(node.provenance.clone()),
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
                        lender_id: VariableID::dummy(),
                        path: Vec::new(),
                        borrows: Vec::new(),
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
                        *var_id,
                        variable_state,
                        borrow_state,
                        &[],
                        BorrowLifeState::ValueReassigned(node.provenance.clone()),
                        InvalidateType::AllBorrows,
                    );
                } else if let ExpressionType::Pointer(ref_ty, _) = &lhs.ty {
                    let lender_id = find_variable_for_lvalue(rhs).as_var();
                    let path = find_path_for_lvalue(rhs);
                    invalidate_borrowers(
                        lender_id,
                        variable_state,
                        borrow_state,
                        &path[..],
                        BorrowLifeState::MutableRefTaken(node.provenance.clone()),
                        if *ref_ty == PointerKind::Unique {
                            InvalidateType::AllBorrows
                        } else {
                            InvalidateType::MutableBorrows
                        },
                    );

                    let borrow = borrow_state.get_mut(var_id).unwrap();
                    *borrow = BorrowState {
                        lender_id,
                        path,
                        state: BorrowLifeState::Assigned {
                            node_id: node.id,
                            provenance: node.provenance.clone(),
                        },
                        borrows: Vec::new(),
                    };

                    if let Some(borrowed) = borrow_state.get_mut(&lender_id) {
                        borrowed.borrows.push((*ref_ty, *var_id));
                    } else {
                        variable_state
                            .get_mut(&lender_id)
                            .unwrap()
                            .borrows
                            .push((*ref_ty, *var_id));
                    };
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
            let mut shared_params: HashMap<VariableID, (Vec<PathSegment>, Option<SourceRange>)> =
                HashMap::new();
            for param in params.iter() {
                merge_results(
                    &mut results,
                    borrow_check_node(ctx, variable_state, borrow_state, param),
                );
                match &param.value {
                    HirNodeValue::TakeUnique(inner) => {
                        let lender_id = find_variable_for_lvalue(inner).as_var();
                        let path = find_path_for_lvalue(inner);

                        let previous_mut_borrow = unique_params
                            .insert(lender_id, (path.clone(), param.provenance.clone()));
                        if let Some((previous_path, previous_provenance)) = previous_mut_borrow {
                            if !are_paths_disjoint(&path[..], &previous_path[..]) {
                                merge_results(
                                    &mut results,
                                    Err(LifetimeError::BorrowUseAfterMutableRefTaken {
                                        use_point: param.provenance.clone(),
                                        ref_point: previous_provenance.clone(),
                                    }),
                                );
                            }
                        }
                        if let Some((previous_path, previous_shared_borrow)) =
                            shared_params.get(&lender_id)
                        {
                            if !are_paths_disjoint(&path[..], &previous_path[..]) {
                                merge_results(
                                    &mut results,
                                    Err(LifetimeError::UniqueBorrowUseAfterSharedRefTaken {
                                        use_point: param.provenance.clone(),
                                        ref_point: previous_shared_borrow.clone(),
                                    }),
                                );
                            }
                        }

                        invalidate_borrowers(
                            lender_id,
                            variable_state,
                            borrow_state,
                            &path[..],
                            BorrowLifeState::MutableRefTaken(node.provenance.clone()),
                            InvalidateType::AllBorrows,
                        );
                    }
                    HirNodeValue::TakeShared(inner) => {
                        let lender_id = find_variable_for_lvalue(inner).as_var();
                        let path = find_path_for_lvalue(inner);

                        shared_params.insert(lender_id, (path.clone(), param.provenance.clone()));
                        if let Some((previous_path, previous_provenance)) =
                            unique_params.get(&lender_id)
                        {
                            if !are_paths_disjoint(&path[..], &previous_path[..]) {
                                merge_results(
                                    &mut results,
                                    Err(LifetimeError::BorrowUseAfterMutableRefTaken {
                                        use_point: param.provenance.clone(),
                                        ref_point: previous_provenance.clone(),
                                    }),
                                );
                            }
                        }
                        invalidate_borrowers(
                            lender_id,
                            variable_state,
                            borrow_state,
                            &path[..],
                            BorrowLifeState::MutableRefTaken(node.provenance.clone()),
                            InvalidateType::MutableBorrows,
                        );
                    }
                    HirNodeValue::VariableReference(AnyID::Variable(var_id)) => {
                        if let Some(BorrowState {
                            lender_id, path, ..
                        }) = borrow_state.get(var_id)
                        {
                            let lender_id = *lender_id;
                            let previous_mut_borrow = unique_params
                                .insert(lender_id, (path.clone(), param.provenance.clone()));
                            if let Some((previous_path, previous_mut_borrow)) = previous_mut_borrow
                            {
                                if !are_paths_disjoint(&path[..], &previous_path[..]) {
                                    merge_results(
                                        &mut results,
                                        Err(LifetimeError::BorrowUseAfterMutableRefTaken {
                                            use_point: param.provenance.clone(),
                                            ref_point: previous_mut_borrow.clone(),
                                        }),
                                    );
                                }
                            }
                            if let Some((previous_path, previous_shared_borrow)) =
                                shared_params.get(&lender_id)
                            {
                                if !are_paths_disjoint(&path[..], &previous_path[..]) {
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
            } else {
                merge_results(
                    &mut results,
                    update_borrow_state(*var_id, variable_state, borrow_state, node),
                );
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
    borrow_variable: VariableID,
    variable_state: &mut HashMap<VariableID, VariableState>,
    borrow_state: &mut HashMap<VariableID, BorrowState>,
    node: &HirNode,
) -> Result<(), LifetimeError> {
    let borrow = borrow_state.get_mut(&borrow_variable).unwrap();
    match &mut borrow.state {
        BorrowLifeState::Initialized => {
            return Err(LifetimeError::UsedUninitBorrow(node.provenance.clone()))
        }
        BorrowLifeState::Assigned {
            node_id,
            provenance,
        } => {
            *node_id = node.id;
            *provenance = node.provenance.clone();
        }
        BorrowLifeState::ValueMoved(move_point) => {
            return Err(LifetimeError::BorrowUseAfterMove {
                use_point: node.provenance.clone(),
                move_point: move_point.clone(),
            })
        }
        BorrowLifeState::ValueReassigned(move_point) => {
            return Err(LifetimeError::BorrowUseAfterReassignment {
                use_point: node.provenance.clone(),
                reassign_point: move_point.clone(),
            })
        }
        BorrowLifeState::MutableRefTaken(provenance) => {
            return Err(LifetimeError::BorrowUseAfterMutableRefTaken {
                use_point: node.provenance.clone(),
                ref_point: provenance.clone(),
            })
        }
    }
    let lender_id = borrow.lender_id;
    if borrow_state.contains_key(&lender_id) {
        update_borrow_state(lender_id, variable_state, borrow_state, node)
    } else {
        let var_state = variable_state.get_mut(&lender_id).unwrap();
        var_state.state = VariableLifeState::Used(node.id, node.provenance.clone());
        Ok(())
    }
}

#[derive(Clone, Copy, Eq, PartialEq)]
enum InvalidateType {
    AllBorrows,
    MutableBorrows,
}

/**
 * The lender may be either a borrow-of-a-borrow or a borrow-of-a-variable,
 * this method mostly exists to dispatch to invalidate_borrows depending on which it is
 */
fn invalidate_borrowers(
    lender_id: VariableID,
    variable_state: &HashMap<VariableID, VariableState>,
    borrow_state: &mut HashMap<VariableID, BorrowState>,
    borrower_path: &[PathSegment],
    new_state: BorrowLifeState,
    invalidate_type: InvalidateType,
) {
    if let Some(borrowed) = borrow_state.get(&lender_id) {
        // clone to avoid mutable and immutable reference at the same time
        let borrows = borrowed.borrows.clone();
        invalidate_borrows(
            &borrows[..],
            variable_state,
            borrow_state,
            borrower_path,
            new_state,
            invalidate_type,
        );
    } else {
        invalidate_borrows(
            &variable_state[&lender_id].borrows[..],
            variable_state,
            borrow_state,
            borrower_path,
            new_state,
            invalidate_type,
        );
    }
}

fn invalidate_borrows(
    borrows: &[(PointerKind, VariableID)],
    variable_state: &HashMap<VariableID, VariableState>,
    borrow_state: &mut HashMap<VariableID, BorrowState>,
    borrower_path: &[PathSegment],
    new_state: BorrowLifeState,
    invalidate_type: InvalidateType,
) {
    for (ref_ty, borrow_id) in borrows.iter() {
        if invalidate_type == InvalidateType::MutableBorrows && *ref_ty == PointerKind::Shared {
            continue;
        }
        let existing_borrow_state = borrow_state.get_mut(borrow_id).unwrap();
        if are_paths_disjoint(borrower_path, &existing_borrow_state.path) {
            continue;
        }
        existing_borrow_state.state = new_state.clone();
        // recursively invalidate borrowers if necessary
        invalidate_borrowers(
            *borrow_id,
            variable_state,
            borrow_state,
            &[],
            new_state.clone(),
            invalidate_type,
        );
    }
}

/**
 * Check if two paths are disjoint, which means they may be borrowed at the same time
 */
fn are_paths_disjoint(a: &[PathSegment], b: &[PathSegment]) -> bool {
    let len = a.len().min(b.len());
    (0..len).any(|i| a[i] != b[i])
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

fn find_path_for_lvalue(lvalue: &HirNode) -> Vec<PathSegment> {
    let mut path = Vec::new();
    build_path_for_lvalue(lvalue, &mut path);
    path
}

fn build_path_for_lvalue(lvalue: &HirNode, path: &mut Vec<PathSegment>) {
    match &lvalue.value {
        HirNodeValue::VariableReference(_) => {}
        HirNodeValue::NullableTraverse(lhs, fields) => {
            build_path_for_lvalue(lhs, path);
            for field in fields.iter() {
                path.push(PathSegment::Access(field.clone()));
            }
        }
        HirNodeValue::Access(lhs, field) => {
            build_path_for_lvalue(lhs, path);
            path.push(PathSegment::Access(field.clone()));
        }
        HirNodeValue::ArrayIndex(_, _) | HirNodeValue::DictIndex(_, _) => todo!("{lvalue:?}"),
        HirNodeValue::Dereference(child)
        | HirNodeValue::TakeUnique(child)
        | HirNodeValue::TakeShared(child) => {
            build_path_for_lvalue(child, path);
        }
        other => panic!("ICE: illegal lvalue: {other:?}"),
    }
}
