use std::collections::{HashMap, VecDeque};

use crate::{
    hir::{
        ArithmeticOp, BinaryLogicalOp, ComparisonOp, HirFunction, HirNode, HirNodeValue,
        UnaryLogicalOp,
    },
    id::{FunctionID, RegisterID, TypeID, VariableID},
    provenance::SourceRange,
    runtime::RuntimeFunction,
    typecheck::{CollectionType, ExpressionType, PrimitiveType, StaticDeclaration},
};

#[derive(Debug)]
pub struct LinearFunction {
    pub id: FunctionID,
    pub body: Vec<LinearNode>,
}

pub fn linearize_function(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    function: HirFunction,
) -> LinearFunction {
    let HirNodeValue::Sequence(block) = function.body.value else {
        unreachable!()
    };
    let mut initial_offset = std::mem::size_of::<usize>();
    if function.is_generator {
        initial_offset += std::mem::size_of::<usize>();
    }
    let body = linearize_nodes(
        declarations,
        &mut HashMap::new(),
        &mut initial_offset,
        block.into(),
    );
    LinearFunction {
        id: function.id,
        body,
    }
}

#[derive(Clone, Debug)]
pub struct LinearNode {
    pub value: LinearNodeValue,
    pub provenance: Option<SourceRange>,
}

impl LinearNode {
    pub fn new(value: LinearNodeValue) -> LinearNode {
        LinearNode {
            value,
            provenance: None,
        }
    }

    fn if_node(
        cond: LinearNode,
        if_block: Vec<LinearNode>,
        else_block: Option<Vec<LinearNode>>,
        provenance: Option<SourceRange>,
    ) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::If(Box::new(cond), if_block, else_block),
            provenance,
        }
    }

    fn ptr_arithmetic(op: ArithmeticOp, lhs: LinearNode, rhs: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Arithmetic(
                op,
                PhysicalPrimitive::PointerSize,
                Box::new(lhs),
                Box::new(rhs),
            ),
            provenance: None,
        }
    }

    fn ptr_comparison(op: ComparisonOp, lhs: LinearNode, rhs: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Comparison(
                op,
                PhysicalPrimitive::PointerSize,
                Box::new(lhs),
                Box::new(rhs),
            ),
            provenance: None,
        }
    }

    fn size(size: usize) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Size(size),
            provenance: None,
        }
    }

    fn heap_alloc_const(size: usize) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::HeapAlloc(Box::new(LinearNode::size(size))),
            provenance: None,
        }
    }

    fn heap_alloc_var(size: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::HeapAlloc(Box::new(size)),
            provenance: None,
        }
    }

    fn write_register(id: RegisterID, value: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::WriteRegister(id, Box::new(value)),
            provenance: None,
        }
    }

    fn read_register(id: RegisterID) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::ReadRegister(id),
            provenance: None,
        }
    }

    fn kill_register(id: RegisterID) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::KillRegister(id),
            provenance: None,
        }
    }

    fn read_memory(location: LinearNode, offset: usize, ty: PhysicalType) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset,
                ty,
            },
            provenance: None,
        }
    }

    fn write_memory(
        location: LinearNode,
        offset: usize,
        ty: PhysicalType,
        value: LinearNode,
    ) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::WriteMemory {
                location: Box::new(location),
                offset,
                ty,
                value: Box::new(value),
            },
            provenance: None,
        }
    }

    fn abort() -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Abort,
            provenance: None,
        }
    }

    #[allow(dead_code)]
    fn debug(inner: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Debug(Box::new(inner)),
            provenance: None,
        }
    }

    fn bool_value(val: bool) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::Byte(if val { 1 } else { 0 }),
            provenance: None,
        }
    }

    fn memcpy(source: LinearNode, dest: LinearNode, size: LinearNode) -> LinearNode {
        LinearNode {
            value: LinearNodeValue::MemoryCopy {
                source: Box::new(source),
                dest: Box::new(dest),
                size: Box::new(size),
            },
            provenance: None,
        }
    }
}

// TODO: split up between 'statement' and 'expression' to reduce need for boxing?
#[derive(Clone, Debug)]
pub enum LinearNodeValue {
    // TODO: how to handle function parameters?

    // Memory
    StackFrame,
    StackAlloc(usize),
    StackDealloc(usize),
    /// Returns the heap pointer
    HeapAlloc(Box<LinearNode>),
    /// Each parameter may only appear once in a given method body
    Parameter(usize),
    ReadMemory {
        location: Box<LinearNode>,
        offset: usize,
        ty: PhysicalType,
    },
    WriteMemory {
        location: Box<LinearNode>,
        offset: usize,
        ty: PhysicalType,
        value: Box<LinearNode>,
    },
    MemoryCopy {
        source: Box<LinearNode>,
        dest: Box<LinearNode>,
        size: Box<LinearNode>,
    },

    // TODO: just full stack machine?
    TopOfStack,
    Discard,

    // Control flow
    Call(Box<LinearNode>, Vec<LinearNode>),
    Return(Option<Box<LinearNode>>),
    If(Box<LinearNode>, Vec<LinearNode>, Option<Vec<LinearNode>>),
    // TODO: labelled breaks?
    Break,
    Loop(Vec<LinearNode>),
    // TODO: stack unwind?
    Abort,

    Sequence(Vec<LinearNode>),
    WriteRegister(RegisterID, Box<LinearNode>),
    ReadRegister(RegisterID),
    // TODO: automatically?
    KillRegister(RegisterID),

    Arithmetic(
        ArithmeticOp,
        PhysicalPrimitive,
        Box<LinearNode>,
        Box<LinearNode>,
    ),
    Comparison(
        ComparisonOp,
        PhysicalPrimitive,
        Box<LinearNode>,
        Box<LinearNode>,
    ),
    BinaryLogical(BinaryLogicalOp, Box<LinearNode>, Box<LinearNode>),
    UnaryLogical(UnaryLogicalOp, Box<LinearNode>),
    Cast {
        value: Box<LinearNode>,
        from: PhysicalPrimitive,
        to: PhysicalPrimitive,
    },
    Size(usize),
    Int(i64),
    Float(f64),
    CharLiteral(char),
    Byte(u8),
    FunctionID(FunctionID),

    // Probably not keeping this around forever
    #[allow(dead_code)]
    Debug(Box<LinearNode>),
}

// TODO: produce a more CFG shaped result?

pub fn linearize_nodes(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    stack_entries: &mut HashMap<VariableID, usize>,
    stack_offset: &mut usize,
    mut nodes: VecDeque<HirNode>,
) -> Vec<LinearNode> {
    let mut values = Vec::new();

    while let Some(node) = nodes.pop_front() {
        match node.value {
            HirNodeValue::Sequence(inner_nodes) => {
                nodes.reserve(inner_nodes.len());
                for node in inner_nodes.into_iter().rev() {
                    nodes.push_front(node);
                }
            }
            HirNodeValue::Declaration(id) => {
                let alloc_size = expression_type_size(declarations, &node.ty);
                values.push(stack_alloc(stack_entries, stack_offset, alloc_size, id));
            }
            HirNodeValue::Parameter(idx, id) => {
                let alloc_size = expression_type_size(declarations, &node.ty);
                values.push(stack_alloc(stack_entries, stack_offset, alloc_size, id));

                let (location, offset) = variable_location(stack_entries, id);
                let ty = expr_ty_to_physical(&node.ty);

                values.push(LinearNode::write_memory(
                    location,
                    offset,
                    ty,
                    LinearNode::new(LinearNodeValue::Parameter(idx)),
                ));
            }
            HirNodeValue::Assignment(lhs, rhs) => {
                let ty = expr_ty_to_physical(&lhs.ty);
                let (location, offset) =
                    lower_lvalue(declarations, stack_entries, stack_offset, *lhs);
                let rhs = lower_expression(declarations, stack_entries, stack_offset, *rhs);
                values.push(LinearNode {
                    value: LinearNodeValue::WriteMemory {
                        location: Box::new(location),
                        offset,
                        value: Box::new(rhs),
                        ty,
                    },
                    provenance: node.provenance,
                });
            }

            HirNodeValue::Return(expr) => {
                let expr = expr.map(|expr| {
                    Box::new(lower_expression(
                        declarations,
                        stack_entries,
                        stack_offset,
                        *expr,
                    ))
                });
                values.push(LinearNode {
                    value: LinearNodeValue::Return(expr),
                    provenance: node.provenance,
                });
            }
            // TODO: should If be an expression in linear IR?
            HirNodeValue::If(cond, if_block, else_block) => {
                let cond = lower_expression(declarations, stack_entries, stack_offset, *cond);
                let HirNodeValue::Sequence(if_block) = if_block.value else {
                    unreachable!()
                };
                let if_block =
                    linearize_nodes(declarations, stack_entries, stack_offset, if_block.into());
                let else_block = else_block.map(|else_block| {
                    let HirNodeValue::Sequence(else_block) = else_block.value else {
                        unreachable!()
                    };
                    linearize_nodes(declarations, stack_entries, stack_offset, else_block.into())
                });
                values.push(LinearNode::if_node(
                    cond,
                    if_block,
                    else_block,
                    node.provenance,
                ));
            }
            HirNodeValue::While(cond, block) => {
                let cond = lower_expression(declarations, stack_entries, stack_offset, *cond);
                let mut vec_deque = VecDeque::new();
                vec_deque.push_back(*block);
                let block = linearize_nodes(declarations, stack_entries, stack_offset, vec_deque);
                values.push(LinearNode {
                    value: LinearNodeValue::Loop(vec![LinearNode::if_node(
                        cond,
                        block,
                        Some(vec![LinearNode::new(LinearNodeValue::Break)]),
                        None,
                    )]),
                    provenance: node.provenance,
                })
            }

            // TODO: auto-returns?
            _ => {
                values.push(lower_expression(
                    declarations,
                    stack_entries,
                    stack_offset,
                    node,
                ));
            }
        }
    }

    values
}

fn lower_expression(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    stack_entries: &mut HashMap<VariableID, usize>,
    stack_offset: &mut usize,
    expression: HirNode,
) -> LinearNode {
    let HirNode {
        id: _,
        value,
        ty,
        provenance,
    } = expression;
    let value = match value {
        HirNodeValue::Int(x) => LinearNodeValue::Int(x),
        HirNodeValue::PointerSize(x) => LinearNodeValue::Size(x),
        HirNodeValue::Float(x) => LinearNodeValue::Float(x),
        HirNodeValue::Bool(x) => LinearNodeValue::Byte(if x { 1 } else { 0 }),
        HirNodeValue::Null => LinearNodeValue::Byte(0),
        // TODO: store resume points somewhere else?
        HirNodeValue::ResumePoint => LinearNodeValue::ReadMemory {
            location: Box::new(LinearNode::new(LinearNodeValue::TopOfStack)),
            offset: std::mem::size_of::<usize>(),
            ty: PhysicalType::Primitive(PhysicalPrimitive::PointerSize),
        },
        HirNodeValue::CharLiteral(x) => LinearNodeValue::CharLiteral(x),
        HirNodeValue::StringLiteral(_x) => todo!(),

        HirNodeValue::Arithmetic(op, lhs, rhs) => {
            let ExpressionType::Primitive(ty) = rhs.ty else {
                unreachable!("binoperands must be primitive not {:?}", ty)
            };
            let ty = primitive_to_physical(ty);
            LinearNodeValue::Arithmetic(
                op,
                ty,
                Box::new(lower_expression(
                    declarations,
                    stack_entries,
                    stack_offset,
                    *lhs,
                )),
                Box::new(lower_expression(
                    declarations,
                    stack_entries,
                    stack_offset,
                    *rhs,
                )),
            )
        }
        HirNodeValue::Comparison(op, lhs, rhs) => {
            let ExpressionType::Primitive(ty) = rhs.ty else {
                unreachable!("binoperands must be primitive not {:?}", ty)
            };
            let ty = primitive_to_physical(ty);
            LinearNodeValue::Comparison(
                op,
                ty,
                Box::new(lower_expression(
                    declarations,
                    stack_entries,
                    stack_offset,
                    *lhs,
                )),
                Box::new(lower_expression(
                    declarations,
                    stack_entries,
                    stack_offset,
                    *rhs,
                )),
            )
        }
        HirNodeValue::BinaryLogical(op, lhs, rhs) => LinearNodeValue::BinaryLogical(
            op,
            Box::new(lower_expression(
                declarations,
                stack_entries,
                stack_offset,
                *lhs,
            )),
            Box::new(lower_expression(
                declarations,
                stack_entries,
                stack_offset,
                *rhs,
            )),
        ),
        HirNodeValue::UnaryLogical(op, child) => LinearNodeValue::UnaryLogical(
            op,
            Box::new(lower_expression(
                declarations,
                stack_entries,
                stack_offset,
                *child,
            )),
        ),
        HirNodeValue::VariableReference(id) => {
            let (location, offset) = variable_location(stack_entries, id.as_var());
            let ty = expr_ty_to_physical(&ty);
            LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset,
                ty,
            }
        }
        HirNodeValue::Call(lhs, params) => {
            let HirNodeValue::VariableReference(fn_id) = lhs.value else {
                unreachable!("lhs of function call must be a function ID")
            };
            let params = params
                .into_iter()
                .map(|param| lower_expression(declarations, stack_entries, stack_offset, param))
                .collect();
            LinearNodeValue::Call(
                Box::new(LinearNode::new(LinearNodeValue::FunctionID(fn_id.as_fn()))),
                params,
            )
        }
        HirNodeValue::Access(lhs, rhs) => {
            if let Some(ty) = lhs.ty.id().and_then(|id| match &declarations[id].value {
                TypeLayoutValue::Union(ty) => Some(ty),
                _ => None,
            }) {
                let (variant_idx, variant_ty) = &ty[&rhs];
                let (location, offset) =
                    lower_lvalue(declarations, stack_entries, stack_offset, *lhs);
                LinearNodeValue::Sequence(vec![LinearNode::if_node(
                    LinearNode::ptr_comparison(
                        ComparisonOp::NotEquals,
                        LinearNode::size(*variant_idx),
                        LinearNode::read_memory(location.clone(), offset, PhysicalType::Pointer),
                    ),
                    vec![LinearNode::bool_value(false)],
                    Some(vec![
                        LinearNode::read_memory(
                            location,
                            offset + UNION_TAG_SIZE,
                            variant_ty.clone(),
                        ),
                        LinearNode::bool_value(true),
                    ]),
                    None,
                )])
            } else {
                let (location, offset) =
                    access_location(declarations, stack_entries, stack_offset, *lhs, rhs);
                LinearNodeValue::ReadMemory {
                    location: Box::new(location),
                    offset,
                    ty: expr_ty_to_physical(&ty),
                }
            }
        }
        HirNodeValue::NullableTraverse(lhs, rhs) => {
            let temp_id = VariableID::new();
            let alloc_size = expression_type_size(declarations, &lhs.ty);
            let alloc = stack_alloc(stack_entries, stack_offset, alloc_size, temp_id);
            let (location, initial_offset) = variable_location(stack_entries, temp_id);
            stack_entries.remove(&temp_id);

            let PhysicalType::Nullable(ty) = expr_ty_to_physical(&lhs.ty) else {
                unreachable!();
            };
            let mut ty = *ty;

            let mut read_offset = initial_offset + NULL_TAG_SIZE;
            for name in rhs {
                let PhysicalType::Referenced(id) = ty else {
                    unreachable!()
                };
                let decl = &declarations[&id];
                let TypeLayoutValue::Structure(fields) = &decl.value else {
                    todo!()
                };
                let (field_offset, field_ty) = fields
                    .iter()
                    .find_map(|(field_name, offset, ty)| {
                        if field_name == &name {
                            Some((offset, ty))
                        } else {
                            None
                        }
                    })
                    .unwrap();
                read_offset += field_offset;
                ty = field_ty.clone();
            }

            // TODO: union RHS

            let lhs_ty = expr_ty_to_physical(&lhs.ty);

            LinearNodeValue::Sequence(vec![
                alloc,
                LinearNode::write_memory(
                    location.clone(),
                    initial_offset,
                    lhs_ty,
                    lower_expression(declarations, stack_entries, stack_offset, *lhs),
                ),
                LinearNode::if_node(
                    LinearNode::read_memory(
                        location.clone(),
                        initial_offset,
                        PhysicalType::Primitive(PhysicalPrimitive::Byte),
                    ),
                    vec![
                        LinearNode::read_memory(location, read_offset, ty),
                        LinearNode::bool_value(true),
                    ],
                    Some(vec![LinearNode::bool_value(false)]),
                    provenance.clone(),
                ),
                LinearNode::new(LinearNodeValue::StackDealloc(alloc_size)),
            ])
        }
        HirNodeValue::ArrayIndex(arr, idx) => {
            let (location, offset) =
                array_index_location(declarations, stack_entries, stack_offset, *arr, *idx, &ty);
            LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset,
                ty: expr_ty_to_physical(&ty),
            }
        }

        HirNodeValue::If(_, _, _) | HirNodeValue::While(_, _) | HirNodeValue::Return(_) => {
            unreachable!("all control flow must be removed from expressions in HIR")
        }

        HirNodeValue::Parameter(_, _) => todo!(),
        HirNodeValue::Declaration(_) => todo!(),
        HirNodeValue::Assignment(_, _) => todo!(),
        HirNodeValue::TakeUnique(inner) | HirNodeValue::TakeShared(inner) => {
            let (ptr, offset) = lower_lvalue(declarations, stack_entries, stack_offset, *inner);
            LinearNodeValue::Arithmetic(
                ArithmeticOp::Add,
                PhysicalPrimitive::PointerSize,
                Box::new(ptr),
                Box::new(LinearNode::size(offset)),
            )
        }
        HirNodeValue::Dereference(inner) => LinearNodeValue::ReadMemory {
            location: Box::new(lower_expression(
                declarations,
                stack_entries,
                stack_offset,
                *inner,
            )),
            offset: 0,
            ty: expr_ty_to_physical(&ty),
        },
        HirNodeValue::Sequence(_) => todo!(),
        HirNodeValue::StructLiteral(struct_id, mut values) => {
            let Some(DeclaredTypeLayout {
                value: TypeLayoutValue::Structure(layouts),
                ..
            }) = declarations.get(&struct_id)
            else {
                unreachable!()
            };
            // TODO: push this down a layer of abstraction
            LinearNodeValue::Sequence(
                layouts
                    .iter()
                    .rev()
                    .map(|(key, _, _)| values.remove(key).unwrap())
                    .map(|value| lower_expression(declarations, stack_entries, stack_offset, value))
                    .collect(),
            )
        }
        HirNodeValue::ArrayLiteral(values) => {
            let ExpressionType::Collection(CollectionType::Array(inner_ty)) = ty else {
                unreachable!()
            };
            let size = expression_type_size(declarations, &inner_ty);

            let length = values.len();

            let inner_ty = *inner_ty;
            let buffer_ptr = RegisterID::new();

            let mut instrs = vec![LinearNode::write_register(
                buffer_ptr,
                LinearNode::heap_alloc_const(size * values.len()),
            )];
            instrs.extend(values.into_iter().enumerate().map(|(idx, value)| {
                LinearNode::write_memory(
                    LinearNode::read_register(buffer_ptr),
                    size * idx,
                    expr_ty_to_physical(&inner_ty),
                    lower_expression(declarations, stack_entries, stack_offset, value),
                )
            }));
            // capacity
            instrs.push(LinearNode::size(length));
            // length
            instrs.push(LinearNode::size(length));
            instrs.push(LinearNode::read_register(buffer_ptr));
            instrs.push(LinearNode::kill_register(buffer_ptr));

            LinearNodeValue::Sequence(instrs)
        }
        HirNodeValue::ArrayLiteralLength(value, length) => {
            let ExpressionType::Collection(CollectionType::Array(inner_ty)) = ty else {
                unreachable!()
            };
            let size = expression_type_size(declarations, &inner_ty);
            let length = lower_expression(declarations, stack_entries, stack_offset, *length);
            let inner_ty = *inner_ty;

            let length_register = RegisterID::new();
            let buffer_register = RegisterID::new();
            let index_register = RegisterID::new();

            LinearNodeValue::Sequence(vec![
                LinearNode::write_register(length_register, length),
                LinearNode::write_register(
                    buffer_register,
                    LinearNode::new(LinearNodeValue::HeapAlloc(Box::new(
                        LinearNode::ptr_arithmetic(
                            ArithmeticOp::Multiply,
                            LinearNode::size(size),
                            LinearNode::read_register(length_register),
                        ),
                    ))),
                ),
                LinearNode::write_register(index_register, LinearNode::size(0)),
                LinearNode::new(LinearNodeValue::Loop(vec![LinearNode::if_node(
                    LinearNode::ptr_comparison(
                        ComparisonOp::EqualTo,
                        LinearNode::read_register(index_register),
                        LinearNode::read_register(length_register),
                    ),
                    vec![LinearNode::new(LinearNodeValue::Break)],
                    Some(vec![
                        // *(ptr + idx * size) = value
                        LinearNode::write_memory(
                            LinearNode::ptr_arithmetic(
                                ArithmeticOp::Add,
                                LinearNode::read_register(buffer_register),
                                LinearNode::ptr_arithmetic(
                                    ArithmeticOp::Add,
                                    LinearNode::size(size),
                                    LinearNode::read_register(index_register),
                                ),
                            ),
                            0,
                            expr_ty_to_physical(&inner_ty),
                            lower_expression(declarations, stack_entries, stack_offset, *value),
                        ),
                        LinearNode::write_register(
                            index_register,
                            LinearNode::ptr_arithmetic(
                                ArithmeticOp::Add,
                                LinearNode::read_register(index_register),
                                LinearNode::size(1),
                            ),
                        ),
                    ]),
                    None,
                )])),
                // Return values
                LinearNode::read_register(length_register),
                LinearNode::read_register(length_register),
                LinearNode::read_register(buffer_register),
                // Cleanup
                LinearNode::kill_register(length_register),
                LinearNode::kill_register(index_register),
                LinearNode::kill_register(buffer_register),
            ])
        }
        HirNodeValue::InterfaceAddress(table) => {
            let (table, offset) = lower_lvalue(declarations, stack_entries, stack_offset, *table);
            LinearNodeValue::ReadMemory {
                location: Box::new(table),
                offset,
                ty: PhysicalType::Pointer,
            }
        }
        HirNodeValue::VtableCall(table, fn_id, params) => {
            let ExpressionType::InstanceOf(ty_id) = &table.ty else {
                unreachable!()
            };
            let Some(DeclaredTypeLayout {
                value: TypeLayoutValue::Interface(fields),
                ..
            }) = declarations.get(ty_id)
            else {
                unreachable!()
            };
            let (table, mut offset) =
                lower_lvalue(declarations, stack_entries, stack_offset, *table);
            offset += POINTER_SIZE;
            offset += fields
                .iter()
                .enumerate()
                .find_map(|(idx, id)| {
                    if *id == fn_id {
                        Some(idx * FUNCTION_ID_SIZE)
                    } else {
                        None
                    }
                })
                .unwrap();
            let params = params
                .into_iter()
                .map(|param| lower_expression(declarations, stack_entries, stack_offset, param))
                .collect();

            LinearNodeValue::Call(
                Box::new(LinearNode::new(LinearNodeValue::ReadMemory {
                    location: Box::new(table),
                    offset,
                    ty: PhysicalType::FunctionPointer,
                })),
                params,
            )
        }
        HirNodeValue::StructToInterface { value, vtable } => {
            let mut values = Vec::new();

            let ExpressionType::InstanceOf(ty_id) = &ty else {
                unreachable!()
            };
            let Some(DeclaredTypeLayout {
                value: TypeLayoutValue::Interface(fields),
                ..
            }) = declarations.get(ty_id)
            else {
                unreachable!()
            };
            for field in fields.iter().rev() {
                values.push(LinearNode::new(LinearNodeValue::FunctionID(vtable[field])));
            }

            let (pointer, offset) = lower_lvalue(declarations, stack_entries, stack_offset, *value);
            let pointer =
                LinearNode::ptr_arithmetic(ArithmeticOp::Add, pointer, LinearNode::size(offset));
            values.push(pointer);

            LinearNodeValue::Sequence(values)
        }
        HirNodeValue::NumericCast { value, from, to } => LinearNodeValue::Cast {
            value: Box::new(lower_expression(
                declarations,
                stack_entries,
                stack_offset,
                *value,
            )),
            from: primitive_to_physical(from),
            to: primitive_to_physical(to),
        },
        HirNodeValue::DictIndex(dict, idx) => {
            let (location, offset) = dict_index_location_or_abort(
                declarations,
                stack_entries,
                stack_offset,
                *dict,
                *idx,
            );
            LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset,
                ty: expr_ty_to_physical(&ty),
            }
        }
        HirNodeValue::DictLiteral(entries) => {
            let ExpressionType::Collection(CollectionType::Dict(key_ty, value_ty)) = ty else {
                unreachable!()
            };
            let key_ty = expr_ty_to_physical(&key_ty);
            let value_ty = expr_ty_to_physical(&value_ty);

            let key_size = key_ty.size(declarations);
            let value_size = value_ty.size(declarations);
            let entry_size = key_size + value_size;

            let length = entries.len();

            let buffer = RegisterID::new();

            let mut instrs = vec![LinearNode::write_register(
                buffer,
                LinearNode::heap_alloc_const(entry_size * length),
            )];
            let (keys, values): (Vec<_>, Vec<_>) = entries.into_iter().unzip();
            instrs.extend(keys.into_iter().enumerate().map(|(idx, value)| {
                LinearNode::write_memory(
                    LinearNode::read_register(buffer),
                    entry_size * idx,
                    key_ty.clone(),
                    lower_expression(declarations, stack_entries, stack_offset, value),
                )
            }));
            instrs.extend(values.into_iter().enumerate().map(|(idx, value)| {
                LinearNode::write_memory(
                    LinearNode::read_register(buffer),
                    entry_size * idx + key_size,
                    value_ty.clone(),
                    lower_expression(declarations, stack_entries, stack_offset, value),
                )
            }));
            instrs.extend([
                LinearNode::size(length),
                LinearNode::size(length),
                LinearNode::read_register(buffer),
                LinearNode::kill_register(buffer),
            ]);

            LinearNodeValue::Sequence(instrs)
        }
        HirNodeValue::UnionLiteral(ty, variant, value) => {
            let TypeLayoutValue::Union(ty) = &declarations[&ty].value else {
                unreachable!()
            };
            LinearNodeValue::Sequence(vec![
                lower_expression(declarations, stack_entries, stack_offset, *value),
                LinearNode::new(LinearNodeValue::Size(ty[&variant].0)),
            ])
        }
        HirNodeValue::NullCoalesce(lhs, rhs) => LinearNodeValue::If(
            Box::new(LinearNode::new(LinearNodeValue::UnaryLogical(
                UnaryLogicalOp::BooleanNot,
                Box::new(lower_expression(
                    declarations,
                    stack_entries,
                    stack_offset,
                    *lhs,
                )),
            ))),
            vec![lower_expression(
                declarations,
                stack_entries,
                stack_offset,
                *rhs,
            )],
            None,
        ),
        HirNodeValue::MakeNullable(value) => LinearNodeValue::Sequence(vec![
            lower_expression(declarations, stack_entries, stack_offset, *value),
            LinearNode::bool_value(true),
        ]),
        HirNodeValue::RuntimeCall(RuntimeFunction::ArrayLength, mut args) => {
            let HirNodeValue::TakeShared(arr) = args.remove(0).value else {
                unreachable!()
            };
            let (location, offset) = lower_lvalue(declarations, stack_entries, stack_offset, *arr);
            LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset: offset + POINTER_SIZE,
                ty: PhysicalType::Pointer,
            }
        }
        HirNodeValue::RuntimeCall(RuntimeFunction::ArrayPush, mut args) => {
            let inserted = args.pop().unwrap();
            let HirNodeValue::TakeUnique(arr) = args.pop().unwrap().value else {
                unreachable!()
            };
            let ExpressionType::Collection(CollectionType::Array(array_inner_ty)) = &arr.ty else {
                unreachable!()
            };
            let array_inner_ty = expr_ty_to_physical(array_inner_ty);
            let (location, offset) = lower_lvalue(declarations, stack_entries, stack_offset, *arr);

            let inserted = lower_expression(declarations, stack_entries, stack_offset, inserted);

            LinearNodeValue::WriteMemory {
                location: Box::new(LinearNode::new(array_alloc_space_to_push(
                    location,
                    offset,
                    array_inner_ty.size(declarations),
                    provenance.clone(),
                ))),
                offset: 0,
                ty: array_inner_ty,
                value: Box::new(inserted),
            }
        }
        HirNodeValue::RuntimeCall(RuntimeFunction::DictionaryInsert, mut args) => {
            let value = args.pop().unwrap();
            let key = args.pop().unwrap();
            let HirNodeValue::TakeUnique(dict) = args.pop().unwrap().value else {
                unreachable!()
            };
            let ExpressionType::Collection(CollectionType::Dict(key_ty, value_ty)) = &dict.ty
            else {
                unreachable!()
            };

            let key_ty = expr_ty_to_physical(key_ty);
            let value_ty = expr_ty_to_physical(value_ty);
            let key_size = key_ty.size(declarations);
            let entry_size = key_size + value_ty.size(declarations);
            let PhysicalType::Primitive(key_ty) = key_ty else {
                unreachable!()
            };

            let (dict_location, dict_offset) =
                lower_lvalue(declarations, stack_entries, stack_offset, *dict.clone());

            let key = lower_expression(declarations, stack_entries, stack_offset, key);
            let value = lower_expression(declarations, stack_entries, stack_offset, value);

            let temp_key_id = VariableID::new();
            let alloc = stack_alloc(stack_entries, stack_offset, key_size, temp_key_id);
            let (temp_key_location, temp_key_offset) =
                variable_location(stack_entries, temp_key_id);

            let ptr = RegisterID::new();
            let entry_register = RegisterID::new();

            LinearNodeValue::Sequence(vec![
                alloc,
                // pointer to dict
                LinearNode::write_register(ptr, dict_location),
                LinearNode::write_memory(
                    temp_key_location.clone(),
                    temp_key_offset,
                    PhysicalType::Primitive(key_ty),
                    key.clone(),
                ),
                LinearNode::if_node(
                    dict_get_entry_for_key(
                        LinearNode::read_memory(
                            LinearNode::read_register(ptr),
                            dict_offset,
                            PhysicalType::Collection(PhysicalCollection::Dict),
                        ),
                        temp_key_location,
                        temp_key_offset,
                        key_ty,
                        entry_size,
                    ),
                    vec![
                        // pointer to existing entry
                        LinearNode::write_register(
                            entry_register,
                            LinearNode::new(LinearNodeValue::TopOfStack),
                        ),
                        LinearNode::write_memory(
                            LinearNode::read_register(entry_register),
                            key_size,
                            value_ty.clone(),
                            value.clone(),
                        ),
                    ],
                    Some(vec![
                        // Pointer to newly allocated entry
                        LinearNode::write_register(
                            entry_register,
                            LinearNode::new(array_alloc_space_to_push(
                                LinearNode::read_register(ptr),
                                dict_offset,
                                entry_size,
                                None,
                            )),
                        ),
                        LinearNode::write_memory(
                            LinearNode::read_register(entry_register),
                            0,
                            PhysicalType::Primitive(key_ty),
                            key,
                        ),
                        LinearNode::write_memory(
                            LinearNode::read_register(entry_register),
                            key_size,
                            value_ty,
                            value,
                        ),
                    ]),
                    provenance.clone(),
                ),
                LinearNode::kill_register(entry_register),
                LinearNode::kill_register(ptr),
            ])
        }
        HirNodeValue::RuntimeCall(RuntimeFunction::DictionaryContains, mut args) => {
            let key = args.pop().unwrap();
            let HirNodeValue::TakeShared(dict) = args.pop().unwrap().value else {
                unreachable!()
            };
            let ExpressionType::Collection(CollectionType::Dict(key_ty, value_ty)) = &dict.ty
            else {
                unreachable!()
            };

            let key_ty = expr_ty_to_physical(key_ty);
            let value_ty = expr_ty_to_physical(value_ty);
            let entry_size = key_ty.size(declarations) + value_ty.size(declarations);
            let PhysicalType::Primitive(key_ty) = key_ty else {
                unreachable!()
            };

            let dict = lower_expression(declarations, stack_entries, stack_offset, *dict);
            let key = lower_expression(declarations, stack_entries, stack_offset, key);

            LinearNodeValue::If(
                Box::new(dict_get_entry_for_key(dict, key, 0, key_ty, entry_size)),
                vec![
                    LinearNode::new(LinearNodeValue::Discard),
                    LinearNode::new(LinearNodeValue::Byte(1)),
                ],
                Some(vec![LinearNode::new(LinearNodeValue::Byte(0))]),
            )
        }
        // Set the resume point, memcpy the stack, and return the value
        HirNodeValue::Yield(_, _) => todo!(),
        // Load the function ID, resume point, and memcpy a stack. Then call the function
        HirNodeValue::GeneratorResume(_, _) => todo!(),
        // Instantiate a generator object then call the underlying function
        // generator object is function ID, resume point, stack contents
        HirNodeValue::CoroutineStart(_, _) => todo!(),
    };

    LinearNode { value, provenance }
}

fn stack_alloc(
    stack_entries: &mut HashMap<VariableID, usize>,
    stack_offset: &mut usize,
    alloc_size: usize,
    id: VariableID,
) -> LinearNode {
    *stack_offset += alloc_size;
    stack_entries.insert(id, *stack_offset);
    LinearNode::new(LinearNodeValue::StackAlloc(alloc_size))
}

fn lower_lvalue(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    stack_entries: &mut HashMap<VariableID, usize>,
    stack_offset: &mut usize,
    lvalue: HirNode,
) -> (LinearNode, usize) {
    match lvalue.value {
        HirNodeValue::VariableReference(id) => variable_location(stack_entries, id.as_var()),
        HirNodeValue::Access(lhs, rhs) => {
            access_location(declarations, stack_entries, stack_offset, *lhs, rhs)
        }
        HirNodeValue::Dereference(inner) => (
            lower_expression(declarations, stack_entries, stack_offset, *inner),
            0,
        ),
        HirNodeValue::ArrayIndex(arr, idx) => array_index_location(
            declarations,
            stack_entries,
            stack_offset,
            *arr,
            *idx,
            &lvalue.ty,
        ),
        HirNodeValue::DictIndex(dict, idx) => {
            dict_index_location_or_abort(declarations, stack_entries, stack_offset, *dict, *idx)
        }

        HirNodeValue::Parameter(_, _) => todo!(),
        HirNodeValue::Declaration(_) => todo!(),
        HirNodeValue::Call(_, _) => todo!(),
        HirNodeValue::Assignment(_, _) => todo!(),
        HirNodeValue::UnaryLogical(_, _) => todo!(),
        HirNodeValue::Arithmetic(_, _, _) => todo!(),
        HirNodeValue::Comparison(_, _, _) => todo!(),
        HirNodeValue::BinaryLogical(_, _, _) => todo!(),
        HirNodeValue::Return(_) => todo!(),
        HirNodeValue::Yield(_, _) => todo!(),
        HirNodeValue::Int(_) => todo!(),
        HirNodeValue::PointerSize(_) => todo!(),
        HirNodeValue::Float(_) => todo!(),
        HirNodeValue::Bool(_) => todo!(),
        HirNodeValue::Null => todo!(),
        HirNodeValue::ResumePoint => todo!(),
        HirNodeValue::CharLiteral(_) => todo!(),
        HirNodeValue::StringLiteral(_) => todo!(),
        HirNodeValue::TakeUnique(_) => todo!(),
        HirNodeValue::TakeShared(_) => todo!(),
        HirNodeValue::Sequence(_) => todo!(),
        HirNodeValue::If(_, _, _) => todo!(),
        HirNodeValue::While(_, _) => todo!(),
        HirNodeValue::StructLiteral(_, _) => todo!(),
        HirNodeValue::VtableCall(_, _, _) => todo!(),
        HirNodeValue::StructToInterface { .. } => todo!(),
        HirNodeValue::InterfaceAddress(_) => todo!(),

        HirNodeValue::ArrayLiteral(_) | HirNodeValue::ArrayLiteralLength(_, _) => unreachable!(),
        HirNodeValue::NumericCast { .. } => todo!(),
        HirNodeValue::DictLiteral(_) => todo!(),
        HirNodeValue::UnionLiteral(_, _, _) => todo!(),
        HirNodeValue::NullCoalesce(_, _) => todo!(),
        HirNodeValue::MakeNullable(_) => todo!(),
        HirNodeValue::NullableTraverse(_, _) => todo!(),
        HirNodeValue::RuntimeCall(_, _) => todo!(),
        HirNodeValue::GeneratorResume(_, _) => todo!(),
        HirNodeValue::CoroutineStart(_, _) => todo!(),
    }
}

fn variable_location(
    stack_entries: &HashMap<VariableID, usize>,
    var_id: VariableID,
) -> (LinearNode, usize) {
    (
        LinearNode::ptr_arithmetic(
            ArithmeticOp::Subtract,
            LinearNode::new(LinearNodeValue::StackFrame),
            LinearNode::size(stack_entries[&var_id]),
        ),
        0,
    )
}

fn access_location(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    stack_entries: &mut HashMap<VariableID, usize>,
    stack_offset: &mut usize,
    lhs: HirNode,
    rhs: String,
) -> (LinearNode, usize) {
    let ty_id = match &lhs.ty {
        ExpressionType::InstanceOf(ty) => ty,
        ExpressionType::Nullable(ty) => match ty as &ExpressionType {
            ExpressionType::InstanceOf(ty) => ty,
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };
    let DeclaredTypeLayout { value, .. } = &declarations[ty_id];
    let (lhs, mut offset) = lower_lvalue(declarations, stack_entries, stack_offset, lhs);
    offset += match value {
        TypeLayoutValue::Structure(fields) => {
            *(fields
                .iter()
                .find_map(|(name, offset, _)| if name == &rhs { Some(offset) } else { None })
                .unwrap())
        }
        TypeLayoutValue::Union(_) => UNION_TAG_SIZE,
        TypeLayoutValue::Interface(_fields) => todo!(), //*fields.get(&rhs).unwrap(),
    };

    (lhs, offset)
}

fn array_index_location(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    stack_entries: &mut HashMap<VariableID, usize>,
    stack_offset: &mut usize,
    arr: HirNode,
    idx: HirNode,
    ty: &ExpressionType,
) -> (LinearNode, usize) {
    let size = expression_type_size(declarations, ty);
    let idx = lower_expression(declarations, stack_entries, stack_offset, idx);
    let arr = lower_expression(declarations, stack_entries, stack_offset, arr);

    let idx_register = RegisterID::new();
    let arr_ptr_register = RegisterID::new();
    let length_register = RegisterID::new();

    (
        LinearNode::new(LinearNodeValue::Sequence(vec![
            LinearNode::write_register(idx_register, idx),
            LinearNode::write_register(arr_ptr_register, arr),
            LinearNode::write_register(
                length_register,
                LinearNode::new(LinearNodeValue::TopOfStack),
            ),
            LinearNode::if_node(
                LinearNode::ptr_comparison(
                    ComparisonOp::GreaterEqualThan,
                    LinearNode::read_register(idx_register),
                    LinearNode::read_register(length_register),
                ),
                vec![LinearNode::abort()],
                None,
                None,
            ),
            LinearNode::new(LinearNodeValue::Discard),
            LinearNode::ptr_arithmetic(
                ArithmeticOp::Add,
                LinearNode::ptr_arithmetic(
                    ArithmeticOp::Multiply,
                    LinearNode::size(size),
                    LinearNode::read_register(idx_register),
                ),
                LinearNode::read_register(arr_ptr_register),
            ),
            LinearNode::kill_register(idx_register),
            LinearNode::kill_register(arr_ptr_register),
            LinearNode::kill_register(length_register),
        ])),
        0,
    )
}

fn dict_index_location_or_abort(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    stack_entries: &mut HashMap<VariableID, usize>,
    stack_offset: &mut usize,
    dict: HirNode,
    idx: HirNode,
) -> (LinearNode, usize) {
    let ExpressionType::Collection(CollectionType::Dict(key_ty, value_ty)) = &dict.ty else {
        unreachable!()
    };
    let ExpressionType::Primitive(idx_ty) = &idx.ty else {
        todo!("non-primitive keys for dictionaries")
    };
    let idx_ty = primitive_to_physical(*idx_ty);

    let key_size = expression_type_size(declarations, key_ty);
    let value_size = expression_type_size(declarations, value_ty);
    let entry_size = key_size + value_size;

    let dict = lower_expression(declarations, stack_entries, stack_offset, dict);
    let idx = lower_expression(declarations, stack_entries, stack_offset, idx);

    let temp_key_id = VariableID::new();
    let alloc = stack_alloc(stack_entries, stack_offset, key_size, temp_key_id);
    let (key_location, key_offset) = variable_location(stack_entries, temp_key_id);

    (
        LinearNode::new(LinearNodeValue::Sequence(vec![
            alloc,
            LinearNode::write_memory(
                key_location.clone(),
                key_offset,
                PhysicalType::Primitive(idx_ty),
                idx,
            ),
            LinearNode::if_node(
                dict_get_entry_for_key(dict, key_location, key_offset, idx_ty, entry_size),
                vec![
                    LinearNode::new(LinearNodeValue::StackDealloc(key_size)),
                    LinearNode::new(LinearNodeValue::TopOfStack),
                ],
                Some(vec![LinearNode::new(LinearNodeValue::Abort)]),
                None,
            ),
        ])),
        key_size,
    )
}

fn array_alloc_space_to_push(
    array_location: LinearNode,
    array_offset: usize,
    elem_size: usize,
    provenance: Option<SourceRange>,
) -> LinearNodeValue {
    let length_offset = array_offset + POINTER_SIZE;
    let capacity_offset = length_offset + POINTER_SIZE;

    let arr_ptr = RegisterID::new();
    let length_register = RegisterID::new();
    let new_capacity_register = RegisterID::new();
    let buffer_register = RegisterID::new();

    LinearNodeValue::Sequence(vec![
        LinearNode::write_register(arr_ptr, LinearNode::debug(array_location)),
        LinearNode::write_register(
            length_register,
            LinearNode::debug(LinearNode::read_memory(
                LinearNode::read_register(arr_ptr),
                length_offset,
                PhysicalType::Pointer,
            )),
        ),
        // if (length + 1) * 2 > capacity, realloc
        LinearNode::if_node(
            LinearNode::ptr_comparison(
                ComparisonOp::GreaterThan,
                LinearNode::ptr_arithmetic(
                    ArithmeticOp::Multiply,
                    LinearNode::ptr_arithmetic(
                        ArithmeticOp::Add,
                        LinearNode::read_register(length_register),
                        LinearNode::size(1),
                    ),
                    LinearNode::size(2),
                ),
                LinearNode::debug(LinearNode::read_memory(
                    LinearNode::read_register(arr_ptr),
                    capacity_offset,
                    PhysicalType::Pointer,
                )),
            ),
            // Increase capacity
            vec![
                // new capacity = (length + 1) * 2
                LinearNode::write_register(
                    new_capacity_register,
                    LinearNode::ptr_arithmetic(
                        ArithmeticOp::Multiply,
                        LinearNode::ptr_arithmetic(
                            ArithmeticOp::Add,
                            LinearNode::read_register(length_register),
                            LinearNode::size(1),
                        ),
                        LinearNode::size(2),
                    ),
                ),
                // allocate new buffer
                LinearNode::write_register(
                    buffer_register,
                    LinearNode::heap_alloc_var(LinearNode::read_register(new_capacity_register)),
                ),
                // copy old buffer to new buffer
                LinearNode::memcpy(
                    LinearNode::read_memory(
                        LinearNode::read_register(arr_ptr),
                        array_offset,
                        PhysicalType::Pointer,
                    ),
                    LinearNode::read_register(buffer_register),
                    LinearNode::ptr_arithmetic(
                        ArithmeticOp::Multiply,
                        LinearNode::read_register(length_register),
                        LinearNode::size(elem_size),
                    ),
                ),
                // write new capacity
                LinearNode::write_memory(
                    LinearNode::read_register(arr_ptr),
                    capacity_offset,
                    PhysicalType::Pointer,
                    LinearNode::read_register(new_capacity_register),
                ),
            ],
            None,
            provenance,
        ),
        // increment length
        LinearNode::write_memory(
            LinearNode::read_register(arr_ptr),
            length_offset,
            PhysicalType::Pointer,
            LinearNode::ptr_arithmetic(
                ArithmeticOp::Add,
                LinearNode::read_register(length_register),
                LinearNode::size(1),
            ),
        ),
        // Return pointer to now-writable location
        LinearNode::ptr_arithmetic(
            ArithmeticOp::Add,
            LinearNode::read_memory(
                LinearNode::read_register(arr_ptr),
                array_offset,
                PhysicalType::Pointer,
            ),
            LinearNode::ptr_arithmetic(
                ArithmeticOp::Multiply,
                LinearNode::read_register(length_register),
                LinearNode::size(elem_size),
            ),
        ),
        LinearNode::kill_register(arr_ptr),
        LinearNode::kill_register(length_register),
        LinearNode::kill_register(new_capacity_register),
        LinearNode::kill_register(buffer_register),
    ])
}

// TODO: make this work as a function, rather than inlined?
/**
 * Returns a nullable pointer to the given dictionary entry
 */
fn dict_get_entry_for_key(
    dict_pointer: LinearNode,
    key_location: LinearNode,
    key_offset: usize,
    key_ty: PhysicalPrimitive,
    entry_size: usize,
) -> LinearNode {
    let key_ptr = RegisterID::new();
    let dict_ptr = RegisterID::new();
    let dict_length = RegisterID::new();
    let index = RegisterID::new();

    LinearNode::new(LinearNodeValue::Sequence(vec![
        LinearNode::write_register(key_ptr, key_location),
        LinearNode::write_register(dict_ptr, dict_pointer),
        LinearNode::write_register(dict_length, LinearNode::new(LinearNodeValue::TopOfStack)),
        LinearNode::new(LinearNodeValue::Discard),
        LinearNode::write_register(index, LinearNode::size(0)),
        LinearNode::new(LinearNodeValue::Loop(vec![
            // Check if we've found the key
            LinearNode::if_node(
                LinearNode::new(LinearNodeValue::Comparison(
                    ComparisonOp::EqualTo,
                    key_ty,
                    Box::new(LinearNode::read_memory(
                        LinearNode::read_register(dict_ptr),
                        0,
                        PhysicalType::Primitive(key_ty),
                    )),
                    Box::new(LinearNode::read_memory(
                        LinearNode::read_register(key_ptr),
                        key_offset,
                        PhysicalType::Primitive(key_ty),
                    )),
                )),
                vec![
                    LinearNode::read_register(dict_ptr),
                    LinearNode::new(LinearNodeValue::Byte(1)),
                    LinearNode::new(LinearNodeValue::Break),
                ],
                None,
                None,
            ),
            // Increment the length counter and check if we've overflowed the bounds
            LinearNode::write_register(
                index,
                LinearNode::ptr_arithmetic(
                    ArithmeticOp::Add,
                    LinearNode::read_register(index),
                    LinearNode::size(1),
                ),
            ),
            LinearNode::write_register(
                dict_ptr,
                LinearNode::ptr_arithmetic(
                    ArithmeticOp::Add,
                    LinearNode::read_register(dict_ptr),
                    LinearNode::size(entry_size),
                ),
            ),
            LinearNode::if_node(
                LinearNode::ptr_comparison(
                    ComparisonOp::EqualTo,
                    LinearNode::read_register(dict_length),
                    LinearNode::read_register(index),
                ),
                vec![
                    LinearNode::new(LinearNodeValue::Byte(0)),
                    LinearNode::new(LinearNodeValue::Break),
                ],
                None,
                None,
            ),
        ])),
        LinearNode::kill_register(key_ptr),
        LinearNode::kill_register(dict_ptr),
        LinearNode::kill_register(dict_length),
        LinearNode::kill_register(index),
    ]))
}

// TODO: this should probably be determined by the backend...
const POINTER_SIZE: usize = 8;
// TODO: is this a good idea
const UNION_TAG_SIZE: usize = 8;
// TODO: alignment
pub const NULL_TAG_SIZE: usize = 4;
const FUNCTION_ID_SIZE: usize = 4;

fn expression_type_size(
    declarations: &HashMap<TypeID, DeclaredTypeLayout>,
    expr: &ExpressionType,
) -> usize {
    // TODO: alignment
    match expr {
        ExpressionType::Void => 0,
        ExpressionType::Unreachable => unreachable!(),
        ExpressionType::Primitive(prim) => primitive_type_size(primitive_to_physical(*prim)),
        ExpressionType::InstanceOf(id) => declarations[id].size(),
        ExpressionType::Pointer(_, _) => POINTER_SIZE,
        ExpressionType::Collection(CollectionType::Array(_)) => POINTER_SIZE * 3,
        ExpressionType::Collection(CollectionType::Dict(..)) => POINTER_SIZE * 3,
        ExpressionType::Null => 1,
        ExpressionType::Nullable(inner) => {
            NULL_TAG_SIZE + expression_type_size(declarations, inner)
        }
        ExpressionType::ReferenceTo(_) => todo!(),
        ExpressionType::TypeParameterReference(_) => todo!(),
        ExpressionType::Generator { .. } => todo!(),
    }
}

fn primitive_to_physical(p: PrimitiveType) -> PhysicalPrimitive {
    match p {
        // TODO: should chars be 4 byte or 1 byte
        PrimitiveType::Char => PhysicalPrimitive::Byte,
        PrimitiveType::String => todo!(),
        PrimitiveType::Int32 => PhysicalPrimitive::Int32,
        PrimitiveType::Float32 => PhysicalPrimitive::Float32,
        PrimitiveType::Int64 => PhysicalPrimitive::Int64,
        PrimitiveType::Float64 => PhysicalPrimitive::Float64,
        PrimitiveType::Bool => PhysicalPrimitive::Byte,
        PrimitiveType::PointerSize => PhysicalPrimitive::PointerSize,
    }
}

fn primitive_type_size(prim: PhysicalPrimitive) -> usize {
    match prim {
        PhysicalPrimitive::Int32 => 4,
        PhysicalPrimitive::Float32 => 4,
        PhysicalPrimitive::Int64 => 8,
        PhysicalPrimitive::Float64 => 8,
        PhysicalPrimitive::Byte => 1,
        PhysicalPrimitive::PointerSize => POINTER_SIZE,
    }
}

// TODO: move to its own module?
#[derive(Debug)]
pub struct DeclaredTypeLayout {
    pub value: TypeLayoutValue,
    // TODO: remove field?
    pub size: usize,
}

impl DeclaredTypeLayout {
    fn size(&self) -> usize {
        self.size
    }
}

#[derive(Debug)]
pub enum TypeLayoutValue {
    Structure(Vec<(String, usize, PhysicalType)>),
    Interface(Vec<FunctionID>),
    Union(HashMap<String, (usize, PhysicalType)>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum PhysicalPrimitive {
    Byte,
    Int32,
    Float32,
    Int64,
    Float64,
    PointerSize,
}

#[derive(Clone, Debug)]
pub enum PhysicalType {
    Primitive(PhysicalPrimitive),
    Referenced(TypeID),
    Nullable(Box<PhysicalType>),
    // TODO: unify with primitive pointer size?
    Pointer,
    FunctionPointer,
    Collection(PhysicalCollection),
}

#[derive(Clone, Debug)]
pub enum PhysicalCollection {
    Array,
    Dict,
}

impl PhysicalType {
    fn size(&self, declarations: &HashMap<TypeID, DeclaredTypeLayout>) -> usize {
        match self {
            PhysicalType::Primitive(p) => primitive_type_size(*p),
            PhysicalType::Referenced(id) => declarations[id].size(),
            PhysicalType::Nullable(_) => todo!(),
            PhysicalType::Pointer | PhysicalType::FunctionPointer => POINTER_SIZE,
            PhysicalType::Collection(ty) => match ty {
                PhysicalCollection::Array => POINTER_SIZE * 3,
                PhysicalCollection::Dict => POINTER_SIZE * 3,
            },
        }
    }
}

pub fn layout_types(
    declarations: &HashMap<String, StaticDeclaration>,
    layouts: &mut HashMap<TypeID, DeclaredTypeLayout>,
) {
    let declarations: HashMap<_, _> = declarations
        .iter()
        .map(|(_, decl)| (decl.id(), decl))
        .collect();
    for decl in declarations.values() {
        layout_static_decl(&declarations, layouts, decl);
    }
}

fn layout_static_decl(
    declarations: &HashMap<TypeID, &StaticDeclaration>,
    layouts: &mut HashMap<TypeID, DeclaredTypeLayout>,
    decl: &StaticDeclaration,
) -> usize {
    if let Some(layout) = layouts.get(&decl.id()) {
        return layout.size;
    }

    let layout = match decl {
        StaticDeclaration::Struct(struct_ty) => {
            let mut size = 0;
            let fields = struct_ty
                .fields
                .iter()
                .map(|(name, field)| {
                    let (field, field_size) = layout_type(declarations, layouts, field);
                    let offset = size;
                    size += field_size;
                    (name.clone(), offset, field)
                })
                .collect();
            DeclaredTypeLayout {
                value: TypeLayoutValue::Structure(fields),
                size,
            }
        }
        StaticDeclaration::Func(_) => {
            return POINTER_SIZE;
        }
        StaticDeclaration::Interface(interface_ty) => {
            let mut size = POINTER_SIZE;
            let fields = interface_ty
                .associated_functions
                .values()
                .map(|decl| {
                    size += FUNCTION_ID_SIZE;
                    let StaticDeclaration::Func(func) = decl else {
                        unreachable!()
                    };
                    func.func_id
                })
                .collect();
            DeclaredTypeLayout {
                value: TypeLayoutValue::Interface(fields),
                size,
            }
        }
        StaticDeclaration::Union(union_ty) => {
            let mut largest_variant = 0;
            let variants = union_ty
                .variants
                .iter()
                .enumerate()
                .map(|(idx, (name, ty))| {
                    let (variant, variant_size) = layout_type(declarations, layouts, ty);
                    if variant_size > largest_variant {
                        largest_variant = variant_size;
                    }

                    (name.clone(), (idx, variant))
                })
                .collect();

            DeclaredTypeLayout {
                value: TypeLayoutValue::Union(variants),
                size: UNION_TAG_SIZE + largest_variant,
            }
        }
        // Modules are completely compiled out
        StaticDeclaration::Module(module) => {
            layout_types(&module.exports, layouts);
            return 0;
        }
    };
    let size = layout.size;
    layouts.insert(decl.id(), layout);

    size
}

fn layout_type(
    declarations: &HashMap<TypeID, &StaticDeclaration>,
    layouts: &mut HashMap<TypeID, DeclaredTypeLayout>,
    ty: &ExpressionType,
) -> (PhysicalType, usize) {
    match ty {
        ExpressionType::Void | ExpressionType::Unreachable | ExpressionType::Null => unreachable!(),
        ExpressionType::Primitive(p) => {
            let p = primitive_to_physical(*p);
            let size = primitive_type_size(p);
            (PhysicalType::Primitive(p), size)
        }
        ExpressionType::InstanceOf(id) => {
            let size = layout_static_decl(declarations, layouts, declarations[id]);
            (PhysicalType::Referenced(*id), size)
        }
        ExpressionType::Pointer(_, _) => (PhysicalType::Pointer, POINTER_SIZE),
        ExpressionType::Collection(CollectionType::Array(_)) => (
            PhysicalType::Collection(PhysicalCollection::Array),
            POINTER_SIZE,
        ),
        ExpressionType::Collection(CollectionType::Dict(_, _)) => (
            PhysicalType::Collection(PhysicalCollection::Dict),
            POINTER_SIZE,
        ),
        ExpressionType::Nullable(inner) => {
            let (inner, size) = layout_type(declarations, layouts, inner);
            (
                PhysicalType::Nullable(Box::new(inner)),
                size + NULL_TAG_SIZE,
            )
        }
        ExpressionType::ReferenceTo(_) => todo!(),
        ExpressionType::TypeParameterReference(_) => todo!(),
        ExpressionType::Generator { .. } => todo!(),
    }
}

fn expr_ty_to_physical(ty: &ExpressionType) -> PhysicalType {
    match ty {
        ExpressionType::Void | ExpressionType::Unreachable | ExpressionType::Null => unreachable!(),
        ExpressionType::Primitive(p) => PhysicalType::Primitive(primitive_to_physical(*p)),
        ExpressionType::InstanceOf(id) => PhysicalType::Referenced(*id),
        ExpressionType::Collection(c) => PhysicalType::Collection(match c {
            CollectionType::Array(_) => PhysicalCollection::Array,
            CollectionType::Dict(_, _) => PhysicalCollection::Dict,
        }),
        ExpressionType::Pointer(_, _) => PhysicalType::Pointer,
        ExpressionType::Nullable(inner) => {
            let ty = expr_ty_to_physical(inner);
            PhysicalType::Nullable(Box::new(ty))
        }
        ExpressionType::ReferenceTo(_) => todo!(),
        ExpressionType::TypeParameterReference(_) => todo!(),
        ExpressionType::Generator { .. } => todo!(),
    }
}
