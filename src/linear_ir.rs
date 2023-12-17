use std::collections::{HashMap, VecDeque};

use crate::{
    hir::{HirBinOp, HirFunction, HirNode, HirNodeValue},
    id::ID,
    provenance::SourceRange,
    typecheck::{ExpressionType, PrimitiveType, StaticDeclaration},
};

#[derive(Debug)]
pub struct LinearFunction {
    pub id: ID,
    // TODO: memory layouts instead of expression types?
    pub params: Vec<ExpressionType>,
    pub returns: Option<ExpressionType>,
    pub body: Vec<LinearNode>,
}

pub fn linearize_function(
    declarations: &HashMap<ID, TypeMemoryLayout>,
    function: HirFunction,
) -> LinearFunction {
    let HirNodeValue::Sequence(block) = function.body.value else {
        unreachable!()
    };
    let body = linearize_nodes(
        declarations,
        &mut HashMap::new(),
        &mut std::mem::size_of::<usize>(),
        block.into(),
    );
    LinearFunction {
        id: function.id,
        body,
        // TODO
        params: Vec::new(),
        returns: None,
    }
}

#[derive(Debug)]
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
}

// TODO: split up between 'statement' and 'expression' to reduce need for boxing?
#[derive(Debug)]
pub enum LinearNodeValue {
    // TODO: how to handle function parameters?

    // Memory
    StackFrame,
    StackAlloc(usize),
    /// Each parameter may only appear once in a given method body
    Parameter(usize),
    ReadMemory {
        location: Box<LinearNode>,
        offset: usize,
        ty: ExpressionType,
    },
    WriteMemory {
        location: Box<LinearNode>,
        offset: usize,
        ty: ExpressionType,
        value: Box<LinearNode>,
    },

    // Control flow
    Call(Box<LinearNode>, Vec<LinearNode>),
    Return(Box<LinearNode>),
    If(Box<LinearNode>, Vec<LinearNode>, Option<Vec<LinearNode>>),
    // TODO: labelled breaks?
    Break,
    Loop(Vec<LinearNode>),

    Sequence(Vec<LinearNode>),

    BinOp(HirBinOp, PrimitiveType, Box<LinearNode>, Box<LinearNode>),
    Size(usize),
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
    CharLiteral(char),
    StringLiteral(String),
    FunctionID(ID),
}

// TODO: produce a more CFG shaped result?

pub fn linearize_nodes(
    declarations: &HashMap<ID, TypeMemoryLayout>,
    stack_entries: &mut HashMap<ID, usize>,
    stack_offset: &mut usize,
    //blocks: &mut VecDeque<LinearBlock>,
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
                *stack_offset += alloc_size;
                stack_entries.insert(id, *stack_offset);
                let alloc = LinearNode::new(LinearNodeValue::StackAlloc(alloc_size));
                values.push(alloc);
            }
            HirNodeValue::Parameter(idx, id) => {
                let alloc_size = expression_type_size(declarations, &node.ty);
                *stack_offset += alloc_size;
                stack_entries.insert(id, *stack_offset);
                let alloc = LinearNode::new(LinearNodeValue::StackAlloc(alloc_size));
                values.push(alloc);

                let (location, offset) = variable_location(stack_entries, &id);

                values.push(LinearNode {
                    value: LinearNodeValue::WriteMemory {
                        location: Box::new(location),
                        offset,
                        value: Box::new(LinearNode::new(LinearNodeValue::Parameter(idx))),
                        // TODO: hm
                        ty: node.ty,
                    },
                    provenance: node.provenance,
                });
            }
            HirNodeValue::Assignment(lhs, rhs) => {
                let ty = lhs.ty.clone();
                let (location, offset) = lower_lvalue(declarations, stack_entries, *lhs);
                let rhs = lower_expression(declarations, stack_entries, *rhs);
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
                let expr = lower_expression(declarations, stack_entries, *expr);
                values.push(LinearNode {
                    value: LinearNodeValue::Return(Box::new(expr)),
                    provenance: node.provenance,
                });
                // TODO: is this a legal optimization?
                break;
            }
            // TODO: should If be an expression in linear IR?
            HirNodeValue::If(cond, if_block, else_block) => {
                let cond = lower_expression(declarations, stack_entries, *cond);
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
                values.push(LinearNode {
                    value: LinearNodeValue::If(Box::new(cond), if_block, else_block),
                    provenance: node.provenance,
                });
            }
            HirNodeValue::While(cond, block) => {
                let cond = lower_expression(declarations, stack_entries, *cond);
                let mut vec_deque = VecDeque::new();
                vec_deque.push_back(*block);
                let block = linearize_nodes(declarations, stack_entries, stack_offset, vec_deque);
                values.push(LinearNode {
                    value: LinearNodeValue::Loop(vec![LinearNode::new(LinearNodeValue::If(
                        Box::new(cond),
                        block,
                        Some(vec![LinearNode::new(LinearNodeValue::Break)]),
                    ))]),
                    provenance: node.provenance,
                })
            }

            HirNodeValue::ArrayLiteral(_) => todo!(),
            HirNodeValue::ArrayLiteralLength(_, _) => todo!(),

            // TODO: auto-returns?
            _ => {
                values.push(lower_expression(declarations, stack_entries, node));
            }
        }
    }

    values
}

fn lower_expression(
    declarations: &HashMap<ID, TypeMemoryLayout>,
    stack_entries: &HashMap<ID, usize>,
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
        HirNodeValue::Float(x) => LinearNodeValue::Float(x),
        HirNodeValue::Bool(x) => LinearNodeValue::Bool(x),
        HirNodeValue::Null => LinearNodeValue::Null,
        HirNodeValue::CharLiteral(x) => LinearNodeValue::CharLiteral(x),
        HirNodeValue::StringLiteral(x) => LinearNodeValue::StringLiteral(x),

        HirNodeValue::BinOp(op, lhs, rhs) => {
            let ExpressionType::Primitive(ty) = rhs.ty else {
                unreachable!("binoperands must be primitive not {:?}", ty)
            };
            LinearNodeValue::BinOp(
                op,
                ty,
                Box::new(lower_expression(declarations, stack_entries, *lhs)),
                Box::new(lower_expression(declarations, stack_entries, *rhs)),
            )
        }
        HirNodeValue::VariableReference(id) => {
            let (location, offset) = variable_location(stack_entries, &id);
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
                .map(|param| lower_expression(declarations, stack_entries, param))
                .collect();
            LinearNodeValue::Call(
                Box::new(LinearNode::new(LinearNodeValue::FunctionID(fn_id))),
                params,
            )
        }
        HirNodeValue::Access(lhs, rhs) => {
            let (location, offset) = access_location(declarations, stack_entries, *lhs, rhs);
            LinearNodeValue::ReadMemory {
                location: Box::new(location),
                offset,
                ty,
            }
        }
        HirNodeValue::Index(_, _) => todo!(),

        HirNodeValue::If(_, _, _) | HirNodeValue::While(_, _) | HirNodeValue::Return(_) => {
            unreachable!("all control flow must be removed from expressions in HIR")
        }

        HirNodeValue::Parameter(_, _) => todo!(),
        HirNodeValue::Declaration(_) => todo!(),
        HirNodeValue::Assignment(_, _) => todo!(),
        HirNodeValue::TakeUnique(inner) | HirNodeValue::TakeShared(inner) => {
            let (ptr, offset) = lower_lvalue(declarations, stack_entries, *inner);
            LinearNodeValue::BinOp(
                HirBinOp::Add,
                PrimitiveType::PointerSize,
                Box::new(ptr),
                Box::new(LinearNode::new(LinearNodeValue::Size(offset))),
            )
        }
        HirNodeValue::Dereference(inner) => LinearNodeValue::ReadMemory {
            location: Box::new(lower_expression(declarations, stack_entries, *inner)),
            offset: 0,
            ty,
        },
        HirNodeValue::Sequence(_) => todo!(),
        HirNodeValue::StructLiteral(struct_id, mut values) => {
            let Some(TypeMemoryLayout {
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
                    .map(|value| lower_expression(declarations, stack_entries, value))
                    .collect(),
            )
        }
        HirNodeValue::ArrayLiteral(_) => todo!(),
        HirNodeValue::ArrayLiteralLength(_, _) => todo!(),
        HirNodeValue::InterfaceAddress(table) => {
            let (table, offset) = lower_lvalue(declarations, stack_entries, *table);
            LinearNodeValue::ReadMemory {
                location: Box::new(table),
                offset,
                ty: ExpressionType::Primitive(PrimitiveType::PointerSize),
            }
        }
        HirNodeValue::VtableCall(table, fn_id, params) => {
            let ExpressionType::DeclaredType(ty_id) = &table.ty else {
                unreachable!()
            };
            let Some(TypeMemoryLayout {
                value: TypeLayoutValue::Interface(fields),
                ..
            }) = declarations.get(ty_id)
            else {
                unreachable!()
            };
            let (table, mut offset) = lower_lvalue(declarations, stack_entries, *table);
            offset += POINTER_SIZE;
            offset += fields
                .iter()
                .enumerate()
                .find_map(|(idx, id)| {
                    if *id == fn_id {
                        // TODO: fix this constant
                        Some(idx * 4)
                    } else {
                        None
                    }
                })
                .unwrap();
            let params = params
                .into_iter()
                .map(|param| lower_expression(declarations, stack_entries, param))
                .collect();

            LinearNodeValue::Call(
                Box::new(LinearNode::new(LinearNodeValue::ReadMemory {
                    location: Box::new(table),
                    offset,
                    ty: ExpressionType::Primitive(PrimitiveType::FunctionID),
                })),
                params,
            )
        }
        HirNodeValue::StructToInterface { value, vtable } => {
            let mut values = Vec::new();

            let ExpressionType::DeclaredType(ty_id) = &ty else {
                unreachable!()
            };
            let Some(TypeMemoryLayout {
                value: TypeLayoutValue::Interface(fields),
                ..
            }) = declarations.get(ty_id)
            else {
                unreachable!()
            };
            for field in fields.iter().rev() {
                values.push(LinearNode::new(LinearNodeValue::FunctionID(
                    *vtable.get(field).unwrap(),
                )));
            }

            let (pointer, offset) = lower_lvalue(declarations, stack_entries, *value);
            let offset = LinearNode::new(LinearNodeValue::Size(offset));
            let pointer = LinearNode::new(LinearNodeValue::BinOp(
                HirBinOp::Add,
                PrimitiveType::PointerSize,
                Box::new(pointer),
                Box::new(offset),
            ));
            values.push(pointer);

            LinearNodeValue::Sequence(values)
        }
    };

    LinearNode { value, provenance }
}

fn lower_lvalue(
    declarations: &HashMap<ID, TypeMemoryLayout>,
    stack_entries: &HashMap<ID, usize>,
    lvalue: HirNode,
) -> (LinearNode, usize) {
    match lvalue.value {
        HirNodeValue::VariableReference(id) => variable_location(stack_entries, &id),
        HirNodeValue::Access(lhs, rhs) => access_location(declarations, stack_entries, *lhs, rhs),
        HirNodeValue::Dereference(inner) => {
            (lower_expression(declarations, stack_entries, *inner), 0)
        }

        HirNodeValue::Parameter(_, _) => todo!(),
        HirNodeValue::Declaration(_) => todo!(),
        HirNodeValue::Call(_, _) => todo!(),
        HirNodeValue::Assignment(_, _) => todo!(),
        HirNodeValue::Index(_, _) => todo!(),
        HirNodeValue::BinOp(_, _, _) => todo!(),
        HirNodeValue::Return(_) => todo!(),
        HirNodeValue::Int(_) => todo!(),
        HirNodeValue::Float(_) => todo!(),
        HirNodeValue::Bool(_) => todo!(),
        HirNodeValue::Null => todo!(),
        HirNodeValue::CharLiteral(_) => todo!(),
        HirNodeValue::StringLiteral(_) => todo!(),
        HirNodeValue::TakeUnique(_) => todo!(),
        HirNodeValue::TakeShared(_) => todo!(),
        HirNodeValue::Sequence(_) => todo!(),
        HirNodeValue::If(_, _, _) => todo!(),
        HirNodeValue::While(_, _) => todo!(),
        HirNodeValue::StructLiteral(_, _) => todo!(),
        HirNodeValue::ArrayLiteral(_) => todo!(),
        HirNodeValue::ArrayLiteralLength(_, _) => todo!(),
        HirNodeValue::VtableCall(_, _, _) => todo!(),
        HirNodeValue::StructToInterface { .. } => todo!(),
        HirNodeValue::InterfaceAddress(_) => todo!(),
    }
}

/*fn read(
    declarations: &HashMap<ID, TypeMemoryLayout>,
    location: Box<LinearNode>,
    offset: usize,
    ty: ExpressionType,
) -> LinearNodeValue {
    LinearNodeValue::ReadMemory {
        location: Box::new(location),
        offset,
        size: expression_type_size(declarations, &ty),
        ty,
    }
}*/

fn variable_location(stack_entries: &HashMap<ID, usize>, var_id: &ID) -> (LinearNode, usize) {
    let offset = stack_entries.get(var_id).unwrap();

    (
        LinearNode::new(LinearNodeValue::BinOp(
            HirBinOp::Subtract,
            PrimitiveType::PointerSize,
            Box::new(LinearNode::new(LinearNodeValue::StackFrame)),
            Box::new(LinearNode::new(LinearNodeValue::Size(*offset))),
        )),
        0,
    )
}

fn access_location(
    declarations: &HashMap<ID, TypeMemoryLayout>,
    stack_entries: &HashMap<ID, usize>,
    lhs: HirNode,
    rhs: String,
) -> (LinearNode, usize) {
    let ExpressionType::DeclaredType(ty_id) = lhs.ty else {
        unreachable!()
    };
    let TypeMemoryLayout { value, .. } = declarations.get(&ty_id).unwrap();
    let (lhs, mut offset) = lower_lvalue(declarations, stack_entries, lhs);
    offset += match value {
        TypeLayoutValue::Structure(fields) => fields
            .iter()
            .find_map(|(name, offset, _)| if name == &rhs { Some(offset) } else { None })
            .unwrap(),
        TypeLayoutValue::Interface(_fields) => todo!(), //*fields.get(&rhs).unwrap(),
        TypeLayoutValue::FunctionPointer => unreachable!(),
    };

    (lhs, offset)
}

// TODO: this should probably be determined by the backend...
const POINTER_SIZE: usize = 8;

fn expression_type_size(
    declarations: &HashMap<ID, TypeMemoryLayout>,
    expr: &ExpressionType,
) -> usize {
    match expr {
        ExpressionType::Void => 0,
        ExpressionType::Primitive(prim) => primitive_type_size(*prim),
        ExpressionType::DeclaredType(id) => declarations.get(id).unwrap().size(),
        ExpressionType::Pointer(_, _) => POINTER_SIZE,
        ExpressionType::Array(_) => POINTER_SIZE,
        ExpressionType::Null => 1,
        ExpressionType::Nullable(inner) => {
            // TODO: would this be an alignment issue?
            1 + expression_type_size(declarations, inner)
        }
    }
}

fn primitive_type_size(prim: PrimitiveType) -> usize {
    match prim {
        PrimitiveType::Char => 1,
        PrimitiveType::String => POINTER_SIZE,
        PrimitiveType::Int32 => 4,
        PrimitiveType::Float32 => 4,
        PrimitiveType::Int64 => 8,
        PrimitiveType::Float64 => 8,
        PrimitiveType::Bool => 1,
        PrimitiveType::PointerSize => POINTER_SIZE,
        PrimitiveType::FunctionID => 4,
    }
}

// TODO

// TODO: move to its own module?
#[derive(Debug)]
pub struct TypeMemoryLayout {
    pub value: TypeLayoutValue,
    // TODO: remove field?
    pub size: usize,
}

#[derive(Debug)]
pub enum TypeLayoutValue {
    Structure(Vec<(String, usize, TypeLayoutField)>),
    Interface(Vec<ID>),
    FunctionPointer,
}

#[derive(Debug)]
pub enum TypeLayoutField {
    Primitive(PrimitiveType),
    Referenced(ID),
    Nullable(Box<TypeLayoutField>),
    Pointer,
}

impl TypeMemoryLayout {
    fn size(&self) -> usize {
        self.size
    }
}

pub fn layout_types(
    declarations: &HashMap<String, StaticDeclaration>,
    layouts: &mut HashMap<ID, TypeMemoryLayout>,
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
    declarations: &HashMap<ID, &StaticDeclaration>,
    layouts: &mut HashMap<ID, TypeMemoryLayout>,
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
            TypeMemoryLayout {
                value: TypeLayoutValue::Structure(fields),
                size,
            }
        }
        StaticDeclaration::Func(_) => TypeMemoryLayout {
            value: TypeLayoutValue::FunctionPointer,
            size: 8,
        },
        StaticDeclaration::Interface(interface_ty) => {
            let mut size = POINTER_SIZE;
            let fields = interface_ty
                .associated_functions
                .values()
                .map(|decl| {
                    // TODO: don't hardcode function ID
                    size += 4;
                    decl.id()
                })
                .collect();
            TypeMemoryLayout {
                value: TypeLayoutValue::Interface(fields),
                size,
            }
        }
        StaticDeclaration::Union(_) => todo!(),

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
    declarations: &HashMap<ID, &StaticDeclaration>,
    layouts: &mut HashMap<ID, TypeMemoryLayout>,
    ty: &ExpressionType,
) -> (TypeLayoutField, usize) {
    match ty {
        ExpressionType::Void => unreachable!(),
        ExpressionType::Null => unreachable!(),
        ExpressionType::Primitive(p) => {
            let size = primitive_type_size(*p);
            (TypeLayoutField::Primitive(*p), size)
        }
        ExpressionType::DeclaredType(id) => {
            let size = layout_static_decl(declarations, layouts, declarations.get(id).unwrap());
            (TypeLayoutField::Referenced(*id), size)
        }
        ExpressionType::Pointer(_, _) => (TypeLayoutField::Pointer, POINTER_SIZE),
        ExpressionType::Array(_) => (TypeLayoutField::Pointer, POINTER_SIZE),
        ExpressionType::Nullable(inner) => {
            let (inner, size) = layout_type(declarations, layouts, inner);
            (TypeLayoutField::Nullable(Box::new(inner)), size + 1)
        }
    }
}
