use std::collections::{HashMap, VecDeque};

use crate::{
    hir::{HirBinOp, HirFunction, HirNode, HirNodeValue},
    id::ID,
    provenance::SourceRange,
    typecheck::{ExpressionType, PrimitiveType},
};

#[derive(Debug)]
pub struct LinearFunction {
    pub id: ID,
    // TODO: memory layouts instead of expression types?
    pub params: Vec<ExpressionType>,
    pub returns: Option<ExpressionType>,
    pub body: LinearBlock,
}

pub fn linearize_function(
    declarations: &HashMap<ID, &TypeMemoryLayout>,
    function: HirFunction,
) -> LinearFunction {
    let HirNodeValue::Sequence(block) = function.body.value else {
        unreachable!()
    };
    let body = linearize_nodes(declarations, &mut HashMap::new(), &mut 0, block.into());
    LinearFunction {
        id: function.id,
        body,
        // TODO
        params: Vec::new(),
        returns: None,
    }
}

#[derive(Debug)]
pub struct LinearBlock {
    pub id: ID,
    pub statements: Vec<LinearNode>,
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
    BasePtr,
    StackAlloc(usize),
    /// Each parameter may only appear once in a given method body
    Parameter(usize),
    Read {
        location: Box<LinearNode>,
        offset: usize,
        size: usize,
        ty: ExpressionType,
    },
    Write {
        location: Box<LinearNode>,
        offset: usize,
        size: usize,
        value: Box<LinearNode>,
    },

    // Control flow
    Call(Box<LinearNode>, Vec<LinearNode>),
    Return(Box<LinearNode>),
    If(Box<LinearNode>, LinearBlock, Option<LinearBlock>),
    // TODO: labelled breaks?
    Break,
    Loop(LinearBlock),

    BinOp(HirBinOp, PrimitiveType, Box<LinearNode>, Box<LinearNode>),
    ID(ID),
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
    declarations: &HashMap<ID, &TypeMemoryLayout>,
    stack_entries: &mut HashMap<ID, usize>,
    stack_offset: &mut usize,
    //blocks: &mut VecDeque<LinearBlock>,
    mut nodes: VecDeque<HirNode>,
) -> LinearBlock {
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
                stack_entries.insert(id, *stack_offset);
                *stack_offset += alloc_size;
                let alloc = LinearNode::new(LinearNodeValue::StackAlloc(alloc_size));
                values.push(alloc);
            }
            HirNodeValue::Parameter(idx, id) => {
                let offset = *stack_offset;

                let alloc_size = expression_type_size(declarations, &node.ty);
                stack_entries.insert(id, *stack_offset);
                *stack_offset += alloc_size;
                let alloc = LinearNode::new(LinearNodeValue::StackAlloc(alloc_size));
                values.push(alloc);

                values.push(LinearNode {
                    value: LinearNodeValue::Write {
                        location: Box::new(LinearNode::new(LinearNodeValue::BasePtr)),
                        offset,
                        size: alloc_size,
                        value: Box::new(LinearNode::new(LinearNodeValue::Parameter(idx))),
                    },
                    provenance: node.provenance,
                });
            }
            HirNodeValue::Assignment(lhs, rhs) => {
                let size = expression_type_size(declarations, &lhs.ty);
                let (location, offset) = lower_lvalue(stack_entries, *lhs);
                let rhs = lower_expression(declarations, stack_entries, *rhs);
                values.push(LinearNode {
                    value: LinearNodeValue::Write {
                        location: Box::new(location),
                        offset,
                        size,
                        value: Box::new(rhs),
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
            HirNodeValue::While(_, _) => todo!(),

            HirNodeValue::TakeUnique(_) => todo!(),
            HirNodeValue::TakeShared(_) => todo!(),
            HirNodeValue::Dereference(_) => todo!(),
            HirNodeValue::StructLiteral(_, _) => todo!(),
            HirNodeValue::ArrayLiteral(_) => todo!(),
            HirNodeValue::ArrayLiteralLength(_, _) => todo!(),
            HirNodeValue::VtableCall(_, _, _) => todo!(),
            HirNodeValue::StructToInterface { .. } => todo!(),

            // TODO: auto-returns?
            _ => {
                values.push(lower_expression(declarations, stack_entries, node));
            }
        }
    }

    LinearBlock {
        id: ID::new(),
        statements: values,
    }
}

fn lower_expression(
    declarations: &HashMap<ID, &TypeMemoryLayout>,
    stack_entries: &HashMap<ID, usize>,
    expression: HirNode,
) -> LinearNode {
    let HirNode {
        id,
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
            let offset = *stack_entries.get(&id).unwrap();
            LinearNodeValue::Read {
                location: Box::new(LinearNode::new(LinearNodeValue::BasePtr)),
                offset,
                size: expression_type_size(declarations, &ty),
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
        HirNodeValue::Access(_, _) => todo!(),
        HirNodeValue::Index(_, _) => todo!(),

        HirNodeValue::If(_, _, _) | HirNodeValue::While(_, _) | HirNodeValue::Return(_) => {
            unreachable!("all control flow must be removed from expressions in HIR")
        }

        HirNodeValue::Parameter(_, _) => todo!(),
        HirNodeValue::Declaration(_) => todo!(),
        HirNodeValue::Assignment(_, _) => todo!(),
        HirNodeValue::TakeUnique(_) => todo!(),
        HirNodeValue::TakeShared(_) => todo!(),
        HirNodeValue::Dereference(_) => todo!(),
        HirNodeValue::Sequence(_) => todo!(),
        HirNodeValue::StructLiteral(_, _) => todo!(),
        HirNodeValue::ArrayLiteral(_) => todo!(),
        HirNodeValue::ArrayLiteralLength(_, _) => todo!(),
        HirNodeValue::VtableCall(_, _, _) => todo!(),
        HirNodeValue::StructToInterface { .. } => todo!(),
    };

    LinearNode { value, provenance }
}

fn lower_lvalue(stack_entries: &HashMap<ID, usize>, lvalue: HirNode) -> (LinearNode, usize) {
    match lvalue.value {
        HirNodeValue::VariableReference(id) => {
            let offset = stack_entries.get(&id).unwrap();

            (LinearNode::new(LinearNodeValue::BasePtr), *offset)
        }

        HirNodeValue::Parameter(_, _) => todo!(),
        HirNodeValue::Declaration(_) => todo!(),
        HirNodeValue::Call(_, _) => todo!(),
        HirNodeValue::Access(_, _) => todo!(),
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
        HirNodeValue::Dereference(_) => todo!(),
        HirNodeValue::Sequence(_) => todo!(),
        HirNodeValue::If(_, _, _) => todo!(),
        HirNodeValue::While(_, _) => todo!(),
        HirNodeValue::StructLiteral(_, _) => todo!(),
        HirNodeValue::ArrayLiteral(_) => todo!(),
        HirNodeValue::ArrayLiteralLength(_, _) => todo!(),
        HirNodeValue::VtableCall(_, _, _) => todo!(),
        HirNodeValue::StructToInterface { value, vtable } => todo!(),
    }
}

const POINTER_SIZE: usize = 4;

fn expression_type_size(
    declarations: &HashMap<ID, &TypeMemoryLayout>,
    expr: &ExpressionType,
) -> usize {
    match expr {
        ExpressionType::Void => 0,
        ExpressionType::Primitive(prim) => match prim {
            PrimitiveType::Char => 1,
            PrimitiveType::String => POINTER_SIZE,
            PrimitiveType::Int32 => 4,
            PrimitiveType::Float32 => 4,
            PrimitiveType::Int64 => 8,
            PrimitiveType::Float64 => 8,
            PrimitiveType::Bool => 1,
        },
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

// TODO

pub struct TypeMemoryLayout {
    pub size: usize,
}

impl TypeMemoryLayout {
    fn size(&self) -> usize {
        self.size
    }

    fn is_function(&self) -> bool {
        false
    }
}
