use std::collections::HashMap;

use typed_arena::Arena;

use crate::{
    id::ID,
    parser::{AstNode, AstNodeValue, BinOp},
    typecheck::TypecheckedFunction,
};

pub struct IrModule<'a> {
    // TODO: include imports, structs, and extern function declaration
    pub functions: Vec<IrFunction<'a>>,
}

pub fn lower_function<'ast, 'ir>(
    arena: &'ir Arena<IrNode<'ir>>,
    func: TypecheckedFunction<'ast>,
) -> IrFunction<'ir> {
    IrFunction {
        id: func.id,
        body: lower_node(arena, &func, func.func.body),
    }
}

// TODO: include types in all IR nodes?

pub struct IrFunction<'a> {
    pub id: ID,
    pub body: IrNode<'a>,
}

fn lower_node<'ast, 'ir>(
    arena: &'ir Arena<IrNode<'ir>>,
    context: &TypecheckedFunction<'ast>,
    node: &'ast AstNode<'ast>,
) -> IrNode<'ir> {
    let value = match &node.value {
        AstNodeValue::Int(x) => IrNodeValue::Int(*x),
        AstNodeValue::Float(x) => IrNodeValue::Float(*x),
        AstNodeValue::Bool(x) => IrNodeValue::Bool(*x),

        AstNodeValue::While(cond, body) => {
            let cond = lower_node_alloc(arena, context, cond);
            let body = lower_node_alloc(arena, context, body);
            IrNodeValue::While(cond, body)
        }
        AstNodeValue::Block(contents) => {
            // TODO: assigning last value returned in block to parent scope?
            let contents = contents
                .iter()
                .map(|node| lower_node(arena, context, node))
                .collect();
            IrNodeValue::Sequence(contents)
        }

        AstNodeValue::Return(inner) => IrNodeValue::Return(lower_node_alloc(arena, context, inner)),
        AstNodeValue::TakeUnique(inner) => {
            IrNodeValue::TakeUnique(lower_node_alloc(arena, context, inner))
        }
        AstNodeValue::TakeShared(inner) => {
            IrNodeValue::Return(lower_node_alloc(arena, context, inner))
        }

        // Statement doesn't actually add a node - the inner expression
        // has what really counts
        AstNodeValue::Statement(inner) => return lower_node(arena, context, inner),

        AstNodeValue::Declaration(_lvalue, rvalue) => {
            let value_to_assign = lower_node(arena, context, rvalue);
            let statements = vec![
                IrNode::from_ast(node, IrNodeValue::Declaration(node.id)),
                IrNode::from_ast(
                    node,
                    IrNodeValue::Assignment(
                        add_node(
                            arena,
                            IrNode::from_ast(node, IrNodeValue::VariableReference(node.id)),
                        ),
                        add_node(arena, value_to_assign),
                    ),
                ),
            ];
            IrNodeValue::Sequence(statements)
        }
        AstNodeValue::Name(_) => IrNodeValue::VariableReference(
            *context
                .referenced_id
                .get(&node.id)
                .expect("referenced ID to be filled in"),
        ),
        AstNodeValue::BinExpr(BinOp::Dot, left, right) => {
            let left = lower_node_alloc(arena, context, left);
            let AstNodeValue::Name(name) = &right.value else {
                unreachable!()
            };
            IrNodeValue::Dot(left, name.clone())
        }
        AstNodeValue::BinExpr(op, left, right) => {
            let left = lower_node_alloc(arena, context, left);
            let right = lower_node_alloc(arena, context, right);

            match op {
                BinOp::Add => IrNodeValue::BinOp(IrBinOp::Add, left, right),
                BinOp::Subtract => IrNodeValue::BinOp(IrBinOp::Subtract, left, right),
                BinOp::Multiply => IrNodeValue::BinOp(IrBinOp::Multiply, left, right),
                BinOp::Divide => IrNodeValue::BinOp(IrBinOp::Divide, left, right),
                BinOp::LessThan => IrNodeValue::BinOp(IrBinOp::LessThan, left, right),
                BinOp::GreaterThan => IrNodeValue::BinOp(IrBinOp::GreaterThan, left, right),
                BinOp::Index => IrNodeValue::Index(left, right),
                BinOp::Assignment => IrNodeValue::Assignment(left, right),
                BinOp::Dot => unreachable!(),
            }
        }
        AstNodeValue::If(_) => todo!(),
        AstNodeValue::Call(func, params) => {
            let func = lower_node_alloc(arena, context, func);
            let params = params
                .iter()
                .map(|param| lower_node(arena, context, param))
                .collect();
            IrNodeValue::Call(func, params)
        }
        AstNodeValue::StructLiteral { name: _, fields } => {
            let id = context
                .referenced_id
                .get(&node.id)
                .expect("referenced fields to be filled in");
            let fields = fields
                .iter()
                .map(|(name, field)| (name.clone(), lower_node(arena, context, field)))
                .collect();
            IrNodeValue::StructLiteral(*id, fields)
        }
        AstNodeValue::ArrayLiteral(_) => todo!(),
        AstNodeValue::ArrayLiteralLength(_, _) => todo!(),

        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::Import(_)
        | AstNodeValue::UniqueType(_)
        | AstNodeValue::SharedType(_)
        | AstNodeValue::ArrayType(_) => unreachable!("Can't have these in a function body"),
    };

    IrNode::from_ast(node, value)
}

fn lower_node_alloc<'ast, 'ir>(
    arena: &'ir Arena<IrNode<'ir>>,
    types: &TypecheckedFunction<'ast>,
    node: &'ast AstNode<'ast>,
) -> &'ir IrNode<'ir> {
    add_node(arena, lower_node(arena, types, node))
}

fn add_node<'a>(context: &'a Arena<IrNode<'a>>, node: IrNode<'a>) -> &'a IrNode<'a> {
    context.alloc(node)
}

pub struct IrNode<'a> {
    pub id: ID,
    pub value: IrNodeValue<'a>,
}

impl<'a> IrNode<'a> {
    pub fn from_ast(ast: &AstNode<'_>, value: IrNodeValue<'a>) -> IrNode<'a> {
        IrNode { id: ast.id, value }
    }
}

// TODO: should struct fields also be referred to via opaque IDs?

pub enum IrNodeValue<'a> {
    VariableReference(ID),
    Declaration(ID),
    // TODO: insert destructors
    Destructor(ID),

    Call(&'a IrNode<'a>, Vec<IrNode<'a>>),
    Dot(&'a IrNode<'a>, String),
    Assignment(&'a IrNode<'a>, &'a IrNode<'a>),
    Index(&'a IrNode<'a>, &'a IrNode<'a>),
    BinOp(IrBinOp, &'a IrNode<'a>, &'a IrNode<'a>),

    Return(&'a IrNode<'a>),

    Int(i64),
    Float(f64),
    Bool(bool),

    TakeUnique(&'a IrNode<'a>),
    TakeShared(&'a IrNode<'a>),
    Dereference(&'a IrNode<'a>),

    /// Like a Block in that it's a collection of nodes, but the IR
    /// doesn't care about scoping or expressions
    Sequence(Vec<IrNode<'a>>),

    // Expressions
    If(&'a IrNode<'a>, &'a IrNode<'a>, &'a IrNode<'a>),
    While(&'a IrNode<'a>, &'a IrNode<'a>),
    StructLiteral(ID, HashMap<String, IrNode<'a>>),
    ArrayLiteral(Vec<IrNode<'a>>),
    ArrayLiteralLength(&'a IrNode<'a>, u64),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IrBinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IrLvalueBinOp {
    Index,
    Assignment,
}
