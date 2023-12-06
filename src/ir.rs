use std::collections::HashMap;

use crate::{
    id::ID,
    parser::{AstNode, AstNodeValue, BinOp, IfDeclaration},
    typecheck::{ExpressionType, StaticDeclaration, TypecheckedFile, TypecheckedFunction},
};

// TODO: should the IR be a stack machine?

pub struct IrModule {
    pub top_level_statements: Vec<IrNode>,
    // TODO: include imports, structs, and extern function declaration
    pub functions: Vec<IrFunction>,
}

pub fn lower_module<'ast>(
    module: TypecheckedFile<'ast>,
    declarations: &HashMap<ID, &'ast StaticDeclaration>,
) -> IrModule {
    let TypecheckedFile {
        expression_types,
        referenced_ids,
        functions,
        top_level_statements,
    } = module;
    IrModule {
        functions: functions
            .into_iter()
            .map(|func| lower_function(&expression_types, declarations, &referenced_ids, func))
            .collect(),
        top_level_statements: top_level_statements
            .into_iter()
            .map(|stmt| lower_node(&expression_types, declarations, &referenced_ids, &stmt))
            .collect(),
    }
}

fn lower_function<'ast>(
    types: &HashMap<ID, ExpressionType>,
    decls: &HashMap<ID, &'ast StaticDeclaration>,
    referenced_ids: &HashMap<ID, ID>,
    func: TypecheckedFunction<'ast>,
) -> IrFunction {
    let mut instructions: Vec<_> = func
        .func
        .params
        .iter()
        .enumerate()
        .map(|(i, param)| IrNode {
            id: param.id,
            value: IrNodeValue::Parameter(i, param.id),
        })
        .collect();
    instructions.push(lower_node(types, decls, &referenced_ids, func.func.body));
    IrFunction {
        id: func.id,
        name: func.name,
        body: IrNode {
            id: func.id,
            value: IrNodeValue::Sequence(instructions),
        },
    }
}

// TODO: include types in all IR nodes?

#[derive(Clone, Debug)]
pub struct IrFunction {
    pub id: ID,
    pub name: String,
    pub body: IrNode,
}

fn lower_node<'ast>(
    types: &HashMap<ID, ExpressionType>,
    decls: &HashMap<ID, &'ast StaticDeclaration>,
    referenced_ids: &HashMap<ID, ID>,
    node: &'ast AstNode<'ast>,
) -> IrNode {
    let value = match &node.value {
        AstNodeValue::Int(x) => IrNodeValue::Int(*x),
        AstNodeValue::Float(x) => IrNodeValue::Float(*x),
        AstNodeValue::Bool(x) => IrNodeValue::Bool(*x),
        AstNodeValue::Null => IrNodeValue::Null,
        AstNodeValue::CharLiteral(x) => IrNodeValue::CharLiteral(*x),
        AstNodeValue::StringLiteral(x) => IrNodeValue::StringLiteral(x.clone()),

        AstNodeValue::While(cond, body) => {
            // TODO: can you assign out of a while?
            let cond = lower_node_alloc(types, decls, referenced_ids, cond);
            let body = lower_node_alloc(types, decls, referenced_ids, body);
            IrNodeValue::While(cond, body)
        }
        AstNodeValue::Block(contents) => {
            let contents = contents
                .iter()
                .map(|node| lower_node(types, decls, referenced_ids, node))
                .collect();

            IrNodeValue::Sequence(contents)
        }
        // TODO
        AstNodeValue::If(IfDeclaration {
            condition,
            if_branch,
            else_branch,
        }) => {
            let condition = lower_node_alloc(types, decls, referenced_ids, condition);
            let if_branch = lower_node_alloc(types, decls, referenced_ids, if_branch);
            let else_branch = else_branch
                .as_ref()
                .map(|else_branch| lower_node_alloc(types, decls, referenced_ids, else_branch));

            IrNodeValue::If(condition, if_branch, else_branch)
        }

        AstNodeValue::TakeUnique(inner) => {
            IrNodeValue::TakeUnique(lower_node_alloc(types, decls, referenced_ids, inner))
        }
        AstNodeValue::TakeShared(inner) => {
            IrNodeValue::Return(lower_node_alloc(types, decls, referenced_ids, inner))
        }

        // Statement doesn't actually add a node - the inner expression
        // has what really counts
        AstNodeValue::Statement(inner) => return lower_node(types, decls, referenced_ids, inner),

        AstNodeValue::Declaration(_lvalue, rvalue) => {
            let lvalue = Box::new(IrNode::from_ast(
                node,
                IrNodeValue::VariableReference(node.id),
            ));
            let rvalue = lower_node_alloc(types, decls, referenced_ids, rvalue);
            let statements = vec![
                IrNode::from_ast(node, IrNodeValue::Declaration(node.id)),
                IrNode::from_ast(node, IrNodeValue::Assignment(lvalue, rvalue)),
            ];
            IrNodeValue::Sequence(statements)
        }
        AstNodeValue::Name(_) => IrNodeValue::VariableReference(
            *referenced_ids
                .get(&node.id)
                .expect("referenced ID to be filled in"),
        ),

        AstNodeValue::Return(inner) => {
            IrNodeValue::Return(lower_node_alloc(types, decls, referenced_ids, inner))
        }
        AstNodeValue::BinExpr(BinOp::Dot, left, right) => {
            let Some(ExpressionType::DeclaredType(expr_ty)) = types.get(&left.id) else {
                panic!("expected left side of dot to be declared type");
            };
            let AstNodeValue::Name(name) = &right.value else {
                unreachable!()
            };
            if let Some(StaticDeclaration::Module(module)) = decls.get(expr_ty) {
                IrNodeValue::VariableReference(
                    module
                        .exports
                        .get(name)
                        .expect("module export to exist")
                        .id(),
                )
            } else {
                let left = lower_node_alloc(types, decls, referenced_ids, left);
                IrNodeValue::Access(left, name.clone())
            }
        }
        AstNodeValue::BinExpr(op, left, right) => {
            let left = lower_node_alloc(types, decls, referenced_ids, left);
            let right = lower_node_alloc(types, decls, referenced_ids, right);

            match op {
                BinOp::AddAssign => IrNodeValue::Assignment(
                    left.clone(),
                    Box::new(IrNode::from_ast(
                        &node,
                        IrNodeValue::BinOp(IrBinOp::Add, left, right),
                    )),
                ),
                BinOp::SubtractAssign => IrNodeValue::Assignment(
                    left.clone(),
                    Box::new(IrNode::from_ast(
                        &node,
                        IrNodeValue::BinOp(IrBinOp::Subtract, left, right),
                    )),
                ),
                BinOp::MultiplyAssign => IrNodeValue::Assignment(
                    left.clone(),
                    Box::new(IrNode::from_ast(
                        &node,
                        IrNodeValue::BinOp(IrBinOp::Multiply, left, right),
                    )),
                ),
                BinOp::DivideAssign => IrNodeValue::Assignment(
                    left.clone(),
                    Box::new(IrNode::from_ast(
                        &node,
                        IrNodeValue::BinOp(IrBinOp::Divide, left, right),
                    )),
                ),
                BinOp::Assignment => IrNodeValue::Assignment(left, right),

                BinOp::Add => IrNodeValue::BinOp(IrBinOp::Add, left, right),
                BinOp::Subtract => IrNodeValue::BinOp(IrBinOp::Subtract, left, right),
                BinOp::Multiply => IrNodeValue::BinOp(IrBinOp::Multiply, left, right),
                BinOp::Divide => IrNodeValue::BinOp(IrBinOp::Divide, left, right),
                BinOp::LessThan => IrNodeValue::BinOp(IrBinOp::LessThan, left, right),
                BinOp::GreaterThan => IrNodeValue::BinOp(IrBinOp::GreaterThan, left, right),
                BinOp::LessEqualThan => IrNodeValue::BinOp(IrBinOp::LessEqualThan, left, right),
                BinOp::GreaterEqualThan => {
                    IrNodeValue::BinOp(IrBinOp::GreaterEqualThan, left, right)
                }
                BinOp::EqualTo => IrNodeValue::BinOp(IrBinOp::EqualTo, left, right),
                BinOp::NotEquals => IrNodeValue::BinOp(IrBinOp::NotEquals, left, right),
                BinOp::Index => IrNodeValue::Index(left, right),
                BinOp::Dot => unreachable!(),
            }
        }
        AstNodeValue::Call(func, params) => {
            let func = lower_node_alloc(types, decls, referenced_ids, func);
            let params = params
                .iter()
                .map(|param| lower_node(types, decls, referenced_ids, param))
                .collect();
            IrNodeValue::Call(func, params)
        }
        AstNodeValue::StructLiteral { name: _, fields } => {
            let id = referenced_ids
                .get(&node.id)
                .expect("referenced fields to be filled in");
            let fields = fields
                .iter()
                .map(|(name, field)| {
                    (
                        name.clone(),
                        lower_node(types, decls, referenced_ids, field),
                    )
                })
                .collect();
            IrNodeValue::StructLiteral(*id, fields)
        }
        AstNodeValue::DictLiteral(_) => todo!(),
        AstNodeValue::ArrayLiteral(_) => todo!(),
        AstNodeValue::ArrayLiteralLength(_, _) => todo!(),

        AstNodeValue::FunctionDeclaration(_)
        | AstNodeValue::ExternFunctionBinding(_)
        | AstNodeValue::StructDeclaration(_)
        | AstNodeValue::UnionDeclaration(_)
        | AstNodeValue::Import(_)
        | AstNodeValue::UniqueType(_)
        | AstNodeValue::SharedType(_)
        | AstNodeValue::NullableType(_)
        | AstNodeValue::ArrayType(_) => unreachable!("Can't have these in a function body"),
    };

    IrNode::from_ast(node, value)
}

fn lower_node_alloc<'ast, 'ir>(
    types: &HashMap<ID, ExpressionType>,
    decls: &HashMap<ID, &'ast StaticDeclaration>,
    context: &HashMap<ID, ID>,
    node: &'ast AstNode<'ast>,
) -> Box<IrNode> {
    Box::new(lower_node(types, decls, context, node))
}

#[derive(Clone, Debug, PartialEq)]
pub struct IrNode {
    pub id: ID,
    pub value: IrNodeValue,
}

impl IrNode {
    pub fn from_ast(ast: &AstNode<'_>, value: IrNodeValue) -> IrNode {
        IrNode { id: ast.id, value }
    }
}

// TODO: should struct fields also be referred to via opaque IDs?

#[derive(Clone, Debug, PartialEq)]
pub enum IrNodeValue {
    /// Give the Nth parameter the given ID
    Parameter(usize, ID),
    VariableReference(ID),
    Declaration(ID),
    // TODO: insert destructors
    Destructor(ID),

    Call(Box<IrNode>, Vec<IrNode>),
    Access(Box<IrNode>, String),
    Assignment(Box<IrNode>, Box<IrNode>),
    Index(Box<IrNode>, Box<IrNode>),
    BinOp(IrBinOp, Box<IrNode>, Box<IrNode>),

    Return(Box<IrNode>),

    Int(i64),
    Float(f64),
    Bool(bool),
    Null,
    CharLiteral(char),
    StringLiteral(String),

    TakeUnique(Box<IrNode>),
    TakeShared(Box<IrNode>),
    Dereference(Box<IrNode>),

    /// Like a Block in that it's a collection of nodes, but the IR
    /// doesn't care about scoping or expressions
    Sequence(Vec<IrNode>),

    // Expressions
    If(Box<IrNode>, Box<IrNode>, Option<Box<IrNode>>),
    While(Box<IrNode>, Box<IrNode>),
    StructLiteral(ID, HashMap<String, IrNode>),
    ArrayLiteral(Vec<IrNode>),
    ArrayLiteralLength(Box<IrNode>, u64),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IrBinOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    LessEqualThan,
    GreaterEqualThan,
    EqualTo,
    NotEquals,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum IrLvalueBinOp {
    Index,
    Assignment,
}
