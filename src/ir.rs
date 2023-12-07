use std::collections::HashMap;

use crate::{
    id::ID,
    parser::AstNode,
    typecheck::{ExpressionType, StaticDeclaration, TypecheckedFile},
};

mod lower;
mod rewrite_associated_functions;

pub fn lower_module<'ast>(
    module: TypecheckedFile<'ast>,
    declarations: &HashMap<ID, &'ast StaticDeclaration>,
) -> IrModule {
    let mut module = lower::lower_module(module, declarations);

    let mut rewrite_assc = |expr: &mut _| rewrite_associated_functions::rewrite(declarations, expr);

    for expr in module.top_level_statements.iter_mut() {
        expr.visit_mut(&mut rewrite_assc);
    }
    for func in module.functions.iter_mut() {
        func.body.visit_mut(&mut rewrite_assc);
    }

    module
}

// TODO: should the IR be a stack machine?

pub struct IrModule {
    pub top_level_statements: Vec<IrNode>,
    // TODO: include imports, structs, and extern function declaration
    pub functions: Vec<IrFunction>,
}

#[derive(Clone, Debug)]
pub struct IrFunction {
    pub id: ID,
    pub name: String,
    pub body: IrNode,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IrNode {
    pub id: ID,
    pub value: IrNodeValue,
    pub ty: ExpressionType,
}

impl IrNode {
    pub fn from_ast(ast: &AstNode<'_>, value: IrNodeValue, ty: ExpressionType) -> IrNode {
        IrNode {
            id: ast.id,
            value,
            ty,
        }
    }

    pub fn from_ast_void(ast: &AstNode<'_>, value: IrNodeValue) -> IrNode {
        Self::from_ast(ast, value, ExpressionType::Void)
    }

    pub fn visit_mut(&mut self, callback: &mut impl FnMut(&mut IrNode)) {
        callback(self);
        match &mut self.value {
            IrNodeValue::Parameter(_, _)
            | IrNodeValue::VariableReference(_)
            | IrNodeValue::Declaration(_)
            | IrNodeValue::Int(_)
            | IrNodeValue::Float(_)
            | IrNodeValue::Bool(_)
            | IrNodeValue::CharLiteral(_)
            | IrNodeValue::StringLiteral(_)
            | IrNodeValue::Null => {}
            IrNodeValue::Call(lhs, params) => {
                lhs.visit_mut(callback);
                for param in params.iter_mut() {
                    param.visit_mut(callback);
                }
            }
            IrNodeValue::Access(child, _)
            | IrNodeValue::TakeUnique(child)
            | IrNodeValue::TakeShared(child)
            | IrNodeValue::Dereference(child)
            | IrNodeValue::ArrayLiteralLength(child, _)
            | IrNodeValue::Return(child) => {
                child.visit_mut(callback);
            }
            IrNodeValue::Assignment(lhs, rhs)
            | IrNodeValue::Index(lhs, rhs)
            | IrNodeValue::While(lhs, rhs)
            | IrNodeValue::BinOp(_, lhs, rhs) => {
                lhs.visit_mut(callback);
                rhs.visit_mut(callback);
            }
            IrNodeValue::Sequence(children) | IrNodeValue::ArrayLiteral(children) => {
                for child in children.iter_mut() {
                    child.visit_mut(callback);
                }
            }
            IrNodeValue::If(cond, if_branch, else_branch) => {
                cond.visit_mut(callback);
                if_branch.visit_mut(callback);
                if let Some(else_branch) = else_branch {
                    else_branch.visit_mut(callback);
                }
            }
            IrNodeValue::StructLiteral(_, fields) => {
                for field in fields.values_mut() {
                    field.visit_mut(callback);
                }
            }
        }
    }
}

// TODO: should struct fields also be referred to via opaque IDs?

#[derive(Clone, Debug, PartialEq)]
pub enum IrNodeValue {
    /// Give the Nth parameter the given ID
    Parameter(usize, ID),
    VariableReference(ID),
    Declaration(ID),

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
