use std::{collections::HashMap, fmt};

use crate::{
    provenance::Provenance,
    tree::{Node, NodePtr, SourceTree},
};

use thiserror::Error;

mod resolve_type_names;
mod scan;
mod typecheck;

pub use resolve_type_names::resolve_type_names;
pub use scan::{scan_top_level, ScanResults};
pub use typecheck::typecheck;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum NumericType {
    Int32,
    Int64,
    Float32,
    Float64,
}

// Make sure to update are_types_equal in typecheck
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IRType {
    Unresolved(String, Provenance),
    // TODO: should Void be represented differently, possibly as a None branch on an
    // Option<IRType>?
    Void,
    Bool,
    Number(NumericType),
    Unique(usize),
    Shared(usize),
    Array(usize),
    Function {
        parameters: Vec<usize>,
        returns: usize,
    },
    Struct {
        fields: HashMap<String, usize>,
    },
}

pub const VOID_KIND: usize = 0;
pub const BOOL_KIND: usize = 1;
pub const I32_KIND: usize = 2;
pub const I64_KIND: usize = 3;
pub const F32_KIND: usize = 4;
pub const F64_KIND: usize = 5;

pub fn new_ir_context() -> IRContext {
    let mut ir_context = IRContext::new(Box::new(traverse));
    ir_context.add_kind(IRType::Void);
    ir_context.add_kind(IRType::Bool);
    ir_context.add_kind(IRType::Number(NumericType::Int32));
    ir_context.add_kind(IRType::Number(NumericType::Int64));
    ir_context.add_kind(IRType::Number(NumericType::Float32));
    ir_context.add_kind(IRType::Number(NumericType::Float64));

    ir_context
}

// TODO: now that types are arena-allocated, they can't be displayed
impl fmt::Display for IRType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use IRType::*;
        use NumericType::*;
        match self {
            Unresolved(string, provenance) => {
                write!(f, "unresolved name {} at {}", string, provenance)
            }
            Void => write!(f, "void"),
            Bool => write!(f, "bool"),
            Number(Int32) => write!(f, "i32"),
            Number(Float32) => write!(f, "f32"),
            Number(Int64) => write!(f, "i64"),
            Number(Float64) => write!(f, "f64"),
            Unique(inner) => write!(f, "unique {}", inner),
            Shared(inner) => write!(f, "shared {}", inner),
            Array(inner) => write!(f, "array {}", inner),
            Function {
                parameters,
                returns,
            } => {
                write!(f, "fn(")?;
                let mut arg_iter = parameters.iter().peekable();
                while let Some(arg) = arg_iter.next() {
                    write!(f, "{}", arg)?;
                    if arg_iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ": {}", returns)
            }
            Struct { fields } => {
                write!(f, "struct {{ ")?;
                let mut fields_iter = fields.iter().peekable();
                while let Some((key, value)) = fields_iter.next() {
                    write!(f, "{}: {},", key, value)?;
                    if fields_iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")
            }
        }
    }
}

pub type IRContext = SourceTree<IRStatement, IRExpression, IRType>;

#[derive(Debug, Error)]
pub enum TypecheckError {
    #[error("Operands types {0} and {1} don't match at {2}")]
    BinaryOperandMismatch(IRType, IRType, Provenance),
    #[error("Attempted to assign to an illegal value at {0}")]
    IllegalLeftHandValue(Provenance),
    #[error("Attempted to call a non-callable expression (type {0} at {1}")]
    NonCallableExpression(IRType, Provenance),
    #[error("Found {found}, expected {expected} at {provenance}")]
    UnexpectedType {
        found: IRType,
        expected: IRType,
        provenance: Provenance,
    },
    #[error("Found {found} arguments, expected {expected} at {provenance}")]
    WrongArgumentCount {
        found: usize,
        expected: usize,
        provenance: Provenance,
    },
    #[error("Attempted to reference unknown name {0} at {1}")]
    UnknownName(String, Provenance),
    #[error("Attempted to reference a field on a non-struct type {0} at {1}")]
    IllegalLeftDotOperand(IRType, Provenance),
    #[error("Attempted to index a non-array-type {0} at {1}")]
    IllegalNonArrayIndex(IRType, Provenance),
    #[error("Attempted to index with non-numeric-type {0} at {1}")]
    IllegalNonNumericIndex(IRType, Provenance),
    #[error("The dot operator must be followed by a name at {0}")]
    IllegalRightHandDotOperand(Provenance),
    #[error("Field {0} not found on {1} at {2}")]
    FieldNotFound(String, IRType, Provenance),
}

#[derive(Debug)]
pub struct IRStatement {
    pub value: IRStatementValue,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug)]
pub enum IRStatementValue {
    FunctionDeclaration(FunDecl),
    Expression(usize),
    Declaration(String, usize),
}

#[derive(Debug)]
pub struct FunDecl {
    pub name: String,
    pub params: Vec<FunctionParameter>,
    pub returns: usize,
    pub body: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionParameter {
    pub name: String,
    pub kind: usize,
}

#[derive(Debug)]
pub struct IRExpression {
    pub value: IRExpressionValue,
    pub kind: usize,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug)]
pub enum IRExpressionValue {
    // More-or-less direct translation from the AST
    Assignment(usize, usize),
    Call(usize, Vec<usize>),
    Bool(bool),
    Int(i64),
    Float(f64),
    LocalVariable(String),
    StructLiteral(HashMap<String, usize>),
    Dot(usize, String),
    BinaryNumeric(BinOpNumeric, usize, usize),
    ArrayIndex(usize, usize),
    ArrayLiteralLength(usize, u64),
    Comparison(BinOpComparison, usize, usize),
    If(usize, usize),
    While(usize, usize),
    TakeUnique(usize),
    TakeShared(usize),
    /// Importantly, Block references statements, not expressions!
    Block(Vec<usize>),

    // Generated by the compiler
    Dereference(usize),
}

#[derive(Debug)]
pub enum BinOpNumeric {
    Add,
    Subtract,
}

#[derive(Debug)]
pub enum BinOpComparison {
    LessThan,
    GreaterThan,
}

#[derive(Clone, Debug)]
pub struct Scope {
    pub declarations: HashMap<String, usize>,
}

fn traverse(root: Node<&IRStatement, &IRExpression, &IRType>, children: &mut Vec<NodePtr>) {
    use IRExpressionValue::*;
    use IRStatementValue::*;
    use IRType::*;

    match root {
        Node::Statement(IRStatement {
            value:
                Declaration(_, child)
                | Expression(child)
                | FunctionDeclaration(FunDecl { body: child, .. }),
            ..
        })
        | Node::Expression(IRExpression {
            value:
                Dereference(child)
                | TakeUnique(child)
                | TakeShared(child)
                | Dot(child, _)
                | ArrayLiteralLength(child, _),
            ..
        }) => {
            children.push(NodePtr::Expression(*child));
        }
        Node::Expression(IRExpression {
            value:
                BinaryNumeric(_, left, right)
                | Comparison(_, left, right)
                | If(left, right)
                | While(left, right)
                | Assignment(left, right)
                | ArrayIndex(left, right),
            ..
        }) => {
            children.push(NodePtr::Expression(*right));
            children.push(NodePtr::Expression(*left));
        }
        Node::Expression(IRExpression {
            value: Call(function, arguments),
            ..
        }) => {
            children.push(NodePtr::Expression(*function));
            for arg in arguments {
                children.push(NodePtr::Expression(*arg));
            }
        }
        Node::Expression(IRExpression {
            value: Block(statements),
            ..
        }) => {
            for statement in statements {
                children.push(NodePtr::Statement(*statement));
            }
        }
        Node::Kind(Function {
            parameters,
            returns,
        }) => {
            children.push(NodePtr::Kind(*returns));
            for kind in parameters {
                children.push(NodePtr::Kind(*kind));
            }
        }
        Node::Expression(IRExpression {
            value: StructLiteral(fields),
            ..
        }) => {
            for field in fields.values() {
                children.push(NodePtr::Expression(*field));
            }
        }
        Node::Kind(Struct { fields, .. }) => {
            for field in fields.values() {
                children.push(NodePtr::Kind(*field));
            }
        }
        Node::Kind(Unique(inner) | Shared(inner) | Array(inner)) => {
            children.push(NodePtr::Kind(*inner));
        }
        Node::Kind(Void | Unresolved(..) | IRType::Bool | Number(_))
        | Node::Expression(IRExpression {
            value: IRExpressionValue::Bool(_) | Int(_) | Float(_) | LocalVariable(_),
            ..
        }) => {}
    }
}
