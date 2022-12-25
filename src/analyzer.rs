use std::{collections::HashMap, fmt};

use crate::{arena::ArenaNode, provenance::Provenance};

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

pub struct IRContext {
    pub nodes: Vec<IRNode>,
    pub types: Vec<IRType>,
}

impl IRContext {
    pub fn new() -> IRContext {
        IRContext {
            nodes: Vec::new(),
            types: vec![
                IRType::Void,
                IRType::Bool,
                IRType::Number(NumericType::Int32),
                IRType::Number(NumericType::Int64),
                IRType::Number(NumericType::Float32),
                IRType::Number(NumericType::Float64),
            ],
        }
    }

    pub fn node(&self, idx: usize) -> &IRNode {
        &self.nodes[idx]
    }

    pub fn kind(&self, idx: usize) -> &IRType {
        &self.types[idx]
    }

    pub fn add_node(&mut self, node: IRNode) -> usize {
        let idx = self.nodes.len();
        self.nodes.push(node);
        idx
    }

    pub fn add_kind(&mut self, kind: IRType) -> usize {
        match kind {
            IRType::Void => VOID_KIND,
            IRType::Bool => BOOL_KIND,
            IRType::Number(NumericType::Int32) => I32_KIND,
            IRType::Number(NumericType::Int64) => I64_KIND,
            IRType::Number(NumericType::Float32) => F32_KIND,
            IRType::Number(NumericType::Float64) => F64_KIND,
            kind => {
                let idx = self.types.len();
                self.types.push(kind);
                idx
            }
        }
    }
}

impl Default for IRContext {
    fn default() -> Self {
        Self::new()
    }
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

#[derive(Debug, Error)]
pub enum TypecheckError {
    #[error("Expected numeric operand, got {0} at {1}")]
    BinaryNonNumericOperand(IRType, Provenance),
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
    #[error("Attempted to index with type {0} at {1}, expected i32")]
    IllegalNonNumericIndex(IRType, Provenance),
    #[error("The dot operator must be followed by a name at {0}")]
    IllegalRightHandDotOperand(Provenance),
    #[error("Field {0} not found on {1} at {2}")]
    FieldNotFound(String, IRType, Provenance),
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
pub struct IRNode {
    pub value: IRNodeValue,
    pub kind: usize,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug)]
pub enum IRNodeValue {
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
    Block(Vec<usize>),
    FunctionDeclaration(FunDecl),
    Expression(usize),
    Declaration(String, usize),

    // Generated by the compiler
    Dereference(usize),
    Promote(usize),
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

impl ArenaNode for IRNode {
    fn write_children(&self, children: &mut Vec<usize>) {
        use IRNodeValue::*;

        match &self.value {
            Declaration(_, child)
            | Expression(child)
            | FunctionDeclaration(FunDecl { body: child, .. })
            | Dereference(child)
            | TakeUnique(child)
            | TakeShared(child)
            | Dot(child, _)
            | Promote(child)
            | ArrayLiteralLength(child, _) => {
                children.push(*child);
            }
            BinaryNumeric(_, left, right)
            | Comparison(_, left, right)
            | If(left, right)
            | While(left, right)
            | Assignment(left, right)
            | ArrayIndex(left, right) => {
                children.push(*right);
                children.push(*left);
            }
            Call(function, arguments) => {
                children.push(*function);
                for arg in arguments {
                    children.push(*arg);
                }
            }
            Block(statements) => {
                for statement in statements {
                    children.push(*statement);
                }
            }
            StructLiteral(fields) => {
                for field in fields.values() {
                    children.push(*field);
                }
            }
            Bool(_) | Int(_) | Float(_) | LocalVariable(_) => {}
        }
    }
}
/*Node::Kind(Function {
    parameters,
    returns,
}) => {
    children.push(NodePtr::Kind(*returns));
    for kind in parameters {
        children.push(NodePtr::Kind(*kind));
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
| Node::Expression(IRNode {
    value: IRNodeValue::Bool(_) | Int(_) | Float(_) | LocalVariable(_),
    ..
}) => {}*/
