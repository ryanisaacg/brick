use std::{collections::HashMap, fmt};

use crate::{
    parser::{
        AstExpression, AstExpressionValue, AstStatement, AstStatementValue, BinOp, ParseTree,
    },
    provenance::Provenance,
    tree::{Node, NodePtr, SourceTree},
};
use thiserror::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum NumericType {
    Int64,
    Float64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Void,
    Bool,
    Number(NumericType),
    // TODO: arena-allocate types?
    Function {
        parameters: Vec<Type>,
        returns: Box<Type>,
    },
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use NumericType::*;
        use Type::*;
        match self {
            Void => write!(f, "void"),
            Bool => write!(f, "bool"),
            Number(Int64) => write!(f, "i64"),
            Number(Float64) => write!(f, "f64"),
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
        }
    }
}

pub type IRContext = SourceTree<IRStatement, IRExpression>;

#[derive(Debug, Error)]
pub enum TypecheckError {
    #[error("Operands types {0} and {1} don't match at {2}")]
    BinaryOperandMismatch(Type, Type, Provenance),
    #[error("Attempted to assign to an illegal value at {0}")]
    IllegalLeftHandValue(Provenance),
    #[error("Attempted to call a non-callable expression (type {0} at {1}")]
    NonCallableExpression(Type, Provenance),
    #[error("Found {found}, expected {expected} at {provenance}")]
    UnexpectedType {
        found: Type,
        expected: Type,
        provenance: Provenance,
    },
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
    pub returns: Type,
    pub body: usize,
}

#[derive(Debug, PartialEq, Eq)]
pub struct FunctionParameter {
    pub name: String,
    pub kind: Type,
}

#[derive(Debug)]
pub struct IRExpression {
    pub value: IRExpressionValue,
    pub kind: Type,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug)]
pub enum IRExpressionValue {
    Assignment(String, usize),
    Call(usize, Vec<usize>),
    Bool(bool),
    Int(i64),
    Float(f64),
    LocalVariable(String),
    BinaryNumeric(BinOpNumeric, usize, usize),
    Comparison(BinOpComparison, usize, usize),
    If(usize, usize),
    While(usize, usize),
    /// Importantly, Block references statements, not expressions!
    Block(Vec<usize>),
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
    declarations: HashMap<String, Type>,
}

// TODO: unification of separate IRContexts?
// TODO: initial header-file like parse

pub fn typecheck(
    statements: impl Iterator<Item = usize>,
    ir_context: &mut IRContext,
    parse_context: &ParseTree,
    scopes: &[Scope],
) -> Result<Vec<IRStatement>, TypecheckError> {
    // TODO: cons cell instead?
    let mut local_scope = Vec::with_capacity(1 + scopes.len());
    local_scope.push(Scope {
        declarations: HashMap::new(),
    });
    local_scope.extend(scopes.iter().cloned());

    let statements = statements
        .map(|statement| {
            let AstStatement { value, start, end } = parse_context.statement(statement);
            let start = *start;
            let end = *end;

            let value = match value {
                AstStatementValue::Expression(expr) => {
                    let expr = typecheck_expression(
                        parse_context.expression(*expr),
                        parse_context,
                        ir_context,
                        &local_scope[..],
                    )?;
                    IRStatementValue::Expression(ir_context.add_expression(expr))
                }
                AstStatementValue::Declaration(name, expr) => {
                    let expr = typecheck_expression(
                        parse_context.expression(*expr),
                        parse_context,
                        ir_context,
                        &local_scope[..],
                    )?;
                    local_scope[0]
                        .declarations
                        .insert(name.to_string(), expr.kind.clone());
                    IRStatementValue::Declaration(name.clone(), ir_context.add_expression(expr))
                }
                AstStatementValue::FunctionDeclaration {
                    name,
                    params,
                    returns,
                    body,
                } => {
                    let params = params
                        .iter()
                        .map(|param| FunctionParameter {
                            name: param.name.to_string(),
                            kind: type_name_to_type(param.kind.as_ref()),
                        })
                        .collect::<Vec<_>>();

                    let returns = returns
                        .as_ref()
                        .map(|type_name| type_name_to_type(type_name.as_ref()))
                        .unwrap_or(Type::Void);

                    local_scope[0].declarations.insert(
                        name.clone(),
                        Type::Function {
                            parameters: params.iter().map(|param| param.kind.clone()).collect(),
                            returns: Box::new(returns.clone()),
                        },
                    );

                    let mut local_scope = local_scope.clone();
                    local_scope.insert(
                        0,
                        Scope {
                            declarations: params
                                .iter()
                                .map(|FunctionParameter { name, kind }| {
                                    (name.to_string(), kind.clone())
                                })
                                .collect(),
                        },
                    );
                    let body = typecheck_expression(
                        parse_context.expression(*body),
                        parse_context,
                        ir_context,
                        &local_scope[..],
                    )?;

                    IRStatementValue::FunctionDeclaration(FunDecl {
                        name: name.to_string(),
                        params,
                        returns,
                        body: ir_context.add_expression(body),
                    })
                }
            };

            Ok(IRStatement { value, start, end })
        })
        .collect::<Result<Vec<IRStatement>, TypecheckError>>()?;

    Ok(statements)
}

// TODO: allow accessing the local environment
pub fn typecheck_expression(
    expression: &AstExpression,
    parse_context: &ParseTree,
    ir_context: &mut IRContext,
    local_scope: &[Scope],
) -> Result<IRExpression, TypecheckError> {
    use AstExpressionValue::*;
    let AstExpression { value, start, end } = expression;
    let start = *start;
    let end = *end;

    // TODO: analyze if, block
    Ok(match value {
        Assignment(target, expr) => {
            // TODO: provide more error diagnostics
            // TODO: be able to assign to more things than just words
            let name = match parse_context.expression(*target) {
                AstExpression {
                    value: AstExpressionValue::Name(name),
                    ..
                } => name,
                AstExpression { start, .. } => {
                    return Err(TypecheckError::IllegalLeftHandValue(*start))
                }
            };
            let local_type = resolve(local_scope, name);
            let expr = parse_context.expression(*expr);
            let expr = typecheck_expression(expr, parse_context, ir_context, local_scope)?;
            match local_type {
                None => todo!(),
                Some(local_type) => {
                    if local_type != &expr.kind {
                        todo!();
                    }
                }
            }
            let expr = ir_context.add_expression(expr);
            IRExpression {
                value: IRExpressionValue::Assignment(name.clone(), expr),
                kind: Type::Void,
                start,
                end,
            }
        }
        Call(function, arguments) => {
            let expr = parse_context.expression(*function);
            let function = typecheck_expression(expr, parse_context, ir_context, local_scope)?;
            let (argument_types, returns) = match &function.kind {
                Type::Function {
                    parameters,
                    returns,
                } => (parameters, returns.as_ref().clone()),
                other => {
                    return Err(TypecheckError::NonCallableExpression(
                        other.clone(),
                        function.end,
                    ))
                }
            };

            let arguments = arguments
                .iter()
                .enumerate()
                .map(|(index, argument)| {
                    let argument = parse_context.expression(*argument);
                    let argument =
                        typecheck_expression(argument, parse_context, ir_context, local_scope)?;
                    if argument.kind != argument_types[index] {
                        Err(TypecheckError::UnexpectedType {
                            found: argument.kind.clone(),
                            expected: argument_types[index].clone(),
                            provenance: argument.start,
                        })
                    } else {
                        Ok(ir_context.add_expression(argument))
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;

            let function = ir_context.add_expression(function);

            IRExpression {
                value: IRExpressionValue::Call(function, arguments),
                kind: returns,
                start,
                end,
            }
        }
        Name(name) => {
            let local_type = resolve(local_scope, name);
            match local_type {
                Some(local_type) => IRExpression {
                    value: IRExpressionValue::LocalVariable(name.clone()),
                    kind: local_type.clone(),
                    start,
                    end,
                },
                None => todo!(),
            }
        }
        Bool(val) => IRExpression {
            value: IRExpressionValue::Bool(*val),
            kind: Type::Bool,
            start,
            end,
        },
        Int(val) => IRExpression {
            value: IRExpressionValue::Int(*val),
            kind: Type::Number(NumericType::Int64),
            start,
            end,
        },
        Float(val) => IRExpression {
            value: IRExpressionValue::Float(*val),
            kind: Type::Number(NumericType::Float64),
            start,
            end,
        },
        If(predicate, block) => {
            let predicate = typecheck_expression(
                parse_context.expression(*predicate),
                parse_context,
                ir_context,
                local_scope,
            )?;
            if predicate.kind != Type::Bool {
                todo!(); // compiler diagnostic
            }
            let block = typecheck_expression(
                parse_context.expression(*block),
                parse_context,
                ir_context,
                local_scope,
            )?;
            // TODO: if-else can return types
            IRExpression {
                value: IRExpressionValue::If(
                    ir_context.add_expression(predicate),
                    ir_context.add_expression(block),
                ),
                kind: Type::Void,
                start,
                end,
            }
        }
        // TODO
        While(predicate, block) => {
            let predicate = typecheck_expression(
                parse_context.expression(*predicate),
                parse_context,
                ir_context,
                local_scope,
            )?;
            if predicate.kind != Type::Bool {
                todo!(); // compiler diagnostic
            }
            let block = typecheck_expression(
                parse_context.expression(*block),
                parse_context,
                ir_context,
                local_scope,
            )?;
            // TODO: if-else can return types
            IRExpression {
                value: IRExpressionValue::While(
                    ir_context.add_expression(predicate),
                    ir_context.add_expression(block),
                ),
                kind: Type::Void,
                start,
                end,
            }
        }
        Block(parse_statements) => {
            let ir_statements = typecheck(
                parse_statements.iter().cloned(),
                ir_context,
                parse_context,
                local_scope,
            )?;
            let ir_statements = ir_statements
                .into_iter()
                .map(|statement| ir_context.add_statement(statement))
                .collect();
            // TODO: support returning last element if non-semicolon
            IRExpression {
                value: IRExpressionValue::Block(ir_statements),
                kind: Type::Void,
                start,
                end,
            }
        }
        BinExpr(
            op @ (BinOp::Add | BinOp::Subtract | BinOp::LessThan | BinOp::GreaterThan),
            left,
            right,
        ) => {
            let left = typecheck_expression(
                parse_context.expression(*left),
                parse_context,
                ir_context,
                local_scope,
            )?;
            let right = typecheck_expression(
                parse_context.expression(*right),
                parse_context,
                ir_context,
                local_scope,
            )?;
            if left.kind != right.kind
                || left.kind != Type::Number(NumericType::Int64)
                    && left.kind != Type::Number(NumericType::Float64)
            {
                return Err(TypecheckError::BinaryOperandMismatch(
                    left.kind, right.kind, start,
                ));
            } else {
                let left_type = left.kind.clone();
                if *op == BinOp::Add || *op == BinOp::Subtract {
                    IRExpression {
                        value: IRExpressionValue::BinaryNumeric(
                            match op {
                                BinOp::Add => BinOpNumeric::Add,
                                BinOp::Subtract => BinOpNumeric::Subtract,
                                _ => unreachable!(),
                            },
                            ir_context.add_expression(left),
                            ir_context.add_expression(right),
                        ),
                        kind: left_type,
                        start,
                        end,
                    }
                } else {
                    IRExpression {
                        value: IRExpressionValue::Comparison(
                            match op {
                                BinOp::LessThan => BinOpComparison::LessThan,
                                BinOp::GreaterThan => BinOpComparison::GreaterThan,
                                _ => unreachable!(),
                            },
                            ir_context.add_expression(left),
                            ir_context.add_expression(right),
                        ),
                        kind: Type::Bool,
                        start,
                        end,
                    }
                }
            }
        }
    })
}

fn type_name_to_type(name: &str) -> Type {
    use NumericType::*;
    use Type::*;

    match name {
        "void" => Void,
        "bool" => Bool,
        "i64" => Number(Int64),
        "f64" => Number(Float64),
        _ => todo!(),
    }
}

fn resolve<'a, 'b>(scope: &'a [Scope], name: &'b str) -> Option<&'a Type> {
    for level in scope {
        if let Some(kind) = level.declarations.get(name) {
            return Some(kind);
        }
    }

    None
}

pub fn traverse(root: Node<&IRStatement, &IRExpression>, children: &mut Vec<NodePtr>) {
    use IRExpressionValue::*;
    use IRStatementValue::*;

    match root {
        Node::Statement(IRStatement {
            value:
                Declaration(_, child)
                | Expression(child)
                | FunctionDeclaration(FunDecl { body: child, .. }),
            ..
        })
        | Node::Expression(IRExpression {
            value: Assignment(_, child),
            ..
        }) => {
            children.push(NodePtr::Expression(*child));
        }
        Node::Expression(IRExpression {
            value:
                BinaryNumeric(_, left, right)
                | Comparison(_, left, right)
                | If(left, right)
                | While(left, right),
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
        Node::Expression(IRExpression {
            value: Bool(_) | Int(_) | Float(_) | LocalVariable(_),
            ..
        }) => {}
    }
}
