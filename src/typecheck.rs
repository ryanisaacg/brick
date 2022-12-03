use std::{collections::HashMap, fmt};

use crate::{
    parser::{
        AstExpression, AstExpressionValue, AstStatement, AstStatementValue, AstType, AstTypeValue,
        BinOp, ParseTree,
    },
    provenance::Provenance,
    tree::{Node, NodePtr, SourceTree},
};
use thiserror::Error;

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum NumericType {
    Int64,
    Float64,
}

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
pub enum IRType {
    Void,
    Bool,
    Number(NumericType),
    Unique(usize),
    Function {
        parameters: Vec<usize>,
        returns: usize,
    },
}

// TODO: now that types are arena-allocated, they can't be displayed
impl fmt::Display for IRType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use IRType::*;
        use NumericType::*;
        match self {
            Void => write!(f, "void"),
            Bool => write!(f, "bool"),
            Number(Int64) => write!(f, "i64"),
            Number(Float64) => write!(f, "f64"),
            Unique(inner) => write!(f, "unique {}", inner),
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
    pub declarations: HashMap<String, usize>,
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
                AstStatementValue::Import(_) => {
                    // TODO: disallow inside functions?
                    return Ok(None);
                }
                AstStatementValue::StructDeclaration { .. } => {
                    // TODO: disallow inside functions?
                    return Ok(None);
                }
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
                        .insert(name.to_string(), expr.kind);
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
                        .map(|param| {
                            let kind = ast_type_to_ir(
                                parse_context.kind(param.kind),
                                parse_context,
                                ir_context,
                            )?;
                            let kind = ir_context.add_kind(kind);
                            Ok(FunctionParameter {
                                name: param.name.to_string(),
                                kind,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let returns = returns
                        .as_ref()
                        .map(|type_name| {
                            ast_type_to_ir(
                                parse_context.kind(*type_name),
                                parse_context,
                                ir_context,
                            )
                        })
                        .unwrap_or(Ok(IRType::Void))?;
                    let returns = ir_context.add_kind(returns);
                    let function_type = ir_context.add_kind(IRType::Function {
                        parameters: params.iter().map(|param| param.kind).collect(),
                        returns,
                    });

                    local_scope[0]
                        .declarations
                        .insert(name.clone(), function_type);

                    let mut local_scope = local_scope.clone();
                    local_scope.insert(
                        0,
                        Scope {
                            declarations: params
                                .iter()
                                .map(|FunctionParameter { name, kind }| {
                                    (name.to_string(), *kind)
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

                    // TODO: verify body actually returns that type
                    IRStatementValue::FunctionDeclaration(FunDecl {
                        name: name.to_string(),
                        params,
                        returns,
                        body: ir_context.add_expression(body),
                    })
                }
            };

            Ok(Some(IRStatement { value, start, end }))
        })
        .filter_map(|x| x.transpose())
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
            let local_type = resolve(ir_context, local_scope, name).cloned();
            let expr = parse_context.expression(*expr);
            let expr = typecheck_expression(expr, parse_context, ir_context, local_scope)?;
            match local_type {
                None => todo!(),
                Some(local_type) => {
                    let expr_type = ir_context.kind(expr.kind);
                    if &local_type != expr_type {
                        todo!();
                    }
                }
            }
            let expr = ir_context.add_expression(expr);
            IRExpression {
                value: IRExpressionValue::Assignment(name.clone(), expr),
                kind: ir_context.add_kind(IRType::Void),
                start,
                end,
            }
        }
        Call(function, arguments) => {
            // TODO: check that the # of arguments matches
            let expr = parse_context.expression(*function);
            let function = typecheck_expression(expr, parse_context, ir_context, local_scope)?;
            let (parameter_types, returns) = match ir_context.kind(function.kind).clone() {
                IRType::Function {
                    parameters,
                    returns,
                } => (parameters, returns),
                other => {
                    return Err(TypecheckError::NonCallableExpression(
                        other,
                        function.end,
                    ))
                }
            };

            if arguments.len() != parameter_types.len() {
                return Err(TypecheckError::WrongArgumentCount {
                    found: arguments.len(),
                    expected: parameter_types.len(),
                    provenance: expr.start,
                });
            }

            let arguments = arguments
                .iter()
                .enumerate()
                .map(|(index, argument)| {
                    let argument = parse_context.expression(*argument);
                    let argument =
                        typecheck_expression(argument, parse_context, ir_context, local_scope)?;
                    let argument_kind = ir_context.kind(argument.kind);
                    let parameter_kind = ir_context.kind(parameter_types[index]);
                    println!(
                        "{:?}",
                        ir_context
                            .iter_from(NodePtr::Kind(argument.kind))
                            .collect::<Vec<_>>()
                    );
                    println!(
                        "{:?}",
                        ir_context
                            .iter_from(NodePtr::Kind(parameter_types[index]))
                            .collect::<Vec<_>>()
                    );
                    // TODO: direct comparison of type objects is now problematic, due to the arena
                    if false && argument_kind != parameter_kind {
                        Err(TypecheckError::UnexpectedType {
                            found: argument_kind.clone(),
                            expected: parameter_kind.clone(),
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
            let local_type = resolve(ir_context, local_scope, name);
            match local_type {
                Some(local_type) => IRExpression {
                    value: IRExpressionValue::LocalVariable(name.clone()),
                    kind: ir_context.add_kind(local_type.clone()),
                    start,
                    end,
                },
                None => {
                    println!("{}", name);
                    todo!();
                }
            }
        }
        Bool(val) => IRExpression {
            value: IRExpressionValue::Bool(*val),
            kind: ir_context.add_kind(IRType::Bool),
            start,
            end,
        },
        Int(val) => IRExpression {
            value: IRExpressionValue::Int(*val),
            kind: ir_context.add_kind(IRType::Number(NumericType::Int64)),
            start,
            end,
        },
        Float(val) => IRExpression {
            value: IRExpressionValue::Float(*val),
            kind: ir_context.add_kind(IRType::Number(NumericType::Float64)),
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
            if ir_context.kind(predicate.kind) != &IRType::Bool {
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
                kind: ir_context.add_kind(IRType::Void),
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
            let predicate_type = ir_context.kind(predicate.kind);
            if predicate_type != &IRType::Bool {
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
                kind: ir_context.add_kind(IRType::Void),
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
                kind: ir_context.add_kind(IRType::Void),
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
            let left_kind = ir_context.kind(left.kind).clone();
            let right_kind = ir_context.kind(right.kind).clone();
            if left_kind != right_kind
                || left_kind != IRType::Number(NumericType::Int64)
                    && left_kind != IRType::Number(NumericType::Float64)
            {
                return Err(TypecheckError::BinaryOperandMismatch(
                    left_kind, right_kind, start,
                ));
            } else {
                let left_type = left.kind;
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
                        kind: ir_context.add_kind(IRType::Bool),
                        start,
                        end,
                    }
                }
            }
        }
    })
}

pub fn ast_type_to_ir(
    ast_type: &AstType,
    parse_context: &ParseTree,
    ir_context: &mut IRContext,
) -> Result<IRType, TypecheckError> {
    use IRType::*;
    use NumericType::*;

    Ok(match &ast_type.value {
        AstTypeValue::Name(string) => match string.as_str() {
            "void" => Void,
            "bool" => Bool,
            "i64" => Number(Int64),
            "f64" => Number(Float64),
            _ => todo!(),
        },
        AstTypeValue::Unique(inner) => {
            let inner = parse_context.kind(*inner);
            let inner = ast_type_to_ir(inner, parse_context, ir_context)?;
            let inner = ir_context.add_kind(inner);

            IRType::Unique(inner)
        }
    })
}

fn resolve<'a, 'b>(
    ir_context: &'a IRContext,
    scope: &'a [Scope],
    name: &'b str,
) -> Option<&'a IRType> {
    for level in scope {
        if let Some(kind) = level.declarations.get(name) {
            return Some(ir_context.kind(*kind));
        }
    }

    None
}

pub fn traverse(root: Node<&IRStatement, &IRExpression, &IRType>, children: &mut Vec<NodePtr>) {
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
        Node::Kind(Function {
            parameters,
            returns,
        }) => {
            children.push(NodePtr::Kind(*returns));
            for kind in parameters {
                children.push(NodePtr::Kind(*kind));
            }
        }
        Node::Kind(Unique(inner)) => {
            children.push(NodePtr::Kind(*inner));
        }
        Node::Kind(Void | IRType::Bool | Number(_))
        | Node::Expression(IRExpression {
            value: IRExpressionValue::Bool(_) | Int(_) | Float(_) | LocalVariable(_),
            ..
        }) => {}
    }
}
