use std::collections::HashMap;

use crate::{
    parser::{
        AstExpression, AstExpressionValue, AstStatement, AstStatementValue, BinOp, ParseTree,
    },
    provenance::Provenance,
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
}

pub struct IRContext {
    pub statements: Vec<IRStatement>,
    pub expressions: Vec<IRExpression>,
}

impl IRContext {
    pub fn expression(&self, index: usize) -> &IRExpression {
        &self.expressions[index]
    }

    pub fn statement(&self, index: usize) -> &IRStatement {
        &self.statements[index]
    }

    fn add_expression(&mut self, expr: IRExpression) -> usize {
        let index = self.expressions.len();
        self.expressions.push(expr);
        index
    }

    fn add_statement(&mut self, statement: IRStatement) -> usize {
        let index = self.statements.len();
        self.statements.push(statement);
        index
    }
}

#[derive(Debug, Error)]
pub enum TypecheckError {
    #[error("Operands don't match")]
    BinaryOperandMismatch,
}

#[derive(Debug)]
pub struct IRStatement {
    pub value: IRStatementValue,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug)]
pub enum IRStatementValue {
    Expression(IRExpression),
    Declaration(String, IRExpression),
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

// TODO: add a generic arena allocator to the top-level

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

pub fn typecheck<'a>(
    statements: impl Iterator<Item = &'a AstStatement>,
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
            let AstStatement { value, start, end } = statement;
            let start = start.clone();
            let end = end.clone();

            let value = match value {
                AstStatementValue::Expression(expr) => {
                    let expr = typecheck_expression(
                        parse_context.expression(*expr),
                        parse_context,
                        ir_context,
                        &local_scope[..],
                    )?;
                    IRStatementValue::Expression(expr)
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
                    IRStatementValue::Declaration(name.clone(), expr)
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
    let start = start.clone();
    let end = end.clone();

    // TODO: analyze if, block
    Ok(match value {
        Assignment(name, expr) => {
            let local_type = resolve(&local_scope[..], &name);
            let expr = parse_context.expression(*expr);
            let expr = typecheck_expression(expr, parse_context, ir_context, &local_scope[..])?;
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
                parse_statements
                    .iter()
                    .map(|statement| parse_context.statement(*statement)),
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
        BinExpr(op @ (BinOp::Add | BinOp::Subtract | BinOp::LessThan | BinOp::GreaterThan), left, right) => {
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
                return Err(TypecheckError::BinaryOperandMismatch);
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

fn resolve<'a, 'b>(scope: &'a [Scope], name: &'b str) -> Option<&'a Type> {
    for level in scope {
        if let Some(kind) = level.declarations.get(name) {
            return Some(kind);
        }
    }

    None
}
