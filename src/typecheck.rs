use std::collections::HashMap;

use crate::{parser::{AstExpressionValue, AstStatementValue, BinOp, AstExpression, AstStatement}, provenance::Provenance};
use thiserror::Error;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    Int64,
    Float64,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
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
    Assignment(String, IRExpression),
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
    Int(i64),
    Float(f64),
    LocalVariable(String),
    BinaryNumeric(BinOpNumeric, usize, usize),
}

#[derive(Debug)]
pub enum BinOpNumeric {
    Add,
    Subtract,
}

pub fn typecheck(
    statements: &Vec<AstStatement>,
    expression_arena: &Vec<AstExpression>,
) -> Result<(Vec<IRStatement>, Vec<IRExpression>), TypecheckError> {
    let mut typed_arena = Vec::new();
    let mut local_environment = HashMap::new();

    let statements = statements
        .iter()
        .map(|statement| {
            let AstStatement { value, start, end } = statement;
            let start = start.clone();
            let end = end.clone();

            let value = match value {
                AstStatementValue::Expression(expr) => {
                    let expr = typecheck_expression(
                        expr,
                        &expression_arena,
                        &mut typed_arena,
                        &local_environment,
                    )?;
                    IRStatementValue::Expression(expr)
                }
                AstStatementValue::Assignment(name, expr) => {
                    let local_type = local_environment.get(name);
                    let expr = typecheck_expression(
                        expr,
                        &expression_arena,
                        &mut typed_arena,
                        &local_environment,
                    )?;
                    match local_type {
                        None => todo!(),
                        Some(local_type) => {
                            if local_type != &expr.kind {
                                todo!();
                            }
                        }
                    }
                    IRStatementValue::Assignment(name.clone(), expr)
                }
                AstStatementValue::Declaration(name, expr) => {
                    let expr = typecheck_expression(
                        expr,
                        &expression_arena,
                        &mut typed_arena,
                        &local_environment,
                    )?;
                    local_environment.insert(name, expr.kind.clone());
                    IRStatementValue::Declaration(name.clone(), expr)
                }
            };

            Ok(IRStatement { value, start, end })
        })
        .collect::<Result<Vec<IRStatement>, TypecheckError>>()?;

    Ok((statements, typed_arena))
}

// TODO: allow accessing the local environment
pub fn typecheck_expression(
    expression: &AstExpression,
    expression_arena: &Vec<AstExpression>,
    typed_arena: &mut Vec<IRExpression>,
    local_environment: &HashMap<&String, Type>,
) -> Result<IRExpression, TypecheckError> {
    use AstExpressionValue::*;
    let AstExpression { value, start, end } = expression;
    let start = start.clone();
    let end = end.clone();

    match value {
        Name(name) => {
            let local_type = local_environment.get(name);
            match local_type {
                Some(local_type) => Ok(IRExpression {
                    value: IRExpressionValue::LocalVariable(name.clone()),
                    kind: local_type.clone(),
                    start,
                    end,
                }),
                None => todo!(),
            }
        }
        Int(val) => Ok(IRExpression {
            value: IRExpressionValue::Int(*val),
            kind: Type::Primitive(PrimitiveType::Int64),
            start,
            end,
        }),
        Float(val) => Ok(IRExpression {
            value: IRExpressionValue::Float(*val),
            kind: Type::Primitive(PrimitiveType::Float64),
            start,
            end,
        }),
        BinExpr(op @ (BinOp::Add | BinOp::Subtract), left, right) => {
            let left = typecheck_expression(
                &expression_arena[*left],
                expression_arena,
                typed_arena,
                local_environment,
            )?;
            let right = typecheck_expression(
                &expression_arena[*right],
                expression_arena,
                typed_arena,
                local_environment,
            )?;
            if left.kind != right.kind
                || left.kind != Type::Primitive(PrimitiveType::Int64)
                    && left.kind != Type::Primitive(PrimitiveType::Float64)
            {
                Err(TypecheckError::BinaryOperandMismatch)
            } else {
                let left_ptr = typed_arena.len();
                let left_type = left.kind.clone();
                typed_arena.push(left);
                typed_arena.push(right);
                Ok(IRExpression {
                    value: IRExpressionValue::BinaryNumeric(
                        match op {
                            BinOp::Add => BinOpNumeric::Add,
                            BinOp::Subtract => BinOpNumeric::Subtract,
                        },
                        left_ptr,
                        left_ptr + 1,
                    ),
                    kind: left_type,
                    start,
                    end,
                })
            }
        }
    }
}
