use std::collections::HashMap;

use crate::parser::{AstExpression, AstStatement, BinOp};
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
pub enum IRStatement {
    Expression(IRExpression),
    Declaration(String, IRExpression),
    Assignment(String, IRExpression),
}

#[derive(Debug)]
pub struct IRExpression(pub IRExpressionValue, pub Type);

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
            Ok(match statement {
                AstStatement::Expression(expr) => {
                    let expr = typecheck_expression(
                        expr,
                        &expression_arena,
                        &mut typed_arena,
                        &local_environment,
                    )?;
                    IRStatement::Expression(expr)
                }
                AstStatement::Assignment(name, expr) => {
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
                            if local_type != &expr.1 {
                                todo!();
                            }
                        }
                    }
                    IRStatement::Assignment(name.clone(), expr)
                }
                AstStatement::Declaration(name, expr) => {
                    let expr = typecheck_expression(
                        expr,
                        &expression_arena,
                        &mut typed_arena,
                        &local_environment,
                    )?;
                    local_environment.insert(name, expr.1.clone());
                    IRStatement::Declaration(name.clone(), expr)
                }
            })
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
    use AstExpression::*;
    match expression {
        Name(name) => {
            let local_type = local_environment.get(name);
            match local_type {
                Some(local_type) => Ok(IRExpression(
                    IRExpressionValue::LocalVariable(name.clone()),
                    local_type.clone(),
                )),
                None => todo!(),
            }
        }
        Int(val) => Ok(IRExpression(
            IRExpressionValue::Int(*val),
            Type::Primitive(PrimitiveType::Int64),
        )),
        Float(val) => Ok(IRExpression(
            IRExpressionValue::Float(*val),
            Type::Primitive(PrimitiveType::Float64),
        )),
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
            if left.1 != right.1 || left.1 != Type::Primitive(PrimitiveType::Int64) && left.1 != Type::Primitive(PrimitiveType::Float64) {
                Err(TypecheckError::BinaryOperandMismatch)
            } else {
                let left_ptr = typed_arena.len();
                let left_type = left.1.clone();
                typed_arena.push(left);
                typed_arena.push(right);
                Ok(IRExpression(
                    IRExpressionValue::BinaryNumeric(
                        match op {
                            BinOp::Add => BinOpNumeric::Add,
                            BinOp::Subtract => BinOpNumeric::Subtract,
                        },
                        left_ptr,
                        left_ptr + 1,
                    ),
                    left_type,
                ))
            }
        }
    }
}
