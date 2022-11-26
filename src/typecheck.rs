use crate::parser::{AstExpression, AstStatement, BinOp};
use thiserror::Error;

#[derive(Debug, PartialEq, Eq)]
pub enum PrimitiveType {
    Int64,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Primitive(PrimitiveType),
}

#[derive(Debug, Error)]
pub enum TypecheckError {
    #[error("Operands don't match")]
    BinaryOperandMismatch,
}

pub enum IRStatement {
    Expression(IRExpression),
}

pub struct IRExpression(pub IRExpressionValue, pub Type);

pub enum IRExpressionValue {
    Int(i64),
    BinaryNumeric(BinOpNumeric, usize, usize),
}

pub enum BinOpNumeric {
    Add,
    Subtract,
}

pub fn typecheck(
    statement: &AstStatement,
    expression_arena: &Vec<AstExpression>,
) -> Result<(IRStatement, Vec<IRExpression>), TypecheckError> {
    let mut typed_arena = Vec::new();

    let statement = match statement {
        AstStatement::Expression(expr) => {
            let expr = typecheck_expression(expr, &expression_arena, &mut typed_arena)?;
            IRStatement::Expression(expr)
        }
        AstStatement::Assignment(_, _) => unimplemented!(),
    };

    Ok((statement, typed_arena))
}

pub fn typecheck_expression(
    expression: &AstExpression,
    expression_arena: &Vec<AstExpression>,
    typed_arena: &mut Vec<IRExpression>,
) -> Result<IRExpression, TypecheckError> {
    use AstExpression::*;
    match expression {
        Name(_) => unimplemented!(),
        Int(val) => Ok(IRExpression(
            IRExpressionValue::Int(*val),
            Type::Primitive(PrimitiveType::Int64),
        )),
        BinExpr(op @ (BinOp::Add | BinOp::Subtract), left, right) => {
            let left =
                typecheck_expression(&expression_arena[*left], expression_arena, typed_arena)?;
            let right =
                typecheck_expression(&expression_arena[*right], expression_arena, typed_arena)?;
            if left.1 != right.1 || left.1 != Type::Primitive(PrimitiveType::Int64) {
                Err(TypecheckError::BinaryOperandMismatch)
            } else {
                let left_ptr = typed_arena.len();
                typed_arena.push(left);
                typed_arena.push(right);
                Ok(IRExpression(
                    IRExpressionValue::BinaryNumeric(
                        match op {
                            BinOp::Add => BinOpNumeric::Add,
                            BinOp::Subtract => BinOpNumeric::Subtract,
                            _ => unimplemented!(),
                        },
                        left_ptr,
                        left_ptr + 1,
                    ),
                    Type::Primitive(PrimitiveType::Int64),
                ))
            }
        }
        _ => unimplemented!(),
    }
}
