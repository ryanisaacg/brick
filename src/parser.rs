use std::iter::Peekable;
use thiserror::Error;

use crate::tokenizer::{Token, TokenError};

#[derive(Debug, PartialEq)]
pub enum AstStatement {
    Declaration(String, AstExpression),
    Assignment(String, AstExpression),
    Expression(AstExpression),
}

#[derive(Debug, PartialEq)]
pub enum AstExpression {
    Name(String),
    Int(i64),
    Float(f64),
    BinExpr(BinOp, usize, usize),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Subtract,
}

#[derive(Debug, Error)]
pub enum ParseError {
    // TODO: reasons for the tokens being unexpected
    #[error("unexpected token: {0}")]
    UnexpectedToken(Token),
    #[error("unexpected end of input")]
    UnexpectedEndOfInput,
    #[error("token error: {0}")]
    TokenError(#[from] TokenError),
}

type TokenIterInner<'a> = &'a mut dyn Iterator<Item = Result<Token, TokenError>>;
type TokenIter<'a> = Peekable<&'a mut dyn Iterator<Item = Result<Token, TokenError>>>;

pub fn parse(
    mut source: impl Iterator<Item = Result<Token, TokenError>>,
) -> Result<(Vec<AstStatement>, Vec<AstExpression>), ParseError> {
    let mut source = (&mut source as TokenIterInner<'_>).peekable();
    let mut arena = Vec::new();

    let mut statements = Vec::new();

    while let Some(token) = source.next() {
        statements.push(match token? {
            Token::Word(word) => parse_assignment(&mut source, word, &mut arena)?,
            _ => todo!(),
        });
        if let Some(Ok(Token::Semicolon)) = source.peek() {
            source.next();
        }
    }

    Ok((statements, arena))
}

fn parse_assignment(
    source: &mut TokenIter,
    assignee: String,
    expression_arena: &mut Vec<AstExpression>,
) -> Result<AstStatement, ParseError> {
    let operand = match source.peek() {
        None => return Ok(AstStatement::Expression(AstExpression::Name(assignee))),
        Some(Ok(op @ (Token::ColonEquals | Token::Equals))) => op.clone(),
        // TODO: gracefully handle non-assignment statements
        Some(_) => return Err(ParseError::UnexpectedToken(source.next().unwrap()?)),
    };
    source.next();
    let value = parse_addition(source, expression_arena)?;
    match operand {
        Token::Equals => Ok(AstStatement::Assignment(assignee, value)),
        Token::ColonEquals => Ok(AstStatement::Declaration(assignee, value)),
        _ => unreachable!(),
    }
}

fn parse_addition(
    source: &mut TokenIter,
    expression_arena: &mut Vec<AstExpression>,
) -> Result<AstExpression, ParseError> {
    let left = next_atom(source)?;
    let operand = match source.peek() {
        None | Some(Ok(Token::Semicolon)) => return Ok(left),
        Some(Ok(Token::Plus)) => BinOp::Add,
        Some(Ok(Token::Minus)) => BinOp::Subtract,
        Some(_) => return Err(ParseError::UnexpectedToken(source.next().unwrap()?)),
    };
    source.next();
    let right = parse_addition(source, expression_arena)?;
    let left_ptr = expression_arena.len();
    expression_arena.push(left);
    expression_arena.push(right);

    Ok(AstExpression::BinExpr(operand, left_ptr, left_ptr + 1))
}

fn next_atom(source: &mut TokenIter) -> Result<AstExpression, ParseError> {
    match next_token(source)? {
        Token::Word(word) => Ok(AstExpression::Name(word)),
        Token::Int(int) => try_decimal(source, int as i64),
        // TODO: should this be treated as a unary operator instead?
        Token::Minus => match next_token(source)? {
            Token::Int(int) => try_decimal(source, -(int as i64)),
            other => Err(ParseError::UnexpectedToken(other)),
        },
        other => Err(ParseError::UnexpectedToken(other)),
    }
}

fn try_decimal(source: &mut TokenIter, num: i64) -> Result<AstExpression, ParseError> {
    // TODO: handle overflow
    if let Some(Ok(Token::Period)) = source.peek() {
        source.next();
        let decimal = next_int(source)? as f64;
        let num = num as f64;
        let num = if decimal != 0.0 {
            num + decimal.copysign(num) * (10f64).powf(-decimal.log(10.0).ceil())
        } else {
            num
        };
        Ok(AstExpression::Float(num))
    } else {
        Ok(AstExpression::Int(num))
    }
}

fn next_int(source: &mut TokenIter) -> Result<u64, ParseError> {
    match source.next().ok_or(ParseError::UnexpectedEndOfInput)?? {
        Token::Int(value) => Ok(value),
        other => Err(ParseError::UnexpectedToken(other)),
    }
}

fn next_token(source: &mut TokenIter) -> Result<Token, ParseError> {
    Ok(source.next().ok_or(ParseError::UnexpectedEndOfInput)??)
}

