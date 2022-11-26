use std::iter::Peekable;
use thiserror::Error;

use crate::{tokenizer::{TokenValue, TokenError, Token}, provenance::Provenance};

#[derive(Debug, PartialEq)]
pub struct AstStatement {
    pub value: AstStatementValue,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug, PartialEq)]
pub enum AstStatementValue {
    Declaration(String, AstExpression),
    Assignment(String, AstExpression),
    Expression(AstExpression),
}

#[derive(Debug, PartialEq)]
pub struct AstExpression {
    pub value: AstExpressionValue,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug, PartialEq)]
pub enum AstExpressionValue {
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
    #[error("unexpected token {0}")]
    UnexpectedToken(Token),
    #[error("unexpected end of input")]
    UnexpectedEndOfInput,
    #[error("token error: {0}")]
    TokenError(#[from] TokenError),
}

type TokenIterInner<'a> = &'a mut dyn Iterator<Item = Result<Token, TokenError>>;
type TokenIter<'a> = Peekable<TokenIterInner<'a>>;

pub fn parse(
    mut source: impl Iterator<Item = Result<Token, TokenError>>,
) -> Result<(Vec<AstStatement>, Vec<AstExpression>), ParseError> {
    let mut source = (&mut source as TokenIterInner<'_>).peekable();
    let mut arena = Vec::new();

    let mut statements = Vec::new();

    while let Some(token) = source.next() {
        statements.push(match token? {
            Token {
                value: TokenValue::Word(word),
                start,
                end
            } => parse_assignment(&mut source, word, start, end, &mut arena)?,
            _ => todo!(),
        });
        if let Some(Ok(Token { value: TokenValue::Semicolon, .. })) = source.peek() {
            source.next();
        }
    }

    Ok((statements, arena))
}

fn parse_assignment(
    source: &mut TokenIter,
    assignee: String,
    start: Provenance,
    end: Provenance,
    expression_arena: &mut Vec<AstExpression>,
) -> Result<AstStatement, ParseError> {
    let operand = match source.peek() {
        None => return Ok(AstStatement {
            value: AstStatementValue::Expression(AstExpression {
                value: AstExpressionValue::Name(assignee),
                start: start.clone(),
                end: end.clone(),
            }),
            start,
            end,
        }),
        Some(Ok(Token { value: op @ (TokenValue::ColonEquals | TokenValue::Equals), .. })) => op.clone(),
        // TODO: gracefully handle non-assignment statements
        Some(_) => return Err(ParseError::UnexpectedToken(source.next().unwrap()?)),
    };
    source.next();
    let value = parse_addition(source, expression_arena)?;
    let end = value.end.clone();
    match operand {
        TokenValue::Equals => Ok(AstStatement {
            value: AstStatementValue::Assignment(assignee, value),
            start,
            end,
        }),
        TokenValue::ColonEquals => Ok(AstStatement {
            value: AstStatementValue::Declaration(assignee, value),
            start,
            end,
        }),
        _ => unreachable!(),
    }
}

fn parse_addition(
    source: &mut TokenIter,
    expression_arena: &mut Vec<AstExpression>,
) -> Result<AstExpression, ParseError> {
    let left = next_atom(source)?;
    let operand = match source.peek() {
        Some(Ok(Token{ value: TokenValue::Plus, .. })) => BinOp::Add,
        Some(Ok(Token { value: TokenValue::Minus, .. })) => BinOp::Subtract,
        None | Some(Ok(Token { value: TokenValue::Semicolon, .. })) => return Ok(left),
        Some(_) => return Err(ParseError::UnexpectedToken(source.next().unwrap()?)),
    };
    source.next();
    let right = parse_addition(source, expression_arena)?;
    let left_ptr = expression_arena.len();
    let start = left.start.clone();
    let end = right.end.clone();
    expression_arena.push(left);
    expression_arena.push(right);

    Ok(AstExpression {
        value: AstExpressionValue::BinExpr(operand, left_ptr, left_ptr + 1),
        start,
        end,
    })
}

fn next_atom(source: &mut TokenIter) -> Result<AstExpression, ParseError> {
    // TODO: expectation reasoning
    let Token { value, start, end }= next_token(source)?;
    match value {
        TokenValue::Word(word) => Ok(AstExpression { value: AstExpressionValue::Name(word), start, end }),
        TokenValue::Int(int) => try_decimal(source, int as i64, start, end),
        // TODO: should this be treated as a unary operator instead?
        TokenValue::Minus => match next_token(source)? {
            Token { value: TokenValue::Int(int), end, .. } => try_decimal(source, -(int as i64), start, end),
            other => Err(ParseError::UnexpectedToken(other)),
        },
        value => Err(ParseError::UnexpectedToken(Token { value, start, end })),
    }
}

fn try_decimal(source: &mut TokenIter, num: i64, start: Provenance, end: Provenance) -> Result<AstExpression, ParseError> {
    // TODO: handle overflow
    if let Some(Ok(Token { value: TokenValue::Period, .. })) = source.peek() {
        source.next();
        let (decimal, end) = match source.next().ok_or(ParseError::UnexpectedEndOfInput)?? {
            Token { value: TokenValue::Int(value), end, .. } => (value as f64, end),
            other => return Err(ParseError::UnexpectedToken(other)),
        };
        let num = num as f64;
        let num = if decimal != 0.0 {
            num + decimal.copysign(num) * (10f64).powf(-decimal.log(10.0).ceil())
        } else {
            num
        };
        Ok(AstExpression {
            value: AstExpressionValue::Float(num),
            start,
            end,
        })
    } else {
        Ok(AstExpression {
            value: AstExpressionValue::Int(num),
            start,
            end,
        })
    }
}

fn next_token(source: &mut TokenIter) -> Result<Token, ParseError> {
    Ok(source.next().ok_or(ParseError::UnexpectedEndOfInput)??)
}

