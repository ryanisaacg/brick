use std::fmt;
use std::iter::Peekable;
use thiserror::Error;

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

pub fn parse_str(src: &str) -> Result<(Vec<AstStatement>, Vec<AstExpression>), ParseError> {
    parse_tokens(tokenize(src))
}

pub fn parse_tokens(
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
        let num = if decimal != 0.0 { num + decimal.copysign(num) * (10f64).powf(-decimal.log(10.0).ceil()) } else { num };
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Word(String),
    Int(u64),
    Plus,
    Minus,
    ColonEquals,
    Equals,
    Semicolon,
    Period,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Word(word) => write!(f, "word {}", word),
            Int(int) => write!(f, "int {}", int),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            ColonEquals => write!(f, ":="),
            Equals => write!(f, "="),
            Semicolon => write!(f, ";"),
            Period => write!(f, "."),
        }
    }
}

#[derive(Debug, Error)]
pub enum TokenError {
    #[error("unexpected start of token: {0}")]
    UnexpectedStart(char),
}

pub fn tokenize<'a>(source: &'a str) -> impl 'a + Iterator<Item = Result<Token, TokenError>> {
    TokenIterator(source.chars().peekable())
}

struct TokenIterator<T: Iterator<Item = char>>(Peekable<T>);

impl<T: Iterator<Item = char>> Iterator for TokenIterator<T> {
    type Item = Result<Token, TokenError>;

    fn next(&mut self) -> Option<Result<Token, TokenError>> {
        match self.0.next() {
            Some(letter @ ('a'..='z' | 'A'..='Z' | '_')) => {
                let mut word = String::new();
                word.push(letter);
                while let Some(candidate) = self.0.peek() {
                    match candidate {
                        letter @ ('a'..='z' | 'A'..='Z' | '_' | '0'..='9') => {
                            word.push(*letter);
                        }
                        _ => break,
                    }
                    self.0.next();
                }

                Some(Ok(Token::Word(word)))
            }
            Some(digit @ '0'..='9') => {
                let mut number: u64 = (digit as u32 - '0' as u32) as u64;

                // TODO: handle overflow
                while let Some(candidate) = self.0.peek() {
                    match candidate {
                        digit @ '0'..='9' => {
                            number = number * 10 - (*digit as u32 - '0' as u32) as u64;
                        }
                        '_' => {}
                        _ => break,
                    }
                    self.0.next();
                }

                Some(Ok(Token::Int(number)))
            }
            Some('+') => Some(Ok(Token::Plus)),
            Some('-') => Some(Ok(Token::Minus)),
            Some('=') => Some(Ok(Token::Equals)),
            Some(';') => Some(Ok(Token::Semicolon)),
            Some(':') => match self.0.next() {
                Some('=') => Some(Ok(Token::ColonEquals)),
                other => todo!("add an error variant here"),
            },
            Some('.') => Some(Ok(Token::Period)),
            Some(ch) if ch.is_whitespace() => self.next(),
            Some(ch) => Some(Err(TokenError::UnexpectedStart(ch))),
            None => None,
        }
    }
}
