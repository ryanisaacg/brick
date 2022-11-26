use std::fmt;
use std::iter::Peekable;
use thiserror::Error;

#[derive(Debug, PartialEq, Eq)]
pub enum AstStatement {
    Expression(AstExpression),
    Assignment(String, AstExpression),
}

#[derive(Debug, PartialEq, Eq)]
pub enum AstExpression {
    Name(String),
    Int(i64),
    BinExpr(BinOp, usize, usize),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Power,

    Multiply,
    Divide,

    BooleanOr,
    BooleanAnd,

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

pub fn parse_str(src: &str) -> Result<(AstStatement, Vec<AstExpression>), ParseError> {
    parse_tokens(tokenize(src))
}

pub fn parse_tokens(
    mut source: impl Iterator<Item = Result<Token, TokenError>>,
) -> Result<(AstStatement, Vec<AstExpression>), ParseError> {
    let mut source = (&mut source as TokenIterInner<'_>).peekable();
    let mut arena = Vec::new();
    let expr = addition(&mut source, &mut arena)?;

    Ok((AstStatement::Expression(expr), arena))
}

fn addition(
    source: &mut TokenIter,
    expression_arena: &mut Vec<AstExpression>,
) -> Result<AstExpression, ParseError> {
    let left = next_arith_operand(source)?;
    let operand = match source.peek() {
        None => return Ok(left),
        Some(Ok(Token::Plus)) => BinOp::Add,
        Some(Ok(Token::Minus)) => BinOp::Subtract,
        Some(_) => return Err(ParseError::UnexpectedToken(source.next().unwrap()?)),
    };
    source.next();
    let right = addition(source, expression_arena)?;
    let left_ptr = expression_arena.len();
    expression_arena.push(left);
    expression_arena.push(right);

    Ok(AstExpression::BinExpr(operand, left_ptr, left_ptr + 1))
}

fn next_arith_operand(source: &mut TokenIter) -> Result<AstExpression, ParseError> {
    match next_token(source)? {
        Token::Word(word) => Ok(AstExpression::Name(word)),
        Token::Int(int) => Ok(AstExpression::Int(int as i64)),
        Token::Minus => match next_token(source)? {
            Token::Int(int) => Ok(AstExpression::Int(-(int as i64))),
            other => Err(ParseError::UnexpectedToken(other)),
        },
        other => Err(ParseError::UnexpectedToken(other)),
    }
}

fn next_token(source: &mut TokenIter) -> Result<Token, ParseError> {
    Ok(source.next().ok_or(ParseError::UnexpectedEndOfInput)??)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Word(String),
    Int(u64),
    Plus,
    Minus,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Word(word) => write!(f, "word {}", word),
            Int(int) => write!(f, "int {}", int),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
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
            Some(ch) if ch.is_whitespace() => self.next(),
            Some(ch) => Some(Err(TokenError::UnexpectedStart(ch))),
            None => None,
        }
    }
}

/*#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn arithmetic() {
        let ast = parse(tokenize("1 + 2 - hello")).unwrap();
        assert_eq!(
            ast,
            Expression::BinExpr(
                BinOp::Add,
                Box::new(Expression::Int(1)),
                Box::new(Expression::BinExpr(
                    BinOp::Subtract,
                    Box::new(Expression::Int(2)),
                    Box::new(Expression::Name("hello".to_string())),
                )),
            )
        );
    }
}*/
