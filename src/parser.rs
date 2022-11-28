use itertools::PeekNth;
use thiserror::Error;

use crate::{
    provenance::Provenance,
    tokenizer::{Token, TokenError, TokenValue},
    tree::{Node, NodePtr, SourceTree},
};

#[derive(Debug, PartialEq)]
pub struct AstStatement {
    pub value: AstStatementValue,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug, PartialEq)]
pub enum AstStatementValue {
    Declaration(String, usize),
    Expression(usize),
}

#[derive(Debug, PartialEq)]
pub struct AstExpression {
    pub value: AstExpressionValue,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug, PartialEq)]
pub enum AstExpressionValue {
    Assignment(String, usize),
    Name(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    BinExpr(BinOp, usize, usize),
    If(usize, usize),
    /// Importantly, Block references statements, not expressions!
    Block(Vec<usize>),
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
type TokenIter<'a> = PeekNth<TokenIterInner<'a>>;

pub type ParseTree = SourceTree<AstStatement, AstExpression>;

pub fn parse(
    mut source: impl Iterator<Item = Result<Token, TokenError>>,
) -> Result<(Vec<AstStatement>, ParseTree), ParseError> {
    let mut source = itertools::peek_nth(&mut source as TokenIterInner<'_>);
    let mut context = ParseTree::new(Box::new(traverse));

    let mut statements = Vec::new();

    while let Some(_) = source.peek() {
        statements.push(parse_statement(&mut source, &mut context)?);
    }

    Ok((statements, context))
}

// TODO
fn parse_statement(
    source: &mut TokenIter,
    context: &mut ParseTree,
) -> Result<AstStatement, ParseError> {
    let statement = match source.peek().ok_or(ParseError::UnexpectedEndOfInput)? {
        Ok(Token {
            value: TokenValue::Let,
            start,
            ..
        }) => {
            let start = start.clone();
            source.next();
            parse_declaration(source, start, context)?
        }
        _ => {
            let expr = parse_expr(source, context)?;
            let start = expr.start.clone();
            let end = expr.end.clone();
            AstStatement {
                value: AstStatementValue::Expression(context.add_expression(expr)),
                start,
                end,
            }
        }
    };
    if let Some(Ok(Token {
        value: TokenValue::Semicolon,
        ..
    })) = source.peek()
    {
        source.next();
    }
    Ok(statement)
}

fn parse_declaration(
    source: &mut TokenIter,
    start: Provenance,
    context: &mut ParseTree,
) -> Result<AstStatement, ParseError> {
    let name = match next_token(source)? {
        Token {
            value: TokenValue::Word(name),
            ..
        } => name,
        other => return Err(ParseError::UnexpectedToken(other)),
    };
    next_token(source)?; // TODO: assert that this is an =
    let value = parse_expr(source, context)?;
    let end = value.end.clone();

    Ok(AstStatement {
        value: AstStatementValue::Declaration(name, context.add_expression(value)),
        start,
        end,
    })
}

fn parse_expr(
    source: &mut TokenIter,
    context: &mut ParseTree,
) -> Result<AstExpression, ParseError> {
    match source.peek() {
        Some(Ok(Token {
            value: TokenValue::If,
            start,
            ..
        })) => {
            let start = start.clone();
            source.next();
            next_if(source, context, start)
        }
        Some(Ok(Token {
            value: TokenValue::OpenBracket,
            start,
            ..
        })) => {
            let start = start.clone();
            source.next();
            next_block(source, context, start)
        }
        _ => parse_assignment(source, context),
    }
}

fn next_if(
    source: &mut TokenIter,
    context: &mut ParseTree,
    start: Provenance,
) -> Result<AstExpression, ParseError> {
    let predicate = parse_expr(source, context)?;
    source.next(); // TODO: assert that this is {
    let block = next_block(source, context, start.clone())?; // TODO: get start

    let predicate_ptr = context.add_expression(predicate);
    let end = block.end.clone();
    let block_ptr = context.add_expression(block);

    Ok(AstExpression {
        value: AstExpressionValue::If(predicate_ptr, block_ptr),
        start,
        end,
    })
}

fn next_block(
    source: &mut TokenIter,
    context: &mut ParseTree,
    start: Provenance,
) -> Result<AstExpression, ParseError> {
    let mut statements = Vec::new();
    loop {
        match source.peek() {
            None => return Err(ParseError::UnexpectedEndOfInput),
            Some(Ok(Token {
                value: TokenValue::CloseBracket,
                end,
                ..
            })) => {
                let end = end.clone();
                source.next();
                return Ok(AstExpression {
                    value: AstExpressionValue::Block(statements),
                    start,
                    end,
                });
            }
            _ => {
                let statement = parse_statement(source, context)?;
                statements.push(context.add_statement(statement));
            }
        }
    }
}

fn parse_assignment(
    source: &mut TokenIter,
    context: &mut ParseTree,
) -> Result<AstExpression, ParseError> {
    // TODO: actually determine assignments
    match source.peek_nth(1) {
        Some(Ok(Token {
            value: TokenValue::Equals,
            ..
        })) => {
            let (name, start) = match next_token(source)? {
                Token {
                    value: TokenValue::Word(name),
                    start,
                    ..
                } => (name, start.clone()),
                other => return Err(ParseError::UnexpectedToken(other)),
            };
            next_token(source)?; // TODO: assert that this is an =
            let value = parse_expr(source, context)?;
            let end = value.end.clone();
            let value = context.add_expression(value);

            Ok(AstExpression {
                value: AstExpressionValue::Assignment(name, value),
                start,
                end,
            })
        }
        _ => parse_addition(source, context),
    }
}

fn parse_addition(
    source: &mut TokenIter,
    context: &mut ParseTree,
) -> Result<AstExpression, ParseError> {
    // TODO: seems busted
    let mut left = next_paren_expr(source, context)?;

    while let Some(Ok(Token {
        value: token @ (TokenValue::Plus | TokenValue::Minus),
        ..
    })) = source.peek()
    {
        let operator = match token {
            TokenValue::Plus => BinOp::Add,
            TokenValue::Minus => BinOp::Subtract,
            _ => unimplemented!(),
        };
        source.next();
        let right = next_paren_expr(source, context)?;
        let start = left.start.clone();
        let end = right.end.clone();
        let left_ptr = context.add_expression(left);
        let right_ptr = context.add_expression(right);

        left = AstExpression {
            value: AstExpressionValue::BinExpr(operator, left_ptr, right_ptr),
            start,
            end,
        };
    }

    Ok(left)
}

fn next_paren_expr(
    source: &mut TokenIter,
    context: &mut ParseTree,
) -> Result<AstExpression, ParseError> {
    if let Some(Ok(Token {
        value: TokenValue::OpenParen,
        ..
    })) = source.peek()
    {
        source.next();
        let expr = parse_expr(source, context)?;
        if let Some(Ok(Token {
            value: TokenValue::CloseParen,
            ..
        })) = source.next()
        {
            Ok(expr)
        } else {
            todo!(); // throw error because not )
        }
    } else {
        next_atom(source)
    }
}

fn next_atom(source: &mut TokenIter) -> Result<AstExpression, ParseError> {
    // TODO: expectation reasoning
    let Token { value, start, end } = next_token(source)?;
    match value {
        TokenValue::True => Ok(AstExpression {
            value: AstExpressionValue::Bool(true),
            start,
            end,
        }),
        TokenValue::False => Ok(AstExpression {
            value: AstExpressionValue::Bool(false),
            start,
            end,
        }),
        TokenValue::Word(word) => Ok(AstExpression {
            value: AstExpressionValue::Name(word),
            start,
            end,
        }),
        TokenValue::Int(int) => try_decimal(source, int as i64, start, end),
        // TODO: should this be treated as a unary operator instead?
        TokenValue::Minus => match next_token(source)? {
            Token {
                value: TokenValue::Int(int),
                end,
                ..
            } => try_decimal(source, -(int as i64), start, end),
            other => Err(ParseError::UnexpectedToken(other)),
        },
        value => panic!("{:?}", value), //Err(ParseError::UnexpectedToken(Token { value, start, end })),
    }
}

fn try_decimal(
    source: &mut TokenIter,
    num: i64,
    start: Provenance,
    end: Provenance,
) -> Result<AstExpression, ParseError> {
    // TODO: handle overflow
    if let Some(Ok(Token {
        value: TokenValue::Period,
        ..
    })) = source.peek()
    {
        source.next();
        let (decimal, end) = match source.next().ok_or(ParseError::UnexpectedEndOfInput)?? {
            Token {
                value: TokenValue::Int(value),
                end,
                ..
            } => (value as f64, end),
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

fn traverse(root: Node<&AstStatement, &AstExpression>, children: &mut Vec<NodePtr>) {
    use AstExpressionValue::*;
    use AstStatementValue::*;

    match root {
        Node::Statement(AstStatement {
            value: Declaration(_, child),
            ..
        })
        | Node::Statement(AstStatement {
            value: Expression(child),
            ..
        })
        | Node::Expression(AstExpression {
            value: Assignment(_, child),
            ..
        }) => {
            children.push(NodePtr::Expression(*child));
        }
        Node::Expression(AstExpression {
            value: BinExpr(_, left, right),
            ..
        })
        | Node::Expression(AstExpression {
            value: If(left, right),
            ..
        }) => {
            children.push(NodePtr::Expression(*left));
            children.push(NodePtr::Expression(*right));
        }
        Node::Expression(AstExpression {
            value: Block(statements),
            ..
        }) => {
            for statement in statements {
                children.push(NodePtr::Statement(*statement));
            }
        }
        Node::Expression(AstExpression {
            value: Name(_) | Int(_) | Float(_) | Bool(_),
            ..
        }) => {}
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn tokens<'a>(
        tokens: &'a [TokenValue],
    ) -> impl 'a + Iterator<Item = Result<Token, TokenError>> {
        let provenance = Provenance::new("test", "test", 0, 0);
        tokens.iter().map(move |token| {
            Ok(Token {
                value: token.clone(),
                start: provenance.clone(),
                end: provenance.clone(),
            })
        })
    }

    #[test]
    fn adding() {
        let (statements, ast) = parse(tokens(&[
            TokenValue::Let,
            TokenValue::Word("a".to_string()),
            TokenValue::Equals,
            TokenValue::OpenParen,
            TokenValue::Int(15),
            TokenValue::Minus,
            TokenValue::Int(10),
            TokenValue::CloseParen,
            TokenValue::Plus,
            TokenValue::Int(3),
            TokenValue::Semicolon,
            TokenValue::If,
            TokenValue::True,
            TokenValue::OpenBracket,
            TokenValue::Word("a".to_string()),
            TokenValue::Equals,
            TokenValue::Int(3),
            TokenValue::Semicolon,
            TokenValue::CloseBracket,
            TokenValue::If,
            TokenValue::False,
            TokenValue::OpenBracket,
            TokenValue::Word("a".to_string()),
            TokenValue::Equals,
            TokenValue::Int(4),
            TokenValue::Semicolon,
            TokenValue::CloseBracket,
            TokenValue::Word("a".to_string()),
        ]))
        .unwrap();
    }
}
