use itertools::PeekNth;
use thiserror::Error;

use crate::{
    provenance::Provenance,
    lexer::{Lexeme, LexError, LexemeValue},
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
    While(usize, usize),
    /// Importantly, Block references statements, not expressions!
    Block(Vec<usize>),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Add,
    Subtract,
    LessThan,
    GreaterThan,
}

#[derive(Debug, Error)]
pub enum ParseError {
    // TODO: reasons for the tokens being unexpected
    #[error("unexpected token {0}")]
    UnexpectedToken(Lexeme),
    #[error("unexpected end of input")]
    UnexpectedEndOfInput,
    #[error("token error: {0}")]
    TokenError(#[from] LexError),
}

type TokenIterInner<'a> = &'a mut dyn Iterator<Item = Result<Lexeme, LexError>>;
type TokenIter<'a> = PeekNth<TokenIterInner<'a>>;

pub type ParseTree = SourceTree<AstStatement, AstExpression>;

pub fn parse(
    mut source: impl Iterator<Item = Result<Lexeme, LexError>>,
) -> Result<(Vec<usize>, ParseTree), ParseError> {
    let mut source = itertools::peek_nth(&mut source as TokenIterInner<'_>);
    let mut context = ParseTree::new(Box::new(traverse));

    let mut statements = Vec::new();

    while let Some(_) = source.peek() {
        let statement = parse_statement(&mut source, &mut context)?;
        let statement = context.add_statement(statement);
        statements.push(statement);
    }

    Ok((statements, context))
}

// TODO
fn parse_statement(
    source: &mut TokenIter,
    context: &mut ParseTree,
) -> Result<AstStatement, ParseError> {
    let statement = match source.peek().ok_or(ParseError::UnexpectedEndOfInput)? {
        Ok(Lexeme {
            value: LexemeValue::Let,
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
    if let Some(Ok(Lexeme {
        value: LexemeValue::Semicolon,
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
        Lexeme {
            value: LexemeValue::Word(name),
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
        Some(Ok(Lexeme {
            value: token @ (LexemeValue::If | LexemeValue::While),
            start,
            ..
        })) => {
            let start = start.clone();
            let token = token.clone();
            source.next();
            next_branch(source, context, token, start)
        }
        Some(Ok(Lexeme {
            value: LexemeValue::OpenBracket,
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

fn next_branch(
    source: &mut TokenIter,
    context: &mut ParseTree,
    token: LexemeValue,
    start: Provenance,
) -> Result<AstExpression, ParseError> {
    let predicate = parse_expr(source, context)?;
    source.next(); // TODO: assert that this is {
    let block = next_block(source, context, start.clone())?; // TODO: get start

    let predicate_ptr = context.add_expression(predicate);
    let end = block.end.clone();
    let block_ptr = context.add_expression(block);

    Ok(AstExpression {
        value: if token == LexemeValue::If {
            AstExpressionValue::If(predicate_ptr, block_ptr)
        } else {
            AstExpressionValue::While(predicate_ptr, block_ptr)
        },
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
            Some(Ok(Lexeme {
                value: LexemeValue::CloseBracket,
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
        Some(Ok(Lexeme {
            value: LexemeValue::Equals,
            ..
        })) => {
            let (name, start) = match next_token(source)? {
                Lexeme {
                    value: LexemeValue::Word(name),
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
        _ => next_compare_expr(source, context),
    }
}

fn next_compare_expr(
    source: &mut TokenIter,
    context: &mut ParseTree,
) -> Result<AstExpression, ParseError> {
    // TODO: seems busted
    let mut left = next_addition_expr(source, context)?;

    while let Some(Ok(Lexeme {
        value: token @ (LexemeValue::LessThan | LexemeValue::GreaterThan),
        ..
    })) = source.peek()
    {
        let operator = match token {
            LexemeValue::LessThan => BinOp::LessThan,
            LexemeValue::GreaterThan => BinOp::GreaterThan,
            _ => unimplemented!(),
        };
        source.next();
        let right = next_compare_expr(source, context)?;
        left = make_bin_expr(context, operator, left, right);
    }

    Ok(left)
}

fn next_addition_expr(
    source: &mut TokenIter,
    context: &mut ParseTree,
) -> Result<AstExpression, ParseError> {
    // TODO: seems busted
    let mut left = next_paren_expr(source, context)?;

    while let Some(Ok(Lexeme {
        value: token @ (LexemeValue::Plus | LexemeValue::Minus),
        ..
    })) = source.peek()
    {
        let operator = match token {
            LexemeValue::Plus => BinOp::Add,
            LexemeValue::Minus => BinOp::Subtract,
            _ => unimplemented!(),
        };
        source.next();
        let right = next_paren_expr(source, context)?;
        left = make_bin_expr(context, operator, left, right);
    }

    Ok(left)
}

fn make_bin_expr(
    context: &mut ParseTree,
    operator: BinOp,
    left: AstExpression,
    right: AstExpression,
) -> AstExpression {
    let start = left.start.clone();
    let end = right.end.clone();

    AstExpression {
        value: AstExpressionValue::BinExpr(
            operator,
            context.add_expression(left),
            context.add_expression(right),
        ),
        start,
        end,
    }
}

fn next_paren_expr(
    source: &mut TokenIter,
    context: &mut ParseTree,
) -> Result<AstExpression, ParseError> {
    if let Some(Ok(Lexeme {
        value: LexemeValue::OpenParen,
        ..
    })) = source.peek()
    {
        source.next();
        let expr = parse_expr(source, context)?;
        if let Some(Ok(Lexeme {
            value: LexemeValue::CloseParen,
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
    let Lexeme { value, start, end } = next_token(source)?;
    match value {
        LexemeValue::True => Ok(AstExpression {
            value: AstExpressionValue::Bool(true),
            start,
            end,
        }),
        LexemeValue::False => Ok(AstExpression {
            value: AstExpressionValue::Bool(false),
            start,
            end,
        }),
        LexemeValue::Word(word) => Ok(AstExpression {
            value: AstExpressionValue::Name(word),
            start,
            end,
        }),
        LexemeValue::Int(int) => try_decimal(source, int as i64, start, end),
        // TODO: should this be treated as a unary operator instead?
        LexemeValue::Minus => match next_token(source)? {
            Lexeme {
                value: LexemeValue::Int(int),
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
    if let Some(Ok(Lexeme {
        value: LexemeValue::Period,
        ..
    })) = source.peek()
    {
        source.next();
        let (decimal, end) = match source.next().ok_or(ParseError::UnexpectedEndOfInput)?? {
            Lexeme {
                value: LexemeValue::Int(value),
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

fn next_token(source: &mut TokenIter) -> Result<Lexeme, ParseError> {
    Ok(source.next().ok_or(ParseError::UnexpectedEndOfInput)??)
}

fn traverse(root: Node<&AstStatement, &AstExpression>, children: &mut Vec<NodePtr>) {
    use AstExpressionValue::*;
    use AstStatementValue::*;

    match root {
        Node::Statement(AstStatement {
            value: Declaration(_, child) | Expression(child),
            ..
        })
        | Node::Expression(AstExpression {
            value: Assignment(_, child),
            ..
        }) => {
            children.push(NodePtr::Expression(*child));
        }
        Node::Expression(AstExpression {
            value: BinExpr(_, left, right) | If(left, right) | While(left, right),
            ..
        }) => {
            children.push(NodePtr::Expression(*right));
            children.push(NodePtr::Expression(*left));
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
    use matches::assert_matches;

    use super::*;

    fn tokens<'a>(
        tokens: &'a [LexemeValue],
    ) -> impl 'a + Iterator<Item = Result<Lexeme, LexError>> {
        let provenance = Provenance::new("test", "test", 0, 0);
        tokens.iter().map(move |token| {
            Ok(Lexeme {
                value: token.clone(),
                start: provenance.clone(),
                end: provenance.clone(),
            })
        })
    }

    #[test]
    fn adding() {
        let (statements, ast) = parse(tokens(&[
            LexemeValue::Let,
            LexemeValue::Word("a".to_string()),
            LexemeValue::Equals,
            LexemeValue::OpenParen,
            LexemeValue::Int(15),
            LexemeValue::Minus,
            LexemeValue::Int(10),
            LexemeValue::CloseParen,
            LexemeValue::Plus,
            LexemeValue::Int(3),
            LexemeValue::Semicolon,
            LexemeValue::If,
            LexemeValue::True,
            LexemeValue::OpenBracket,
            LexemeValue::Word("a".to_string()),
            LexemeValue::Equals,
            LexemeValue::Int(3),
            LexemeValue::Semicolon,
            LexemeValue::CloseBracket,
            LexemeValue::If,
            LexemeValue::False,
            LexemeValue::OpenBracket,
            LexemeValue::Word("a".to_string()),
            LexemeValue::Equals,
            LexemeValue::Int(4),
            LexemeValue::Semicolon,
            LexemeValue::CloseBracket,
            LexemeValue::Word("a".to_string()),
        ]))
        .unwrap();
        let nodes = statements.into_iter().map(|statement| {
            let mut line = Vec::new();
            line.extend(ast.iter_from(NodePtr::Statement(statement)));
            line
        }).collect::<Vec<_>>();
        let matchable_nodes = nodes.iter().map(|line| &line[..]).collect::<Vec<_>>();

        use Node::{Statement, Expression as Expr};
        use AstStatementValue::*;
        use AstExpressionValue::*;
        assert_matches!(matchable_nodes.as_slice(), &[
            &[
                Statement(AstStatement { value: Declaration(_, _), .. }),
                Expr(AstExpression { value: BinExpr(BinOp::Add, _, _), .. }),
                Expr(AstExpression { value: BinExpr(BinOp::Subtract, _, _), .. }),
                Expr(AstExpression { value: Int(15), .. }),
                Expr(AstExpression { value: Int(10), .. }),
                Expr(AstExpression { value: Int(3), .. }),
            ],
            &[
                Statement(AstStatement { value: Expression(_), .. }),
                Expr(AstExpression { value: If(_, _), .. }),
                Expr(AstExpression { value: Bool(true), .. }),
                Expr(AstExpression { value: Block(_), .. }),
                Statement(AstStatement { value: Expression(_), .. }),
                Expr(AstExpression { value: Assignment(_, _), .. }),
                Expr(AstExpression { value: Int(3), .. }),
            ],
            &[
                Statement(AstStatement { value: Expression(_), .. }),
                Expr(AstExpression { value: Word(_), .. }),
            ],
        ]);
    }
}
