use std::iter::Peekable;

use thiserror::Error;

use crate::{
    lexer::{LexError, Lexeme, LexemeValue},
    provenance::Provenance,
    tree::{Node, NodePtr, SourceTree},
};

#[derive(Debug, PartialEq, Eq)]
pub struct AstStatement {
    pub value: AstStatementValue,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AstStatementValue {
    FunctionDeclaration {
        name: String,
        params: Vec<NameAndType>,
        returns: Option<usize>,
        body: usize,
    },
    StructDeclaration {
        name: String,
        fields: Vec<NameAndType>,
    },
    Declaration(String, usize),
    Expression(usize),
    Import(String),
}

#[derive(Debug, PartialEq, Eq)]
pub struct NameAndType {
    pub name: String,
    pub kind: usize,
}

#[derive(Debug, PartialEq)]
pub struct AstExpression {
    pub value: AstExpressionValue,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug, PartialEq)]
pub enum AstExpressionValue {
    Assignment(usize, usize),
    Name(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    BinExpr(BinOp, usize, usize),
    If(usize, usize),
    While(usize, usize),
    Call(usize, Vec<usize>),
    /// Importantly, Block references statements, not expressions!
    Block(Vec<usize>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct AstType {
    pub value: AstTypeValue,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Debug, PartialEq, Eq)]
pub enum AstTypeValue {
    Name(String),
    Unique(usize),
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
    #[error("unexpected token {0}, {1}")]
    UnexpectedLexeme(Box<Lexeme>, &'static str),
    #[error("unexpected end of input at {0}, {1}")]
    UnexpectedEndOfInput(Provenance, &'static str),
    #[error("expected type for parameter at {0}")]
    MissingTypeForParam(Provenance),
    #[error("token error: {0}")]
    TokenError(#[from] LexError),
}

type TokenIterInner<'a> = &'a mut dyn Iterator<Item = Result<Lexeme, LexError>>;
type TokenIter<'a> = Peekable<TokenIterInner<'a>>;

pub type ParseTree = SourceTree<AstStatement, AstExpression, AstType>;

pub fn parse(
    mut source: impl Iterator<Item = Result<Lexeme, LexError>>,
) -> Result<(Vec<usize>, ParseTree), ParseError> {
    let mut source = (&mut source as TokenIterInner<'_>).peekable();
    let mut context = ParseTree::new(Box::new(traverse));

    let mut statements = Vec::new();

    while let Some(lexeme) = peek_token_optional(&mut source)? {
        let start = lexeme.start;
        let statement = parse_statement(&mut source, &mut context, start)?;
        let statement = context.add_statement(statement);
        statements.push(statement);
    }

    Ok((statements, context))
}

fn parse_statement(
    source: &mut TokenIter,
    context: &mut ParseTree,
    start: Provenance,
) -> Result<AstStatement, ParseError> {
    let statement = match peek_token(source, start, "expected let, fn, or expression")? {
        Lexeme {
            value:
                value @ (LexemeValue::Let
                | LexemeValue::Import
                | LexemeValue::Function
                | LexemeValue::Struct),
            start,
            ..
        } => {
            let start = *start;
            let value = value.clone();
            source.next();
            match value {
                LexemeValue::Let => parse_var_declaration(source, context, start)?,
                LexemeValue::Import => parse_import(source, start)?,
                LexemeValue::Function => parse_function_declaration(source, context, start)?,
                LexemeValue::Struct => parse_struct(source, context, start)?,
                _ => unreachable!(),
            }
        }
        _ => {
            let expr = parse_expr(source, context, start)?;
            let start = expr.start;
            let end = expr.end;
            AstStatement {
                value: AstStatementValue::Expression(context.add_expression(expr)),
                start,
                end,
            }
        }
    };
    if let Some(Lexeme {
        value: LexemeValue::Semicolon,
        ..
    }) = peek_token_optional(source)?
    {
        source.next();
    }
    Ok(statement)
}

fn parse_struct(
    source: &mut TokenIter,
    context: &mut ParseTree,
    start: Provenance,
) -> Result<AstStatement, ParseError> {
    let (name, _, end) = next_word(source, start, "expected name after 'struct'")?;
    assert_next_lexeme_eq(
        source.next(),
        LexemeValue::OpenBracket,
        end,
        "expected open parenthesis to start parameters",
    )?;

    let mut params = Vec::new();
    let mut pos = end;
    loop {
        let token = peek_token(source, pos, "expected either parameters or close paren")?;
        pos = token.start;
        match token.value {
            LexemeValue::CloseBracket => break,
            LexemeValue::Comma => {
                source.next();
            }
            _ => {
                let (name, end, type_hint) =
                    parse_name_and_type_hint(source, context, pos, "expected parameter")?;
                let kind = type_hint.ok_or(ParseError::MissingTypeForParam(end))?;
                let kind = context.add_kind(kind);
                pos = end;
                params.push(NameAndType { name, kind });
            }
        }
    }
    assert_next_lexeme_eq(
        source.next(),
        LexemeValue::CloseBracket,
        start,
        "expected closing bracket to end struct declaration",
    )?;

    Ok(AstStatement {
        value: AstStatementValue::StructDeclaration {
            name,
            fields: Vec::new(),
        },
        start,
        end,
    })
}

fn parse_function_declaration(
    source: &mut TokenIter,
    context: &mut ParseTree,
    start: Provenance,
) -> Result<AstStatement, ParseError> {
    let (name, _, end) = next_word(source, start, "expected name after 'fn'")?;

    let mut pos = assert_next_lexeme_eq(
        source.next(),
        LexemeValue::OpenParen,
        end,
        "expected open parenthesis to start parameters",
    )?
    .end;
    let mut params = Vec::new();
    loop {
        let token = peek_token(source, pos, "expected either parameters or close paren")?;
        pos = token.start;
        match token.value {
            LexemeValue::CloseParen => break,
            LexemeValue::Comma => {
                source.next();
            }
            _ => {
                let (name, end, type_hint) =
                    parse_name_and_type_hint(source, context, pos, "expected parameter")?;
                let kind = type_hint.ok_or(ParseError::MissingTypeForParam(end))?;
                let kind = context.add_kind(kind);
                pos = end;
                params.push(NameAndType { name, kind });
            }
        }
    }
    assert_next_lexeme_eq(
        source.next(),
        LexemeValue::CloseParen,
        pos,
        "expected closing parenthesis to end parameters",
    )?;
    let returns = if let Lexeme {
        value: LexemeValue::Colon,
        start,
        ..
    } = peek_token(source, start, "expected body after function declaration")?
    {
        let start = *start;
        source.next();
        let kind = parse_type(source, context, start)?;
        let kind = context.add_kind(kind);
        Some(kind)
    } else {
        None
    };
    let body = next_block(source, context, start)?;
    let end = body.end;

    Ok(AstStatement {
        value: AstStatementValue::FunctionDeclaration {
            name,
            params,
            returns,
            body: context.add_expression(body),
        },
        start,
        end,
    })
}

fn parse_import(source: &mut TokenIter, start: Provenance) -> Result<AstStatement, ParseError> {
    let (name, _, end) = next_word(source, start, "expected word after 'import'")?;

    Ok(AstStatement {
        value: AstStatementValue::Import(name),
        start,
        end,
    })
}

fn parse_var_declaration(
    source: &mut TokenIter,
    context: &mut ParseTree,
    start: Provenance,
) -> Result<AstStatement, ParseError> {
    // TODO: store type hint in declarations
    let (name, start, _) = parse_name_and_type_hint(
        source,
        context,
        start,
        "expected word after 'let' in declaration",
    )?;
    assert_next_lexeme_eq(
        source.next(),
        LexemeValue::Equals,
        start,
        "expected = after let binding target",
    )?;
    let value = parse_expr(source, context, start)?;
    let end = value.end;

    Ok(AstStatement {
        value: AstStatementValue::Declaration(name, context.add_expression(value)),
        start,
        end,
    })
}

fn parse_name_and_type_hint(
    source: &mut TokenIter,
    context: &mut ParseTree,
    start: Provenance,
    reason: &'static str,
) -> Result<(String, Provenance, Option<AstType>), ParseError> {
    let (name, _, mut end) = next_word(source, start, reason)?;
    let type_hint = if let Some(Ok(Lexeme {
        value: LexemeValue::Colon,
        ..
    })) = source.peek()
    {
        source.next();
        let type_hint = parse_type(source, context, start)?;
        end = type_hint.end;
        Some(type_hint)
    } else {
        None
    };

    Ok((name, end, type_hint))
}

fn parse_type(
    source: &mut TokenIter,
    context: &mut ParseTree,
    start: Provenance,
) -> Result<AstType, ParseError> {
    let next = next_token(source, start, "expected type")?;
    match next.value {
        LexemeValue::Unique => {
            let subtype = parse_type(source, context, next.end)?;
            let end = subtype.end;
            let subtype = context.add_kind(subtype);
            Ok(AstType {
                value: AstTypeValue::Unique(subtype),
                start: next.start,
                end,
            })
        }
        LexemeValue::Word(name) => Ok(AstType {
            value: AstTypeValue::Name(name),
            start: next.start,
            end: next.end,
        }),
        _ => Err(ParseError::UnexpectedLexeme(
            Box::new(next),
            "expected either 'unique' or type name",
        )),
    }
}

fn parse_expr(
    source: &mut TokenIter,
    context: &mut ParseTree,
    provenance: Provenance,
) -> Result<AstExpression, ParseError> {
    match peek_token(source, provenance, "expected expression")? {
        Lexeme {
            value: token @ (LexemeValue::If | LexemeValue::While),
            start,
            ..
        } => {
            let start = *start;
            let token = token.clone();
            source.next();
            next_branch(source, context, token, start)
        }
        Lexeme {
            value: LexemeValue::OpenBracket,
            start,
            ..
        } => {
            let start = *start;
            next_block(source, context, start)
        }
        _ => assignment_step(source, context, provenance),
    }
}

fn next_branch(
    source: &mut TokenIter,
    context: &mut ParseTree,
    token: LexemeValue,
    start: Provenance,
) -> Result<AstExpression, ParseError> {
    let predicate = parse_expr(source, context, start)?;
    let block = next_block(source, context, start)?;

    let predicate_ptr = context.add_expression(predicate);
    let end = block.end;
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
    let start = assert_next_lexeme_eq(
        source.next(),
        LexemeValue::OpenBracket,
        start,
        "expected { to start a block",
    )?
    .start;
    let mut statements = Vec::new();
    let mut end = start;
    loop {
        match peek_token(source, start, "expected statement or close bracket")? {
            Lexeme {
                value: LexemeValue::CloseBracket,
                end,
                ..
            } => {
                let end = *end;
                source.next();
                return Ok(AstExpression {
                    value: AstExpressionValue::Block(statements),
                    start,
                    end,
                });
            }
            _ => {
                let statement = parse_statement(source, context, end)?;
                end = statement.end;
                statements.push(context.add_statement(statement));
            }
        }
    }
}

fn assignment_step(
    source: &mut TokenIter,
    context: &mut ParseTree,
    provenance: Provenance,
) -> Result<AstExpression, ParseError> {
    let mut root = call_step(source, context, provenance)?;

    while let Some(Lexeme {
        value: LexemeValue::Equals,
        start,
        ..
    }) = peek_token_optional(source)?
    {
        let start = *start;
        source.next(); // remove the peeked token
        let right = assignment_step(source, context, start)?;

        let Span {
            left,
            right,
            start,
            end,
        } = span(context, root, right);

        root = AstExpression {
            value: AstExpressionValue::Assignment(left, right),
            start,
            end,
        };
    }

    Ok(root)
}

fn call_step(
    source: &mut TokenIter,
    context: &mut ParseTree,
    provenance: Provenance,
) -> Result<AstExpression, ParseError> {
    let mut root = comparison_step(source, context, provenance)?;

    while let Some(Lexeme {
        value: LexemeValue::OpenParen,
        ..
    }) = peek_token_optional(source)?
    {
        let mut arguments = Vec::new();
        let mut end;
        source.next(); // remove the peeked token

        loop {
            let argument = parse_expr(source, context, provenance)?;
            end = argument.end;
            arguments.push(context.add_expression(argument));

            if let LexemeValue::Comma =
                peek_token(source, end, "expected comma or ) to end function call")?.value
            {
                source.next();
            }

            if let LexemeValue::CloseParen =
                peek_token(source, end, "expected ) or next argument")?.value
            {
                source.next();
                break;
            }
        }

        let start = root.start;
        let function = context.add_expression(root);

        root = AstExpression {
            value: AstExpressionValue::Call(function, arguments),
            start,
            end,
        }
    }

    Ok(root)
}

fn comparison_step(
    source: &mut TokenIter,
    context: &mut ParseTree,
    provenance: Provenance,
) -> Result<AstExpression, ParseError> {
    let mut left = sum_step(source, context, provenance)?;

    while let Some(Lexeme {
        value: token @ (LexemeValue::LessThan | LexemeValue::GreaterThan),
        ..
    }) = peek_token_optional(source)?
    {
        let operator = match token {
            LexemeValue::LessThan => BinOp::LessThan,
            LexemeValue::GreaterThan => BinOp::GreaterThan,
            _ => unreachable!(),
        };
        source.next();
        let right = comparison_step(source, context, provenance)?;
        left = make_bin_expr(context, operator, left, right);
    }

    Ok(left)
}

fn sum_step(
    source: &mut TokenIter,
    context: &mut ParseTree,
    provenance: Provenance,
) -> Result<AstExpression, ParseError> {
    let mut left = paren_step(source, context, provenance)?;

    while let Some(Lexeme {
        value: token @ (LexemeValue::Plus | LexemeValue::Minus),
        ..
    }) = peek_token_optional(source)?
    {
        let operator = match token {
            LexemeValue::Plus => BinOp::Add,
            LexemeValue::Minus => BinOp::Subtract,
            _ => unimplemented!(),
        };
        source.next();
        let right = paren_step(source, context, provenance)?;
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
    let start = left.start;
    let end = right.end;

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

struct Span {
    left: usize,
    right: usize,
    start: Provenance,
    end: Provenance,
}

fn span(context: &mut ParseTree, left: AstExpression, right: AstExpression) -> Span {
    let start = left.start;
    let end = right.end;
    let left = context.add_expression(left);
    let right = context.add_expression(right);

    Span {
        left,
        right,
        start,
        end,
    }
}

fn paren_step(
    source: &mut TokenIter,
    context: &mut ParseTree,
    provenance: Provenance,
) -> Result<AstExpression, ParseError> {
    if let Some(Ok(Lexeme {
        value: LexemeValue::OpenParen,
        start,
        ..
    })) = source.peek()
    {
        let start = *start;
        source.next();
        let expr = parse_expr(source, context, start)?;
        assert_next_lexeme_eq(
            source.next(),
            LexemeValue::CloseParen,
            expr.start,
            "parenthesized expressions must end with a )",
        )?;

        Ok(expr)
    } else {
        next_atom(source, provenance, "TODO")
    }
}

fn next_atom(
    source: &mut TokenIter,
    provenance: Provenance,
    reason: &'static str,
) -> Result<AstExpression, ParseError> {
    // TODO: expectation reasoning
    let Lexeme { value, start, end } = next_token(source, provenance, reason)?;
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
        LexemeValue::Minus => {
            match next_token(source, start, "expected digit after negative sign")? {
                Lexeme {
                    value: LexemeValue::Int(int),
                    end,
                    ..
                } => try_decimal(source, -(int as i64), start, end),
                other => Err(ParseError::UnexpectedLexeme(
                    Box::new(other),
                    "expected number after -",
                )),
            }
        }
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
    if let Some(Lexeme {
        value: LexemeValue::Period,
        ..
    }) = peek_token_optional(source)?
    {
        source.next();
        let (decimal, end) = match source.next().ok_or(ParseError::UnexpectedEndOfInput(
            start,
            "expected digit after decimal point",
        ))?? {
            Lexeme {
                value: LexemeValue::Int(value),
                end,
                ..
            } => (value as f64, end),
            other => {
                return Err(ParseError::UnexpectedLexeme(
                    Box::new(other),
                    "expected number after decimal point",
                ))
            }
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

fn next_token(
    source: &mut TokenIter,
    provenance: Provenance,
    reason: &'static str,
) -> Result<Lexeme, ParseError> {
    let lex = source
        .next()
        .ok_or(ParseError::UnexpectedEndOfInput(provenance, reason))??;
    Ok(lex)
}

fn next_word(
    source: &mut TokenIter,
    provenance: Provenance,
    reason: &'static str,
) -> Result<(String, Provenance, Provenance), ParseError> {
    match next_token(source, provenance, reason)? {
        Lexeme {
            value: LexemeValue::Word(name),
            start,
            end,
            ..
        } => Ok((name, start, end)),
        other => Err(ParseError::UnexpectedLexeme(Box::new(other), reason)),
    }
}

fn peek_token<'a>(
    source: &'a mut TokenIter,
    provenance: Provenance,
    reason: &'static str,
) -> Result<&'a Lexeme, ParseError> {
    Ok(source
        .peek()
        .ok_or(ParseError::UnexpectedEndOfInput(provenance, reason))?
        .as_ref()
        .map_err(|e| e.clone())?)
}

fn peek_token_optional<'a>(source: &'a mut TokenIter) -> Result<Option<&'a Lexeme>, ParseError> {
    Ok(source
        .peek()
        .map(|result| result.as_ref().map_err(|e| e.clone()))
        .transpose()?)
}

fn traverse(root: Node<&AstStatement, &AstExpression, &AstType>, children: &mut Vec<NodePtr>) {
    use AstExpressionValue::*;
    use AstStatementValue::*;

    match root {
        Node::Statement(AstStatement {
            value:
                Declaration(_, child) | Expression(child) | FunctionDeclaration { body: child, .. },
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
            value: Call(function, parameters),
            ..
        }) => {
            children.push(NodePtr::Expression(*function));
            for expression in parameters {
                children.push(NodePtr::Expression(*expression));
            }
        }
        Node::Expression(AstExpression {
            value: Block(statements),
            ..
        }) => {
            for statement in statements {
                children.push(NodePtr::Statement(*statement));
            }
        }
        Node::Kind(AstType {
            value: AstTypeValue::Unique(kind),
            ..
        }) => {
            children.push(NodePtr::Kind(*kind));
        }
        Node::Kind(AstType {
            value: AstTypeValue::Name(_),
            ..
        })
        | Node::Statement(AstStatement {
            value: Import(_) | StructDeclaration { .. },
            ..
        })
        | Node::Expression(AstExpression {
            value: Name(_) | Int(_) | Float(_) | Bool(_),
            ..
        }) => {}
    }
}

fn assert_next_lexeme_eq(
    lexeme: Option<Result<Lexeme, LexError>>,
    target: LexemeValue,
    provenance: Provenance,
    reason: &'static str,
) -> Result<Lexeme, ParseError> {
    let lexeme = lexeme.ok_or(ParseError::UnexpectedEndOfInput(provenance, reason))??;
    assert_lexeme_eq(&lexeme, target, reason)?;

    Ok(lexeme)
}

fn assert_lexeme_eq(
    lexeme: &Lexeme,
    target: LexemeValue,
    reason: &'static str,
) -> Result<(), ParseError> {
    if lexeme.value == target {
        Ok(())
    } else {
        Err(ParseError::UnexpectedLexeme(
            Box::new(lexeme.clone()),
            reason,
        ))
    }
}

#[cfg(test)]
mod test {
    use matches::assert_matches;

    use super::*;

    fn tokens(tokens: &[LexemeValue]) -> impl '_ + Iterator<Item = Result<Lexeme, LexError>> {
        let provenance = Provenance::new("test", "test", 0, 0);
        tokens.iter().map(move |token| {
            Ok(Lexeme {
                value: token.clone(),
                start: provenance,
                end: provenance,
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
            LexemeValue::Word("a".to_string()),
        ]))
        .unwrap();
        let nodes = statements
            .into_iter()
            .map(|statement| {
                let mut line = Vec::new();
                line.extend(ast.iter_from(NodePtr::Statement(statement)));
                line
            })
            .collect::<Vec<_>>();
        let matchable_nodes = nodes.iter().map(|line| &line[..]).collect::<Vec<_>>();

        use AstExpressionValue::*;
        use AstStatementValue::*;
        use Node::{Expression as Expr, Statement};
        assert_matches!(
            matchable_nodes.as_slice(),
            &[
                &[
                    Statement(AstStatement {
                        value: Declaration(_, _),
                        ..
                    }),
                    Expr(AstExpression {
                        value: BinExpr(BinOp::Add, _, _),
                        ..
                    }),
                    Expr(AstExpression {
                        value: BinExpr(BinOp::Subtract, _, _),
                        ..
                    }),
                    Expr(AstExpression { value: Int(15), .. }),
                    Expr(AstExpression { value: Int(10), .. }),
                    Expr(AstExpression { value: Int(3), .. }),
                ],
                &[
                    Statement(AstStatement {
                        value: Expression(_),
                        ..
                    }),
                    Expr(AstExpression {
                        value: If(_, _),
                        ..
                    }),
                    Expr(AstExpression {
                        value: Bool(true),
                        ..
                    }),
                    Expr(AstExpression {
                        value: Block(_),
                        ..
                    }),
                    Statement(AstStatement {
                        value: Expression(_),
                        ..
                    }),
                    Expr(AstExpression {
                        value: Assignment(_, _),
                        ..
                    }),
                    Expr(AstExpression { value: Int(3), .. }),
                ],
                &[
                    Statement(AstStatement {
                        value: Expression(_),
                        ..
                    }),
                    Expr(AstExpression { value: Name(_), .. }),
                ],
            ]
        );
    }

    #[test]
    fn function() {
        let (statements, ast) = parse(tokens(&[
            LexemeValue::Function,
            LexemeValue::Word("f".to_string()),
            LexemeValue::OpenParen,
            LexemeValue::Word("argument".to_string()),
            LexemeValue::Colon,
            LexemeValue::Word("i64".to_string()),
            LexemeValue::CloseParen,
            LexemeValue::Colon,
            LexemeValue::Word("i64".to_string()),
            LexemeValue::OpenBracket,
            LexemeValue::Int(3),
            LexemeValue::Plus,
            LexemeValue::Int(5),
            LexemeValue::CloseBracket,
        ]))
        .unwrap();
        let nodes = statements
            .into_iter()
            .map(|statement| {
                let mut line = Vec::new();
                line.extend(ast.iter_from(NodePtr::Statement(statement)));
                line
            })
            .collect::<Vec<_>>();
        let matchable_nodes = nodes.iter().map(|line| &line[..]).collect::<Vec<_>>();

        use AstExpressionValue::*;
        use AstStatementValue::*;
        use Node::{Expression as Expr, Statement};
        assert_matches!(
            matchable_nodes.as_slice(),
            &[&[
                Statement(AstStatement {
                    value: FunctionDeclaration {
                        returns: Some(_),
                        ..
                    },
                    ..
                }),
                Expr(AstExpression {
                    value: Block(_),
                    ..
                }),
                Statement(AstStatement {
                    value: Expression(_),
                    ..
                }),
                Expr(AstExpression {
                    value: BinExpr(BinOp::Add, _, _),
                    ..
                }),
                Expr(AstExpression { value: Int(3), .. }),
                Expr(AstExpression { value: Int(5), .. }),
            ],]
        );
    }
}
