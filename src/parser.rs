use std::{collections::HashMap, iter::Peekable};

use thiserror::Error;

use crate::{
    arena::ArenaNode,
    provenance::Provenance,
    tokenizer::{LexError, Token, TokenValue},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NameAndType {
    pub name: String,
    pub kind: usize,
}

#[derive(Debug, PartialEq)]
pub struct AstNode {
    pub value: AstNodeValue,
    pub start: Provenance,
    pub end: Provenance,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstNodeValue {
    // Statements
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
    Expression(usize), // TODO: should I just remove this wrapper?
    Import(String),

    Name(String),

    // Expressions
    Assignment(usize, usize),
    Int(i64),
    Float(f64),
    Bool(bool),
    BinExpr(BinOp, usize, usize),
    If(usize, usize),
    While(usize, usize),
    Call(usize, Vec<usize>),
    TakeUnique(usize),
    TakeShared(usize),
    StructLiteral {
        name: String,
        fields: HashMap<String, usize>,
    },
    ArrayLiteral(Vec<usize>),
    ArrayLiteralLength(usize, u64),
    Block(Vec<usize>),

    // Types
    Unique(usize),
    Shared(usize),
    Array(usize),
}

impl ArenaNode for AstNode {
    fn write_children(&self, children: &mut Vec<usize>) {
        use AstNodeValue::*;

        match &self.value {
            Declaration(_, child)
            | Expression(child)
            | FunctionDeclaration { body: child, .. }
            | TakeShared(child)
            | TakeUnique(child)
            | ArrayLiteralLength(child, _)
            | Unique(child)
            | Shared(child)
            | Array(child) => {
                children.push(*child);
            }
            Assignment(left, right)
            | BinExpr(_, left, right)
            | If(left, right)
            | While(left, right) => {
                children.push(*right);
                children.push(*left);
            }
            ArrayLiteral(values) | Block(values) => {
                for value in values {
                    children.push(*value);
                }
            }
            StructLiteral { fields, .. } => {
                for expression in fields.values() {
                    children.push(*expression);
                }
            }
            Call(function, parameters) => {
                children.push(*function);
                for expression in parameters {
                    children.push(*expression);
                }
            }
            Name(_) | Import(_) | StructDeclaration { .. } | Int(_) | Float(_) | Bool(_) => {}
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Dot,
    Add,
    Subtract,
    LessThan,
    GreaterThan,
    Index,
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("unexpected token {0}, {1}")]
    UnexpectedLexeme(Box<Token>, &'static str),
    #[error("unexpected end of input at {0}, {1}")]
    UnexpectedEndOfInput(Provenance, &'static str),
    #[error("expected type for parameter at {0}")]
    MissingTypeForParam(Provenance),
    #[error("token error: {0}")]
    TokenError(#[from] LexError),
}

type TokenIterInner<'a> = &'a mut dyn Iterator<Item = Result<Token, LexError>>;
type TokenIter<'a> = Peekable<TokenIterInner<'a>>;

pub fn parse(
    mut source: impl Iterator<Item = Result<Token, LexError>>,
) -> Result<(Vec<usize>, Vec<AstNode>), ParseError> {
    let mut source = (&mut source as TokenIterInner<'_>).peekable();
    let mut context = Vec::new();

    let mut top_level = Vec::new();

    while let Some(lexeme) = peek_token_optional(&mut source)? {
        let start = lexeme.start;
        let statement = statement(&mut source, &mut context, start)?;
        top_level.push(add_node(&mut context, statement));
    }

    Ok((top_level, context))
}

fn add_node(context: &mut Vec<AstNode>, node: AstNode) -> usize {
    let idx = context.len();
    context.push(node);

    idx
}

fn statement(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    let statement = match peek_token(source, start, "expected let, fn, or expression")? {
        Token {
            value:
                value @ (TokenValue::Let
                | TokenValue::Import
                | TokenValue::Function
                | TokenValue::Struct),
            start,
            ..
        } => {
            let start = *start;
            let value = value.clone();
            source.next();
            match value {
                TokenValue::Let => variable_declaration(source, context, start)?,
                TokenValue::Import => import_declaration(source, start)?,
                TokenValue::Function => function_declaration(source, context, start)?,
                TokenValue::Struct => struct_declaration(source, context, start)?,
                _ => unreachable!(),
            }
        }
        _ => {
            let expr = expression(source, context, start)?;
            let start = expr.start;
            let end = expr.end;
            AstNode {
                value: AstNodeValue::Expression(add_node(context, expr)),
                start,
                end,
            }
        }
    };
    if let Some(Token {
        value: TokenValue::Semicolon,
        ..
    }) = peek_token_optional(source)?
    {
        source.next();
    }
    Ok(statement)
}

fn struct_declaration(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    let (name, _, end) = word(source, start, "expected name after 'struct'")?;
    assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenBracket,
        end,
        "expected open parenthesis to start parameters",
    )?;

    let mut fields = Vec::new();
    let mut pos = end;
    let mut closed = peek_for_closed(
        source,
        TokenValue::CloseBracket,
        pos,
        "expected either fields or close bracket",
    )?;

    while !closed {
        let (name, end, type_hint) =
            name_and_type_hint(source, context, pos, "expected parameter")?;
        let kind = type_hint.ok_or(ParseError::MissingTypeForParam(end))?;
        let kind = add_node(context, kind);
        pos = end;
        fields.push(NameAndType { name, kind });

        let (should_end, end) = comma_or_end_list(
            source,
            TokenValue::CloseBracket,
            pos,
            "expected either fields or close bracket",
        )?;
        closed = should_end;
        pos = end;
    }

    Ok(AstNode {
        value: AstNodeValue::StructDeclaration { name, fields },
        start,
        end,
    })
}

fn function_declaration(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    let (name, _, end) = word(source, start, "expected name after 'fn'")?;

    let mut pos = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenParen,
        end,
        "expected open parenthesis to start parameters",
    )?
    .end;
    let mut params = Vec::new();
    loop {
        let token = peek_token(source, pos, "expected either parameters or close paren")?;
        pos = token.start;
        match token.value {
            TokenValue::CloseParen => break,
            TokenValue::Comma => {
                source.next();
            }
            _ => {
                let (name, end, type_hint) =
                    name_and_type_hint(source, context, pos, "expected parameter")?;
                let kind = type_hint.ok_or(ParseError::MissingTypeForParam(end))?;
                let kind = add_node(context, kind);
                pos = end;
                params.push(NameAndType { name, kind });
            }
        }
    }
    assert_next_lexeme_eq(
        source.next(),
        TokenValue::CloseParen,
        pos,
        "expected closing parenthesis to end parameters",
    )?;
    let returns = if let Token {
        value: TokenValue::Colon,
        start,
        ..
    } = peek_token(source, start, "expected body after function declaration")?
    {
        let start = *start;
        source.next();
        let kind = type_expression(source, context, start)?;
        let kind = add_node(context, kind);
        Some(kind)
    } else {
        None
    };
    let body = block(source, context, start)?;
    let end = body.end;

    Ok(AstNode {
        value: AstNodeValue::FunctionDeclaration {
            name,
            params,
            returns,
            body: add_node(context, body),
        },
        start,
        end,
    })
}

fn import_declaration(source: &mut TokenIter, start: Provenance) -> Result<AstNode, ParseError> {
    let (name, _, end) = word(source, start, "expected word after 'import'")?;

    Ok(AstNode {
        value: AstNodeValue::Import(name),
        start,
        end,
    })
}

fn variable_declaration(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    // TODO: store type hint in declarations
    let (name, start, _) = name_and_type_hint(
        source,
        context,
        start,
        "expected word after 'let' in declaration",
    )?;
    assert_next_lexeme_eq(
        source.next(),
        TokenValue::Equals,
        start,
        "expected = after let binding target",
    )?;
    let value = expression(source, context, start)?;
    let end = value.end;

    Ok(AstNode {
        value: AstNodeValue::Declaration(name, add_node(context, value)),
        start,
        end,
    })
}

fn name_and_type_hint(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
    reason: &'static str,
) -> Result<(String, Provenance, Option<AstNode>), ParseError> {
    let (name, _, mut end) = word(source, start, reason)?;
    let type_hint = if let Some(Ok(Token {
        value: TokenValue::Colon,
        ..
    })) = source.peek()
    {
        source.next();
        let type_hint = type_expression(source, context, start)?;
        end = type_hint.end;
        Some(type_hint)
    } else {
        None
    };

    Ok((name, end, type_hint))
}

fn type_expression(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    let next = token(source, start, "expected type")?;
    match next.value {
        ptr @ (TokenValue::Unique | TokenValue::Shared) => {
            let subtype = type_expression(source, context, next.end)?;
            let end = subtype.end;
            let subtype = add_node(context, subtype);
            Ok(AstNode {
                value: match ptr {
                    TokenValue::Unique => AstNodeValue::Unique(subtype),
                    TokenValue::Shared => AstNodeValue::Shared(subtype),
                    _ => unreachable!(),
                },
                start: next.start,
                end,
            })
        }
        TokenValue::OpenSquare => {
            let subtype = type_expression(source, context, next.end)?;
            let end = subtype.end;
            assert_next_lexeme_eq(
                source.next(),
                TokenValue::CloseSquare,
                end,
                "expected ] after array type",
            )?;
            let subtype = add_node(context, subtype);
            Ok(AstNode {
                value: AstNodeValue::Array(subtype),
                start: next.start,
                end,
            })
        }
        TokenValue::Word(name) => Ok(AstNode {
            value: AstNodeValue::Name(name),
            start: next.start,
            end: next.end,
        }),
        _ => Err(ParseError::UnexpectedLexeme(
            Box::new(next),
            "expected either 'unique' or type name",
        )),
    }
}

fn expression(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
) -> Result<AstNode, ParseError> {
    match peek_token(source, provenance, "expected expression")? {
        Token {
            value: token @ (TokenValue::If | TokenValue::While),
            start,
            ..
        } => {
            let start = *start;
            let token = token.clone();
            source.next();
            branch(source, context, token, start)
        }
        Token {
            value: TokenValue::OpenBracket,
            start,
            ..
        } => {
            let start = *start;
            block(source, context, start)
        }
        _ => assignment_step(source, context, provenance),
    }
}

fn branch(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    token: TokenValue,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    let predicate = expression(source, context, start)?;
    let block = block(source, context, start)?;

    let predicate_ptr = add_node(context, predicate);
    let end = block.end;
    let block_ptr = add_node(context, block);

    Ok(AstNode {
        value: if token == TokenValue::If {
            AstNodeValue::If(predicate_ptr, block_ptr)
        } else {
            AstNodeValue::While(predicate_ptr, block_ptr)
        },
        start,
        end,
    })
}

fn block(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    let start = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenBracket,
        start,
        "expected { to start a block",
    )?
    .start;
    let mut statements = Vec::new();
    let mut end = start;
    loop {
        match peek_token(source, start, "expected statement or close bracket")? {
            Token {
                value: TokenValue::CloseBracket,
                end,
                ..
            } => {
                let end = *end;
                source.next();
                return Ok(AstNode {
                    value: AstNodeValue::Block(statements),
                    start,
                    end,
                });
            }
            _ => {
                let statement = statement(source, context, end)?;
                end = statement.end;
                statements.push(add_node(context, statement));
            }
        }
    }
}

fn assignment_step(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
) -> Result<AstNode, ParseError> {
    let mut root = reference_step(source, context, provenance)?;

    while let Some(Token {
        value: TokenValue::Equals,
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

        root = AstNode {
            value: AstNodeValue::Assignment(left, right),
            start,
            end,
        };
    }

    Ok(root)
}

fn reference_step(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
) -> Result<AstNode, ParseError> {
    let next = peek_token(source, provenance, "expected reference or expression")?;
    let start = next.start;
    match &next.value {
        lex @ (TokenValue::Shared | TokenValue::Unique) => {
            let lex = lex.clone();
            source.next();
            let child = reference_step(source, context, start)?;
            let end = child.end;
            let child = add_node(context, child);
            Ok(AstNode {
                value: match lex {
                    TokenValue::Shared => AstNodeValue::TakeShared(child),
                    TokenValue::Unique => AstNodeValue::TakeUnique(child),
                    _ => unreachable!(),
                },
                start,
                end,
            })
        }
        _ => comparison_step(source, context, provenance),
    }
}

fn comparison_step(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
) -> Result<AstNode, ParseError> {
    let mut left = sum_step(source, context, provenance)?;

    while let Some(Token {
        value: token @ (TokenValue::LessThan | TokenValue::GreaterThan),
        ..
    }) = peek_token_optional(source)?
    {
        let operator = match token {
            TokenValue::LessThan => BinOp::LessThan,
            TokenValue::GreaterThan => BinOp::GreaterThan,
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
    context: &mut Vec<AstNode>,
    provenance: Provenance,
) -> Result<AstNode, ParseError> {
    let mut left = call_step(source, context, provenance)?;

    while let Some(Token {
        value: token @ (TokenValue::Plus | TokenValue::Minus),
        ..
    }) = peek_token_optional(source)?
    {
        let operator = match token {
            TokenValue::Plus => BinOp::Add,
            TokenValue::Minus => BinOp::Subtract,
            _ => unimplemented!(),
        };
        source.next();
        let right = call_step(source, context, provenance)?;
        left = make_bin_expr(context, operator, left, right);
    }

    Ok(left)
}

fn make_bin_expr(
    context: &mut Vec<AstNode>,
    operator: BinOp,
    left: AstNode,
    right: AstNode,
) -> AstNode {
    let start = left.start;
    let end = right.end;

    AstNode {
        value: AstNodeValue::BinExpr(operator, add_node(context, left), add_node(context, right)),
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

fn span(context: &mut Vec<AstNode>, left: AstNode, right: AstNode) -> Span {
    let start = left.start;
    let end = right.end;
    let left = add_node(context, left);
    let right = add_node(context, right);

    Span {
        left,
        right,
        start,
        end,
    }
}

fn call_step(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
) -> Result<AstNode, ParseError> {
    let mut root = dot_step(source, context, provenance)?;

    while let Some(Token {
        value: opening @ (TokenValue::OpenParen | TokenValue::OpenSquare),
        end,
        ..
    }) = peek_token_optional(source)?
    {
        let is_square = opening == &TokenValue::OpenSquare;
        let mut end = *end;
        source.next(); // remove the peeked token
        if is_square {
            let index = expression(source, context, end)?;
            assert_next_lexeme_eq(
                source.next(),
                TokenValue::CloseSquare,
                end,
                "expected ] to follow array index",
            )?;
            let index = add_node(context, index);
            let start = root.start;
            let left = add_node(context, root);
            root = AstNode {
                value: AstNodeValue::BinExpr(BinOp::Index, left, index),
                start,
                end,
            };
        } else {
            let mut arguments = Vec::new();

            let mut closed = peek_for_closed(
                source,
                TokenValue::CloseParen,
                end,
                "expected ) or next argument",
            )?;

            while !closed {
                let argument = expression(source, context, provenance)?;
                end = argument.end;
                arguments.push(add_node(context, argument));

                let (should_break, new_end) = comma_or_end_list(
                    source,
                    TokenValue::CloseParen,
                    end,
                    "expected comma or ) to end function call",
                )?;
                end = new_end;
                closed = should_break;
            }

            let start = root.start;
            let function = add_node(context, root);

            root = AstNode {
                value: AstNodeValue::Call(function, arguments),
                start,
                end,
            }
        }
    }

    Ok(root)
}

fn dot_step(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
) -> Result<AstNode, ParseError> {
    let mut root = paren_step(source, context, provenance)?;
    while let Some(Token {
        value: TokenValue::Period,
        ..
    }) = peek_token_optional(source)?
    {
        source.next();
        let (name, start, end) = word(source, provenance, "expected a name after dot operator")?;
        let right = add_node(
            context,
            AstNode {
                value: AstNodeValue::Name(name),
                start,
                end,
            },
        );
        let start = root.start;
        let left = add_node(context, root);
        root = AstNode {
            value: AstNodeValue::BinExpr(BinOp::Dot, left, right),
            start,
            end,
        };
    }

    Ok(root)
}

fn paren_step(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
) -> Result<AstNode, ParseError> {
    let peeked = peek_token(source, provenance, "expected (, [, or expression")?;
    match peeked.value {
        TokenValue::OpenParen => {
            let start = peeked.start;
            source.next();
            paren(source, context, start)
        }
        TokenValue::OpenSquare => {
            let start = peeked.start;
            source.next();
            array_literal(source, context, start)
        }
        _ => struct_literal_step(source, context, provenance),
    }
}

fn paren(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
) -> Result<AstNode, ParseError> {
    let expr = expression(source, context, provenance)?;
    assert_next_lexeme_eq(
        source.next(),
        TokenValue::CloseParen,
        expr.start,
        "parenthesized expressions must end with a )",
    )?;

    Ok(expr)
}

fn array_literal(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    let expr = expression(source, context, start)?;
    let separator = token(source, start, "expected comma, semicolon or ]")?;
    match separator.value {
        TokenValue::Comma => {
            let mut end = separator.end;
            let mut children = vec![add_node(context, expr)];

            let mut closed = peek_for_closed(
                source,
                TokenValue::CloseSquare,
                end,
                "expected ] or next argument",
            )?;

            while !closed {
                let peeked = peek_token(source, end, "expected , or ]")?;
                if TokenValue::CloseSquare == peeked.value {
                    source.next();
                    break;
                }
                end = peeked.end;
                let expr = expression(source, context, end)?;
                end = expr.end;
                children.push(add_node(context, expr));

                let (should_break, new_end) = comma_or_end_list(
                    source,
                    TokenValue::CloseSquare,
                    end,
                    "expected comma or ] to end array literal",
                )?;
                end = new_end;
                closed = should_break;
            }
            Ok(AstNode {
                value: AstNodeValue::ArrayLiteral(children),
                start,
                end,
            })
        }
        TokenValue::Semicolon => {
            let (length, _, end) =
                integer(source, start, "expected number after ; in array literal")?;
            let close = assert_next_lexeme_eq(
                source.next(),
                TokenValue::CloseSquare,
                end,
                "expected ] after array length in array literal",
            )?;
            let expr = add_node(context, expr);
            Ok(AstNode {
                value: AstNodeValue::ArrayLiteralLength(expr, length),
                start,
                end: close.end,
            })
        }
        TokenValue::CloseSquare => {
            let expr = add_node(context, expr);
            Ok(AstNode {
                value: AstNodeValue::ArrayLiteral(vec![expr]),
                start,
                end: separator.end,
            })
        }
        _ => Err(ParseError::UnexpectedLexeme(
            Box::new(separator),
            "expected comma, semicolon or ]",
        )),
    }
}

fn struct_literal_step(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
) -> Result<AstNode, ParseError> {
    let atom = atom(source, provenance, "expected a word or atom")?;
    match (atom, peek_token_optional(source)?) {
        (
            AstNode {
                value: AstNodeValue::Name(name),
                start,
                ..
            },
            Some(Token {
                value: TokenValue::OpenBracket,
                end,
                ..
            }),
        ) => {
            let name = name;
            let start = start;
            let mut end = *end;
            source.next();
            let mut fields = HashMap::new();

            loop {
                if TokenValue::CloseBracket
                    == peek_token(source, end, "expected } or next field")?.value
                {
                    source.next();
                    break;
                }

                let (field, field_start, field_end) =
                    word(source, end, "expected field in struct literal")?;
                if let lex @ (TokenValue::Comma | TokenValue::CloseBracket) =
                    &peek_token(source, end, "expected comma or } to end struct literal")?.value
                {
                    let lex = lex.clone();
                    source.next();
                    let argument = AstNode {
                        value: AstNodeValue::Name(field.clone()),
                        start: field_start,
                        end: field_end,
                    };
                    fields.insert(field, add_node(context, argument));
                    if lex == TokenValue::CloseBracket {
                        break;
                    }
                } else {
                    assert_next_lexeme_eq(
                        source.next(),
                        TokenValue::Colon,
                        end,
                        "expected colon after field name",
                    )?;
                    let argument = expression(source, context, provenance)?;
                    end = argument.end;
                    fields.insert(field, add_node(context, argument));

                    if let TokenValue::Comma =
                        peek_token(source, end, "expected comma or ) to end function call")?.value
                    {
                        source.next();
                    } else {
                        assert_next_lexeme_eq(
                            source.next(),
                            TokenValue::CloseBracket,
                            end,
                            "expected close parenthesis or comma after argument",
                        )?;
                        break;
                    }
                }
            }
            Ok(AstNode {
                value: AstNodeValue::StructLiteral { name, fields },
                start,
                end,
            })
        }
        (atom, _) => Ok(atom),
    }
}

fn atom(
    source: &mut TokenIter,
    provenance: Provenance,
    reason: &'static str,
) -> Result<AstNode, ParseError> {
    // TODO: expectation reasoning
    let Token { value, start, end } = token(source, provenance, reason)?;
    match value {
        TokenValue::True => Ok(AstNode {
            value: AstNodeValue::Bool(true),
            start,
            end,
        }),
        TokenValue::False => Ok(AstNode {
            value: AstNodeValue::Bool(false),
            start,
            end,
        }),
        TokenValue::Word(word) => Ok(AstNode {
            value: AstNodeValue::Name(word),
            start,
            end,
        }),
        TokenValue::Int(int) => try_decimal(source, int as i64, start, end),
        // TODO: should this be treated as a unary operator instead?
        TokenValue::Minus => {
            let (int, _, end) = integer(source, start, "expected digit after negative sign")?;
            try_decimal(source, -(int as i64), start, end)
        }
        value => Err(ParseError::UnexpectedLexeme(
            Box::new(Token { value, start, end }),
            reason,
        )),
    }
}

fn integer(
    source: &mut TokenIter,
    start: Provenance,
    reason: &'static str,
) -> Result<(u64, Provenance, Provenance), ParseError> {
    match token(source, start, reason)? {
        Token {
            value: TokenValue::Int(int),
            start,
            end,
        } => Ok((int, start, end)),
        other => Err(ParseError::UnexpectedLexeme(Box::new(other), reason)),
    }
}

fn try_decimal(
    source: &mut TokenIter,
    num: i64,
    start: Provenance,
    end: Provenance,
) -> Result<AstNode, ParseError> {
    // TODO: handle overflow
    if let Some(Token {
        value: TokenValue::Period,
        ..
    }) = peek_token_optional(source)?
    {
        source.next();
        let (decimal, end) = match source.next().ok_or(ParseError::UnexpectedEndOfInput(
            start,
            "expected digit after decimal point",
        ))?? {
            Token {
                value: TokenValue::Int(value),
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
        Ok(AstNode {
            value: AstNodeValue::Float(num),
            start,
            end,
        })
    } else {
        Ok(AstNode {
            value: AstNodeValue::Int(num),
            start,
            end,
        })
    }
}

fn token(
    source: &mut TokenIter,
    provenance: Provenance,
    reason: &'static str,
) -> Result<Token, ParseError> {
    let lex = source
        .next()
        .ok_or(ParseError::UnexpectedEndOfInput(provenance, reason))??;
    Ok(lex)
}

fn word(
    source: &mut TokenIter,
    provenance: Provenance,
    reason: &'static str,
) -> Result<(String, Provenance, Provenance), ParseError> {
    match token(source, provenance, reason)? {
        Token {
            value: TokenValue::Word(name),
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
) -> Result<&'a Token, ParseError> {
    Ok(source
        .peek()
        .ok_or(ParseError::UnexpectedEndOfInput(provenance, reason))?
        .as_ref()
        .map_err(|e| e.clone())?)
}

fn peek_token_optional<'a>(source: &'a mut TokenIter) -> Result<Option<&'a Token>, ParseError> {
    Ok(source
        .peek()
        .map(|result| result.as_ref().map_err(|e| e.clone()))
        .transpose()?)
}

fn assert_next_lexeme_eq(
    lexeme: Option<Result<Token, LexError>>,
    target: TokenValue,
    provenance: Provenance,
    reason: &'static str,
) -> Result<Token, ParseError> {
    let lexeme = lexeme.ok_or(ParseError::UnexpectedEndOfInput(provenance, reason))??;
    assert_lexeme_eq(&lexeme, target, reason)?;

    Ok(lexeme)
}

fn assert_lexeme_eq(
    lexeme: &Token,
    target: TokenValue,
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

/**
 * Returns (did_list_end, end_of_token)
 */
fn comma_or_end_list(
    source: &mut TokenIter,
    list_end: TokenValue,
    start: Provenance,
    reason: &'static str,
) -> Result<(bool, Provenance), ParseError> {
    let next = token(source, start, reason)?;
    match next.value {
        TokenValue::Comma => match peek_token_optional(source)? {
            Some(token) if token.value == list_end => {
                let end = token.end;
                source.next();
                Ok((true, end))
            }
            _ => Ok((false, next.end)),
        },
        other if other == list_end => Ok((true, next.end)),
        _ => Err(ParseError::UnexpectedLexeme(Box::new(next), reason)),
    }
}

fn peek_for_closed(
    source: &mut TokenIter,
    list_end: TokenValue,
    provenance: Provenance,
    reason: &'static str,
) -> Result<bool, ParseError> {
    let closed = peek_token(source, provenance, reason)?.value == list_end;
    if closed {
        source.next();
    }

    Ok(closed)
}

#[cfg(test)]
mod test {
    use matches::assert_matches;

    use crate::arena::ArenaIter;

    use super::*;

    fn tokens(tokens: &[TokenValue]) -> impl '_ + Iterator<Item = Result<Token, LexError>> {
        let provenance = Provenance::new("test", "test", 0, 0);
        tokens.iter().map(move |token| {
            Ok(Token {
                value: token.clone(),
                start: provenance,
                end: provenance,
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
            TokenValue::Word("a".to_string()),
        ]))
        .unwrap();
        let lines = statements
            .iter()
            .map(|statement| {
                ArenaIter::iter_from(&ast, *statement)
                    .map(|(_, node)| &node.value)
                    .collect()
            })
            .collect::<Vec<Vec<_>>>();
        let matchable_nodes = lines.iter().map(|line| line.as_slice()).collect::<Vec<_>>();

        use AstNodeValue::*;
        assert_matches!(
            matchable_nodes.as_slice(),
            &[
                &[
                    Declaration(_, _),
                    BinExpr(BinOp::Add, _, _),
                    BinExpr(BinOp::Subtract, _, _),
                    Int(15),
                    Int(10),
                    Int(3),
                ],
                &[
                    Expression(_),
                    If(..),
                    Bool(true),
                    Block(_),
                    Expression(_),
                    Assignment(_, _),
                    Name(_),
                    Int(3),
                ],
                &[Expression(_), Name(_),],
            ]
        );
    }

    #[test]
    fn function() {
        let (statements, ast) = parse(tokens(&[
            TokenValue::Function,
            TokenValue::Word("f".to_string()),
            TokenValue::OpenParen,
            TokenValue::Word("argument".to_string()),
            TokenValue::Colon,
            TokenValue::Word("i64".to_string()),
            TokenValue::CloseParen,
            TokenValue::Colon,
            TokenValue::Word("i64".to_string()),
            TokenValue::OpenBracket,
            TokenValue::Int(3),
            TokenValue::Plus,
            TokenValue::Int(5),
            TokenValue::CloseBracket,
        ]))
        .unwrap();
        let lines = statements
            .iter()
            .map(|statement| {
                ArenaIter::iter_from(&ast, *statement)
                    .map(|(_, node)| &node.value)
                    .collect()
            })
            .collect::<Vec<Vec<_>>>();
        let matchable_nodes = lines.iter().map(|line| line.as_slice()).collect::<Vec<_>>();

        use AstNodeValue::*;
        assert_matches!(
            matchable_nodes.as_slice(),
            &[&[
                FunctionDeclaration {
                    returns: Some(_),
                    ..
                },
                Block(_),
                Expression(_),
                BinExpr(BinOp::Add, _, _),
                Int(3),
                Int(5),
            ],]
        );
    }
}
