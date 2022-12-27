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
        is_extern: bool,
    },
    ExternFunctionBinding {
        name: String,
        params: Vec<NameAndType>,
        returns: Option<usize>,
    },
    StructDeclaration {
        name: String,
        fields: Vec<NameAndType>,
    },
    Declaration(String, usize),
    Expression(usize), // TODO: should I just remove this wrapper?
    Import(String),
    Return(usize),

    Name(String),

    // Expressions
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
    UniqueType(usize),
    SharedType(usize),
    ArrayType(usize),
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
            | UniqueType(child)
            | SharedType(child)
            | Return(child)
            | ArrayType(child) => {
                children.push(*child);
            }
            BinExpr(_, left, right) | If(left, right) | While(left, right) => {
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
            Name(_)
            | Import(_)
            | StructDeclaration { .. }
            | Int(_)
            | Float(_)
            | Bool(_)
            | ExternFunctionBinding { .. } => {}
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Dot,
    Add,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    GreaterThan,
    Index,
    Assignment,
}

#[derive(Debug, Error)]
pub enum ParseError {
    #[error("unexpected token {0}, {1}")]
    UnexpectedToken(Box<Token>, &'static str),
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
    let statement = match peek_token(source, start, "expected let, fn, or expression")?.value {
        TokenValue::Let
        | TokenValue::Import
        | TokenValue::Function
        | TokenValue::Extern
        | TokenValue::Struct
        | TokenValue::Return => {
            let Token { start, value, .. } = already_peeked_token(source)?;
            match value {
                TokenValue::Let => variable_declaration(source, context, start)?,
                TokenValue::Import => import_declaration(source, start)?,
                TokenValue::Extern => extern_function_declaration(source, context, start)?,
                TokenValue::Function => function_declaration(source, context, start)?,
                TokenValue::Struct => struct_declaration(source, context, start)?,
                TokenValue::Return => return_declaration(source, context, start)?,
                _ => unreachable!(),
            }
        }
        _ => {
            let expr = expression(source, context, start, true)?;
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

fn return_declaration(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    let value = expression(source, context, start, true)?;
    let end = value.end;
    let value = add_node(context, value);

    Ok(AstNode {
        value: AstNodeValue::Return(value),
        start,
        end,
    })
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

fn extern_function_declaration(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    assert_next_lexeme_eq(
        source.next(),
        TokenValue::Function,
        start,
        "expected 'fn' after 'extern'",
    )?;
    let FunctionHeader {
        name,
        params,
        returns,
        end,
    } = function_header(source, context, start)?;

    let next = token(source, end, "expected ; or { after extern fn decl")?;
    let (value, end) = match &next.value {
        TokenValue::Semicolon => (
            AstNodeValue::ExternFunctionBinding {
                name,
                params,
                returns,
            },
            next.end,
        ),
        TokenValue::OpenBracket => {
            let body = block(source, context, end)?;
            let end = body.end;
            (
                AstNodeValue::FunctionDeclaration {
                    name,
                    params,
                    returns,
                    body: add_node(context, body),
                    is_extern: false,
                },
                end,
            )
        }
        _ => {
            return Err(ParseError::UnexpectedToken(
                Box::new(next),
                "expected ; or { after extern fn decl",
            ))
        }
    };

    Ok(AstNode { value, start, end })
}

fn function_declaration(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    let FunctionHeader {
        name,
        params,
        returns,
        end: _,
    } = function_header(source, context, start)?;
    let Token { end, .. } = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenBracket,
        start,
        "expected { after function declaration",
    )?;
    let body = block(source, context, end)?;
    let end = body.end;

    Ok(AstNode {
        value: AstNodeValue::FunctionDeclaration {
            name,
            params,
            returns,
            body: add_node(context, body),
            is_extern: false,
        },
        start,
        end,
    })
}

struct FunctionHeader {
    name: String,
    params: Vec<NameAndType>,
    returns: Option<usize>,
    end: Provenance,
}

fn function_header(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
) -> Result<FunctionHeader, ParseError> {
    let (name, _, mut end) = word(source, start, "expected name after 'fn'")?;

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
    end = assert_next_lexeme_eq(
        source.next(),
        TokenValue::CloseParen,
        pos,
        "expected closing parenthesis to end parameters",
    )?
    .end;
    let returns = if let Token {
        value: TokenValue::Colon,
        start,
        end: colon_end,
    } = peek_token(source, start, "expected body after function declaration")?
    {
        let start = *start;
        end = *colon_end;
        source.next();
        let kind = type_expression(source, context, start)?;
        let kind = add_node(context, kind);

        Some(kind)
    } else {
        None
    };

    Ok(FunctionHeader {
        name,
        params,
        returns,
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
    let value = expression(source, context, start, true)?;
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
                    TokenValue::Unique => AstNodeValue::UniqueType(subtype),
                    TokenValue::Shared => AstNodeValue::SharedType(subtype),
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
                value: AstNodeValue::ArrayType(subtype),
                start: next.start,
                end,
            })
        }
        TokenValue::Word(name) => Ok(AstNode {
            value: AstNodeValue::Name(name),
            start: next.start,
            end: next.end,
        }),
        _ => Err(ParseError::UnexpectedToken(
            Box::new(next),
            "expected either 'unique' or type name",
        )),
    }
}

fn expression(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
    can_be_struct: bool, // TODO: refactor grammar?
) -> Result<AstNode, ParseError> {
    expression_pratt(source, context, provenance, 0, can_be_struct)
}

// Pratt parser based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn expression_pratt(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    provenance: Provenance,
    min_binding: u8,
    can_be_struct: bool,
) -> Result<AstNode, ParseError> {
    let Token { value, start, end } = token(source, provenance, "expected expression")?;
    let mut left = match value {
        // TODO: should this be treated as a unary operator instead?
        TokenValue::Minus => {
            let (int, _, end) = integer(source, end, "expected digit after negative sign")?;
            try_decimal(source, -(int as i64), start, end)?
        }
        TokenValue::OpenParen => {
            let left = expression_pratt(source, context, end, 0, can_be_struct)?;
            assert_next_lexeme_eq(
                source.next(),
                TokenValue::CloseParen,
                left.end,
                "expected ) to match (",
            )?;

            left
        }
        TokenValue::OpenSquare => array_literal(source, context, end, can_be_struct)?,
        token @ (TokenValue::If | TokenValue::While) => branch(source, context, token, start)?,
        TokenValue::OpenBracket => block(source, context, start)?,
        // Atoms
        TokenValue::True => AstNode {
            value: AstNodeValue::Bool(true),
            start,
            end,
        },
        TokenValue::False => AstNode {
            value: AstNodeValue::Bool(false),
            start,
            end,
        },
        TokenValue::Word(word) => AstNode {
            value: AstNodeValue::Name(word),
            start,
            end,
        },
        TokenValue::Int(int) => try_decimal(source, int as i64, start, end)?,
        // Prefix operator
        value => {
            let Some(((), right_binding)) = prefix_binding_power(&value) else {
                return Err(ParseError::UnexpectedToken(
                    Box::new(Token { value, start, end }),
                    "expected an expression",
                ))
            };
            let right = expression_pratt(source, context, start, right_binding, can_be_struct)?;
            let end = right.end;
            let right = add_node(context, right);
            AstNode {
                value: match value {
                    TokenValue::Shared => AstNodeValue::TakeShared(right),
                    TokenValue::Unique => AstNodeValue::TakeUnique(right),
                    other => unreachable!("prefix operator {:?}", other),
                },
                start,
                end,
            }
        }
    };

    loop {
        let op = match peek_token_optional(source)? {
            None => break,
            Some(token) => token,
        };

        if let Some((binding, ())) = postfix_binding_power(&op.value) {
            if binding < min_binding {
                break;
            }
            if op.value == TokenValue::OpenBracket && !can_be_struct {
                break;
            }
            let Token { value, end, .. } = already_peeked_token(source)?;

            match value {
                TokenValue::OpenParen => {
                    let mut end = end;
                    let mut arguments = Vec::new();

                    let mut closed = peek_for_closed(
                        source,
                        TokenValue::CloseParen,
                        end,
                        "expected ) or next argument",
                    )?;

                    while !closed {
                        let argument = expression(source, context, provenance, can_be_struct)?;
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

                    left = AstNode {
                        value: AstNodeValue::Call(add_node(context, left), arguments),
                        start,
                        end,
                    };
                }
                TokenValue::OpenSquare => {
                    let index = expression(source, context, end, can_be_struct)?;
                    let Token { end, .. } = assert_next_lexeme_eq(
                        source.next(),
                        TokenValue::CloseSquare,
                        index.end,
                        "expected ] to follow array index",
                    )?;
                    left = AstNode {
                        value: AstNodeValue::BinExpr(
                            BinOp::Index,
                            add_node(context, left),
                            add_node(context, index),
                        ),
                        start,
                        end,
                    };
                }
                TokenValue::OpenBracket if can_be_struct => {
                    let mut end = end;
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
                            &peek_token(source, end, "expected comma or } to end struct literal")?
                                .value
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
                            let argument = expression(source, context, provenance, can_be_struct)?;
                            end = argument.end;
                            fields.insert(field, add_node(context, argument));

                            if let TokenValue::Comma =
                                peek_token(source, end, "expected comma or ) to end function call")?
                                    .value
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

                    let AstNodeValue::Name(name) = left.value else {
                        todo!("non-name struct literal");
                    };

                    left = AstNode {
                        value: AstNodeValue::StructLiteral { name, fields },
                        start,
                        end,
                    };
                }
                token => unreachable!("postfix operator {:?}", token),
            }

            continue;
        }

        if let Some((left_binding, right_binding)) = infix_binding_power(&op.value) {
            if left_binding < min_binding {
                break;
            }
            let Token {
                value,
                end: current,
                ..
            } = already_peeked_token(source)?;

            let right = expression_pratt(source, context, current, right_binding, can_be_struct)?;

            let bin_op = match value {
                TokenValue::Equals => BinOp::Assignment,
                TokenValue::LessThan => BinOp::LessThan,
                TokenValue::GreaterThan => BinOp::GreaterThan,
                TokenValue::Plus => BinOp::Add,
                TokenValue::Minus => BinOp::Subtract,
                TokenValue::Period => BinOp::Dot,
                TokenValue::Asterisk => BinOp::Multiply,
                TokenValue::ForwardSlash => BinOp::Divide,
                token => unreachable!("binary operator {:?}", token),
            };

            let start = left.start;
            let end = right.end;
            left = AstNode {
                value: AstNodeValue::BinExpr(
                    bin_op,
                    add_node(context, left),
                    add_node(context, right),
                ),
                start,
                end,
            };

            continue;
        }

        break;
    }

    Ok(left)
}

const ASSIGNMENT: u8 = 2;
const REFERENCE: u8 = ASSIGNMENT + 1;
const COMPARE: u8 = REFERENCE + 2;
const SUM: u8 = COMPARE + 2;
const FACTOR: u8 = SUM + 2;
const DOT: u8 = FACTOR + 2;
const CALL: u8 = DOT + 1;

fn prefix_binding_power(op: &TokenValue) -> Option<((), u8)> {
    let res = match op {
        TokenValue::Shared | TokenValue::Unique => ((), REFERENCE),
        _ => return None,
    };
    Some(res)
}

fn postfix_binding_power(op: &TokenValue) -> Option<(u8, ())> {
    let res = match op {
        TokenValue::OpenParen | TokenValue::OpenSquare | TokenValue::OpenBracket => (CALL, ()),
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: &TokenValue) -> Option<(u8, u8)> {
    let res = match op {
        TokenValue::Equals => (ASSIGNMENT, ASSIGNMENT - 1),
        TokenValue::LessThan | TokenValue::GreaterThan => (COMPARE, COMPARE - 1),
        TokenValue::Plus | TokenValue::Minus => (SUM, SUM - 1),
        TokenValue::Asterisk | TokenValue::ForwardSlash => (FACTOR, FACTOR - 1),
        TokenValue::Period => (DOT - 1, DOT),
        _ => return None,
    };
    Some(res)
}

fn array_literal(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    start: Provenance,
    can_be_struct: bool,
) -> Result<AstNode, ParseError> {
    let expr = expression(source, context, start, can_be_struct)?;
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
                let expr = expression(source, context, end, can_be_struct)?;
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
        _ => Err(ParseError::UnexpectedToken(
            Box::new(separator),
            "expected comma, semicolon or ]",
        )),
    }
}

fn branch(
    source: &mut TokenIter,
    context: &mut Vec<AstNode>,
    token: TokenValue,
    start: Provenance,
) -> Result<AstNode, ParseError> {
    let predicate = expression(source, context, start, false)?;
    assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenBracket,
        predicate.end,
        "expected { after predicate",
    )?;
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
        other => Err(ParseError::UnexpectedToken(Box::new(other), reason)),
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
                return Err(ParseError::UnexpectedToken(
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
        other => Err(ParseError::UnexpectedToken(Box::new(other), reason)),
    }
}

fn already_peeked_token(source: &mut TokenIter) -> Result<Token, ParseError> {
    Ok(source.next().expect("already peeked that token exists")?)
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
        Err(ParseError::UnexpectedToken(
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
        _ => Err(ParseError::UnexpectedToken(Box::new(next), reason)),
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
                    BinExpr(BinOp::Assignment, _, _),
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
