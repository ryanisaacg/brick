use std::{collections::HashMap, iter::Peekable};

use thiserror::Error;

use crate::{
    arena::ArenaNode,
    id::{IDMap, ID},
    provenance::{SourceMarker, SourceRange},
    tokenizer::{LexError, Token, TokenValue},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct NameAndType {
    pub name: String,
    pub type_: ID,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AstNode {
    pub value: AstNodeValue,
    pub provenance: SourceRange,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FunctionDeclarationValue {
    pub name: String,
    pub params: Vec<NameAndType>,
    pub returns: Option<ID>,
    pub body: ID,
    /**
     * Whether this function is available to extern. Distinct from declaring an extern function
     * is available in the environment
     */
    pub is_extern: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ExternFunctionBindingValue {
    pub name: String,
    pub params: Vec<NameAndType>,
    pub returns: Option<ID>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclarationValue {
    pub name: String,
    pub fields: Vec<NameAndType>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AstNodeValue {
    // Statements
    FunctionDeclaration(FunctionDeclarationValue),
    ExternFunctionBinding(ExternFunctionBindingValue),
    StructDeclaration(StructDeclarationValue),
    Declaration(String, ID),
    Import(String),
    Return(ID),

    Name(String),

    // Expressions
    Int(i64),
    Float(f64),
    Bool(bool),
    BinExpr(BinOp, ID, ID),
    If(ID, ID),
    While(ID, ID),
    Call(ID, Vec<ID>),
    TakeUnique(ID),
    TakeShared(ID),
    StructLiteral {
        name: String,
        fields: HashMap<String, ID>,
    },
    ArrayLiteral(Vec<ID>),
    ArrayLiteralLength(ID, u64),
    Block(Vec<ID>),

    // Types
    UniqueType(ID),
    SharedType(ID),
    ArrayType(ID),
}

impl ArenaNode for AstNode {
    fn write_children(&self, children: &mut Vec<ID>) {
        use AstNodeValue::*;

        match &self.value {
            Declaration(_, child)
            | AstNodeValue::FunctionDeclaration(FunctionDeclarationValue { body: child, .. })
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
    UnexpectedEndOfInput(SourceMarker, &'static str),
    #[error("expected type for parameter at {0}")]
    MissingTypeForParam(SourceMarker),
    #[error("token error: {0}")]
    TokenError(#[from] LexError),
    #[error("unexpected top-level statement at {0}")]
    UnexpectedTopLevelStatement(SourceRange),
}

type TokenIterInner<'a> = &'a mut dyn Iterator<Item = Result<Token, LexError>>;
type TokenIter<'a> = Peekable<TokenIterInner<'a>>;

pub struct ParsedSourceFile {
    pub nodes: IDMap<AstNode>,
    pub imports: Vec<ID>,
    pub functions: Vec<ID>,
    pub types: Vec<ID>,
}

pub fn parse(
    mut source: impl Iterator<Item = Result<Token, LexError>>,
) -> Result<ParsedSourceFile, ParseError> {
    let mut source = (&mut source as TokenIterInner<'_>).peekable();
    let mut context = IDMap::new();

    let mut imports = Vec::new();
    let mut functions = Vec::new();
    let mut types = Vec::new();

    while let Some(lexeme) = peek_token_optional(&mut source)? {
        let cursor = lexeme.range.start();
        let statement = statement(&mut source, &mut context, cursor)?;
        let id = add_node(&mut context, statement);

        let statement = &context[&id];
        match statement.value {
            AstNodeValue::Import(_) => imports.push(id),
            AstNodeValue::StructDeclaration(_) => types.push(id),
            AstNodeValue::FunctionDeclaration(_) | AstNodeValue::ExternFunctionBinding(_) => {
                functions.push(id);
            }
            _ => {
                return Err(ParseError::UnexpectedTopLevelStatement(
                    statement.provenance.clone(),
                ));
            }
        }
    }

    Ok(ParsedSourceFile {
        nodes: context,
        imports,
        functions,
        types,
    })
}

fn add_node(context: &mut IDMap<AstNode>, node: AstNode) -> ID {
    let id = ID::new();
    context.insert(id, node);

    id
}

fn statement(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    cursor: SourceMarker,
) -> Result<AstNode, ParseError> {
    let statement = match peek_token(source, cursor, "expected let, fn, or expression")?.value {
        TokenValue::Let
        | TokenValue::Import
        | TokenValue::Function
        | TokenValue::Extern
        | TokenValue::Struct
        | TokenValue::Return => {
            let Token { range, value, .. } = already_peeked_token(source)?;
            let cursor = range.end();
            match value {
                TokenValue::Let => variable_declaration(source, context, cursor)?,
                TokenValue::Import => import_declaration(source, cursor)?,
                TokenValue::Extern => extern_function_declaration(source, context, cursor)?,
                TokenValue::Function => function_declaration(source, context, cursor)?,
                TokenValue::Struct => struct_declaration(source, context, cursor)?,
                TokenValue::Return => return_declaration(source, context, cursor)?,
                _ => unreachable!(),
            }
        }
        _ => expression(source, context, cursor, true)?,
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
    context: &mut IDMap<AstNode>,
    cursor: SourceMarker,
) -> Result<AstNode, ParseError> {
    let value = expression(source, context, cursor, true)?;
    let provenance = value.provenance.clone();
    let value = add_node(context, value);

    Ok(AstNode {
        value: AstNodeValue::Return(value),
        provenance,
    })
}

fn struct_declaration(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    cursor: SourceMarker,
) -> Result<AstNode, ParseError> {
    let (name, mut provenance) = word(source, cursor, "expected name after 'struct'")?;
    let mut cursor = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenBracket,
        provenance.end(),
        "expected open parenthesis to start parameters",
    )?
    .range
    .end();

    let mut fields = Vec::new();
    let mut closed = peek_for_closed(
        source,
        TokenValue::CloseBracket,
        cursor,
        "expected either fields or close bracket",
    )?;

    while !closed {
        let (name, range, type_hint) =
            name_and_type_hint(source, context, cursor, "expected parameter")?;
        cursor = range.end();
        let kind = type_hint.ok_or(ParseError::MissingTypeForParam(cursor))?;
        let kind = add_node(context, kind);
        fields.push(NameAndType { name, type_: kind });

        let (should_end, range) = comma_or_end_list(
            source,
            TokenValue::CloseBracket,
            cursor,
            "expected either fields or close bracket",
        )?;
        closed = should_end;
        cursor = range.end();
    }
    provenance.set_end(cursor);

    Ok(AstNode {
        value: AstNodeValue::StructDeclaration(StructDeclarationValue { name, fields }),
        provenance,
    })
}

fn extern_function_declaration(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    start: SourceMarker,
) -> Result<AstNode, ParseError> {
    let mut provenance = assert_next_lexeme_eq(
        source.next(),
        TokenValue::Function,
        start,
        "expected 'fn' after 'extern'",
    )?
    .range;
    let FunctionHeader {
        name,
        params,
        returns,
        end,
    } = function_header(source, context, provenance.end())?;
    provenance.set_end(end);

    let next = token(source, end, "expected ; or { after extern fn decl")?;
    let (value, end) = match &next.value {
        TokenValue::Semicolon => (
            AstNodeValue::ExternFunctionBinding(ExternFunctionBindingValue {
                name,
                params,
                returns,
            }),
            next.range.end(),
        ),
        TokenValue::OpenBracket => {
            let body = block(source, context, next.range.end())?;
            let end = body.provenance.end();
            (
                AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                    name,
                    params,
                    returns,
                    body: add_node(context, body),
                    is_extern: true,
                }),
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
    provenance.set_end(end);

    Ok(AstNode { value, provenance })
}

fn function_declaration(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    start: SourceMarker,
) -> Result<AstNode, ParseError> {
    let FunctionHeader {
        name,
        params,
        returns,
        end,
    } = function_header(source, context, start)?;
    let mut provenance = SourceRange::new(start, end);
    let next_token = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenBracket,
        start,
        "expected { after function declaration",
    )?;
    let body = block(source, context, next_token.range.end())?;
    provenance.set_end(body.provenance.end());

    Ok(AstNode {
        value: AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
            name,
            params,
            returns,
            body: add_node(context, body),
            is_extern: false,
        }),
        provenance,
    })
}

struct FunctionHeader {
    name: String,
    params: Vec<NameAndType>,
    returns: Option<ID>,
    end: SourceMarker,
}

fn function_header(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    cursor: SourceMarker,
) -> Result<FunctionHeader, ParseError> {
    let (name, provenance) = word(source, cursor, "expected name after 'fn'")?;

    let next_token = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenParen,
        provenance.end(),
        "expected open parenthesis to start parameters",
    )?;
    let mut cursor = next_token.range.end();
    let mut params = Vec::new();
    loop {
        let token = peek_token(source, cursor, "expected either parameters or close paren")?;
        cursor = token.range.start();
        match token.value {
            TokenValue::CloseParen => break,
            TokenValue::Comma => {
                source.next();
            }
            _ => {
                let (name, range, type_hint) =
                    name_and_type_hint(source, context, cursor, "expected parameter")?;
                cursor = range.end();
                let kind = type_hint.ok_or(ParseError::MissingTypeForParam(cursor))?;
                let kind = add_node(context, kind);
                params.push(NameAndType { name, type_: kind });
            }
        }
    }
    let next_token = assert_next_lexeme_eq(
        source.next(),
        TokenValue::CloseParen,
        cursor,
        "expected closing parenthesis to end parameters",
    )?;
    cursor = next_token.range.end();
    let returns = if let Token {
        value: TokenValue::Colon,
        range,
    } = peek_token(source, cursor, "expected body after function declaration")?
    {
        let start = range.start();
        cursor = range.end();
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
        end: cursor,
    })
}

fn import_declaration(source: &mut TokenIter, cursor: SourceMarker) -> Result<AstNode, ParseError> {
    let (name, provenance) = word(source, cursor, "expected word after 'import'")?;

    Ok(AstNode {
        value: AstNodeValue::Import(name),
        provenance: SourceRange::new(cursor, provenance.end()),
    })
}

fn variable_declaration(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    cursor: SourceMarker,
) -> Result<AstNode, ParseError> {
    // TODO: store type hint in declarations
    let (name, mut provenance, _) = name_and_type_hint(
        source,
        context,
        cursor,
        "expected word after 'let' in declaration",
    )?;
    let cursor = assert_next_lexeme_eq(
        source.next(),
        TokenValue::Equals,
        provenance.end(),
        "expected = after let binding target",
    )?
    .range
    .end();
    let value = expression(source, context, cursor, true)?;
    provenance.set_end(value.provenance.end());

    Ok(AstNode {
        value: AstNodeValue::Declaration(name, add_node(context, value)),
        provenance,
    })
}

fn name_and_type_hint(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    cursor: SourceMarker,
    reason: &'static str,
) -> Result<(String, SourceRange, Option<AstNode>), ParseError> {
    let (name, mut provenance) = word(source, cursor, reason)?;

    let type_hint = if let Some(Ok(Token {
        value: TokenValue::Colon,
        range: token_range,
        ..
    })) = source.peek()
    {
        let cursor = token_range.end();
        source.next();
        let type_hint = type_expression(source, context, cursor)?;
        provenance.set_end(type_hint.provenance.end());
        Some(type_hint)
    } else {
        None
    };

    Ok((name, provenance, type_hint))
}

fn type_expression(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    cursor: SourceMarker,
) -> Result<AstNode, ParseError> {
    let next = token(source, cursor, "expected type")?;
    match next.value {
        ptr @ (TokenValue::Unique | TokenValue::Shared) => {
            let subtype = type_expression(source, context, next.range.end())?;
            let end = subtype.provenance.end();
            let subtype = add_node(context, subtype);
            Ok(AstNode {
                value: match ptr {
                    TokenValue::Unique => AstNodeValue::UniqueType(subtype),
                    TokenValue::Shared => AstNodeValue::SharedType(subtype),
                    _ => unreachable!(),
                },
                provenance: SourceRange::new(next.range.start(), end),
            })
        }
        TokenValue::OpenSquare => {
            let subtype = type_expression(source, context, next.range.end())?;
            let end = subtype.provenance.end();
            assert_next_lexeme_eq(
                source.next(),
                TokenValue::CloseSquare,
                end,
                "expected ] after array type",
            )?;
            let subtype = add_node(context, subtype);
            Ok(AstNode {
                value: AstNodeValue::ArrayType(subtype),
                provenance: SourceRange::new(next.range.start(), end),
            })
        }
        TokenValue::Word(name) => Ok(AstNode {
            value: AstNodeValue::Name(name),
            provenance: next.range,
        }),
        _ => Err(ParseError::UnexpectedToken(
            Box::new(next),
            "expected either 'unique' or type name",
        )),
    }
}

fn expression(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    provenance: SourceMarker,
    can_be_struct: bool, // TODO: refactor grammar?
) -> Result<AstNode, ParseError> {
    expression_pratt(source, context, provenance, 0, can_be_struct)
}

// Pratt parser based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn expression_pratt(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    start: SourceMarker,
    min_binding: u8,
    can_be_struct: bool,
) -> Result<AstNode, ParseError> {
    let Token { value, range } = token(source, start, "expected expression")?;
    let cursor = range.end();
    let mut left = match value {
        // TODO: should this be treated as a unary operator instead?
        TokenValue::Minus => {
            let (int, range) = integer(source, cursor, "expected digit after negative sign")?;
            try_decimal(source, -(int as i64), range)?
        }
        TokenValue::OpenParen => {
            let left = expression_pratt(source, context, cursor, 0, can_be_struct)?;
            assert_next_lexeme_eq(
                source.next(),
                TokenValue::CloseParen,
                left.provenance.end(),
                "expected ) to match (",
            )?;

            left
        }
        TokenValue::OpenSquare => array_literal(source, context, cursor, can_be_struct)?,
        token @ (TokenValue::If | TokenValue::While) => if_or_while(source, context, token, start)?,
        TokenValue::OpenBracket => block(source, context, start)?,
        // Atoms
        TokenValue::True => AstNode {
            value: AstNodeValue::Bool(true),
            provenance: SourceRange::new(start, cursor),
        },
        TokenValue::False => AstNode {
            value: AstNodeValue::Bool(false),
            provenance: SourceRange::new(start, cursor),
        },
        TokenValue::Word(word) => AstNode {
            value: AstNodeValue::Name(word),
            provenance: SourceRange::new(start, cursor),
        },
        TokenValue::Int(int) => try_decimal(source, int as i64, SourceRange::new(start, cursor))?,
        // Prefix operator
        value => {
            let Some(((), right_binding)) = prefix_binding_power(&value) else {
                return Err(ParseError::UnexpectedToken(
                    Box::new(Token { value, range: SourceRange::new(start, cursor)}),
                    "expected an expression",
                ))
            };
            let right = expression_pratt(source, context, start, right_binding, can_be_struct)?;
            let end = right.provenance.end();
            let right = add_node(context, right);
            AstNode {
                value: match value {
                    TokenValue::Shared => AstNodeValue::TakeShared(right),
                    TokenValue::Unique => AstNodeValue::TakeUnique(right),
                    other => unreachable!("prefix operator {:?}", other),
                },
                provenance: SourceRange::new(start, end),
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
            let Token { value, range } = already_peeked_token(source)?;

            match value {
                TokenValue::OpenParen => {
                    let mut end = range.end();
                    let mut arguments = Vec::new();

                    let mut closed = peek_for_closed(
                        source,
                        TokenValue::CloseParen,
                        end,
                        "expected ) or next argument",
                    )?;

                    while !closed {
                        let argument = expression(source, context, end, can_be_struct)?;
                        end = argument.provenance.end();
                        arguments.push(add_node(context, argument));

                        let (should_break, new_end) = comma_or_end_list(
                            source,
                            TokenValue::CloseParen,
                            end,
                            "expected comma or ) to end function call",
                        )?;
                        end = new_end.end();
                        closed = should_break;
                    }

                    left = AstNode {
                        value: AstNodeValue::Call(add_node(context, left), arguments),
                        provenance: SourceRange::new(start, end),
                    };
                }
                TokenValue::OpenSquare => {
                    let index = expression(source, context, range.end(), can_be_struct)?;
                    let Token { range, .. } = assert_next_lexeme_eq(
                        source.next(),
                        TokenValue::CloseSquare,
                        index.provenance.end(),
                        "expected ] to follow array index",
                    )?;
                    left = AstNode {
                        value: AstNodeValue::BinExpr(
                            BinOp::Index,
                            add_node(context, left),
                            add_node(context, index),
                        ),
                        provenance: SourceRange::new(start, range.end()),
                    };
                }
                TokenValue::OpenBracket if can_be_struct => {
                    let mut end = range.end();
                    let mut fields = HashMap::new();

                    loop {
                        if TokenValue::CloseBracket
                            == peek_token(source, end, "expected } or next field")?.value
                        {
                            source.next();
                            break;
                        }

                        let (field, field_range) =
                            word(source, end, "expected field in struct literal")?;
                        if let lex @ (TokenValue::Comma | TokenValue::CloseBracket) =
                            &peek_token(source, end, "expected comma or } to end struct literal")?
                                .value
                        {
                            let lex = lex.clone();
                            source.next();
                            let argument = AstNode {
                                value: AstNodeValue::Name(field.clone()),
                                provenance: field_range,
                            };
                            fields.insert(field, add_node(context, argument));
                            if lex == TokenValue::CloseBracket {
                                break;
                            }
                        } else {
                            let token = assert_next_lexeme_eq(
                                source.next(),
                                TokenValue::Colon,
                                end,
                                "expected colon after field name",
                            )?;
                            let cursor = token.range.end();
                            let argument = expression(source, context, cursor, can_be_struct)?;
                            end = argument.provenance.end();
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
                        provenance: SourceRange::new(start, end),
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
                range: current,
                ..
            } = already_peeked_token(source)?;

            let right =
                expression_pratt(source, context, current.end(), right_binding, can_be_struct)?;

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

            let start = left.provenance.start();
            let end = right.provenance.end();
            left = AstNode {
                value: AstNodeValue::BinExpr(
                    bin_op,
                    add_node(context, left),
                    add_node(context, right),
                ),
                provenance: SourceRange::new(start, end),
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
    context: &mut IDMap<AstNode>,
    start: SourceMarker,
    can_be_struct: bool,
) -> Result<AstNode, ParseError> {
    let expr = expression(source, context, start, can_be_struct)?;
    let separator = token(source, start, "expected comma, semicolon or ]")?;
    match separator.value {
        TokenValue::Comma => {
            let mut end = separator.range.end();
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
                end = peeked.range.end();
                let expr = expression(source, context, end, can_be_struct)?;
                end = expr.provenance.end();
                children.push(add_node(context, expr));

                let (should_break, new_end) = comma_or_end_list(
                    source,
                    TokenValue::CloseSquare,
                    end,
                    "expected comma or ] to end array literal",
                )?;
                end = new_end.end();
                closed = should_break;
            }
            Ok(AstNode {
                value: AstNodeValue::ArrayLiteral(children),
                provenance: SourceRange::new(start, end),
            })
        }
        TokenValue::Semicolon => {
            let (length, range) =
                integer(source, start, "expected number after ; in array literal")?;
            let close = assert_next_lexeme_eq(
                source.next(),
                TokenValue::CloseSquare,
                range.end(),
                "expected ] after array length in array literal",
            )?;
            let expr = add_node(context, expr);
            Ok(AstNode {
                value: AstNodeValue::ArrayLiteralLength(expr, length),
                provenance: SourceRange::new(start, close.range.end()),
            })
        }
        TokenValue::CloseSquare => {
            let expr = add_node(context, expr);
            Ok(AstNode {
                value: AstNodeValue::ArrayLiteral(vec![expr]),
                provenance: SourceRange::new(start, separator.range.end()),
            })
        }
        _ => Err(ParseError::UnexpectedToken(
            Box::new(separator),
            "expected comma, semicolon or ]",
        )),
    }
}

fn if_or_while(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    if_or_while: TokenValue,
    cursor: SourceMarker,
) -> Result<AstNode, ParseError> {
    let predicate = expression(source, context, cursor, false)?;
    let token = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenBracket,
        predicate.provenance.end(),
        "expected { after predicate",
    )?;
    let cursor = token.range.end();
    let block = block(source, context, cursor)?;

    let predicate_ptr = add_node(context, predicate);
    let provenance = SourceRange::new(cursor, block.provenance.end());
    let block_ptr = add_node(context, block);

    Ok(AstNode {
        value: if if_or_while == TokenValue::If {
            AstNodeValue::If(predicate_ptr, block_ptr)
        } else {
            AstNodeValue::While(predicate_ptr, block_ptr)
        },
        provenance,
    })
}

fn block(
    source: &mut TokenIter,
    context: &mut IDMap<AstNode>,
    cursor: SourceMarker,
) -> Result<AstNode, ParseError> {
    let mut statements = Vec::new();
    let mut provenance = SourceRange::new(cursor, cursor);
    loop {
        match peek_token(source, cursor, "expected statement or close bracket")? {
            Token {
                value: TokenValue::CloseBracket,
                range,
                ..
            } => {
                provenance.set_end(range.end());
                source.next();
                return Ok(AstNode {
                    value: AstNodeValue::Block(statements),
                    provenance,
                });
            }
            _ => {
                let statement = statement(source, context, provenance.end())?;
                provenance.set_end(statement.provenance.end());
                statements.push(add_node(context, statement));
            }
        }
    }
}

fn integer(
    source: &mut TokenIter,
    cursor: SourceMarker,
    reason: &'static str,
) -> Result<(u64, SourceRange), ParseError> {
    match token(source, cursor, reason)? {
        Token {
            value: TokenValue::Int(int),
            range,
        } => Ok((int, range)),
        other => Err(ParseError::UnexpectedToken(Box::new(other), reason)),
    }
}

fn try_decimal(
    source: &mut TokenIter,
    num: i64,
    range: SourceRange,
) -> Result<AstNode, ParseError> {
    // TODO: handle overflow
    if let Some(Token {
        value: TokenValue::Period,
        ..
    }) = peek_token_optional(source)?
    {
        source.next();
        let (decimal, end) = match source.next().ok_or(ParseError::UnexpectedEndOfInput(
            range.start(),
            "expected digit after decimal point",
        ))?? {
            Token {
                value: TokenValue::Int(value),
                range,
                ..
            } => (value as f64, range.end()),
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
        let mut provenance = range.clone();
        provenance.set_end(end);
        Ok(AstNode {
            value: AstNodeValue::Float(num),
            provenance,
        })
    } else {
        Ok(AstNode {
            value: AstNodeValue::Int(num),
            provenance: range,
        })
    }
}

fn token(
    source: &mut TokenIter,
    cursor: SourceMarker,
    reason: &'static str,
) -> Result<Token, ParseError> {
    let lex = source
        .next()
        .ok_or(ParseError::UnexpectedEndOfInput(cursor, reason))??;
    Ok(lex)
}

fn word(
    source: &mut TokenIter,
    cursor: SourceMarker,
    reason: &'static str,
) -> Result<(String, SourceRange), ParseError> {
    match token(source, cursor, reason)? {
        Token {
            value: TokenValue::Word(name),
            range,
        } => Ok((name, range)),
        other => Err(ParseError::UnexpectedToken(Box::new(other), reason)),
    }
}

fn already_peeked_token(source: &mut TokenIter) -> Result<Token, ParseError> {
    Ok(source.next().expect("already peeked that token exists")?)
}

fn peek_token<'a>(
    source: &'a mut TokenIter,
    cursor: SourceMarker,
    reason: &'static str,
) -> Result<&'a Token, ParseError> {
    Ok(source
        .peek()
        .ok_or(ParseError::UnexpectedEndOfInput(cursor, reason))?
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
    provenance: SourceMarker,
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
    start: SourceMarker,
    reason: &'static str,
) -> Result<(bool, SourceRange), ParseError> {
    let next = token(source, start, reason)?;
    match next.value {
        TokenValue::Comma => match peek_token_optional(source)? {
            Some(token) if token.value == list_end => {
                let range = token.range.clone();
                source.next();
                Ok((true, range))
            }
            _ => Ok((false, next.range)),
        },
        other if other == list_end => Ok((true, next.range)),
        _ => Err(ParseError::UnexpectedToken(Box::new(next), reason)),
    }
}

fn peek_for_closed(
    source: &mut TokenIter,
    list_end: TokenValue,
    provenance: SourceMarker,
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
        let provenance = SourceMarker::new("test", "test", 0, 0);
        tokens.iter().map(move |token| {
            Ok(Token {
                value: token.clone(),
                range: SourceRange::new(provenance, provenance),
            })
        })
    }

    // TODO: take ParsedSourceModule for these tests rather than the old raw AST

    #[test]
    fn adding() {
        let ParsedSourceFile { nodes, .. } = parse(tokens(&[
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
        let lines = nodes
            .iter()
            .map(|(_id, statement)| {
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
                    If(..),
                    Bool(true),
                    Block(_),
                    BinExpr(BinOp::Assignment, _, _),
                    Name(_),
                    Int(3),
                ],
                &[Name(_),],
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
                FunctionDeclaration(FunctionDeclarationValue {
                    returns: Some(_),
                    ..
                }),
                Block(_),
                BinExpr(BinOp::Add, _, _),
                Int(3),
                Int(5),
            ],]
        );
    }
}
