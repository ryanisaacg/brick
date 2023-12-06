use std::{collections::HashMap, iter::Peekable};

use thiserror::Error;
use typed_arena::Arena;

use crate::{
    arena::ArenaNode,
    id::ID,
    provenance::{SourceMarker, SourceRange},
    tokenizer::{LexError, Token, TokenValue},
};

#[derive(Clone, Debug, PartialEq)]
pub struct NameAndType<'a> {
    pub id: ID,
    pub name: String,
    pub type_: &'a AstNode<'a>,
}

#[derive(Debug, PartialEq)]
pub struct AstNode<'parse> {
    pub id: ID,
    pub value: AstNodeValue<'parse>,
    pub provenance: SourceRange,
}

impl<'a> AstNode<'a> {
    pub fn new(value: AstNodeValue<'a>, provenance: SourceRange) -> AstNode<'a> {
        AstNode {
            id: ID::new(),
            value,
            provenance,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclarationValue<'a> {
    pub name: String,
    pub params: Vec<NameAndType<'a>>,
    pub returns: Option<&'a mut AstNode<'a>>,
    pub body: &'a mut AstNode<'a>,
    /**
     * Whether this function is available to extern. Distinct from declaring an extern function
     * is available in the environment
     */
    pub is_extern: bool,
}

#[derive(Debug, PartialEq)]
pub struct FunctionHeaderValue<'a> {
    pub name: String,
    pub params: Vec<NameAndType<'a>>,
    pub returns: Option<&'a mut AstNode<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct StructDeclarationValue<'a> {
    pub name: String,
    pub fields: Vec<NameAndType<'a>>,
    pub associated_functions: Vec<AstNode<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct UnionDeclarationValue<'a> {
    pub name: String,
    pub variants: Vec<NameAndType<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct InterfaceDeclarationValue<'a> {
    pub name: String,
    pub fields: Vec<NameAndType<'a>>,
    pub associated_functions: Vec<AstNode<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct IfDeclaration<'a> {
    pub condition: &'a mut AstNode<'a>,
    pub if_branch: &'a mut AstNode<'a>,
    pub else_branch: Option<&'a mut AstNode<'a>>,
}

#[derive(Debug, PartialEq)]
pub enum AstNodeValue<'a> {
    // Statements
    FunctionDeclaration(FunctionDeclarationValue<'a>),
    ExternFunctionBinding(FunctionHeaderValue<'a>),
    StructDeclaration(StructDeclarationValue<'a>),
    UnionDeclaration(UnionDeclarationValue<'a>),
    InterfaceDeclaration(InterfaceDeclarationValue<'a>),
    RequiredFunction(FunctionHeaderValue<'a>),
    Declaration(String, &'a mut AstNode<'a>),
    Import(String),
    Return(&'a mut AstNode<'a>),
    // Any non-specific expression that ends in ; is a statement
    Statement(&'a mut AstNode<'a>),

    Name(String),

    // Expressions
    Int(i64),
    Float(f64),
    Bool(bool),
    CharLiteral(char),
    StringLiteral(String),
    Null,
    BinExpr(BinOp, &'a mut AstNode<'a>, &'a mut AstNode<'a>),
    If(IfDeclaration<'a>),
    While(&'a mut AstNode<'a>, &'a mut AstNode<'a>),
    Call(&'a mut AstNode<'a>, Vec<AstNode<'a>>),
    TakeUnique(&'a mut AstNode<'a>),
    TakeShared(&'a mut AstNode<'a>),
    StructLiteral {
        name: String,
        fields: HashMap<String, AstNode<'a>>,
    },
    DictLiteral(Vec<(AstNode<'a>, AstNode<'a>)>),
    ArrayLiteral(Vec<AstNode<'a>>),
    ArrayLiteralLength(&'a mut AstNode<'a>, u64),
    Block(Vec<AstNode<'a>>),

    // Types
    UniqueType(&'a mut AstNode<'a>),
    SharedType(&'a mut AstNode<'a>),
    ArrayType(&'a mut AstNode<'a>),
    NullableType(&'a mut AstNode<'a>),
}

impl<'a> ArenaNode<'a> for AstNode<'a> {
    fn write_children(&'a self, children: &mut Vec<&'a Self>) {
        use AstNodeValue::*;

        match &self.value {
            Declaration(_, child)
            | TakeShared(child)
            | TakeUnique(child)
            | ArrayLiteralLength(child, _)
            | UniqueType(child)
            | SharedType(child)
            | NullableType(child)
            | Return(child)
            | Statement(child)
            | ArrayType(child) => {
                children.push(*child);
            }
            BinExpr(_, left, right) | While(left, right) => {
                children.push(*right);
                children.push(*left);
            }
            If(IfDeclaration {
                condition,
                if_branch,
                else_branch,
            }) => {
                children.push(*condition);
                children.push(*if_branch);
                if let Some(else_branch) = else_branch {
                    children.push(*else_branch);
                }
            }
            ArrayLiteral(values) | Block(values) => {
                for value in values.iter() {
                    children.push(value);
                }
            }
            StructLiteral { fields, .. } => {
                for expression in fields.values() {
                    children.push(expression);
                }
            }
            DictLiteral(entries) => {
                for (left, right) in entries.iter() {
                    children.push(left);
                    children.push(right);
                }
            }
            Call(function, parameters) => {
                children.push(*function);
                for expression in parameters.iter() {
                    children.push(expression);
                }
            }
            AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                body,
                params,
                returns,
                ..
            }) => {
                children.push(*body);
                for param in params.iter() {
                    children.push(param.type_);
                }
                if let Some(returns) = returns {
                    children.push(*returns);
                }
            }
            ExternFunctionBinding(FunctionHeaderValue {
                params, returns, ..
            })
            | RequiredFunction(FunctionHeaderValue {
                params, returns, ..
            }) => {
                for param in params.iter() {
                    children.push(param.type_);
                }
                if let Some(returns) = returns {
                    children.push(*returns);
                }
            }
            StructDeclaration(StructDeclarationValue { fields, .. })
            | InterfaceDeclaration(InterfaceDeclarationValue { fields, .. })
            | UnionDeclaration(UnionDeclarationValue {
                variants: fields, ..
            }) => {
                for field in fields.iter() {
                    children.push(field.type_);
                }
            }
            Name(_) | Import(_) | Int(_) | Float(_) | Bool(_) | Null | CharLiteral(_)
            | StringLiteral(_) => {}
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
    LessEqualThan,
    GreaterEqualThan,
    EqualTo,
    NotEquals,
    Index,

    Assignment,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,
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

pub struct ParsedModule<'a> {
    pub arena: Arena<AstNode<'a>>,
    pub top_level_nodes: Vec<AstNode<'a>>,
}

pub fn parse<'a>(
    arena: &'a Arena<AstNode<'a>>,
    mut source: impl Iterator<Item = Result<Token, LexError>>,
) -> Result<Vec<AstNode<'a>>, ParseError> {
    let mut source = (&mut source as TokenIterInner<'_>).peekable();

    let mut top_level_nodes = Vec::new();

    while let Some(lexeme) = peek_token_optional(&mut source)? {
        let cursor = lexeme.range.start();
        let statement = statement(&mut source, &arena, cursor)?;

        top_level_nodes.push(statement);
    }

    Ok(top_level_nodes)
}

fn add_node<'a>(context: &'a Arena<AstNode<'a>>, node: AstNode<'a>) -> &'a mut AstNode<'a> {
    context.alloc(node)
}

fn statement<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    Ok(
        match peek_token(source, cursor, "expected let, fn, or expression")?.value {
            TokenValue::Let
            | TokenValue::Import
            | TokenValue::Function
            | TokenValue::Extern
            | TokenValue::Struct
            | TokenValue::Union
            | TokenValue::Interface
            | TokenValue::Return => {
                let Token { range, value, .. } = already_peeked_token(source)?;
                let cursor = range.end();
                match value {
                    TokenValue::Let => {
                        let statement = variable_declaration(source, context, cursor)?;
                        assert_next_lexeme_eq(
                            source.next(),
                            TokenValue::Semicolon,
                            statement.provenance.end(),
                            "expected ; after 'let' statement",
                        )?;

                        statement
                    }
                    TokenValue::Import => {
                        let statement = import_declaration(source, cursor)?;
                        if let Some(Token {
                            value: TokenValue::Semicolon,
                            ..
                        }) = peek_token_optional(source)?
                        {
                            source.next();
                        }

                        statement
                    }
                    TokenValue::Extern => extern_function_declaration(source, context, cursor)?,
                    TokenValue::Function => function_declaration(source, context, cursor)?,
                    TokenValue::Struct => struct_declaration(source, context, cursor)?,
                    TokenValue::Union => union_declaration(source, context, cursor)?,
                    TokenValue::Interface => interface_declaration(source, context, cursor)?,
                    TokenValue::Return => {
                        let statement = return_declaration(source, context, cursor)?;
                        assert_next_lexeme_eq(
                            source.next(),
                            TokenValue::Semicolon,
                            statement.provenance.end(),
                            "expected ; after 'import' statement",
                        )?;

                        statement
                    }
                    _ => unreachable!(),
                }
            }
            _ => {
                let expr = expression(source, context, cursor, true)?;
                if let Some(Token {
                    value: TokenValue::Semicolon,
                    ..
                }) = peek_token_optional(source)?
                {
                    let token = already_peeked_token(source)?;
                    let provenance = SourceRange::new(expr.provenance.start(), token.range.end());
                    let expr = add_node(context, expr);
                    AstNode::new(AstNodeValue::Statement(expr), provenance)
                } else {
                    expr
                }
            }
        },
    )
}

fn return_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let value = expression(source, context, cursor, true)?;
    let provenance = value.provenance.clone();
    let value = add_node(context, value);

    Ok(AstNode::new(AstNodeValue::Return(value), provenance))
}

fn struct_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let (name, provenance) = word(source, cursor, "expected name after 'struct'")?;
    let (end, fields, associated_functions) =
        interface_or_struct_body(source, context, provenance.end(), false)?;

    Ok(AstNode::new(
        AstNodeValue::StructDeclaration(StructDeclarationValue {
            name,
            fields,
            associated_functions,
        }),
        SourceRange::new(cursor, end),
    ))
}

fn interface_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let (name, provenance) = word(source, cursor, "expected name after 'interface'")?;
    let (end, fields, associated_functions) =
        interface_or_struct_body(source, context, provenance.end(), true)?;

    Ok(AstNode::new(
        AstNodeValue::InterfaceDeclaration(InterfaceDeclarationValue {
            name,
            fields,
            associated_functions,
        }),
        SourceRange::new(cursor, end),
    ))
}

fn interface_or_struct_body<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
    is_interface: bool,
) -> Result<(SourceMarker, Vec<NameAndType<'a>>, Vec<AstNode<'a>>), ParseError> {
    let mut cursor = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenBracket,
        cursor,
        "expected open bracket to start fields",
    )?
    .range
    .end();

    let mut fields = Vec::new();
    let mut associated_functions = Vec::new();

    loop {
        if peek_for_closed(
            source,
            TokenValue::CloseBracket,
            cursor,
            "expected either fields or close bracket",
        )? {
            break;
        }

        if peek_token(source, cursor, "expected associated function, field, or }")?.value
            == TokenValue::Function
        {
            let start = cursor;
            let token = already_peeked_token(source)?;
            cursor = token.range.end();
            let FunctionHeader {
                name,
                params,
                returns,
                end,
            } = function_header(source, context, cursor)?;
            cursor = end;

            let next = peek_token(source, cursor, "expected ',', }, or body")?;
            if is_interface
                && (next.value == TokenValue::Comma || next.value == TokenValue::CloseBracket)
            {
                if next.value == TokenValue::Comma {
                    already_peeked_token(source)?;
                }
                associated_functions.push(AstNode::new(
                    AstNodeValue::RequiredFunction(FunctionHeaderValue {
                        name,
                        params,
                        returns,
                    }),
                    SourceRange::new(start, cursor),
                ));
            } else {
                let token = assert_next_lexeme_eq(
                    source.next(),
                    TokenValue::OpenBracket,
                    cursor,
                    "expected open bracket to start function body",
                )?;
                cursor = token.range.end();
                // TODO: if it's an interface, possible required function
                let body = block(source, context, cursor)?;
                cursor = body.provenance.end();
                associated_functions.push(AstNode::new(
                    AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                        name,
                        params,
                        returns,
                        body: add_node(context, body),
                        is_extern: true,
                    }),
                    SourceRange::new(start, cursor),
                ));
            }
        } else {
            let (name, range, type_hint) =
                name_and_type_hint(source, context, cursor, "expected parameter")?;
            cursor = range.end();
            let kind = type_hint.ok_or(ParseError::MissingTypeForParam(cursor))?;
            let kind = add_node(context, kind);
            fields.push(NameAndType {
                id: ID::new(),
                name,
                type_: kind,
            });

            let (should_end, range) = comma_or_end_list(
                source,
                TokenValue::CloseBracket,
                cursor,
                "expected either fields, comma, or close bracket",
            )?;
            if should_end {
                break;
            }
            cursor = range.end();
        }
    }

    Ok((cursor, fields, associated_functions))
}

fn union_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let (name, mut provenance) = word(source, cursor, "expected name after 'union'")?;
    let mut cursor = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenBracket,
        provenance.end(),
        "expected open bracket to start variants",
    )?
    .range
    .end();

    let mut variants = Vec::new();

    let mut closed = false;
    while !closed {
        let (name, name_range) = word(source, cursor, "expected variant name")?;
        let paren = assert_next_lexeme_eq(
            source.next(),
            TokenValue::OpenParen,
            name_range.end(),
            "expected ( after variant name",
        )?;
        let ty = type_expression(source, context, paren.range.end())?;
        let paren = assert_next_lexeme_eq(
            source.next(),
            TokenValue::CloseParen,
            ty.provenance.end(),
            "expected ) after variant type",
        )?;
        variants.push(NameAndType {
            id: ID::new(),
            name,
            type_: add_node(context, ty),
        });

        let (should_end, range) = comma_or_end_list(
            source,
            TokenValue::CloseBracket,
            paren.range.end(),
            "expected either more variants or close bracket",
        )?;
        closed = should_end;
        cursor = range.end();
    }
    provenance.set_end(cursor);

    Ok(AstNode::new(
        AstNodeValue::UnionDeclaration(UnionDeclarationValue { name, variants }),
        provenance,
    ))
}

fn extern_function_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    start: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
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
            AstNodeValue::ExternFunctionBinding(FunctionHeaderValue {
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

    Ok(AstNode::new(value, provenance))
}

fn function_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    start: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
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

    Ok(AstNode::new(
        AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
            name,
            params,
            returns,
            body: add_node(context, body),
            is_extern: false,
        }),
        provenance,
    ))
}

struct FunctionHeader<'a> {
    name: String,
    params: Vec<NameAndType<'a>>,
    returns: Option<&'a mut AstNode<'a>>,
    end: SourceMarker,
}

fn function_header<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<FunctionHeader<'a>, ParseError> {
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
                params.push(NameAndType {
                    id: ID::new(),
                    name,
                    type_: kind,
                });
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

fn import_declaration<'a>(
    source: &mut TokenIter,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let (name, provenance) = word(source, cursor, "expected word after 'import'")?;

    Ok(AstNode::new(
        AstNodeValue::Import(name),
        SourceRange::new(cursor, provenance.end()),
    ))
}

fn variable_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    // TODO: store type hint in declarations
    let (name, mut provenance, _) = name_and_type_hint(
        source,
        context,
        cursor,
        "expected word after 'let' in declaration",
    )?;
    let cursor = assert_next_lexeme_eq(
        source.next(),
        TokenValue::Assign,
        provenance.end(),
        "expected = after let binding target",
    )?
    .range
    .end();
    let value = expression(source, context, cursor, true)?;
    provenance.set_end(value.provenance.end());

    Ok(AstNode::new(
        AstNodeValue::Declaration(name, add_node(context, value)),
        provenance,
    ))
}

fn name_and_type_hint<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
    reason: &'static str,
) -> Result<(String, SourceRange, Option<AstNode<'a>>), ParseError> {
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

fn type_expression<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let next = token(source, cursor, "expected type")?;
    let node = match next.value {
        ptr @ (TokenValue::Unique | TokenValue::Shared) => {
            let subtype = type_expression(source, context, next.range.end())?;
            let end = subtype.provenance.end();
            let subtype = add_node(context, subtype);
            AstNode::new(
                match ptr {
                    TokenValue::Unique => AstNodeValue::UniqueType(subtype),
                    TokenValue::Shared => AstNodeValue::SharedType(subtype),
                    _ => unreachable!(),
                },
                SourceRange::new(next.range.start(), end),
            )
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
            AstNode::new(
                AstNodeValue::ArrayType(subtype),
                SourceRange::new(next.range.start(), end),
            )
        }
        TokenValue::Word(name) => AstNode::new(AstNodeValue::Name(name), next.range),
        _ => {
            return Err(ParseError::UnexpectedToken(
                Box::new(next),
                "expected either 'unique' or type name",
            ))
        }
    };

    if matches!(
        peek_token_optional(source)?,
        Some(Token {
            value: TokenValue::QuestionMark,
            ..
        })
    ) {
        let question_mark = already_peeked_token(source)?;
        let inner = add_node(context, node);
        let provenance = SourceRange::new(inner.provenance.start(), question_mark.range.end());
        Ok(AstNode::new(AstNodeValue::NullableType(inner), provenance))
    } else {
        Ok(node)
    }
}

fn expression<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    provenance: SourceMarker,
    can_be_struct: bool, // TODO: refactor grammar?
) -> Result<AstNode<'a>, ParseError> {
    expression_pratt(source, context, provenance, 0, can_be_struct)
}

// Pratt parser based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn expression_pratt<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    start: SourceMarker,
    min_binding: u8,
    can_be_struct: bool,
) -> Result<AstNode<'a>, ParseError> {
    let Token { value, range } = token(source, start, "expected expression")?;
    let start = range.start();
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
        TokenValue::Dict => dict_literal(source, context, cursor)?,
        TokenValue::OpenSquare => array_literal(source, context, cursor, can_be_struct)?,
        token @ (TokenValue::If | TokenValue::While) => {
            if_or_while(source, context, token, cursor)?
        }
        TokenValue::OpenBracket => block(source, context, cursor)?,
        // Atoms
        TokenValue::True => AstNode::new(AstNodeValue::Bool(true), range),
        TokenValue::False => AstNode::new(AstNodeValue::Bool(false), range),
        TokenValue::Word(word) => AstNode::new(AstNodeValue::Name(word), range),
        TokenValue::Null => AstNode::new(AstNodeValue::Null, range),
        TokenValue::CharacterLiteral(c) => AstNode::new(AstNodeValue::CharLiteral(c), range),
        TokenValue::StringLiteral(s) => AstNode::new(AstNodeValue::StringLiteral(s), range),
        TokenValue::Int(int) => try_decimal(source, int as i64, range)?,
        // Prefix operator
        value => {
            let Some(((), right_binding)) = prefix_binding_power(&value) else {
                return Err(ParseError::UnexpectedToken(
                    Box::new(Token { value, range }),
                    "expected an expression",
                ));
            };
            let right = expression_pratt(source, context, cursor, right_binding, can_be_struct)?;
            let end = right.provenance.end();
            let right = add_node(context, right);
            AstNode::new(
                match value {
                    TokenValue::Shared => AstNodeValue::TakeShared(right),
                    TokenValue::Unique => AstNodeValue::TakeUnique(right),
                    other => unreachable!("prefix operator {:?}", other),
                },
                SourceRange::new(range.start(), end),
            )
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
                        arguments.push(argument);

                        let (should_break, new_end) = comma_or_end_list(
                            source,
                            TokenValue::CloseParen,
                            end,
                            "expected comma or ) to end function call",
                        )?;
                        end = new_end.end();
                        closed = should_break;
                    }

                    left = AstNode::new(
                        AstNodeValue::Call(add_node(context, left), arguments),
                        SourceRange::new(start, end),
                    );
                }
                TokenValue::OpenSquare => {
                    let index = expression(source, context, range.end(), can_be_struct)?;
                    let Token { range, .. } = assert_next_lexeme_eq(
                        source.next(),
                        TokenValue::CloseSquare,
                        index.provenance.end(),
                        "expected ] to follow array index",
                    )?;
                    left = AstNode::new(
                        AstNodeValue::BinExpr(
                            BinOp::Index,
                            add_node(context, left),
                            add_node(context, index),
                        ),
                        SourceRange::new(start, range.end()),
                    );
                }
                TokenValue::OpenBracket if can_be_struct => {
                    // TODO: extract struct literals into their own method
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
                            let argument =
                                AstNode::new(AstNodeValue::Name(field.clone()), field_range);
                            fields.insert(field, argument);
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
                            fields.insert(field, argument);

                            if let TokenValue::Comma = peek_token(
                                source,
                                end,
                                "expected comma or } to end struct literal",
                            )?
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

                    left = AstNode::new(
                        AstNodeValue::StructLiteral { name, fields },
                        SourceRange::new(start, end),
                    );
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
                TokenValue::Assign => BinOp::Assignment,
                TokenValue::PlusEquals => BinOp::AddAssign,
                TokenValue::MinusEquals => BinOp::SubtractAssign,
                TokenValue::AsteriskEquals => BinOp::MultiplyAssign,
                TokenValue::ForwardSlashEquals => BinOp::DivideAssign,
                TokenValue::LessThan => BinOp::LessThan,
                TokenValue::GreaterThan => BinOp::GreaterThan,
                TokenValue::LessEqualThan => BinOp::LessEqualThan,
                TokenValue::GreaterEqualThan => BinOp::GreaterEqualThan,
                TokenValue::EqualTo => BinOp::EqualTo,
                TokenValue::NotEquals => BinOp::NotEquals,
                TokenValue::Plus => BinOp::Add,
                TokenValue::Minus => BinOp::Subtract,
                TokenValue::Period => BinOp::Dot,
                TokenValue::Asterisk => BinOp::Multiply,
                TokenValue::ForwardSlash => BinOp::Divide,
                token => unreachable!("binary operator {:?}", token),
            };

            let start = left.provenance.start();
            let end = right.provenance.end();
            left = AstNode::new(
                AstNodeValue::BinExpr(bin_op, add_node(context, left), add_node(context, right)),
                SourceRange::new(start, end),
            );

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
const CALL: u8 = FACTOR + 2;
const DOT: u8 = CALL + 1;

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
        TokenValue::Assign
        | TokenValue::PlusEquals
        | TokenValue::MinusEquals
        | TokenValue::AsteriskEquals
        | TokenValue::ForwardSlashEquals => (ASSIGNMENT, ASSIGNMENT - 1),
        TokenValue::LessThan
        | TokenValue::GreaterThan
        | TokenValue::GreaterEqualThan
        | TokenValue::LessEqualThan
        | TokenValue::NotEquals
        | TokenValue::EqualTo => (COMPARE, COMPARE - 1),
        TokenValue::Plus | TokenValue::Minus => (SUM, SUM - 1),
        TokenValue::Asterisk | TokenValue::ForwardSlash => (FACTOR, FACTOR - 1),
        TokenValue::Period => (DOT - 1, DOT),
        _ => return None,
    };
    Some(res)
}

fn array_literal<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    start: SourceMarker,
    can_be_struct: bool,
) -> Result<AstNode<'a>, ParseError> {
    let expr = expression(source, context, start, can_be_struct)?;
    let separator = token(source, start, "expected comma, semicolon or ]")?;
    match separator.value {
        TokenValue::Comma => {
            let mut end = separator.range.end();
            let mut children = vec![expr];

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
                children.push(expr);

                let (should_break, new_end) = comma_or_end_list(
                    source,
                    TokenValue::CloseSquare,
                    end,
                    "expected comma or ] to end array literal",
                )?;
                end = new_end.end();
                closed = should_break;
            }
            Ok(AstNode::new(
                AstNodeValue::ArrayLiteral(children),
                SourceRange::new(start, end),
            ))
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
            Ok(AstNode::new(
                AstNodeValue::ArrayLiteralLength(expr, length),
                SourceRange::new(start, close.range.end()),
            ))
        }
        TokenValue::CloseSquare => Ok(AstNode::new(
            AstNodeValue::ArrayLiteral(vec![expr]),
            SourceRange::new(start, separator.range.end()),
        )),
        _ => Err(ParseError::UnexpectedToken(
            Box::new(separator),
            "expected comma, semicolon or ]",
        )),
    }
}

fn dict_literal<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    start: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let token = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenSquare,
        start,
        "expected [ to start dict literal",
    )?;
    let mut cursor = token.range.end();
    let mut entries = Vec::new();
    loop {
        let key = match peek_token(source, cursor, "expected } or next field")?.value {
            TokenValue::CloseBracket => {
                source.next();
                break;
            }
            TokenValue::OpenSquare => {
                let key = expression(source, context, cursor, true)?;
                let token = assert_next_lexeme_eq(
                    source.next(),
                    TokenValue::CloseSquare,
                    cursor,
                    "Expected ] after key expression",
                )?;
                cursor = token.range.end();

                key
            }
            _ => {
                let (key, key_range) = word(source, cursor, "expected key in dict literal")?;
                cursor = key_range.end();

                if let lex @ (TokenValue::Comma | TokenValue::CloseBracket) =
                    &peek_token(source, cursor, "expected comma or } to end struct literal")?.value
                {
                    let lex = lex.clone();
                    let token = already_peeked_token(source)?;
                    cursor = token.range.end();

                    let value = AstNode::new(AstNodeValue::Name(key.clone()), key_range.clone());
                    let key = AstNode::new(AstNodeValue::StringLiteral(key), key_range);

                    entries.push((key, value));

                    if lex == TokenValue::Comma {
                        continue;
                    } else {
                        break;
                    }
                }

                AstNode::new(AstNodeValue::StringLiteral(key), key_range)
            }
        };
        let token = assert_next_lexeme_eq(
            source.next(),
            TokenValue::Colon,
            cursor,
            "Expected : after key in dict",
        )?;
        cursor = token.range.end();
        let value = expression(source, context, cursor, true)?;
        cursor = value.provenance.end();
        entries.push((key, value));
        let (did_end, range) = comma_or_end_list(
            source,
            TokenValue::CloseSquare,
            cursor,
            "expected , or ] in dict literal",
        )?;
        cursor = range.end();
        if did_end {
            break;
        }
    }

    Ok(AstNode::new(
        AstNodeValue::DictLiteral(entries),
        SourceRange::new(start, cursor),
    ))
}

fn if_or_while<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    if_or_while: TokenValue,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let predicate = expression(source, context, cursor, false)?;
    let token = assert_next_lexeme_eq(
        source.next(),
        TokenValue::OpenBracket,
        predicate.provenance.end(),
        "expected { after predicate",
    )?;
    let cursor = token.range.end();
    let if_block = block(source, context, cursor)?;

    let predicate_ptr = add_node(context, predicate);
    let provenance = SourceRange::new(cursor, if_block.provenance.end());
    let block_ptr = add_node(context, if_block);

    Ok(if if_or_while == TokenValue::If {
        match peek_token_optional(source)? {
            Some(Token {
                value: TokenValue::Word(word),
                ..
            }) if word == "else" => {
                let else_token = already_peeked_token(source)?;
                let token = assert_next_lexeme_eq(
                    source.next(),
                    TokenValue::OpenBracket,
                    else_token.range.end(),
                    "expected { after else",
                )?;
                let else_block = block(source, context, token.range.end())?;
                let provenance = SourceRange::new(cursor, else_block.provenance.end());
                let else_ptr = add_node(context, else_block);
                AstNode::new(
                    AstNodeValue::If(IfDeclaration {
                        condition: predicate_ptr,
                        if_branch: block_ptr,
                        else_branch: Some(else_ptr),
                    }),
                    provenance,
                )
            }
            _ => AstNode::new(
                AstNodeValue::If(IfDeclaration {
                    condition: predicate_ptr,
                    if_branch: block_ptr,
                    else_branch: None,
                }),
                provenance,
            ),
        }
    } else {
        AstNode::new(AstNodeValue::While(predicate_ptr, block_ptr), provenance)
    })
}

fn block<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
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
                return Ok(AstNode::new(AstNodeValue::Block(statements), provenance));
            }
            _ => {
                let statement = statement(source, context, provenance.end())?;
                provenance.set_end(statement.provenance.end());
                statements.push(statement);
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

fn try_decimal<'a>(
    source: &mut TokenIter,
    num: i64,
    range: SourceRange,
) -> Result<AstNode<'a>, ParseError> {
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
        Ok(AstNode::new(AstNodeValue::Float(num), provenance))
    } else {
        Ok(AstNode::new(AstNodeValue::Int(num), range))
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
