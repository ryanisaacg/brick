use std::{collections::HashMap, iter::Peekable, sync::OnceLock};

use thiserror::Error;
use typed_arena::Arena;

use crate::{
    id::{AnyID, VariableID},
    provenance::{SourceMarker, SourceRange},
    tokenizer::{LexError, Token, TokenValue},
    typecheck::ExpressionType,
};

#[derive(Clone, Debug, PartialEq)]
pub struct NameAndType<'a> {
    pub name: String,
    pub ty: &'a AstNode<'a>,
    pub provenance: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct AstNode<'parse> {
    pub value: AstNodeValue<'parse>,
    pub provenance: SourceRange,
    pub ty: OnceLock<ExpressionType>,
}

impl<'a> AstNode<'a> {
    pub fn new(value: AstNodeValue<'a>, provenance: SourceRange) -> AstNode<'a> {
        AstNode {
            value,
            provenance,
            ty: OnceLock::new(),
        }
    }

    pub fn children(&'a self, mut callback: impl FnMut(&'a AstNode<'a>)) {
        use AstNodeValue::*;

        match &self.value {
            Return(Some(child))
            | Yield(Some(child))
            | TakeRef(child)
            | TakeUnique(child)
            | ArrayLiteralLength(child, _)
            | UniqueType(child)
            | SharedType(child)
            | NullableType(child)
            | RcType(child)
            | Statement(child)
            | Deref(child)
            | UnaryExpr(_, child)
            | ArrayType(child)
            | CellType(child)
            | Loop(child)
            | ReferenceCountLiteral(child)
            | CellLiteral(child)
            | BorrowDeclaration(_, child, _) => {
                callback(child);
            }
            DictType(left, right)
            | BinExpr(_, left, right)
            | While(left, right)
            | GeneratorType {
                yield_ty: left,
                param_ty: right,
            } => {
                callback(right);
                callback(left);
            }
            If(IfDeclaration {
                condition,
                if_branch,
                else_branch,
            }) => {
                callback(condition);
                callback(if_branch);
                if let Some(else_branch) = else_branch {
                    callback(else_branch);
                }
            }
            Declaration(_, type_hint, child, _) => {
                if let Some(type_hint) = type_hint {
                    callback(type_hint);
                }
                callback(child);
            }
            ArrayLiteral(values) | Block(values) => {
                for value in values.iter() {
                    callback(value);
                }
            }
            RecordLiteral { fields, .. } => {
                for expression in fields.values() {
                    callback(expression);
                }
            }
            DictLiteral(entries) => {
                for (left, right) in entries.iter() {
                    callback(left);
                    callback(right);
                }
            }
            Call(function, parameters) => {
                callback(function);
                for expression in parameters.iter() {
                    callback(expression);
                }
            }
            AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                body,
                params,
                returns,
                ..
            }) => {
                callback(body);
                for (_, param) in params.iter() {
                    callback(param.ty);
                }
                if let Some(returns) = returns {
                    callback(returns);
                }
            }
            ExternFunctionBinding(FunctionHeaderValue {
                params, returns, ..
            })
            | RequiredFunction(FunctionHeaderValue {
                params, returns, ..
            }) => {
                for param in params.iter() {
                    callback(param.ty);
                }
                if let Some(returns) = returns {
                    callback(returns);
                }
            }
            StructDeclaration(StructDeclarationValue {
                fields,
                associated_functions,
                ..
            }) => {
                for field in fields.iter() {
                    callback(field.ty);
                }
                for node in associated_functions.iter() {
                    callback(node);
                }
            }
            InterfaceDeclaration(InterfaceDeclarationValue {
                associated_functions: fields,
                ..
            }) => {
                for field in fields.iter() {
                    callback(field);
                }
            }
            UnionDeclaration(UnionDeclarationValue {
                variants: fields, ..
            }) => {
                for field in fields.iter() {
                    match field {
                        UnionDeclarationVariant::WithValue(NameAndType { ty, .. }) => callback(ty),
                        UnionDeclarationVariant::WithoutValue(_) => {}
                    }
                }
            }
            Match(case) => {
                callback(case.value);
                for case in case.cases.iter() {
                    callback(&case.body);
                }
            }
            Name { .. }
            | Import(_)
            | Int(_)
            | Float(_)
            | Bool(_)
            | Null
            | CharLiteral(_)
            | StringLiteral(_)
            | Return(None)
            | Yield(None)
            | VoidType => {}
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct FunctionDeclarationValue<'a> {
    pub name: String,
    pub params: Vec<(VariableID, NameAndType<'a>)>,
    pub returns: Option<&'a mut AstNode<'a>>,
    pub body: &'a mut AstNode<'a>,
    /**
     * Whether this function is available to extern. Distinct from declaring an extern function
     * is available in the environment
     */
    pub is_extern: bool,
    pub is_coroutine: bool,
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
    pub properties: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub struct UnionDeclarationValue<'a> {
    pub name: String,
    pub variants: Vec<UnionDeclarationVariant<'a>>,
    pub properties: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum UnionDeclarationVariant<'a> {
    WithValue(NameAndType<'a>),
    WithoutValue(String),
}

#[derive(Debug, PartialEq)]
pub struct InterfaceDeclarationValue<'a> {
    pub name: String,
    pub associated_functions: Vec<AstNode<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct IfDeclaration<'a> {
    pub condition: &'a mut AstNode<'a>,
    pub if_branch: &'a mut AstNode<'a>,
    pub else_branch: Option<&'a mut AstNode<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct MatchDeclaration<'a> {
    pub value: &'a mut AstNode<'a>,
    pub cases: Vec<MatchCaseDeclaration<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct MatchCaseDeclaration<'a> {
    pub variants: Vec<MatchCaseVariant>,
    pub var_id: VariableID,
    pub body: AstNode<'a>,
    pub provenance: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct MatchCaseVariant {
    pub name: String,
    pub bindings: Vec<String>,
    pub provenance: SourceRange,
    pub ty: OnceLock<Option<ExpressionType>>,
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
    Declaration(
        String,
        Option<&'a mut AstNode<'a>>,
        &'a mut AstNode<'a>,
        VariableID,
    ),
    BorrowDeclaration(String, &'a mut AstNode<'a>, VariableID),
    Import(String),
    Return(Option<&'a mut AstNode<'a>>),
    Yield(Option<&'a mut AstNode<'a>>),
    // Any non-specific expression that ends in ; is a statement
    Statement(&'a mut AstNode<'a>),

    Name {
        value: String,
        referenced_id: OnceLock<AnyID>,
    },

    // Expressions
    Int(i64),
    Float(f64),
    Bool(bool),
    CharLiteral(char),
    StringLiteral(String),
    Null,
    UnaryExpr(UnaryOp, &'a mut AstNode<'a>),
    BinExpr(BinOp, &'a mut AstNode<'a>, &'a mut AstNode<'a>),
    If(IfDeclaration<'a>),
    While(&'a mut AstNode<'a>, &'a mut AstNode<'a>),
    Loop(&'a mut AstNode<'a>),
    Call(&'a mut AstNode<'a>, Vec<AstNode<'a>>),
    TakeUnique(&'a mut AstNode<'a>),
    TakeRef(&'a mut AstNode<'a>),
    RecordLiteral {
        name: &'a mut AstNode<'a>,
        fields: HashMap<String, AstNode<'a>>,
    },
    DictLiteral(Vec<(AstNode<'a>, AstNode<'a>)>),
    ArrayLiteral(Vec<AstNode<'a>>),
    ArrayLiteralLength(&'a mut AstNode<'a>, &'a mut AstNode<'a>),
    ReferenceCountLiteral(&'a mut AstNode<'a>),
    CellLiteral(&'a mut AstNode<'a>),
    Block(Vec<AstNode<'a>>),
    Deref(&'a mut AstNode<'a>),
    Match(MatchDeclaration<'a>),

    // Types
    // TODO: unify
    VoidType,
    UniqueType(&'a mut AstNode<'a>),
    SharedType(&'a mut AstNode<'a>),
    ArrayType(&'a mut AstNode<'a>),
    DictType(&'a mut AstNode<'a>, &'a mut AstNode<'a>),
    RcType(&'a mut AstNode<'a>),
    NullableType(&'a mut AstNode<'a>),
    CellType(&'a mut AstNode<'a>),
    GeneratorType {
        yield_ty: &'a mut AstNode<'a>,
        param_ty: &'a mut AstNode<'a>,
    },
}

impl<'a> AstNodeValue<'a> {
    fn name(value: String) -> AstNodeValue<'a> {
        AstNodeValue::Name {
            value,
            referenced_id: OnceLock::new(),
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum UnaryOp {
    BooleanNot,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum BinOp {
    Dot,
    Index,
    Concat,

    NullCoalesce,
    NullChaining,

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

    Assignment,
    AddAssign,
    SubtractAssign,
    MultiplyAssign,
    DivideAssign,

    BooleanAnd,
    BooleanOr,
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

pub fn parse<'a>(
    arena: &'a Arena<AstNode<'a>>,
    mut source: impl Iterator<Item = Result<Token, LexError>>,
) -> Result<Vec<AstNode<'a>>, ParseError> {
    let mut source = (&mut source as TokenIterInner<'_>).peekable();

    let mut top_level_nodes = Vec::new();

    while let Some(lexeme) = peek_token_optional(&mut source)? {
        let cursor = lexeme.range.start();
        let statement = statement(&mut source, arena, cursor)?;

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
            | TokenValue::Borrow
            | TokenValue::Import
            | TokenValue::Function
            | TokenValue::Gen
            | TokenValue::Extern
            | TokenValue::Struct
            | TokenValue::Union
            | TokenValue::Interface
            | TokenValue::Return => {
                let Token { range, value, .. } = already_peeked_token(source)?;
                let cursor = range.end();
                match value {
                    TokenValue::Let => variable_declaration(source, context, cursor)?,
                    TokenValue::Borrow => borrow_declaration(source, context, cursor)?,
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
                    TokenValue::Function => function_declaration(source, context, cursor, false)?,
                    TokenValue::Gen => {
                        let token = assert_next_lexeme_eq(
                            source,
                            TokenValue::Function,
                            cursor,
                            "expected fn after gen",
                        )?;
                        let cursor = token.range.end();
                        function_declaration(source, context, cursor, true)?
                    }
                    TokenValue::Struct => struct_declaration(source, context, cursor)?,
                    TokenValue::Union => union_declaration(source, context, cursor)?,
                    TokenValue::Interface => interface_declaration(source, context, cursor)?,
                    TokenValue::Return => {
                        let statement = return_declaration(source, context, cursor)?;
                        if let Some(Token {
                            value: TokenValue::Semicolon,
                            ..
                        }) = peek_token_optional(source)?
                        {
                            already_peeked_token(source)?;
                        }

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
    if let Some(Token {
        value: TokenValue::Semicolon,
        ..
    }) = peek_token_optional(source)?
    {
        let token = already_peeked_token(source)?;
        return Ok(AstNode::new(
            AstNodeValue::Return(None),
            SourceRange::new(cursor, token.range.end()),
        ));
    }
    let value = expression(source, context, cursor, true)?;
    let provenance = value.provenance.clone();
    let value = add_node(context, value);

    Ok(AstNode::new(AstNodeValue::Return(Some(value)), provenance))
}

fn struct_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    mut cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let start = cursor;
    let (name, provenance) = word(source, cursor, "expected name after 'struct'")?;
    cursor = provenance.end();
    let (properties, cursor) = property_list(source, cursor)?;
    let (end, fields, associated_functions) =
        interface_or_struct_body(source, context, cursor, false)?;

    Ok(AstNode::new(
        AstNodeValue::StructDeclaration(StructDeclarationValue {
            name,
            fields,
            associated_functions,
            properties,
        }),
        SourceRange::new(start, end),
    ))
}

fn property_list(
    source: &mut TokenIter,
    mut cursor: SourceMarker,
) -> Result<(Vec<String>, SourceMarker), ParseError> {
    Ok((
        if peek_token(source, cursor, "unexpected EOL in struct")?.value == TokenValue::Colon {
            cursor = already_peeked_token(source)?.range.end();
            let mut properties = Vec::new();
            loop {
                let (word, range) = word(source, cursor, "expected property name")?;
                properties.push(word);
                cursor = range.end();
                if peek_token(source, cursor, "unexpected EOL in struct properties")?.value
                    == TokenValue::Comma
                {
                    cursor = already_peeked_token(source)?.range.end();
                }
                if peek_token(source, cursor, "unexpected EOL in struct properties")?.value
                    == TokenValue::OpenBracket
                {
                    break;
                }
            }
            properties
        } else {
            Vec::new()
        },
        cursor,
    ))
}

fn interface_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let (name, provenance) = word(source, cursor, "expected name after 'interface'")?;
    let (end, _, associated_functions) =
        interface_or_struct_body(source, context, provenance.end(), true)?;

    Ok(AstNode::new(
        AstNodeValue::InterfaceDeclaration(InterfaceDeclarationValue {
            name,
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
        source,
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
                    source,
                    TokenValue::OpenBracket,
                    cursor,
                    "expected open bracket to start function body",
                )?;
                cursor = token.range.end();
                let body = block(source, context, cursor)?;
                cursor = body.provenance.end();
                associated_functions.push(AstNode::new(
                    AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                        name,
                        params: params.into_iter().map(|p| (VariableID::new(), p)).collect(),
                        returns,
                        body: add_node(context, body),
                        is_extern: true,
                        is_coroutine: false,
                    }),
                    SourceRange::new(start, cursor),
                ));
            }
        } else {
            if is_interface {
                let next = already_peeked_token(source)?;
                return Err(ParseError::UnexpectedToken(
                    Box::new(next),
                    "expected associated function in interface",
                ));
            }
            let (name, range, type_hint) =
                name_and_type_hint(source, context, cursor, "expected parameter")?;
            cursor = range.end();
            let kind = type_hint.ok_or(ParseError::MissingTypeForParam(cursor))?;
            let kind = add_node(context, kind);
            fields.push(NameAndType {
                name,
                ty: kind,
                provenance: SourceRange::new(range.start(), kind.provenance.end()),
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
    mut cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let (name, mut provenance) = word(source, cursor, "expected name after 'union'")?;
    cursor = provenance.end();
    let (properties, mut cursor) = property_list(source, cursor)?;

    cursor = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        cursor,
        "expected open bracket to start variants",
    )?
    .range
    .end();

    let mut variants = Vec::new();

    let mut closed = false;
    while !closed {
        let (name, name_range) = word(source, cursor, "expected variant name")?;
        cursor = name_range.end();
        if peek_token(source, cursor, "unexpected EOF in union declaration")?.value
            == TokenValue::OpenParen
        {
            let paren = already_peeked_token(source)?;
            let ty = type_expression(source, context, paren.range.end())?;
            let ty = add_node(context, ty);
            let paren = assert_next_lexeme_eq(
                source,
                TokenValue::CloseParen,
                ty.provenance.end(),
                "expected ) after variant type",
            )?;
            cursor = paren.range.end();

            variants.push(UnionDeclarationVariant::WithValue(NameAndType {
                name,
                ty,
                provenance: SourceRange::new(name_range.start(), ty.provenance.end()),
            }));
        } else {
            variants.push(UnionDeclarationVariant::WithoutValue(name));
        }

        let (should_end, range) = comma_or_end_list(
            source,
            TokenValue::CloseBracket,
            cursor,
            "expected either more variants or close bracket",
        )?;
        closed = should_end;
        cursor = range.end();
    }
    provenance.set_end(cursor);

    Ok(AstNode::new(
        AstNodeValue::UnionDeclaration(UnionDeclarationValue {
            name,
            variants,
            properties,
        }),
        provenance,
    ))
}

fn extern_function_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    start: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let mut provenance = assert_next_lexeme_eq(
        source,
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

    let next = next_token(source, end, "expected ; or { after extern fn decl")?;
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
                    params: params.into_iter().map(|p| (VariableID::new(), p)).collect(),
                    returns,
                    body: add_node(context, body),
                    is_extern: true,
                    is_coroutine: false,
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
    is_generator: bool,
) -> Result<AstNode<'a>, ParseError> {
    let FunctionHeader {
        name,
        params,
        returns,
        end,
    } = function_header(source, context, start)?;
    let mut provenance = SourceRange::new(start, end);
    let next_token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        start,
        "expected { after function declaration",
    )?;
    let body = block(source, context, next_token.range.end())?;
    provenance.set_end(body.provenance.end());

    Ok(AstNode::new(
        AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
            name,
            params: params.into_iter().map(|p| (VariableID::new(), p)).collect(),
            returns,
            body: add_node(context, body),
            is_extern: false,
            is_coroutine: is_generator,
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
        source,
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
                    name,
                    ty: kind,
                    provenance: SourceRange::new(range.start(), kind.provenance.end()),
                });
            }
        }
    }
    let next_token = assert_next_lexeme_eq(
        source,
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
    let (name, mut provenance, type_hint) = name_and_type_hint(
        source,
        context,
        cursor,
        "expected word after 'let' in declaration",
    )?;
    let cursor = assert_next_lexeme_eq(
        source,
        TokenValue::Assign,
        provenance.end(),
        "expected = after let binding target",
    )?
    .range
    .end();
    let value = expression(source, context, cursor, true)?;
    provenance.set_end(value.provenance.end());

    let type_hint = type_hint.map(|type_hint| add_node(context, type_hint));

    assert_next_lexeme_eq(
        source,
        TokenValue::Semicolon,
        provenance.end(),
        "expected ; after 'let' statement",
    )?;

    Ok(AstNode::new(
        AstNodeValue::Declaration(name, type_hint, add_node(context, value), VariableID::new()),
        provenance,
    ))
}

fn borrow_declaration<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    mut cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let start = cursor;
    let (name, span) = word(source, cursor, "expected name after borrow")?;
    cursor = span.end();
    cursor = assert_next_lexeme_eq(
        source,
        TokenValue::Assign,
        cursor,
        "expected = after let binding target",
    )?
    .range
    .end();
    let rhs = expression(source, context, cursor, true)?;
    cursor = rhs.provenance.end();
    let rhs = context.alloc(rhs);

    assert_next_lexeme_eq(
        source,
        TokenValue::Semicolon,
        cursor,
        "expected ; after 'borrow' statement",
    )?;

    Ok(AstNode::new(
        AstNodeValue::BorrowDeclaration(name, rhs, VariableID::new()),
        SourceRange::new(start, cursor),
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
    let next = next_token(source, cursor, "expected type")?;
    let node = match next.value {
        TokenValue::Void => AstNode::new(AstNodeValue::VoidType, next.range),
        ptr @ (TokenValue::Unique | TokenValue::Ref) => {
            let subtype = type_expression(source, context, next.range.end())?;
            let end = subtype.provenance.end();
            let subtype = add_node(context, subtype);
            AstNode::new(
                match ptr {
                    TokenValue::Unique => AstNodeValue::UniqueType(subtype),
                    TokenValue::Ref => AstNodeValue::SharedType(subtype),
                    _ => unreachable!(),
                },
                SourceRange::new(next.range.start(), end),
            )
        }
        TokenValue::Dict => {
            let token = assert_next_lexeme_eq(
                source,
                TokenValue::OpenSquare,
                next.range.end(),
                "expected [ after dict type",
            )?;
            let key = type_expression(source, context, token.range.end())?;
            let end = key.provenance.end();
            let key = add_node(context, key);

            let token = assert_next_lexeme_eq(
                source,
                TokenValue::Comma,
                end,
                "expected , after dict key type",
            )?;

            let value = type_expression(source, context, token.range.end())?;
            let end = value.provenance.end();
            let value = add_node(context, value);

            let token = assert_next_lexeme_eq(
                source,
                TokenValue::CloseSquare,
                end,
                "expected ] after dict type",
            )?;

            AstNode::new(
                AstNodeValue::DictType(key, value),
                SourceRange::new(next.range.start(), token.range.end()),
            )
        }
        TokenValue::List => {
            let token = assert_next_lexeme_eq(
                source,
                TokenValue::OpenSquare,
                next.range.end(),
                "expected [ after array type",
            )?;

            let value = type_expression(source, context, token.range.end())?;
            let end = value.provenance.end();
            let value = add_node(context, value);

            let token = assert_next_lexeme_eq(
                source,
                TokenValue::CloseSquare,
                end,
                "expected ] after array type",
            )?;

            AstNode::new(
                AstNodeValue::ArrayType(value),
                SourceRange::new(next.range.start(), token.range.end()),
            )
        }
        TokenValue::Rc => {
            let token = assert_next_lexeme_eq(
                source,
                TokenValue::OpenSquare,
                next.range.end(),
                "expected [ after rc type",
            )?;

            let value = type_expression(source, context, token.range.end())?;
            let end = value.provenance.end();
            let value = add_node(context, value);

            let token = assert_next_lexeme_eq(
                source,
                TokenValue::CloseSquare,
                end,
                "expected ] after rc type",
            )?;

            AstNode::new(
                AstNodeValue::RcType(value),
                SourceRange::new(next.range.start(), token.range.end()),
            )
        }
        TokenValue::Cell => {
            let token = assert_next_lexeme_eq(
                source,
                TokenValue::OpenSquare,
                next.range.end(),
                "expected [ after cell type",
            )?;
            let inner_ty = type_expression(source, context, token.range.end())?;
            let token = assert_next_lexeme_eq(
                source,
                TokenValue::CloseSquare,
                inner_ty.provenance.end(),
                "expected ] after cell type",
            )?;

            AstNode::new(
                AstNodeValue::CellType(context.alloc(inner_ty)),
                SourceRange::new(next.range.start(), token.range.end()),
            )
        }
        TokenValue::Word(name) => match name.as_str() {
            "generator" => {
                let token = assert_next_lexeme_eq(
                    source,
                    TokenValue::OpenSquare,
                    next.range.end(),
                    "expected [ after generator type",
                )?;

                let yield_ty = type_expression(source, context, token.range.end())?;
                let cursor = yield_ty.provenance.end();
                let yield_ty = add_node(context, yield_ty);

                assert_next_lexeme_eq(
                    source,
                    TokenValue::Comma,
                    cursor,
                    "expected , after yield type",
                )?;

                let param_ty = type_expression(source, context, token.range.end())?;
                let cursor = param_ty.provenance.end();
                let param_ty = add_node(context, param_ty);

                let token = assert_next_lexeme_eq(
                    source,
                    TokenValue::CloseSquare,
                    cursor,
                    "expected ] after generator type",
                )?;

                AstNode::new(
                    AstNodeValue::GeneratorType { yield_ty, param_ty },
                    SourceRange::new(next.range.start(), token.range.end()),
                )
            }
            _ => AstNode::new(AstNodeValue::name(name), next.range),
        },
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
    can_be_struct: bool,
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
    let Token { value, range } = next_token(source, start, "expected expression")?;
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
                source,
                TokenValue::CloseParen,
                left.provenance.end(),
                "expected ) to match (",
            )?;

            left
        }
        TokenValue::Dict => dict_literal(source, context, cursor)?,
        TokenValue::List => array_literal(source, context, cursor, can_be_struct)?,
        TokenValue::Rc => rc_literal(source, context, cursor)?,
        TokenValue::Cell => cell_literal(source, context, cursor)?,
        token @ (TokenValue::If | TokenValue::While) => {
            if_or_while(source, context, token, cursor)?
        }
        TokenValue::Case => match_statement(source, context, cursor)?,
        TokenValue::Loop => parse_loop(source, context, cursor)?,
        TokenValue::OpenBracket => block(source, context, cursor)?,
        // Atoms
        TokenValue::True => AstNode::new(AstNodeValue::Bool(true), range),
        TokenValue::False => AstNode::new(AstNodeValue::Bool(false), range),
        TokenValue::Word(word) => AstNode::new(AstNodeValue::name(word), range),
        TokenValue::Null => AstNode::new(AstNodeValue::Null, range),
        TokenValue::CharacterLiteral(c) => AstNode::new(AstNodeValue::CharLiteral(c), range),
        TokenValue::StringLiteral(s) => AstNode::new(AstNodeValue::StringLiteral(s), range),
        TokenValue::Int(int) => try_decimal(source, int as i64, range)?,
        TokenValue::Yield => {
            let next = peek_token(source, cursor, "expected yielded value after yield")?;
            if next.value.is_expression_boundary() {
                let range = SourceRange::new(start, cursor);
                AstNode::new(AstNodeValue::Yield(None), range)
            } else {
                let inner = expression(source, context, cursor, true)?;
                let range = SourceRange::new(start, inner.provenance.end());
                let inner = context.alloc(inner);
                AstNode::new(AstNodeValue::Yield(Some(inner)), range)
            }
        }
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
                    TokenValue::Ref => AstNodeValue::TakeRef(right),
                    TokenValue::Unique => AstNodeValue::TakeUnique(right),
                    TokenValue::Asterisk => AstNodeValue::Deref(right),
                    TokenValue::Exclamation => AstNodeValue::UnaryExpr(UnaryOp::BooleanNot, right),
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
                        source,
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
                                AstNode::new(AstNodeValue::name(field.clone()), field_range);
                            fields.insert(field, argument);
                            if lex == TokenValue::CloseBracket {
                                break;
                            }
                        } else {
                            let token = assert_next_lexeme_eq(
                                source,
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
                                    source,
                                    TokenValue::CloseBracket,
                                    end,
                                    "expected close parenthesis or comma after argument",
                                )?;
                                break;
                            }
                        }
                    }

                    let AstNodeValue::Name { .. } = left.value else {
                        todo!("non-name struct literal");
                    };

                    left = AstNode::new(
                        AstNodeValue::RecordLiteral {
                            name: context.alloc(left),
                            fields,
                        },
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
                TokenValue::Concat => BinOp::Concat,
                TokenValue::Asterisk => BinOp::Multiply,
                TokenValue::ForwardSlash => BinOp::Divide,
                TokenValue::BooleanAnd => BinOp::BooleanAnd,
                TokenValue::BooleanOr => BinOp::BooleanOr,
                TokenValue::NullCoalesce => BinOp::NullCoalesce,
                TokenValue::NullChaining => BinOp::NullChaining,
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
const NULL_COALESCE: u8 = ASSIGNMENT + 2;
// bools
const BOOLEAN_OR: u8 = NULL_COALESCE + 2;
const BOOLEAN_AND: u8 = BOOLEAN_OR + 2;
const BOOLEAN_NOT: u8 = BOOLEAN_AND + 1;
const COMPARE: u8 = BOOLEAN_NOT + 2;
// math
const SUM: u8 = COMPARE + 2;
const FACTOR: u8 = SUM + 2;
// misc
const CONCAT: u8 = FACTOR + 2;
const REFERENCE: u8 = CONCAT + 1;
const CALL: u8 = REFERENCE + 2;
const NULL_CHAINING: u8 = CALL + 1;
const DOT: u8 = NULL_CHAINING + 1;

fn prefix_binding_power(op: &TokenValue) -> Option<((), u8)> {
    let res = match op {
        TokenValue::Ref | TokenValue::Unique | TokenValue::Asterisk => ((), REFERENCE),
        TokenValue::Exclamation => ((), BOOLEAN_NOT),
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
        TokenValue::Concat => (CONCAT, CONCAT - 1),
        TokenValue::Period => (DOT - 1, DOT),
        TokenValue::NullChaining => (NULL_CHAINING - 1, NULL_CHAINING),
        TokenValue::BooleanAnd => (BOOLEAN_AND, BOOLEAN_AND - 1),
        TokenValue::BooleanOr => (BOOLEAN_OR, BOOLEAN_OR - 1),
        TokenValue::NullCoalesce => (NULL_COALESCE, NULL_COALESCE - 1),
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
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenSquare,
        start,
        "expected [ to start list literal",
    )?;
    let expr = expression(source, context, token.range.start(), can_be_struct)?;
    let separator = next_token(
        source,
        expr.provenance.start(),
        "expected comma, semicolon or ]",
    )?;
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
                    "expected comma or ] to end list literal",
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
            let length = expression(source, context, separator.range.end(), true)?;
            let close = assert_next_lexeme_eq(
                source,
                TokenValue::CloseSquare,
                length.provenance.end(),
                "expected ] after list length in list literal",
            )?;
            let expr = add_node(context, expr);
            let length = add_node(context, length);
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

fn rc_literal<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    start: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        start,
        "expected { to start rc literal",
    )?;
    let mut cursor = token.range.end();
    let inner = expression(source, context, cursor, true)?;
    cursor = inner.provenance.end();
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::CloseBracket,
        cursor,
        "expected } to end rc literal",
    )?;
    cursor = token.range.end();
    let inner = context.alloc(inner);
    Ok(AstNode::new(
        AstNodeValue::ReferenceCountLiteral(inner),
        SourceRange::new(start, cursor),
    ))
}

fn cell_literal<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    start: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        start,
        "expected { to start cell literal",
    )?;
    let mut cursor = token.range.end();
    let inner = expression(source, context, cursor, true)?;
    cursor = inner.provenance.end();
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::CloseBracket,
        cursor,
        "expected } to end cell literal",
    )?;
    cursor = token.range.end();
    let inner = context.alloc(inner);
    Ok(AstNode::new(
        AstNodeValue::CellLiteral(inner),
        SourceRange::new(start, cursor),
    ))
}

fn dict_literal<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    start: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        start,
        "expected { to start dict literal",
    )?;
    let mut cursor = token.range.end();
    let mut entries = Vec::new();
    loop {
        let key = match peek_token(source, cursor, "expected } or next entry")?.value {
            TokenValue::CloseBracket => {
                source.next();
                break;
            }
            TokenValue::OpenSquare => {
                already_peeked_token(source)?;
                let key = expression(source, context, cursor, true)?;
                let token = assert_next_lexeme_eq(
                    source,
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
                    &peek_token(source, cursor, "expected comma or } to end dict literal")?.value
                {
                    let lex = lex.clone();
                    let token = already_peeked_token(source)?;
                    cursor = token.range.end();

                    let value = AstNode::new(AstNodeValue::name(key.clone()), key_range.clone());
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
            source,
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
            TokenValue::CloseBracket,
            cursor,
            "expected , or } in dict literal",
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
    is_if_or_while: TokenValue,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let predicate = expression(source, context, cursor, false)?;
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        predicate.provenance.end(),
        "expected { after predicate",
    )?;
    let cursor = token.range.end();
    let if_block = block(source, context, cursor)?;

    let predicate_ptr = add_node(context, predicate);
    let provenance = SourceRange::new(cursor, if_block.provenance.end());
    let block_ptr = add_node(context, if_block);

    Ok(if is_if_or_while == TokenValue::If {
        match peek_token_optional(source)? {
            Some(Token {
                value: TokenValue::Word(word),
                ..
            }) if word == "else" => {
                let else_token = already_peeked_token(source)?;
                let next_token = peek_token(
                    source,
                    else_token.range.end(),
                    "expected { or if after else",
                )?;
                let else_block = match next_token.value {
                    TokenValue::OpenBracket => {
                        let token = already_peeked_token(source)?;
                        block(source, context, token.range.end())?
                    }
                    TokenValue::If => {
                        let token = already_peeked_token(source)?;
                        let if_node =
                            if_or_while(source, context, TokenValue::If, token.range.end())?;
                        let provenance = if_node.provenance.clone();
                        AstNode::new(AstNodeValue::Block(vec![if_node]), provenance)
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken(
                            Box::new(already_peeked_token(source)?),
                            "expected { or if after else",
                        ))
                    }
                };
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

fn match_statement<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let start = cursor;
    let match_value = expression(source, context, cursor, false)?;
    let mut cursor = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        match_value.provenance.end(),
        "expected { after case value",
    )?
    .range
    .end();
    let match_value = context.alloc(match_value);
    let mut cases = Vec::new();
    while peek_token(source, cursor, "unexpected EOF in case statement")?.value
        != TokenValue::CloseBracket
    {
        let next_case = match_case_statement(source, context, cursor)?;
        cursor = next_case.provenance.end();
        cases.push(next_case);
    }
    cursor = already_peeked_token(source)?.range.end();

    Ok(AstNode::new(
        AstNodeValue::Match(MatchDeclaration {
            value: match_value,
            cases,
        }),
        SourceRange::new(start, cursor),
    ))
}

fn match_case_statement<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    mut cursor: SourceMarker,
) -> Result<MatchCaseDeclaration<'a>, ParseError> {
    let start = cursor;
    let variant = match_case_variant(source, cursor)?;
    cursor = variant.provenance.end();
    let mut variants = vec![variant];

    while peek_token(source, cursor, "expected | or => after case variant")?.value
        != TokenValue::CaseRocket
    {
        cursor = assert_next_lexeme_eq(
            source,
            TokenValue::VerticalPipe,
            cursor,
            "expected | or => after case variant",
        )?
        .range
        .end();
        let variant = match_case_variant(source, cursor)?;
        cursor = variant.provenance.end();
        variants.push(variant);
    }
    cursor = already_peeked_token(source)?.range.end();
    let body = if peek_token(source, cursor, "expected { or expression after =>")?.value
        == TokenValue::OpenBracket
    {
        cursor = already_peeked_token(source)?.range.end();
        let body = block(source, context, cursor)?;
        cursor = body.provenance.end();
        body
    } else {
        let expr = expression(source, context, cursor, true)?;
        cursor = assert_next_lexeme_eq(
            source,
            TokenValue::Comma,
            expr.provenance.end(),
            "expected , after variant expression",
        )?
        .range
        .end();
        expr
    };

    Ok(MatchCaseDeclaration {
        variants,
        var_id: VariableID::new(),
        body,
        provenance: SourceRange::new(start, cursor),
    })
}

fn match_case_variant(
    source: &mut TokenIter,
    mut cursor: SourceMarker,
) -> Result<MatchCaseVariant, ParseError> {
    let start = cursor;
    let (name, range) = word(source, cursor, "expected name of case variant")?;
    cursor = range.end();

    let mut bindings = Vec::new();
    if peek_token(source, cursor, "expected (, | or => after case name")?.value
        == TokenValue::OpenParen
    {
        loop {
            cursor = already_peeked_token(source)?.range.end();
            let (binding, range) = word(source, cursor, "expected binding in case statement")?;
            bindings.push(binding);
            cursor = range.end();

            let (list_ended, range) =
                comma_or_end_list(source, TokenValue::CloseParen, cursor, "expected , or )")?;
            cursor = range.end();
            if list_ended {
                break;
            }
        }
    }

    Ok(MatchCaseVariant {
        name,
        bindings,
        provenance: SourceRange::new(start, cursor),
        ty: OnceLock::new(),
    })
}

fn parse_loop<'a>(
    source: &mut TokenIter,
    context: &'a Arena<AstNode<'a>>,
    cursor: SourceMarker,
) -> Result<AstNode<'a>, ParseError> {
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        cursor,
        "expected { after loop",
    )?;

    let body = block(source, context, token.range.start())?;
    let provenance = SourceRange::new(cursor, body.provenance.end());
    let body = add_node(context, body);

    Ok(AstNode::new(AstNodeValue::Loop(body), provenance))
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
    match next_token(source, cursor, reason)? {
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

fn word(
    source: &mut TokenIter,
    cursor: SourceMarker,
    reason: &'static str,
) -> Result<(String, SourceRange), ParseError> {
    match next_token(source, cursor, reason)? {
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

fn assert_next_lexeme_eq(
    source: &mut TokenIter,
    target: TokenValue,
    provenance: SourceMarker,
    reason: &'static str,
) -> Result<Token, ParseError> {
    skim_off_comments(source);
    let lexeme = source
        .next()
        .ok_or(ParseError::UnexpectedEndOfInput(provenance, reason))??;
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
    let next = next_token(source, start, reason)?;
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

fn next_token(
    source: &mut TokenIter,
    cursor: SourceMarker,
    reason: &'static str,
) -> Result<Token, ParseError> {
    skim_off_comments(source);
    let lex = source
        .next()
        .ok_or(ParseError::UnexpectedEndOfInput(cursor, reason))??;
    Ok(lex)
}

fn peek_token<'a>(
    source: &'a mut TokenIter,
    cursor: SourceMarker,
    reason: &'static str,
) -> Result<&'a Token, ParseError> {
    skim_off_comments(source);
    Ok(source
        .peek()
        .ok_or(ParseError::UnexpectedEndOfInput(cursor, reason))?
        .as_ref()
        .map_err(|e| e.clone())?)
}

fn peek_token_optional<'a>(source: &'a mut TokenIter) -> Result<Option<&'a Token>, ParseError> {
    skim_off_comments(source);
    Ok(source
        .peek()
        .map(|result| result.as_ref().map_err(|e| e.clone()))
        .transpose()?)
}

fn skim_off_comments(source: &mut TokenIter) {
    while let Some(Ok(Token {
        value: TokenValue::LineComment(_),
        ..
    })) = source.peek()
    {
        source.next().unwrap().unwrap();
    }
}
