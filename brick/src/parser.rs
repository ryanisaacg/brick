use std::{collections::HashMap, error::Error, fmt::Display, iter::Peekable, sync::OnceLock};

use crate::{
    diagnostic::{Diagnostic, DiagnosticContents, DiagnosticMarker},
    id::{AnyID, ConstantID, VariableID},
    provenance::{SourceMarker, SourceRange},
    tokenizer::{LexError, Token, TokenValue},
    typecheck::ExpressionType,
};

#[derive(Clone, Debug, PartialEq)]
pub struct NameAndType {
    pub name: String,
    pub ty: AstNodeId,
    pub provenance: SourceRange,
}

#[derive(Default)]
pub struct AstArena {
    nodes: Vec<AstNode>,
}

impl AstArena {
    pub fn get(&self, idx: AstNodeId) -> &AstNode {
        &self.nodes[idx.0 as usize]
    }

    pub fn get_mut(&mut self, idx: AstNodeId) -> &mut AstNode {
        &mut self.nodes[idx.0 as usize]
    }

    pub fn add(&mut self, node: AstNode) -> AstNodeId {
        let idx = self.nodes.len() as u32;
        self.nodes.push(node);
        AstNodeId(idx)
    }

    pub fn iter(&self) -> impl Iterator<Item = &AstNode> {
        self.nodes.iter()
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut AstNode> {
        self.nodes.iter_mut()
    }
}

#[derive(Debug, PartialEq)]
pub struct AstNode {
    pub value: AstNodeValue,
    pub provenance: SourceRange,
    pub ty: OnceLock<ExpressionType>,
}

impl AstNode {
    pub fn new(value: AstNodeValue, provenance: SourceRange) -> AstNode {
        AstNode {
            value,
            provenance,
            ty: OnceLock::new(),
        }
    }

    pub fn children<'a>(&self, arena: &'a AstArena, mut callback: impl FnMut(&'a AstNode)) {
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
                callback(arena.get(*child));
            }
            DictType(left, right)
            | BinExpr(_, left, right)
            | While(left, right)
            | GeneratorType {
                yield_ty: left,
                param_ty: right,
            } => {
                callback(arena.get(*right));
                callback(arena.get(*left));
            }
            If(IfDeclaration {
                condition,
                if_branch,
                else_branch,
            }) => {
                callback(arena.get(*condition));
                callback(arena.get(*if_branch));
                if let Some(else_branch) = else_branch {
                    callback(arena.get(*else_branch));
                }
            }
            ConstDeclaration {
                type_hint,
                value: child,
                ..
            }
            | Declaration(_, type_hint, child, _) => {
                if let Some(type_hint) = type_hint {
                    callback(arena.get(*type_hint));
                }
                callback(arena.get(*child));
            }
            ArrayLiteral(values) | Block(values) => {
                for value in values.iter() {
                    callback(arena.get(*value));
                }
            }
            RecordLiteral { fields, .. } => {
                for expression in fields.values() {
                    callback(arena.get(*expression));
                }
            }
            DictLiteral(entries) => {
                for (left, right) in entries.iter() {
                    callback(arena.get(*left));
                    callback(arena.get(*right));
                }
            }
            Call(function, parameters) => {
                callback(arena.get(*function));
                for expression in parameters.iter() {
                    callback(arena.get(*expression));
                }
            }
            AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                body,
                params,
                returns,
                ..
            }) => {
                callback(arena.get(*body));
                for (_, param) in params.iter() {
                    callback(arena.get(param.ty));
                }
                if let Some(returns) = returns {
                    callback(arena.get(*returns));
                }
            }
            ExternFunctionBinding(FunctionHeaderValue {
                params, returns, ..
            })
            | RequiredFunction(FunctionHeaderValue {
                params, returns, ..
            }) => {
                for param in params.iter() {
                    callback(arena.get(param.ty));
                }
                if let Some(returns) = returns {
                    callback(arena.get(*returns));
                }
            }
            StructDeclaration(StructDeclarationValue {
                fields,
                associated_functions,
                ..
            }) => {
                for field in fields.iter() {
                    callback(arena.get(field.ty));
                }
                for node in associated_functions.iter() {
                    callback(arena.get(*node));
                }
            }
            InterfaceDeclaration(InterfaceDeclarationValue {
                associated_functions: fields,
                ..
            }) => {
                for field in fields.iter() {
                    callback(arena.get(*field));
                }
            }
            UnionDeclaration(UnionDeclarationValue {
                variants: fields, ..
            }) => {
                for field in fields.iter() {
                    match field {
                        UnionDeclarationVariant::WithValue(NameAndType { ty, .. }) => {
                            callback(arena.get(*ty))
                        }
                        UnionDeclarationVariant::WithoutValue(_) => {}
                    }
                }
            }
            Match(case) => {
                callback(arena.get(case.value));
                for case in case.cases.iter() {
                    callback(arena.get(case.body));
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
pub struct FunctionDeclarationValue {
    pub name: String,
    pub self_param: Option<SelfParameter>,
    pub params: Vec<(VariableID, NameAndType)>,
    pub returns: Option<AstNodeId>,
    pub body: AstNodeId,
    /**
     * Whether this function is available to extern. Distinct from declaring an extern function
     * is available in the environment
     */
    pub is_extern: bool,
    pub is_coroutine: bool,
}

#[derive(Debug, PartialEq)]
pub struct FunctionHeaderValue {
    pub name: String,
    pub self_param: Option<SelfParameter>,
    pub params: Vec<NameAndType>,
    pub returns: Option<AstNodeId>,
}

#[derive(Debug, PartialEq)]
pub struct StructDeclarationValue {
    pub name: String,
    pub fields: Vec<NameAndType>,
    pub associated_functions: Vec<AstNodeId>,
    pub properties: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub struct UnionDeclarationValue {
    pub name: String,
    pub variants: Vec<UnionDeclarationVariant>,
    pub properties: Vec<String>,
}

#[derive(Debug, PartialEq)]
pub enum UnionDeclarationVariant {
    WithValue(NameAndType),
    WithoutValue(String),
}

#[derive(Debug, PartialEq)]
pub struct InterfaceDeclarationValue {
    pub name: String,
    pub associated_functions: Vec<AstNodeId>,
}

#[derive(Debug, PartialEq)]
pub struct IfDeclaration {
    pub condition: AstNodeId,
    pub if_branch: AstNodeId,
    pub else_branch: Option<AstNodeId>,
}

#[derive(Debug, PartialEq)]
pub struct MatchDeclaration {
    pub value: AstNodeId,
    pub cases: Vec<MatchCaseDeclaration>,
}

#[derive(Debug, PartialEq)]
pub struct MatchCaseDeclaration {
    pub variants: Vec<MatchCaseVariant>,
    pub var_id: VariableID,
    pub body: AstNodeId,
    pub provenance: SourceRange,
}

#[derive(Debug, PartialEq)]
pub struct MatchCaseVariant {
    pub name: String,
    pub bindings: Vec<String>,
    pub provenance: SourceRange,
    pub ty: OnceLock<Option<ExpressionType>>,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct AstNodeId(u32);

#[derive(Debug, PartialEq)]
pub enum AstNodeValue {
    // Statements
    FunctionDeclaration(FunctionDeclarationValue),
    ExternFunctionBinding(FunctionHeaderValue),
    StructDeclaration(StructDeclarationValue),
    UnionDeclaration(UnionDeclarationValue),
    InterfaceDeclaration(InterfaceDeclarationValue),
    RequiredFunction(FunctionHeaderValue),
    Declaration(String, Option<AstNodeId>, AstNodeId, VariableID),
    BorrowDeclaration(String, AstNodeId, VariableID),
    ConstDeclaration {
        name: String,
        type_hint: Option<AstNodeId>,
        value: AstNodeId,
        variable_id: ConstantID,
    },
    Import(Vec<String>),
    Return(Option<AstNodeId>),
    Yield(Option<AstNodeId>),
    // Any non-specific expression that ends in ; is a statement
    Statement(AstNodeId),

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
    UnaryExpr(UnaryOp, AstNodeId),
    BinExpr(BinOp, AstNodeId, AstNodeId),
    If(IfDeclaration),
    While(AstNodeId, AstNodeId),
    Loop(AstNodeId),
    Call(AstNodeId, Vec<AstNodeId>),
    TakeUnique(AstNodeId),
    TakeRef(AstNodeId),
    RecordLiteral {
        name: AstNodeId,
        fields: HashMap<String, AstNodeId>,
    },
    DictLiteral(Vec<(AstNodeId, AstNodeId)>),
    ArrayLiteral(Vec<AstNodeId>),
    ArrayLiteralLength(AstNodeId, AstNodeId),
    ReferenceCountLiteral(AstNodeId),
    CellLiteral(AstNodeId),
    Block(Vec<AstNodeId>),
    Deref(AstNodeId),
    Match(MatchDeclaration),

    // Types
    // TODO: unify
    VoidType,
    UniqueType(AstNodeId),
    SharedType(AstNodeId),
    ArrayType(AstNodeId),
    DictType(AstNodeId, AstNodeId),
    RcType(AstNodeId),
    NullableType(AstNodeId),
    CellType(AstNodeId),
    GeneratorType {
        yield_ty: AstNodeId,
        param_ty: AstNodeId,
    },
}

impl AstNodeValue {
    fn name(value: String) -> AstNodeValue {
        AstNodeValue::Name {
            value,
            referenced_id: OnceLock::new(),
        }
    }

    fn is_block_expression(&self) -> bool {
        matches!(
            self,
            AstNodeValue::Block(_)
                | AstNodeValue::If(_)
                | AstNodeValue::While(_, _)
                | AstNodeValue::Loop(_)
                | AstNodeValue::Match(_)
        )
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

impl BinOp {
    pub fn binding_power(&self) -> u8 {
        match self {
            BinOp::Dot => DOT,
            BinOp::Index => 255,
            BinOp::Concat => CONCAT,
            BinOp::NullCoalesce => NULL_COALESCE,
            BinOp::NullChaining => NULL_CHAINING,
            BinOp::Add | BinOp::Subtract => SUM,
            BinOp::Multiply | BinOp::Divide => FACTOR,
            BinOp::LessThan
            | BinOp::GreaterThan
            | BinOp::LessEqualThan
            | BinOp::GreaterEqualThan
            | BinOp::EqualTo
            | BinOp::NotEquals => COMPARE,
            BinOp::Assignment
            | BinOp::AddAssign
            | BinOp::SubtractAssign
            | BinOp::MultiplyAssign
            | BinOp::DivideAssign => ASSIGNMENT,
            BinOp::BooleanAnd => BOOLEAN_AND,
            BinOp::BooleanOr => BOOLEAN_OR,
        }
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(Box<Token>, &'static str),
    UnexpectedEndOfInput(SourceMarker, &'static str),
    MissingTypeForParam(SourceMarker),
    ExpectedSemicolonAfterExpression(SourceRange),
    TokenError(LexError),
}

impl From<LexError> for ParseError {
    fn from(value: LexError) -> Self {
        ParseError::TokenError(value)
    }
}

impl Error for ParseError {
    fn source(&self) -> Option<&(dyn Error + 'static)> {
        match self {
            Self::TokenError(t) => Some(t),
            _ => None,
        }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.contents().fmt(f)
    }
}

impl Diagnostic for ParseError {
    fn contents(&self) -> DiagnosticContents {
        DiagnosticContents::Scalar(match self {
            ParseError::UnexpectedToken(token, message) => {
                DiagnosticMarker::error(token.range.clone(), message)
            }
            ParseError::UnexpectedEndOfInput(range, message) => {
                DiagnosticMarker::error(SourceRange::new(range.clone(), range), message)
            }
            ParseError::MissingTypeForParam(marker) => DiagnosticMarker::error(
                SourceRange::new(marker.clone(), marker),
                "expected type for parameter",
            ),
            ParseError::TokenError(token) => return token.contents(),
            ParseError::ExpectedSemicolonAfterExpression(range) => {
                DiagnosticMarker::error(range.clone(), "expected semicolon after expression")
            }
        })
    }
}

type TokenIterInner<'a> = &'a mut dyn Iterator<Item = Result<Token, LexError>>;
type TokenIter<'a> = Peekable<TokenIterInner<'a>>;

pub struct ParsedFile {
    pub arena: AstArena,
    pub top_level: Vec<AstNode>,
}

impl ParsedFile {
    pub fn parse(
        mut source: impl Iterator<Item = Result<Token, LexError>>,
    ) -> Result<Self, ParseError> {
        let mut source = (&mut source as TokenIterInner<'_>).peekable();

        let mut arena = AstArena::default();
        let mut top_level = Vec::new();

        while let Some(lexeme) = peek_token_optional(&mut source)? {
            let cursor = lexeme.range.start();
            let statement = statement(&mut source, &mut arena, &cursor)?;

            top_level.push(statement);
        }

        Ok(ParsedFile { arena, top_level })
    }

    pub fn iter(&self) -> impl Iterator<Item = &AstNode> {
        self.arena.iter().chain(self.top_level.iter())
    }

    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut AstNode> {
        self.arena.iter_mut().chain(self.top_level.iter_mut())
    }
}

fn add_node(context: &mut AstArena, node: AstNode) -> AstNodeId {
    context.add(node)
}

fn statement(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    Ok(
        match peek_token(source, cursor, "expected let, fn, or expression")?.value {
            TokenValue::Let
            | TokenValue::Const
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
                    TokenValue::Let => variable_declaration(source, context, &cursor)?,
                    TokenValue::Borrow => borrow_declaration(source, context, &cursor)?,
                    TokenValue::Const => const_declaration(source, context, &cursor)?,
                    TokenValue::Import => {
                        let statement = import_declaration(source, &cursor)?;
                        if let Some(Token {
                            value: TokenValue::Semicolon,
                            ..
                        }) = peek_token_optional(source)?
                        {
                            source.next();
                        }

                        statement
                    }
                    TokenValue::Extern => extern_function_declaration(source, context, &cursor)?,
                    TokenValue::Function => function_declaration(source, context, &cursor, false)?,
                    TokenValue::Gen => {
                        let token = assert_next_lexeme_eq(
                            source,
                            TokenValue::Function,
                            &cursor,
                            "expected fn after gen",
                        )?;
                        let cursor = token.range.end();
                        function_declaration(source, context, &cursor, true)?
                    }
                    TokenValue::Struct => struct_declaration(source, context, &cursor)?,
                    TokenValue::Union => union_declaration(source, context, &cursor)?,
                    TokenValue::Interface => interface_declaration(source, context, &cursor)?,
                    TokenValue::Return => {
                        let statement = return_declaration(source, context, &cursor)?;
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
                    let provenance = SourceRange::new(expr.provenance.start(), &token.range.end());
                    let expr = add_node(context, expr);
                    AstNode::new(AstNodeValue::Statement(expr), provenance)
                } else if !expr.value.is_block_expression() {
                    // Ensure next token is a valid bounadry - otherwise the expression requires a
                    // closing semicolon. an EOF is a valid boundary
                    if let Some(token) = peek_token_optional(source)? {
                        if !matches!(
                            token.value,
                            TokenValue::Semicolon | TokenValue::Comma | TokenValue::CloseBracket,
                        ) {
                            return Err(ParseError::ExpectedSemicolonAfterExpression(
                                token.range.clone(),
                            ));
                        }
                    }
                    expr
                } else {
                    expr
                }
            }
        },
    )
}

fn return_declaration(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    if let Some(Token {
        value: TokenValue::Semicolon,
        ..
    }) = peek_token_optional(source)?
    {
        let token = already_peeked_token(source)?;
        return Ok(AstNode::new(
            AstNodeValue::Return(None),
            SourceRange::new(cursor.clone(), &token.range.end()),
        ));
    }
    let value = expression(source, context, cursor, true)?;
    let provenance = value.provenance.clone();
    let value = add_node(context, value);

    Ok(AstNode::new(AstNodeValue::Return(Some(value)), provenance))
}

fn struct_declaration(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let start = cursor;
    let (name, provenance) = word(source, cursor, "expected name after 'struct'")?;
    let cursor = provenance.end();
    let (properties, cursor) = property_list(source, &cursor)?;
    let (end, fields, associated_functions) =
        interface_or_struct_body(source, context, &cursor, false)?;

    Ok(AstNode::new(
        AstNodeValue::StructDeclaration(StructDeclarationValue {
            name,
            fields,
            associated_functions,
            properties,
        }),
        SourceRange::new(start.clone(), &end),
    ))
}

fn property_list(
    source: &mut TokenIter,
    cursor: &SourceMarker,
) -> Result<(Vec<String>, SourceMarker), ParseError> {
    let mut cursor = cursor.clone();
    Ok((
        if peek_token(source, &cursor, "unexpected EOL in struct")?.value == TokenValue::Colon {
            cursor = already_peeked_token(source)?.range.end();
            let mut properties = Vec::new();
            loop {
                let (word, range) = word(source, &cursor, "expected property name")?;
                properties.push(word);
                cursor = range.end();
                if peek_token(source, &cursor, "unexpected EOL in struct properties")?.value
                    == TokenValue::Comma
                {
                    cursor = already_peeked_token(source)?.range.end();
                }
                if peek_token(source, &cursor, "unexpected EOL in struct properties")?.value
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

fn interface_declaration(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let (name, provenance) = word(source, cursor, "expected name after 'interface'")?;
    let (end, _, associated_functions) =
        interface_or_struct_body(source, context, &provenance.end(), true)?;

    Ok(AstNode::new(
        AstNodeValue::InterfaceDeclaration(InterfaceDeclarationValue {
            name,
            associated_functions,
        }),
        SourceRange::new(cursor.clone(), &end),
    ))
}

fn interface_or_struct_body(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
    is_interface: bool,
) -> Result<(SourceMarker, Vec<NameAndType>, Vec<AstNodeId>), ParseError> {
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
            &cursor,
            "expected either fields or close bracket",
        )? {
            break;
        }

        if peek_token(source, &cursor, "expected associated function, field, or }")?.value
            == TokenValue::Function
        {
            let start = cursor;
            let token = already_peeked_token(source)?;
            cursor = token.range.end();
            let FunctionHeader {
                name,
                self_param,
                params,
                returns,
                end,
            } = function_header(source, context, &cursor)?;
            cursor = end;

            let next = peek_token(source, &cursor, "expected ',', }, or body")?;
            if is_interface
                && (next.value == TokenValue::Comma || next.value == TokenValue::CloseBracket)
            {
                if next.value == TokenValue::Comma {
                    already_peeked_token(source)?;
                }
                associated_functions.push(context.add(AstNode::new(
                    AstNodeValue::RequiredFunction(FunctionHeaderValue {
                        name,
                        self_param,
                        params,
                        returns,
                    }),
                    SourceRange::new(start, &cursor),
                )));
            } else {
                let token = assert_next_lexeme_eq(
                    source,
                    TokenValue::OpenBracket,
                    &cursor,
                    "expected open bracket to start function body",
                )?;
                cursor = token.range.end();
                let body = block(source, context, &cursor)?;
                cursor = body.provenance.end();
                let body = add_node(context, body);
                associated_functions.push(context.add(AstNode::new(
                    AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                        name,
                        self_param,
                        params: params.into_iter().map(|p| (VariableID::new(), p)).collect(),
                        returns,
                        body,
                        is_extern: false,
                        is_coroutine: false,
                    }),
                    SourceRange::new(start, &cursor),
                )));
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
                name_and_type_hint(source, context, &cursor, "expected parameter")?;
            cursor = range.end();
            let kind = type_hint.ok_or(ParseError::MissingTypeForParam(cursor.clone()))?;
            let end = kind.provenance.end();
            let kind = add_node(context, kind);
            fields.push(NameAndType {
                name,
                ty: kind,
                provenance: SourceRange::new(range.start(), &end),
            });

            let (should_end, range) = comma_or_end_list(
                source,
                TokenValue::CloseBracket,
                &cursor,
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

fn union_declaration(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let (name, mut provenance) = word(source, cursor, "expected name after 'union'")?;
    let cursor = provenance.end();
    let (properties, mut cursor) = property_list(source, &cursor)?;

    cursor = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        &cursor,
        "expected open bracket to start variants",
    )?
    .range
    .end();

    let mut variants = Vec::new();

    let mut closed = false;
    while !closed {
        let (name, name_range) = word(source, &cursor, "expected variant name")?;
        cursor = name_range.end();
        if peek_token(source, &cursor, "unexpected EOF in union declaration")?.value
            == TokenValue::OpenParen
        {
            let paren = already_peeked_token(source)?;
            let ty = type_expression(source, context, &paren.range.end())?;
            let end = ty.provenance.end();
            let ty = add_node(context, ty);
            let paren = assert_next_lexeme_eq(
                source,
                TokenValue::CloseParen,
                &end,
                "expected ) after variant type",
            )?;
            cursor = paren.range.end();

            variants.push(UnionDeclarationVariant::WithValue(NameAndType {
                name,
                ty,
                provenance: SourceRange::new(name_range.start(), &end),
            }));
        } else {
            variants.push(UnionDeclarationVariant::WithoutValue(name));
        }

        let (should_end, range) = comma_or_end_list(
            source,
            TokenValue::CloseBracket,
            &cursor,
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

fn extern_function_declaration(
    source: &mut TokenIter,
    context: &mut AstArena,
    start: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let mut provenance = assert_next_lexeme_eq(
        source,
        TokenValue::Function,
        start,
        "expected 'fn' after 'extern'",
    )?
    .range;
    let FunctionHeader {
        name,
        self_param,
        params,
        returns,
        end,
    } = function_header(source, context, &provenance.end())?;
    provenance.set_end(end.clone());

    let next = next_token(source, &end, "expected ; or { after extern fn decl")?;
    let (value, end) = match &next.value {
        TokenValue::Semicolon => (
            AstNodeValue::ExternFunctionBinding(FunctionHeaderValue {
                name,
                self_param,
                params,
                returns,
            }),
            next.range.end(),
        ),
        TokenValue::OpenBracket => {
            let body = block(source, context, &next.range.end())?;
            let end = body.provenance.end();
            (
                AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
                    name,
                    self_param,
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

fn function_declaration(
    source: &mut TokenIter,
    context: &mut AstArena,
    start: &SourceMarker,
    is_generator: bool,
) -> Result<AstNode, ParseError> {
    let FunctionHeader {
        name,
        self_param,
        params,
        returns,
        end,
    } = function_header(source, context, start)?;
    let mut provenance = SourceRange::new(start.clone(), &end);
    let next_token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        start,
        "expected { after function declaration",
    )?;
    let body = block(source, context, &next_token.range.end())?;
    provenance.set_end(body.provenance.end());

    Ok(AstNode::new(
        AstNodeValue::FunctionDeclaration(FunctionDeclarationValue {
            name,
            self_param,
            params: params.into_iter().map(|p| (VariableID::new(), p)).collect(),
            returns,
            body: add_node(context, body),
            is_extern: false,
            is_coroutine: is_generator,
        }),
        provenance,
    ))
}

struct FunctionHeader {
    name: String,
    self_param: Option<SelfParameter>,
    params: Vec<NameAndType>,
    returns: Option<AstNodeId>,
    end: SourceMarker,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum SelfParameter {
    Unique,
    Shared,
    Owned,
}

fn function_header(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<FunctionHeader, ParseError> {
    let (name, provenance) = word(source, cursor, "expected name after 'fn'")?;

    let open_paren = assert_next_lexeme_eq(
        source,
        TokenValue::OpenParen,
        &provenance.end(),
        "expected open parenthesis to start parameters",
    )?;
    let mut cursor = open_paren.range.end();

    let self_param =
        match peek_token(source, &cursor, "expected either parameters or close paren")?.value {
            TokenValue::Unique => {
                cursor = already_peeked_token(source)?.range.end();
                cursor = assert_next_lexeme_eq(
                    source,
                    TokenValue::SelfKeyword,
                    &cursor,
                    "expected self after unique",
                )?
                .range
                .end();
                Some(SelfParameter::Unique)
            }
            TokenValue::Ref => {
                cursor = already_peeked_token(source)?.range.end();
                cursor = assert_next_lexeme_eq(
                    source,
                    TokenValue::SelfKeyword,
                    &cursor,
                    "expected self after ref",
                )?
                .range
                .end();
                Some(SelfParameter::Shared)
            }
            TokenValue::SelfKeyword => {
                cursor = already_peeked_token(source)?.range.end();
                Some(SelfParameter::Owned)
            }
            _ => None,
        };

    if self_param.is_some() {
        let next = peek_token(source, &cursor, "expected comma or close paren")?;
        if next.value == TokenValue::Comma {
            cursor = already_peeked_token(source)?.range.end();
        }
    }

    let mut params = Vec::new();
    loop {
        let token = peek_token(source, &cursor, "expected either parameters or close paren")?;
        cursor = token.range.start();
        match token.value {
            TokenValue::CloseParen => break,
            TokenValue::Comma => {
                source.next();
            }
            _ => {
                let (name, range, type_hint) =
                    name_and_type_hint(source, context, &cursor, "expected parameter")?;
                cursor = range.end();
                let kind = type_hint.ok_or(ParseError::MissingTypeForParam(cursor.clone()))?;
                let end = kind.provenance.end();
                let kind = add_node(context, kind);

                params.push(NameAndType {
                    name,
                    ty: kind,
                    provenance: SourceRange::new(range.start(), &end),
                });
            }
        }
    }
    let next_token = assert_next_lexeme_eq(
        source,
        TokenValue::CloseParen,
        &cursor,
        "expected closing parenthesis to end parameters",
    )?;
    cursor = next_token.range.end();
    let returns = if let Token {
        value: TokenValue::Colon,
        range,
    } = peek_token(source, &cursor, "expected body after function declaration")?
    {
        let start = range.start();
        cursor = range.end();
        source.next();
        let kind = type_expression(source, context, &start)?;
        let kind = add_node(context, kind);

        Some(kind)
    } else {
        None
    };

    Ok(FunctionHeader {
        name,
        self_param,
        params,
        returns,
        end: cursor,
    })
}

fn import_declaration(source: &mut TokenIter, start: &SourceMarker) -> Result<AstNode, ParseError> {
    let (name, provenance) =
        if peek_token(source, start, "expected import path")?.value == TokenValue::SelfKeyword {
            ("self".to_string(), already_peeked_token(source)?.range)
        } else {
            word(source, start, "expected word after 'import'")?
        };
    let mut cursor = provenance.end();
    let mut components = vec![name];

    loop {
        let next = next_token(source, &cursor, "expected . or ; in import declaration")?;
        cursor = next.range.end();
        match &next.value {
            TokenValue::Semicolon => break,
            TokenValue::Period => {
                let (name, provenance) = word(source, &cursor, "expected word after . in import")?;
                cursor = provenance.end();
                components.push(name);
            }
            _ => {
                return Err(ParseError::UnexpectedToken(
                    Box::new(next),
                    "expected . or ; in import declaration",
                ))
            }
        }
    }

    Ok(AstNode::new(
        AstNodeValue::Import(components),
        SourceRange::new(start.clone(), &provenance.end()),
    ))
}

fn variable_declaration(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let (name, mut provenance, type_hint) = name_and_type_hint(
        source,
        context,
        cursor,
        "expected word after 'let' in declaration",
    )?;
    let cursor = assert_next_lexeme_eq(
        source,
        TokenValue::Assign,
        &provenance.end(),
        "expected = after let binding target",
    )?
    .range
    .end();
    let value = expression(source, context, &cursor, true)?;
    provenance.set_end(value.provenance.end());

    let type_hint = type_hint.map(|type_hint| add_node(context, type_hint));

    assert_next_lexeme_eq(
        source,
        TokenValue::Semicolon,
        &provenance.end(),
        "expected ; after 'let' statement",
    )?;

    Ok(AstNode::new(
        AstNodeValue::Declaration(name, type_hint, add_node(context, value), VariableID::new()),
        provenance,
    ))
}

fn borrow_declaration(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let start = cursor;
    let (name, span) = word(source, cursor, "expected name after borrow")?;
    let mut cursor = span.end();
    cursor = assert_next_lexeme_eq(
        source,
        TokenValue::Assign,
        &cursor,
        "expected = after let binding target",
    )?
    .range
    .end();
    let rhs = expression(source, context, &cursor, true)?;
    cursor = rhs.provenance.end();
    let rhs = context.add(rhs);

    assert_next_lexeme_eq(
        source,
        TokenValue::Semicolon,
        &cursor,
        "expected ; after 'borrow' statement",
    )?;

    Ok(AstNode::new(
        AstNodeValue::BorrowDeclaration(name, rhs, VariableID::new()),
        SourceRange::new(start.clone(), &cursor),
    ))
}

fn const_declaration(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let (name, mut provenance, type_hint) = name_and_type_hint(
        source,
        context,
        cursor,
        "expected word after 'let' in declaration",
    )?;
    let cursor = assert_next_lexeme_eq(
        source,
        TokenValue::Assign,
        &provenance.end(),
        "expected = after let binding target",
    )?
    .range
    .end();
    let value = expression(source, context, &cursor, true)?;
    provenance.set_end(value.provenance.end());

    let type_hint = type_hint.map(|type_hint| add_node(context, type_hint));

    assert_next_lexeme_eq(
        source,
        TokenValue::Semicolon,
        &provenance.end(),
        "expected ; after 'let' statement",
    )?;

    Ok(AstNode::new(
        AstNodeValue::ConstDeclaration {
            name,
            type_hint,
            value: add_node(context, value),
            variable_id: ConstantID::new(),
        },
        provenance,
    ))
}

fn name_and_type_hint(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
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
        let type_hint = type_expression(source, context, &cursor)?;
        provenance.set_end(type_hint.provenance.end());
        Some(type_hint)
    } else {
        None
    };

    Ok((name, provenance, type_hint))
}

fn type_expression(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let next = next_token(source, cursor, "expected type")?;
    let node = match next.value {
        TokenValue::Void => AstNode::new(AstNodeValue::VoidType, next.range),
        ptr @ (TokenValue::Unique | TokenValue::Ref) => {
            let subtype = type_expression(source, context, &next.range.end())?;
            let end = subtype.provenance.end();
            let subtype = add_node(context, subtype);
            AstNode::new(
                match ptr {
                    TokenValue::Unique => AstNodeValue::UniqueType(subtype),
                    TokenValue::Ref => AstNodeValue::SharedType(subtype),
                    _ => unreachable!(),
                },
                SourceRange::new(next.range.start(), &end),
            )
        }
        TokenValue::Dict => {
            let token = assert_next_lexeme_eq(
                source,
                TokenValue::OpenSquare,
                &next.range.end(),
                "expected [ after dict type",
            )?;
            let key = type_expression(source, context, &token.range.end())?;
            let end = key.provenance.end();
            let key = add_node(context, key);

            let token = assert_next_lexeme_eq(
                source,
                TokenValue::Comma,
                &end,
                "expected , after dict key type",
            )?;

            let value = type_expression(source, context, &token.range.end())?;
            let end = value.provenance.end();
            let value = add_node(context, value);

            let token = assert_next_lexeme_eq(
                source,
                TokenValue::CloseSquare,
                &end,
                "expected ] after dict type",
            )?;

            AstNode::new(
                AstNodeValue::DictType(key, value),
                SourceRange::new(next.range.start(), &token.range.end()),
            )
        }
        TokenValue::List => {
            let token = assert_next_lexeme_eq(
                source,
                TokenValue::OpenSquare,
                &next.range.end(),
                "expected [ after array type",
            )?;

            let value = type_expression(source, context, &token.range.end())?;
            let end = value.provenance.end();
            let value = add_node(context, value);

            let token = assert_next_lexeme_eq(
                source,
                TokenValue::CloseSquare,
                &end,
                "expected ] after array type",
            )?;

            AstNode::new(
                AstNodeValue::ArrayType(value),
                SourceRange::new(next.range.start(), &token.range.end()),
            )
        }
        TokenValue::Rc => {
            let token = assert_next_lexeme_eq(
                source,
                TokenValue::OpenSquare,
                &next.range.end(),
                "expected [ after rc type",
            )?;

            let value = type_expression(source, context, &token.range.end())?;
            let end = value.provenance.end();
            let value = add_node(context, value);

            let token = assert_next_lexeme_eq(
                source,
                TokenValue::CloseSquare,
                &end,
                "expected ] after rc type",
            )?;

            AstNode::new(
                AstNodeValue::RcType(value),
                SourceRange::new(next.range.start(), &token.range.end()),
            )
        }
        TokenValue::Cell => {
            let token = assert_next_lexeme_eq(
                source,
                TokenValue::OpenSquare,
                &next.range.end(),
                "expected [ after cell type",
            )?;
            let inner_ty = type_expression(source, context, &token.range.end())?;
            let token = assert_next_lexeme_eq(
                source,
                TokenValue::CloseSquare,
                &inner_ty.provenance.end(),
                "expected ] after cell type",
            )?;

            AstNode::new(
                AstNodeValue::CellType(context.add(inner_ty)),
                SourceRange::new(next.range.start(), &token.range.end()),
            )
        }
        TokenValue::Word(name) => match name.as_str() {
            "generator" => {
                let token = assert_next_lexeme_eq(
                    source,
                    TokenValue::OpenSquare,
                    &next.range.end(),
                    "expected [ after generator type",
                )?;

                let yield_ty = type_expression(source, context, &token.range.end())?;
                let cursor = yield_ty.provenance.end();
                let yield_ty = add_node(context, yield_ty);

                assert_next_lexeme_eq(
                    source,
                    TokenValue::Comma,
                    &cursor,
                    "expected , after yield type",
                )?;

                let param_ty = type_expression(source, context, &token.range.end())?;
                let cursor = param_ty.provenance.end();
                let param_ty = add_node(context, param_ty);

                let token = assert_next_lexeme_eq(
                    source,
                    TokenValue::CloseSquare,
                    &cursor,
                    "expected ] after generator type",
                )?;

                AstNode::new(
                    AstNodeValue::GeneratorType { yield_ty, param_ty },
                    SourceRange::new(next.range.start(), &token.range.end()),
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
        let start = node.provenance.start();
        let inner = add_node(context, node);
        let provenance = SourceRange::new(start, &question_mark.range.end());
        Ok(AstNode::new(AstNodeValue::NullableType(inner), provenance))
    } else {
        Ok(node)
    }
}

fn expression(
    source: &mut TokenIter,
    context: &mut AstArena,
    provenance: &SourceMarker,
    can_be_struct: bool,
) -> Result<AstNode, ParseError> {
    expression_pratt(source, context, provenance, 0, can_be_struct)
}

// Pratt parser based on https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html

fn expression_pratt(
    source: &mut TokenIter,
    context: &mut AstArena,
    start: &SourceMarker,
    min_binding: u8,
    can_be_struct: bool,
) -> Result<AstNode, ParseError> {
    let Token { value, range } = next_token(source, start, "expected expression")?;
    let start = range.start();
    let cursor = range.end();
    let mut left = match value {
        // TODO: should this be treated as a unary operator instead?
        TokenValue::Minus => {
            let (int, range) = integer(source, &cursor, "expected digit after negative sign")?;
            try_decimal(source, -(int as i64), range)?
        }
        TokenValue::OpenParen => {
            let left = expression_pratt(source, context, &cursor, 0, can_be_struct)?;
            assert_next_lexeme_eq(
                source,
                TokenValue::CloseParen,
                &left.provenance.end(),
                "expected ) to match (",
            )?;

            left
        }
        TokenValue::Dict => dict_literal(source, context, &cursor)?,
        TokenValue::List => array_literal(source, context, &cursor, can_be_struct)?,
        TokenValue::Rc => rc_literal(source, context, &cursor)?,
        TokenValue::Cell => cell_literal(source, context, &cursor)?,
        token @ (TokenValue::If | TokenValue::While) => {
            if_or_while(source, context, token, &cursor)?
        }
        TokenValue::Case => match_statement(source, context, &cursor)?,
        TokenValue::Loop => parse_loop(source, context, &cursor)?,
        TokenValue::OpenBracket => block(source, context, &cursor)?,
        // Atoms
        TokenValue::True => AstNode::new(AstNodeValue::Bool(true), range),
        TokenValue::False => AstNode::new(AstNodeValue::Bool(false), range),
        TokenValue::Word(word) => AstNode::new(AstNodeValue::name(word), range),
        TokenValue::SelfKeyword => AstNode::new(AstNodeValue::name("self".to_string()), range),
        TokenValue::Null => AstNode::new(AstNodeValue::Null, range),
        TokenValue::CharacterLiteral(c) => AstNode::new(AstNodeValue::CharLiteral(c), range),
        TokenValue::StringLiteral(s) => AstNode::new(AstNodeValue::StringLiteral(s), range),
        TokenValue::Int(int) => try_decimal(source, int as i64, range)?,
        TokenValue::Yield => {
            let next = peek_token(source, &cursor, "expected yielded value after yield")?;
            if next.value.is_expression_boundary() {
                let range = SourceRange::new(start.clone(), &cursor);
                AstNode::new(AstNodeValue::Yield(None), range)
            } else {
                let inner = expression(source, context, &cursor, true)?;
                let range = SourceRange::new(start.clone(), &inner.provenance.end());
                let inner = context.add(inner);
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
            let right = expression_pratt(source, context, &cursor, right_binding, can_be_struct)?;
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
                SourceRange::new(range.start(), &end),
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
                        &end,
                        "expected ) or next argument",
                    )?;

                    while !closed {
                        let argument = expression(source, context, &end, can_be_struct)?;
                        end = argument.provenance.end();
                        arguments.push(context.add(argument));

                        let (should_break, new_end) = comma_or_end_list(
                            source,
                            TokenValue::CloseParen,
                            &end,
                            "expected comma or ) to end function call",
                        )?;
                        end = new_end.end();
                        closed = should_break;
                    }

                    left = AstNode::new(
                        AstNodeValue::Call(add_node(context, left), arguments),
                        SourceRange::new(start.clone(), &end),
                    );
                }
                TokenValue::OpenSquare => {
                    let index = expression(source, context, &range.end(), can_be_struct)?;
                    let Token { range, .. } = assert_next_lexeme_eq(
                        source,
                        TokenValue::CloseSquare,
                        &index.provenance.end(),
                        "expected ] to follow array index",
                    )?;
                    left = AstNode::new(
                        AstNodeValue::BinExpr(
                            BinOp::Index,
                            add_node(context, left),
                            add_node(context, index),
                        ),
                        SourceRange::new(start.clone(), &range.end()),
                    );
                }
                TokenValue::OpenBracket if can_be_struct => {
                    // TODO: extract struct literals into their own method
                    let mut end = range.end();
                    let mut fields = HashMap::new();

                    loop {
                        if TokenValue::CloseBracket
                            == peek_token(source, &end, "expected } or next field")?.value
                        {
                            source.next();
                            break;
                        }

                        let (field, field_range) =
                            word(source, &end, "expected field in struct literal")?;
                        if let lex @ (TokenValue::Comma | TokenValue::CloseBracket) =
                            &peek_token(source, &end, "expected comma or } to end struct literal")?
                                .value
                        {
                            let lex = lex.clone();
                            source.next();
                            let argument =
                                AstNode::new(AstNodeValue::name(field.clone()), field_range);
                            fields.insert(field, context.add(argument));
                            if lex == TokenValue::CloseBracket {
                                break;
                            }
                        } else {
                            let token = assert_next_lexeme_eq(
                                source,
                                TokenValue::Colon,
                                &end,
                                "expected colon after field name",
                            )?;
                            let cursor = token.range.end();
                            let argument = expression(source, context, &cursor, can_be_struct)?;
                            end = argument.provenance.end();
                            fields.insert(field, context.add(argument));

                            if let TokenValue::Comma = peek_token(
                                source,
                                &end,
                                "expected comma or } to end struct literal",
                            )?
                            .value
                            {
                                source.next();
                            } else {
                                assert_next_lexeme_eq(
                                    source,
                                    TokenValue::CloseBracket,
                                    &end,
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
                            name: context.add(left),
                            fields,
                        },
                        SourceRange::new(start.clone(), &end),
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

            let right = expression_pratt(
                source,
                context,
                &current.end(),
                right_binding,
                can_be_struct,
            )?;

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
                SourceRange::new(start, &end),
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

fn array_literal(
    source: &mut TokenIter,
    context: &mut AstArena,
    start: &SourceMarker,
    can_be_struct: bool,
) -> Result<AstNode, ParseError> {
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenSquare,
        start,
        "expected [ to start list literal",
    )?;
    let expr = expression(source, context, &token.range.start(), can_be_struct)?;
    let separator = next_token(
        source,
        &expr.provenance.start(),
        "expected comma, semicolon or ]",
    )?;
    match separator.value {
        TokenValue::Comma => {
            let mut end = separator.range.end();
            let mut children = vec![context.add(expr)];

            let mut closed = peek_for_closed(
                source,
                TokenValue::CloseSquare,
                &end,
                "expected ] or next argument",
            )?;

            while !closed {
                let peeked = peek_token(source, &end, "expected , or ]")?;
                if TokenValue::CloseSquare == peeked.value {
                    source.next();
                    break;
                }
                end = peeked.range.end();
                let expr = expression(source, context, &end, can_be_struct)?;
                end = expr.provenance.end();
                children.push(context.add(expr));

                let (should_break, new_end) = comma_or_end_list(
                    source,
                    TokenValue::CloseSquare,
                    &end,
                    "expected comma or ] to end list literal",
                )?;
                end = new_end.end();
                closed = should_break;
            }
            Ok(AstNode::new(
                AstNodeValue::ArrayLiteral(children),
                SourceRange::new(start.clone(), &end),
            ))
        }
        TokenValue::Semicolon => {
            let length = expression(source, context, &separator.range.end(), true)?;
            let close = assert_next_lexeme_eq(
                source,
                TokenValue::CloseSquare,
                &length.provenance.end(),
                "expected ] after list length in list literal",
            )?;
            let expr = add_node(context, expr);
            let length = add_node(context, length);
            Ok(AstNode::new(
                AstNodeValue::ArrayLiteralLength(expr, length),
                SourceRange::new(start.clone(), &close.range.end()),
            ))
        }
        TokenValue::CloseSquare => Ok(AstNode::new(
            AstNodeValue::ArrayLiteral(vec![context.add(expr)]),
            SourceRange::new(start.clone(), &separator.range.end()),
        )),
        _ => Err(ParseError::UnexpectedToken(
            Box::new(separator),
            "expected comma, semicolon or ]",
        )),
    }
}

fn rc_literal(
    source: &mut TokenIter,
    context: &mut AstArena,
    start: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        start,
        "expected { to start rc literal",
    )?;
    let mut cursor = token.range.end();
    let inner = expression(source, context, &cursor, true)?;
    cursor = inner.provenance.end();
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::CloseBracket,
        &cursor,
        "expected } to end rc literal",
    )?;
    cursor = token.range.end();
    let inner = context.add(inner);
    Ok(AstNode::new(
        AstNodeValue::ReferenceCountLiteral(inner),
        SourceRange::new(start.clone(), &cursor),
    ))
}

fn cell_literal(
    source: &mut TokenIter,
    context: &mut AstArena,
    start: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        start,
        "expected { to start cell literal",
    )?;
    let mut cursor = token.range.end();
    let inner = expression(source, context, &cursor, true)?;
    cursor = inner.provenance.end();
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::CloseBracket,
        &cursor,
        "expected } to end cell literal",
    )?;
    cursor = token.range.end();
    let inner = context.add(inner);
    Ok(AstNode::new(
        AstNodeValue::CellLiteral(inner),
        SourceRange::new(start.clone(), &cursor),
    ))
}

fn dict_literal(
    source: &mut TokenIter,
    context: &mut AstArena,
    start: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        start,
        "expected { to start dict literal",
    )?;
    let mut cursor = token.range.end();
    let mut entries = Vec::new();
    loop {
        let key = match peek_token(source, &cursor, "expected } or next entry")?.value {
            TokenValue::CloseBracket => {
                source.next();
                break;
            }
            TokenValue::OpenSquare => {
                already_peeked_token(source)?;
                let key = expression(source, context, &cursor, true)?;
                let token = assert_next_lexeme_eq(
                    source,
                    TokenValue::CloseSquare,
                    &cursor,
                    "Expected ] after key expression",
                )?;
                cursor = token.range.end();

                key
            }
            _ => {
                let (key, key_range) = word(source, &cursor, "expected key in dict literal")?;
                cursor = key_range.end();

                if let lex @ (TokenValue::Comma | TokenValue::CloseBracket) =
                    &peek_token(source, &cursor, "expected comma or } to end dict literal")?.value
                {
                    let lex = lex.clone();
                    let token = already_peeked_token(source)?;
                    cursor = token.range.end();

                    let value = AstNode::new(AstNodeValue::name(key.clone()), key_range.clone());
                    let key = AstNode::new(AstNodeValue::StringLiteral(key), key_range);

                    entries.push((context.add(key), context.add(value)));

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
            &cursor,
            "Expected : after key in dict",
        )?;
        cursor = token.range.end();
        let value = expression(source, context, &cursor, true)?;
        cursor = value.provenance.end();
        entries.push((context.add(key), context.add(value)));
        let (did_end, range) = comma_or_end_list(
            source,
            TokenValue::CloseBracket,
            &cursor,
            "expected , or } in dict literal",
        )?;
        cursor = range.end();
        if did_end {
            break;
        }
    }

    Ok(AstNode::new(
        AstNodeValue::DictLiteral(entries),
        SourceRange::new(start.clone(), &cursor),
    ))
}

fn if_or_while(
    source: &mut TokenIter,
    context: &mut AstArena,
    is_if_or_while: TokenValue,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let predicate = expression(source, context, cursor, false)?;
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        &predicate.provenance.end(),
        "expected { after predicate",
    )?;
    let cursor = token.range.end();
    let if_block = block(source, context, &cursor)?;

    let predicate_ptr = add_node(context, predicate);
    let provenance = SourceRange::new(cursor.clone(), &if_block.provenance.end());
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
                    &else_token.range.end(),
                    "expected { or if after else",
                )?;
                let else_block = match next_token.value {
                    TokenValue::OpenBracket => {
                        let token = already_peeked_token(source)?;
                        block(source, context, &token.range.end())?
                    }
                    TokenValue::If => {
                        let token = already_peeked_token(source)?;
                        let if_node =
                            if_or_while(source, context, TokenValue::If, &token.range.end())?;
                        let provenance = if_node.provenance.clone();
                        AstNode::new(AstNodeValue::Block(vec![context.add(if_node)]), provenance)
                    }
                    _ => {
                        return Err(ParseError::UnexpectedToken(
                            Box::new(already_peeked_token(source)?),
                            "expected { or if after else",
                        ))
                    }
                };
                let provenance = SourceRange::new(cursor.clone(), &else_block.provenance.end());
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

fn match_statement(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let start = cursor;
    let match_value = expression(source, context, cursor, false)?;
    let mut cursor = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        &match_value.provenance.end(),
        "expected { after case value",
    )?
    .range
    .end();
    let match_value = context.add(match_value);
    let mut cases = Vec::new();
    while peek_token(source, &cursor, "unexpected EOF in case statement")?.value
        != TokenValue::CloseBracket
    {
        let next_case = match_case_statement(source, context, &cursor)?;
        cursor = next_case.provenance.end();
        cases.push(next_case);
    }
    cursor = already_peeked_token(source)?.range.end();

    Ok(AstNode::new(
        AstNodeValue::Match(MatchDeclaration {
            value: match_value,
            cases,
        }),
        SourceRange::new(start.clone(), &cursor),
    ))
}

fn match_case_statement(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<MatchCaseDeclaration, ParseError> {
    let start = cursor;
    let variant = match_case_variant(source, cursor)?;
    let mut cursor = variant.provenance.end();
    let mut variants = vec![variant];

    while peek_token(source, &cursor, "expected | or => after case variant")?.value
        != TokenValue::CaseRocket
    {
        cursor = assert_next_lexeme_eq(
            source,
            TokenValue::VerticalPipe,
            &cursor,
            "expected | or => after case variant",
        )?
        .range
        .end();
        let variant = match_case_variant(source, &cursor)?;
        cursor = variant.provenance.end();
        variants.push(variant);
    }
    cursor = already_peeked_token(source)?.range.end();
    let body = if peek_token(source, &cursor, "expected { or expression after =>")?.value
        == TokenValue::OpenBracket
    {
        cursor = already_peeked_token(source)?.range.end();
        let body = block(source, context, &cursor)?;
        cursor = body.provenance.end();
        body
    } else {
        let expr = expression(source, context, &cursor, true)?;
        cursor = assert_next_lexeme_eq(
            source,
            TokenValue::Comma,
            &expr.provenance.end(),
            "expected , after variant expression",
        )?
        .range
        .end();
        expr
    };

    Ok(MatchCaseDeclaration {
        variants,
        var_id: VariableID::new(),
        body: context.add(body),
        provenance: SourceRange::new(start.clone(), &cursor),
    })
}

fn match_case_variant(
    source: &mut TokenIter,
    cursor: &SourceMarker,
) -> Result<MatchCaseVariant, ParseError> {
    let start = cursor;
    let (name, range) = word(source, cursor, "expected name of case variant")?;
    let mut cursor = range.end();

    let mut bindings = Vec::new();
    if peek_token(source, &cursor, "expected (, | or => after case name")?.value
        == TokenValue::OpenParen
    {
        loop {
            cursor = already_peeked_token(source)?.range.end();
            let (binding, range) = word(source, &cursor, "expected binding in case statement")?;
            bindings.push(binding);
            cursor = range.end();

            let (list_ended, range) =
                comma_or_end_list(source, TokenValue::CloseParen, &cursor, "expected , or )")?;
            cursor = range.end();
            if list_ended {
                break;
            }
        }
    }

    Ok(MatchCaseVariant {
        name,
        bindings,
        provenance: SourceRange::new(start.clone(), &cursor),
        ty: OnceLock::new(),
    })
}

fn parse_loop(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let token = assert_next_lexeme_eq(
        source,
        TokenValue::OpenBracket,
        cursor,
        "expected { after loop",
    )?;

    let body = block(source, context, &token.range.start())?;
    let provenance = SourceRange::new(cursor.clone(), &body.provenance.end());
    let body = add_node(context, body);

    Ok(AstNode::new(AstNodeValue::Loop(body), provenance))
}

fn block(
    source: &mut TokenIter,
    context: &mut AstArena,
    cursor: &SourceMarker,
) -> Result<AstNode, ParseError> {
    let mut statements = Vec::new();
    let mut provenance = SourceRange::new(cursor.clone(), cursor);
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
                let statement = statement(source, context, &provenance.end())?;
                provenance.set_end(statement.provenance.end());
                statements.push(context.add(statement));
            }
        }
    }
}

fn integer(
    source: &mut TokenIter,
    cursor: &SourceMarker,
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
        let num = if decimal == 0.0 {
            // can't get the log of 0
            num
        } else if decimal == 1.0 {
            // log of 1 is 0, special-case it
            num + 0.1f64.copysign(num)
        } else {
            num + decimal.copysign(num) * (10f64).powf(-decimal.log(10.0).ceil())
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
    cursor: &SourceMarker,
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
    provenance: &SourceMarker,
    reason: &'static str,
) -> Result<Token, ParseError> {
    skim_off_comments(source);
    let lexeme = source
        .next()
        .ok_or_else(|| ParseError::UnexpectedEndOfInput(provenance.clone(), reason))??;
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
    start: &SourceMarker,
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
    provenance: &SourceMarker,
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
    cursor: &SourceMarker,
    reason: &'static str,
) -> Result<Token, ParseError> {
    skim_off_comments(source);
    let lex = source
        .next()
        .ok_or_else(|| ParseError::UnexpectedEndOfInput(cursor.clone(), reason))??;
    Ok(lex)
}

fn peek_token<'a>(
    source: &'a mut TokenIter,
    cursor: &SourceMarker,
    reason: &'static str,
) -> Result<&'a Token, ParseError> {
    skim_off_comments(source);
    if let Some(token) = source.peek() {
        Ok(token.as_ref().map_err(|e| e.clone())?)
    } else {
        Err(ParseError::UnexpectedEndOfInput(cursor.clone(), reason))
    }
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
