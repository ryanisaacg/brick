use std::fmt;
use std::iter::Peekable;
use thiserror::Error;

use crate::provenance::{SourceMarker, SourceRange};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Token {
    pub value: TokenValue,
    pub range: SourceRange,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} at {}", self.value, self.range)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TokenValue {
    Word(String),
    Int(u64),
    CharacterLiteral(char),
    StringLiteral(String),

    // Math operators
    Plus,
    Minus,
    Asterisk,
    ForwardSlash,
    PlusEquals,
    MinusEquals,
    AsteriskEquals,
    ForwardSlashEquals,

    LessThan,
    GreaterThan,
    LessEqualThan,
    GreaterEqualThan,
    EqualTo,
    NotEquals,

    // Boolean operators
    BooleanAnd,
    BooleanOr,

    // Misc operators
    Period,
    Concat,

    // Nullability
    NullCoalesce,
    NullChaining,

    // Markers
    Assign,
    Colon,
    Comma,
    Semicolon,
    QuestionMark,
    Exclamation,
    CaseRocket,
    VerticalPipe,

    // Braces
    OpenParen,
    CloseParen,
    OpenBracket,
    CloseBracket,
    OpenSquare,
    CloseSquare,

    // Keywords
    If,
    While,
    Loop,
    Let,
    True,
    False,
    Function,
    Gen,
    Import,
    Struct,
    Union,
    Unique,
    Ref,
    Return,
    Extern,
    Null,
    Dict,
    List,
    Rc,
    Interface,
    Yield,
    Void,
    Case,
    Borrow,

    // Comments
    LineComment(String),
}

impl TokenValue {
    /**
     * Created for `yield`, to detect if it's returning a value or standlone
     */
    pub fn is_expression_boundary(&self) -> bool {
        match self {
            TokenValue::Word(_)
            | TokenValue::Int(_)
            | TokenValue::OpenParen
            | TokenValue::OpenBracket
            | TokenValue::CharacterLiteral(_)
            | TokenValue::OpenSquare
            | TokenValue::If
            | TokenValue::While
            | TokenValue::Loop
            | TokenValue::Dict
            | TokenValue::Rc
            | TokenValue::List
            | TokenValue::True
            | TokenValue::False
            | TokenValue::Unique
            | TokenValue::Ref
            | TokenValue::Null
            | TokenValue::Yield
            | TokenValue::Case
            | TokenValue::StringLiteral(_) => false,
            TokenValue::Plus
            | TokenValue::Minus
            | TokenValue::Asterisk
            | TokenValue::ForwardSlash
            | TokenValue::PlusEquals
            | TokenValue::MinusEquals
            | TokenValue::AsteriskEquals
            | TokenValue::ForwardSlashEquals
            | TokenValue::LessThan
            | TokenValue::GreaterThan
            | TokenValue::LessEqualThan
            | TokenValue::GreaterEqualThan
            | TokenValue::EqualTo
            | TokenValue::NotEquals
            | TokenValue::BooleanAnd
            | TokenValue::BooleanOr
            | TokenValue::Period
            | TokenValue::Concat
            | TokenValue::NullCoalesce
            | TokenValue::NullChaining
            | TokenValue::Assign
            | TokenValue::Colon
            | TokenValue::Comma
            | TokenValue::Semicolon
            | TokenValue::QuestionMark
            | TokenValue::Exclamation
            | TokenValue::CaseRocket
            | TokenValue::VerticalPipe
            | TokenValue::CloseParen
            | TokenValue::CloseBracket
            | TokenValue::CloseSquare
            | TokenValue::Let
            | TokenValue::Borrow
            | TokenValue::Function
            | TokenValue::Gen
            | TokenValue::Import
            | TokenValue::Struct
            | TokenValue::Union
            | TokenValue::Return
            | TokenValue::Extern
            | TokenValue::Interface
            | TokenValue::Void
            | TokenValue::LineComment(_) => true,
        }
    }
}

impl fmt::Display for TokenValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenValue::*;
        match self {
            Word(word) => write!(f, "word {}", word),
            Int(int) => write!(f, "int {}", int),
            CharacterLiteral(c) => write!(f, "character literal {}", c),
            StringLiteral(s) => write!(f, "string literal {}", s),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            Asterisk => write!(f, "*"),
            ForwardSlash => write!(f, "/"),
            Assign => write!(f, "="),
            PlusEquals => write!(f, "+="),
            MinusEquals => write!(f, "-="),
            AsteriskEquals => write!(f, "*="),
            ForwardSlashEquals => write!(f, "/="),
            Semicolon => write!(f, ";"),
            Comma => write!(f, ","),
            Colon => write!(f, ":"),
            Period => write!(f, "."),
            Concat => write!(f, "++"),
            OpenParen => write!(f, "("),
            CloseParen => write!(f, ")"),
            OpenBracket => write!(f, "{{"),
            CloseBracket => write!(f, "}}"),
            OpenSquare => write!(f, "["),
            CloseSquare => write!(f, "]"),
            LessThan => write!(f, "<"),
            GreaterThan => write!(f, ">"),
            LessEqualThan => write!(f, "<="),
            GreaterEqualThan => write!(f, ">="),
            EqualTo => write!(f, "=="),
            NotEquals => write!(f, "!="),
            BooleanAnd => write!(f, "and"),
            BooleanOr => write!(f, "or"),
            QuestionMark => write!(f, "?"),
            NullCoalesce => write!(f, "??"),
            NullChaining => write!(f, "?."),
            Exclamation => write!(f, "!"),
            CaseRocket => write!(f, "=>"),
            VerticalPipe => write!(f, "|"),
            Let => write!(f, "keyword let"),
            Borrow => write!(f, "keyword borrow"),
            If => write!(f, "keyword if"),
            While => write!(f, "keyword while"),
            Loop => write!(f, "keyword loop"),
            True => write!(f, "keyword true"),
            False => write!(f, "keyword false"),
            Function => write!(f, "keyword fn"),
            Gen => write!(f, "keyword gen"),
            Import => write!(f, "keyword import"),
            Struct => write!(f, "keyword struct"),
            Union => write!(f, "keyword union"),
            Unique => write!(f, "keyword unique"),
            Ref => write!(f, "keyword ref"),
            Return => write!(f, "keyword return"),
            Extern => write!(f, "keyword extern"),
            Null => write!(f, "keyword null"),
            Dict => write!(f, "keyword dict"),
            Rc => write!(f, "keyword rc"),
            List => write!(f, "keyword list"),
            Interface => write!(f, "keyword interface"),
            Yield => write!(f, "keyword yield"),
            Void => write!(f, "keyword void"),
            Case => write!(f, "keyword case"),
            LineComment(comment) => write!(f, "// {}", comment),
        }
    }
}

#[derive(Clone, Debug, Error)]
pub enum LexError {
    #[error("unexpected character {0} at {1}")]
    UnexpectedStart(char, SourceMarker),
    #[error("illegal null byte in source code at {0}")]
    IllegalNullByte(SourceMarker),
    #[error("unterminated literal in source code starting at {0}")]
    UnterminatedLiteral(SourceMarker),
    #[error("illegal escape sequence at {0}")]
    IllegalEscapeSequence(SourceMarker),
}

pub fn lex<'a>(
    source_name: &'static str,
    source_text: String,
) -> impl 'a + Iterator<Item = Result<Token, LexError>> {
    let source_text = Box::leak(source_text.into_boxed_str());
    let source = source_text.chars().peekable();

    TokenIterator {
        source,
        source_name,
        source_text,
        line: 1,
        offset: 0,
    }
}

struct TokenIterator<T: Iterator<Item = char>> {
    source: Peekable<T>,
    source_name: &'static str,
    source_text: &'static str,
    line: u32,
    offset: u32,
}

impl<T: Iterator<Item = char>> TokenIterator<T> {
    fn next_char(&mut self) -> Option<(char, SourceMarker)> {
        match self.source.next() {
            None => None,
            Some('\n') => {
                self.line += 1;
                self.offset = 0;
                self.next_char()
            }
            Some(chr) => {
                self.offset += 1;
                Some((
                    chr,
                    SourceMarker::new(self.source_name, self.source_text, self.line, self.offset),
                ))
            }
        }
    }

    fn next_char_literal(&mut self, start: SourceMarker) -> Result<(char, SourceRange), LexError> {
        match self
            .next_char()
            .ok_or(LexError::UnterminatedLiteral(start))?
        {
            ('\\', cursor) => {
                let (ch, end) = self
                    .next_char()
                    .ok_or(LexError::UnterminatedLiteral(cursor))?;
                Ok((
                    match ch {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '0' => '\0',
                        '\'' => '\'',
                        '"' => '"',
                        // TODO: byte and unicode escapes
                        _ => return Err(LexError::IllegalEscapeSequence(cursor)),
                    },
                    SourceRange::new(start, end),
                ))
            }
            (ch, start) => Ok((ch, SourceRange::new(start, start))),
        }
    }
}

impl<T: Iterator<Item = char>> Iterator for TokenIterator<T> {
    type Item = Result<Token, LexError>;

    fn next(&mut self) -> Option<Result<Token, LexError>> {
        if let Some((chr, start)) = self.next_char() {
            let mut end = None;
            let value = match chr {
                letter @ ('a'..='z' | 'A'..='Z' | '_') => {
                    let mut word = String::new();
                    word.push(letter);
                    while let Some(candidate) = self.source.peek() {
                        match candidate {
                            letter @ ('a'..='z' | 'A'..='Z' | '_' | '0'..='9') => {
                                word.push(*letter);
                            }
                            _ => break,
                        }
                        let (_, p) = self.next_char().unwrap();
                        end = Some(p);
                    }

                    match word.as_str() {
                        "if" => TokenValue::If,
                        "while" => TokenValue::While,
                        "loop" => TokenValue::Loop,
                        "true" => TokenValue::True,
                        "false" => TokenValue::False,
                        "let" => TokenValue::Let,
                        "borrow" => TokenValue::Borrow,
                        "fn" => TokenValue::Function,
                        "gen" => TokenValue::Gen,
                        "import" => TokenValue::Import,
                        "struct" => TokenValue::Struct,
                        "union" => TokenValue::Union,
                        "unique" => TokenValue::Unique,
                        "ref" => TokenValue::Ref,
                        "return" => TokenValue::Return,
                        "extern" => TokenValue::Extern,
                        "null" => TokenValue::Null,
                        "dict" => TokenValue::Dict,
                        "list" => TokenValue::List,
                        "rc" => TokenValue::Rc,
                        "interface" => TokenValue::Interface,
                        "and" => TokenValue::BooleanAnd,
                        "or" => TokenValue::BooleanOr,
                        "yield" => TokenValue::Yield,
                        "void" => TokenValue::Void,
                        "case" => TokenValue::Case,
                        _ => TokenValue::Word(word),
                    }
                }
                digit @ '0'..='9' => {
                    let mut number: u64 = (digit as u32 - '0' as u32) as u64;

                    // TODO: handle overflow
                    while let Some(candidate) = self.source.peek() {
                        match candidate {
                            digit @ '0'..='9' => {
                                number = number * 10 + (*digit as u32 - '0' as u32) as u64;
                            }
                            '_' => {}
                            _ => break,
                        }
                        let (_, p) = self.next_char().unwrap();
                        end = Some(p);
                    }

                    TokenValue::Int(number)
                }
                '!' => {
                    if let Some('=') = self.source.peek() {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::NotEquals
                    } else {
                        TokenValue::Exclamation
                    }
                }
                '?' => match self.source.peek() {
                    Some('?') => {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::NullCoalesce
                    }
                    Some('.') => {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::NullChaining
                    }
                    _ => TokenValue::QuestionMark,
                },
                '=' => match self.source.peek() {
                    Some('=') => {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::EqualTo
                    }
                    Some('>') => {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::CaseRocket
                    }
                    _ => TokenValue::Assign,
                },
                '<' => {
                    if let Some('=') = self.source.peek() {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::LessEqualThan
                    } else {
                        TokenValue::LessThan
                    }
                }
                '>' => {
                    if let Some('=') = self.source.peek() {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::GreaterEqualThan
                    } else {
                        TokenValue::GreaterThan
                    }
                }
                '+' => match self.source.peek() {
                    Some('=') => {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::PlusEquals
                    }
                    Some('+') => {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::Concat
                    }
                    _ => TokenValue::Plus,
                },
                '-' => {
                    if let Some('=') = self.source.peek() {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::MinusEquals
                    } else {
                        TokenValue::Minus
                    }
                }
                '*' => {
                    if let Some('=') = self.source.peek() {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::AsteriskEquals
                    } else {
                        TokenValue::Asterisk
                    }
                }
                '/' => match self.source.peek() {
                    Some('=') => {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::ForwardSlashEquals
                    }
                    Some('/') => {
                        let mut comment = String::new();
                        loop {
                            match self.source.next() {
                                Some('\n') | None => {
                                    end = Some(SourceMarker::new(
                                        self.source_name,
                                        self.source_text,
                                        self.line,
                                        self.offset,
                                    ));
                                    self.line += 1;
                                    self.offset = 0;
                                    break;
                                }
                                Some(ch) => {
                                    comment.push(ch);
                                    self.offset += 1;
                                }
                            }
                        }
                        TokenValue::LineComment(comment)
                    }
                    _ => TokenValue::ForwardSlash,
                },
                ',' => TokenValue::Comma,
                ';' => TokenValue::Semicolon,
                ':' => TokenValue::Colon,
                '.' => TokenValue::Period,
                '(' => TokenValue::OpenParen,
                ')' => TokenValue::CloseParen,
                '{' => TokenValue::OpenBracket,
                '}' => TokenValue::CloseBracket,
                '[' => TokenValue::OpenSquare,
                ']' => TokenValue::CloseSquare,
                '|' => TokenValue::VerticalPipe,
                '\0' => return Some(Err(LexError::IllegalNullByte(start))),
                '\'' => {
                    let (value, idx) = match self.next_char_literal(start) {
                        Ok(val) => val,
                        Err(err) => return Some(Err(err)),
                    };
                    // TODO: handle escape sequences
                    let Some(('\'', end_pos)) = self.next_char() else {
                        return Some(Err(LexError::UnterminatedLiteral(idx.end())));
                    };
                    end = Some(end_pos);

                    TokenValue::CharacterLiteral(value)
                }
                '"' => {
                    let mut string = String::new();
                    loop {
                        let (next, idx) = match self.next_char_literal(start) {
                            Ok(val) => val,
                            Err(err) => return Some(Err(err)),
                        };
                        end = Some(idx.end());
                        if next == '"' {
                            break TokenValue::StringLiteral(string);
                        }
                        string.push(next);
                    }
                }
                ch if ch.is_whitespace() => return self.next(),
                ch => return Some(Err(LexError::UnexpectedStart(ch, start))),
            };

            Some(Ok(Token {
                value,
                range: SourceRange::new(start, end.unwrap_or(start)),
            }))
        } else {
            None
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use TokenValue::*;

    #[test]
    fn reserved_words() {
        let result = lex("test", "if let true false fn word".to_string())
            .map(|token| token.map(|token| token.value))
            .collect::<Result<Vec<_>, _>>()
            .unwrap();

        assert_eq!(
            result,
            vec![If, Let, True, False, Function, Word("word".to_string())]
        );
    }
}
