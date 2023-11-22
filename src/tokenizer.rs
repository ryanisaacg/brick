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

    // Misc operators
    Period,

    // Markers
    Assign,
    Colon,
    Comma,
    Semicolon,
    QuestionMark,

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
    Let,
    True,
    False,
    Function,
    Import,
    Struct,
    Union,
    Unique,
    Shared,
    Return,
    Extern,
    Null,
}

impl fmt::Display for TokenValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use TokenValue::*;
        match self {
            Word(word) => write!(f, "word {}", word),
            Int(int) => write!(f, "int {}", int),
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
            QuestionMark => write!(f, "?"),
            Let => write!(f, "keyword let"),
            If => write!(f, "keyword if"),
            While => write!(f, "keyword while"),
            True => write!(f, "keyword true"),
            False => write!(f, "keyword false"),
            Function => write!(f, "keyword fn"),
            Import => write!(f, "keyword import"),
            Struct => write!(f, "keyword struct"),
            Union => write!(f, "keyword union"),
            Unique => write!(f, "keyword unique"),
            Shared => write!(f, "keyword shared"),
            Return => write!(f, "keyword return"),
            Extern => write!(f, "keyword extern"),
            Null => write!(f, "keyword null"),
        }
    }
}

#[derive(Clone, Debug, Error)]
pub enum LexError {
    #[error("unexpected character {0} at {1}")]
    UnexpectedStart(char, SourceMarker),
    #[error("illegal null byte in source code at {0}")]
    IllegalNullByte(SourceMarker),
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
                        "true" => TokenValue::True,
                        "false" => TokenValue::False,
                        "let" => TokenValue::Let,
                        "fn" => TokenValue::Function,
                        "import" => TokenValue::Import,
                        "struct" => TokenValue::Struct,
                        "union" => TokenValue::Union,
                        "unique" => TokenValue::Unique,
                        "shared" => TokenValue::Shared,
                        "return" => TokenValue::Return,
                        "extern" => TokenValue::Extern,
                        "null" => TokenValue::Null,
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
                        todo!()
                    }
                }
                '?' => TokenValue::QuestionMark,
                '=' => {
                    if let Some('=') = self.source.peek() {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::EqualTo
                    } else {
                        TokenValue::Assign
                    }
                }
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
                '+' => {
                    if let Some('=') = self.source.peek() {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::PlusEquals
                    } else {
                        TokenValue::Plus
                    }
                }
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
                '/' => {
                    if let Some('=') = self.source.peek() {
                        end = Some(self.next_char().unwrap().1);
                        TokenValue::ForwardSlashEquals
                    } else {
                        TokenValue::ForwardSlash
                    }
                }
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
                '\0' => return Some(Err(LexError::IllegalNullByte(start))),
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
