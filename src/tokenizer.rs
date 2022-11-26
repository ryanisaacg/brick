use std::fmt;
use thiserror::Error;
use std::iter::Peekable;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Token {
    Word(String),
    Int(u64),
    Plus,
    Minus,
    ColonEquals,
    Equals,
    Semicolon,
    Period,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Token::*;
        match self {
            Word(word) => write!(f, "word {}", word),
            Int(int) => write!(f, "int {}", int),
            Plus => write!(f, "+"),
            Minus => write!(f, "-"),
            ColonEquals => write!(f, ":="),
            Equals => write!(f, "="),
            Semicolon => write!(f, ";"),
            Period => write!(f, "."),
        }
    }
}

#[derive(Debug, Error)]
pub enum TokenError {
    #[error("unexpected start of token: {0}")]
    UnexpectedStart(char),
}

pub fn tokenize<'a>(source: &'a str) -> impl 'a + Iterator<Item = Result<Token, TokenError>> {
    TokenIterator(source.chars().peekable())
}

struct TokenIterator<T: Iterator<Item = char>>(Peekable<T>);

impl<T: Iterator<Item = char>> Iterator for TokenIterator<T> {
    type Item = Result<Token, TokenError>;

    fn next(&mut self) -> Option<Result<Token, TokenError>> {
        match self.0.next() {
            Some(letter @ ('a'..='z' | 'A'..='Z' | '_')) => {
                let mut word = String::new();
                word.push(letter);
                while let Some(candidate) = self.0.peek() {
                    match candidate {
                        letter @ ('a'..='z' | 'A'..='Z' | '_' | '0'..='9') => {
                            word.push(*letter);
                        }
                        _ => break,
                    }
                    self.0.next();
                }

                Some(Ok(Token::Word(word)))
            }
            Some(digit @ '0'..='9') => {
                let mut number: u64 = (digit as u32 - '0' as u32) as u64;

                // TODO: handle overflow
                while let Some(candidate) = self.0.peek() {
                    match candidate {
                        digit @ '0'..='9' => {
                            number = number * 10 - (*digit as u32 - '0' as u32) as u64;
                        }
                        '_' => {}
                        _ => break,
                    }
                    self.0.next();
                }

                Some(Ok(Token::Int(number)))
            }
            Some('+') => Some(Ok(Token::Plus)),
            Some('-') => Some(Ok(Token::Minus)),
            Some('=') => Some(Ok(Token::Equals)),
            Some(';') => Some(Ok(Token::Semicolon)),
            Some(':') => match self.0.next() {
                Some('=') => Some(Ok(Token::ColonEquals)),
                _other => todo!("add an error variant here"),
            },
            Some('.') => Some(Ok(Token::Period)),
            Some(ch) if ch.is_whitespace() => self.next(),
            Some(ch) => Some(Err(TokenError::UnexpectedStart(ch))),
            None => None,
        }
    }
}
