//! Contains the Span, Token the Lexer types for lexing source code on demand.

mod string_intern;

use crate::source_error::Spanned;
use crate::token::prelude::*;
use string_intern::StringInterner;
use thiserror::Error;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Lexer<'source> {
    source: &'source str,
    /// The current position of the lexer in the source code.
    position: Position,
    /// The current line of the lexer in the source code.
    line: Line,
    /// A table of interned strings.
    interned_strings: StringInterner,
}

#[derive(Error, Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Error {
    #[error("invalid character: {0}")]
    InvalidCharacter(char),
    #[error("unterminated string literal")]
    UnterminatedString,
    #[error("invalid escape sequence")]
    InvalidEscapeSequence,
    #[error("invalid unicode escape sequence")]
    InvalidUnicodeEscapeSequence,
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '-'
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-'
}

pub fn lex(source: &str) -> Result<Vec<Token>, Spanned<Error>> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.lex_token()? {
        tokens.push(token);
    }
    Ok(tokens)
}

fn hex_digit(c: char) -> Option<u8> {
    match c {
        '0'..='9' => Some(c as u8 - b'0'),
        'a'..='f' => Some(c as u8 - b'a' + 10),
        'A'..='F' => Some(c as u8 - b'A' + 10),
        _ => None,
    }
}

macro_rules! expect_char {
    ($lexer:expr; $( $char:pat => $result:expr ),+ $(,)?) => {
        match $lexer.current_char() {
            $( $char => {
                $lexer.advance_char();
                $result
            } )+
        }
    }
}

impl<'source> Lexer<'source> {
    pub fn new(source: &'source str) -> Lexer<'source> {
        Lexer {
            source,
            position: 0,
            line: 0,
            interned_strings: StringInterner::new(),
        }
    }

    fn make_error(&self, start: Position, source: Error) -> Spanned<Error> {
        let span = Span {
            start,
            end: self.position,
            line: self.line,
        };
        Spanned::new(span, source)
    }

    fn make_token(&mut self, start: Position, kind: TokenKind) -> Token {
        let end = self.position;
        let span = Span {
            start,
            end,
            line: self.line,
        };
        Token { kind, span }
    }

    pub fn lex_token(&mut self) -> Result<Option<Token>, Spanned<Error>> {
        self.skip_whitespace();

        let start = self.position;

        if let Some(c) = self.current_char() {
            if let Some(integer) = self.advance_int_literal()? {
                Ok(Some(self.make_token(start, TokenKind::IntLiteral(integer))))
            } else if let Some(string) = self.advance_string_literal()? {
                let string = self.interned_strings.intern(&string);
                Ok(Some(self.make_token(start, TokenKind::String(string))))
            } else if let Some(kind) = TokenKind::from_single_char_token(c) {
                self.advance_char();
                Ok(Some(self.make_token(start, kind)))
            } else if let Some((kind, len)) = TokenKind::from_multi_char_token(self.rest()) {
                self.advance_bytes(len);
                Ok(Some(self.make_token(start, kind)))
            } else if let Some(identifier) = self.advance_identifier() {
                let kind = {
                    TokenKind::from_keyword(&identifier).unwrap_or_else(|| {
                        let interned = self.interned_strings.intern(identifier);
                        TokenKind::Identifier(interned)
                    })
                };
                Ok(Some(self.make_token(start, kind)))
            } else {
                self.advance_char();
                Err(self.make_error(start, Error::InvalidCharacter(c)))
            }
        } else {
            Ok(None)
        }
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char() {
            if c.is_whitespace() {
                self.advance_char();
            } else {
                break;
            }
        }
    }

    fn advance_escape_char(&mut self) -> Result<Option<char>, Spanned<Error>> {
        let start = self.position;
        expect_char! { self;
            Some('\\') => {
                expect_char! { self;
                    Some('n') => Ok('\n'),
                    Some('r') => Ok('\r'),
                    Some('t') => Ok('\t'),
                    Some('\\') => Ok('\\'),
                    Some('\'') => Ok('\''),
                    Some('\"') => Ok('\"'),
                    Some('\0') => Ok('\0'),
                    Some('x') => {
                        let c1 = self.advance_char().and_then(hex_digit);
                        let c2 = self.advance_char().and_then(hex_digit);
                        if let (Some(c1), Some(c2)) = (c1, c2) {
                            char::from_u32(((c1 as u32) << 4) | (c2 as u32))
                            .ok_or_else(|| self.make_error(
                                start,
                                Error::InvalidEscapeSequence
                            ))
                        } else {
                            Err(self.make_error(
                                start,
                                Error::InvalidEscapeSequence,
                            ))
                        }
                    },
                    Some('u') => {
                        let c1 = self.advance_char().and_then(hex_digit);
                        let c2 = self.advance_char().and_then(hex_digit);
                        let c3 = self.advance_char().and_then(hex_digit);
                        let c4 = self.advance_char().and_then(hex_digit);
                        if let (Some(c1), Some(c2), Some(c3), Some(c4)) = (c1, c2, c3, c4) {
                            char::from_u32(
                                ((c1 as u32) << 12) |
                                ((c2 as u32) << 8) |
                                ((c3 as u32) << 4) |
                                ((c4 as u32) << 0)
                            )
                            .ok_or_else(|| self.make_error(
                                start,
                                Error::InvalidUnicodeEscapeSequence
                            ))
                        } else {
                            Err(self.make_error(
                                start,
                                Error::InvalidUnicodeEscapeSequence,
                            ))
                        }
                    },
                    _ => Err(self.make_error(
                        self.position,
                        Error::InvalidEscapeSequence,
                    ))
                }
                .map(Some)
            },
            Some(c) => Ok(Some(c)),
            None => Ok(None),
        }
    }

    fn advance_string_literal(&mut self) -> Result<Option<String>, Spanned<Error>> {
        if let Some(quote @ ('"' | '\'')) = self.current_char() {
            self.advance_char();
            let start = self.position;
            // Advance until we find the closing quote.
            let mut ret = String::new();
            while self.current_char() != Some(quote) {
                ret.push(
                    self
                    .advance_escape_char()?
                    .ok_or_else(|| self.make_error(start, Error::UnterminatedString))?
                );
            }
            self.advance_char();
            Ok(Some(ret))
        } else {
            Ok(None)
        }
    }

    fn advance_int_literal(&mut self) -> Result<Option<i64>, Spanned<Error>> {
        if let Some('0'..='9') = self.current_char() {
            let start = self.position;
            while let Some('0'..='9') = self.current_char() {
                self.advance_char();
            }
            let value = self.source[start..self.position].parse().unwrap();
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }

    fn advance_identifier(&mut self) -> Option<&'source str> {
        if is_ident_start(self.current_char()?) {
            let start = self.position;
            while self.current_char().map(is_ident_char).unwrap_or(false) {
                self.advance_char();
            }
            Some(&self.source[start..self.position])
        } else {
            None
        }
    }

    pub fn current_char(&self) -> Option<char> {
        self.source[self.position..].chars().next()
    }

    pub fn advance_char(&mut self) -> Option<char> {
        let c = self.current_char()?;
        // advance the indices
        self.position += c.len_utf8();
        if c == '\n' {
            self.line += 1;
        }
        Some(c)
    }

    /// Advances the lexer by the given amount of bytes.
    /// If not enough bytes are available, will return as many as possible.
    pub fn advance_bytes(&mut self, n: usize) -> &str {
        let slice = &self.source[self.position..][..n];
        // note: using slice.len() here instead of n because it's possible that
        // the slice is shorter than n bytes.
        self.position += slice.len();
        let newline_count = slice.chars().filter(|c| *c == '\n').count();
        self.line += newline_count;
        slice
    }

    pub fn rest(&self) -> &str {
        &self.source[self.position..]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn advance_int_literal() {
        let mut lexer = Lexer::new("123");
        assert_eq!(lexer.advance_int_literal().unwrap(), Some(123));
        assert_eq!(lexer.advance_int_literal().unwrap(), None);
    }

    #[test]
    fn advance_identifier() {
        let mut lexer = Lexer::new("hello");
        assert_eq!(lexer.advance_identifier(), Some("hello"));
        assert_eq!(lexer.advance_identifier(), None);
    }

    #[test]
    fn advance_identifier_with_underscore() {
        let mut lexer = Lexer::new("hello_world");
        assert_eq!(lexer.advance_identifier(), Some("hello_world"));
        assert_eq!(lexer.advance_identifier(), None);
    }

    #[test]
    fn advance_identifier_with_hyphen() {
        let mut lexer = Lexer::new("hello-world");
        assert_eq!(lexer.advance_identifier(), Some("hello-world"));
        assert_eq!(lexer.advance_identifier(), None);
    }

    #[test]
    fn advance_identifier_with_underscore_and_hyphen() {
        let mut lexer = Lexer::new("hello_world-world");
        assert_eq!(lexer.advance_identifier(), Some("hello_world-world"));
        assert_eq!(lexer.advance_identifier(), None);
    }

    #[test]
    fn advance_identifier_with_numbers() {
        let mut lexer = Lexer::new("hello123");
        assert_eq!(lexer.advance_identifier(), Some("hello123"));
        assert_eq!(lexer.advance_identifier(), None);
    }

    #[test]
    fn advance_string_literal() {
        let mut lexer = Lexer::new("\"hello\"");
        assert_eq!(lexer.advance_string_literal().unwrap(), Some("hello".into()));
        assert_eq!(lexer.advance_string_literal().unwrap(), None);
    }

    #[test]
    fn advance_string_literal_with_escaped_quote() {
        let mut lexer = Lexer::new("\"hello\\\"\"");
        assert_eq!(lexer.advance_string_literal().unwrap(), Some("hello\"".into()));
        assert_eq!(lexer.advance_string_literal().unwrap(), None);
    }
}
