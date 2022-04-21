//! Contains the Span, Token the Lexer types for lexing source code on demand.

mod string_intern;

use crate::token::prelude::*;
use crate::source_error::SourceError;
use string_intern::StringInterner;
use std::fmt::{self, Display, Formatter};


pub struct Lexer<'source> {
    source: &'source str,
    /// The current position of the lexer in the source code.
    position: Position,
    /// The current line of the lexer in the source code.
    line: Line,
    /// A table of interned strings.
    interned_strings: StringInterner,
}

#[derive(Debug)]
pub struct LexError {
    pub kind: LexErrorKind,
    pub span: Span,
}

#[derive(Debug)]
pub enum LexErrorKind {
    InvalidCharacter(char),
    UnterminatedString,
}

impl Display for LexErrorKind {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        use LexErrorKind::*;
        match self {
            InvalidCharacter(c) => write!(f, "Invalid character: {}", c),
            UnterminatedString => write!(f, "Unterminated string"),
        }
    }
}

impl Display for LexError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl std::error::Error for LexError {}

impl SourceError for LexError {
    fn get_span(&self) -> Span {
        self.span
    }
}


fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '-'
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-'
}


pub fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.lex_token()? {
        tokens.push(token);
    }
    Ok(tokens)
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

    fn make_error(&self, start: Position, kind: LexErrorKind) -> LexError {
        let span = Span { start, end: self.position, line: self.line };
        LexError { kind, span }
    }

    fn make_token(&mut self, start: Position, kind: TokenKind) -> Token {
        let end = self.position;
        let span = Span { start, end, line: self.line };
        Token { kind, span }
    }

    pub fn lex_token(&mut self) -> Result<Option<Token>, LexError> {
        self.skip_whitespace();

        let start = self.position;

        if let Some(c) = self.current_char() {
            if let Some(integer) = self.advance_int_literal()? {
                Ok(Some(self.make_token(start, TokenKind::IntLiteral(integer))))
            } else if let Some(string) = self.advance_string_literal()? {
                let string = self.interned_strings.intern(string);
                Ok(Some(self.make_token(start, TokenKind::String(string))))
            } else if let Some(kind) = TokenKind::from_single_char_token(c) {
                self.advance_char();
                Ok(Some(self.make_token(start, kind)))
            } else if let Some((kind, len)) = TokenKind::from_multi_char_token(self.rest()) {
                self.advance_bytes(len);
                Ok(Some(self.make_token(start, kind)))
            } else if let Some(identifier) = self.advance_identifier() {
                let kind = {
                    TokenKind::from_keyword(&identifier)
                    .unwrap_or_else(|| {
                        let interned = self.interned_strings.intern(identifier);
                        TokenKind::Identifier(interned)
                    })
                };
                Ok(Some(self.make_token(start, kind)))
            } else {
                self.advance_char();
                Err(self.make_error(start, LexErrorKind::InvalidCharacter(c)))
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

    fn advance_string_literal(&mut self) -> Result<Option<&'source str>, LexError> {
        if let Some(quote @ ('"' | '\'')) = self.current_char() {
            self.advance_char();
            let start = self.position;
            // Advance until we find the closing quote.
            while self.advance_char().ok_or_else(|| {
                self.make_error(start, LexErrorKind::UnterminatedString)
            })? != quote {}
            Ok(Some(&self.source[start..self.position-1]))
        } else {
            Ok(None)
        }
    }

    fn advance_int_literal(&mut self) -> Result<Option<i32>, LexError> {
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
        assert_eq!(lexer.advance_string_literal().unwrap(), Some("hello"));
        assert_eq!(lexer.advance_string_literal().unwrap(), None);
    }

    #[test]
    fn advance_string_literal_with_escaped_quote() {
        let mut lexer = Lexer::new("\"hello\\\"\"");
        assert_eq!(lexer.advance_string_literal().unwrap(), Some("hello\""));
        assert_eq!(lexer.advance_string_literal().unwrap(), None);
    }
}
