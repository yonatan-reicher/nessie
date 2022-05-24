//! Contains the Span, Token the Lexer types for lexing source code on demand.

mod string_intern;

use crate::reporting::annotation::{Line, Located, Position, Region};
use crate::reporting::error::lexer::Error;
use crate::token::prelude::*;
use string_intern::StringInterner;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Lexer<'source> {
    source: &'source str,
    /// The current position of the lexer in the source code.
    position: usize,
    /// The current line of the lexer in the source code.
    line: Position,
    /// A table of interned strings.
    interned_strings: StringInterner,
}

fn is_ident_start(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c == '-'
}

fn is_ident_char(c: char) -> bool {
    c.is_alphanumeric() || c == '_' || c == '-'
}

pub fn lex(source: &str) -> Result<Vec<Located<Token>>, Located<Error>> {
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
            line: Position { line: 0, column: 0 },
            interned_strings: StringInterner::new(),
        }
    }

    pub fn located<T>(&self, start: Position, value: T) -> Located<T> {
        let region = Region(start, self.line);
        Located { region, value }
    }

    pub fn located_here<T>(&self, value: T) -> Located<T> {
        let region = Region::point(self.line);
        Located { region, value }
    }

    pub fn lex_token(&mut self) -> Result<Option<Located<Token>>, Located<Error>> {
        self.skip_whitespace();

        let start = self.line;

        if let Some(c) = self.current_char() {
            if let Some(integer) = self.advance_int_literal()? {
                Ok(Some(self.located(start, Token::IntLiteral(integer))))
            } else if let Some(string) = self.advance_string_literal()? {
                let string = self.interned_strings.intern(&string);
                Ok(Some(self.located(start, Token::String(string))))
            } else if let Some(kind) = Token::from_single_char_token(c) {
                self.advance_char();
                Ok(Some(self.located(start, kind)))
            } else if let Some((kind, len)) = Token::from_multi_char_token(self.rest()) {
                self.advance_bytes(len);
                Ok(Some(self.located(start, kind)))
            } else if let Some(identifier) = self.advance_identifier() {
                let kind = {
                    Token::from_keyword(&identifier).unwrap_or_else(|| {
                        let interned = self.interned_strings.intern(identifier);
                        Token::Identifier(interned)
                    })
                };
                Ok(Some(self.located(start, kind)))
            } else {
                self.advance_char();
                Err(self.located(start, Error::InvalidCharacter(c)))
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

    fn advance_escape_char(&mut self) -> Result<Option<char>, Located<Error>> {
        let start = self.line;
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
                            .ok_or_else(|| self.located(
                                start,
                                Error::InvalidEscapeSequence
                            ))
                        } else {
                            Err(self.located(
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
                            .ok_or_else(|| self.located(
                                start,
                                Error::InvalidUnicodeEscapeSequence
                            ))
                        } else {
                            Err(self.located(
                                start,
                                Error::InvalidUnicodeEscapeSequence,
                            ))
                        }
                    },
                    _ => Err(self.located_here(Error::InvalidEscapeSequence)),
                }
                .map(Some)
            },
            Some(c) => Ok(Some(c)),
            None => Ok(None),
        }
    }

    fn advance_string_literal(&mut self) -> Result<Option<String>, Located<Error>> {
        if let Some(quote @ ('"' | '\'')) = self.current_char() {
            self.advance_char();
            let start = self.line;
            // Advance until we find the closing quote.
            let mut ret = String::new();
            while self.current_char() != Some(quote) {
                ret.push(
                    self.advance_escape_char()?
                        .ok_or_else(|| self.located(start, Error::UnterminatedString))?,
                );
            }
            self.advance_char();
            Ok(Some(ret))
        } else {
            Ok(None)
        }
    }

    fn advance_int_literal(&mut self) -> Result<Option<i64>, Located<Error>> {
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
        self.line.column += 1;
        if c == '\n' {
            self.line.line += 1;
            self.line.column = 0;
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
        self.line.line += newline_count as Line;
        self.line.column = slice
            .chars()
            .rev()
            .position(|c| c == '\n')
            .map(|x| x as u16)
            .unwrap_or(self.line.column + slice.len() as u16);
        slice
    }

    pub fn rest(&self) -> &str {
        &self.source[self.position..]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    fn l<T>(line1: Line, column1: u16, line2: Line, column2: u16, value: T) -> Located<T> {
        Located {
            region: Region(
                Position {
                    line: line1,
                    column: column1,
                },
                Position {
                    line: line2,
                    column: column2,
                },
            ),
            value,
        }
    }

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
        assert_eq!(
            lexer.advance_string_literal().unwrap(),
            Some("hello".into())
        );
        assert_eq!(lexer.advance_string_literal().unwrap(), None);
    }

    #[test]
    fn advance_string_literal_with_escaped_quote() {
        let mut lexer = Lexer::new("\"hello\\\"\"");
        assert_eq!(
            lexer.advance_string_literal().unwrap(),
            Some("hello\"".into())
        );
        assert_eq!(lexer.advance_string_literal().unwrap(), None);
    }

    macro_rules! make_token_tests {
        {
            $( $name:ident $code:literal $token:expr),* $(,)?
        } => {
            $(
                #[test]
                fn $name() {
                    assert_eq!(
                        lex($code),
                        Ok(vec![l(0, 0, 0, $code.len() as _, $token)]),
                    );
                }
            )+
        }
    }

    make_token_tests! {
        let_token "let" Token::Let,
        in_token "in" Token::In,
        if_token "if" Token::If,
        true_token "true" Token::True,
        false_token "false" Token::False,
        left_paren_token "(" Token::LeftParen,
        right_paren_token ")" Token::RightParen,
        left_brace_token "{" Token::LeftBrace,
        right_brace_token "}" Token::RightBrace,
        left_bracket_token "[" Token::LeftBracket,
        right_bracket_token "]" Token::RightBracket,
        comma_token "," Token::Comma,
        semicolon_token ";" Token::Semicolon,
        colon_token ":" Token::Colon,
        dot_token "." Token::Dot,
        star_token "*" Token::Star,
        slash_token "/" Token::Slash,
        percent_token "%" Token::Percent,
        tilde_token "~" Token::Tilde,
        question_token "?" Token::Question,
        pipe_token "|" Token::Pipe,
        caret_token "^" Token::Caret,
        ampersand_token "&" Token::Ampersand,
        arrow_token "->" Token::Arrow,
        fat_arrow_token "=>" Token::FatArrow,
        minus_token "-" Token::Minus,
        plus_plus_token "++" Token::PlusPlus,
        plus_token "+" Token::Plus,
        bang_equal_token "!=" Token::BangEqual,
        equal_equal_token "==" Token::EqualEqual,
        bang_token "!" Token::Bang,
        equal_token "=" Token::Equal,
        greater_equal_token ">=" Token::GreaterEqual,
        lesser_equal_token "<=" Token::LesserEqual,
        greater_token ">" Token::Greater,
        lesser_token "<" Token::Lesser,
        int_literal_token "1294" Token::IntLiteral(1294),
        string_literal_single_quote_token "'hello ! \" '" Token::String("hello ! \" ".into()),
        string_literal_double_quote_token "\" Hen1$!\"" Token::String(" Hen1$!".into()),
        identifier_token "whats-up" Token::Identifier("whats-up".into()),
    }

    #[test]
    fn lex_calculations() {
        let tokens = lex(indoc! {"
            1 + 2 * (3 - 21)
        "})
        .expect("could not lex");
        assert_eq!(
            tokens,
            vec![
                l(0, 0, 0, 1, Token::IntLiteral(1)),
                l(0, 2, 0, 3, Token::Plus),
                l(0, 4, 0, 5, Token::IntLiteral(2)),
                l(0, 6, 0, 7, Token::Star),
                l(0, 8, 0, 9, Token::LeftParen),
                l(0, 9, 0, 10, Token::IntLiteral(3)),
                l(0, 11, 0, 12, Token::Minus),
                l(0, 13, 0, 15, Token::IntLiteral(21)),
                l(0, 15, 0, 16, Token::RightParen),
            ],
        )
    }

    #[test]
    fn lex_program() {
        let tokens = lex(indoc! {"
            let fib = n: int =>
                if n <= 1 then n
                else recurse (n - 1) + recurse (n - 2)
            in
            fib 36 == 14930352
        "})
        .expect("could no lex.");
        let expected = vec![
            l(0, 0, 0, 3, Token::Let),
            l(0, 4, 0, 7, Token::Identifier("fib".into())),
            l(0, 8, 0, 9, Token::Equal),
            l(0, 10, 0, 11, Token::Identifier("n".into())),
            l(0, 11, 0, 12, Token::Colon),
            l(0, 13, 0, 16, Token::Identifier("int".into())),
            l(0, 17, 0, 19, Token::FatArrow),
            l(1, 4, 1, 6, Token::If),
            l(1, 7, 1, 8, Token::Identifier("n".into())),
            l(1, 9, 1, 11, Token::LesserEqual),
            l(1, 12, 1, 13, Token::IntLiteral(1)),
            l(1, 14, 1, 18, Token::Then),
            l(1, 19, 1, 20, Token::Identifier("n".into())),
            l(2, 4, 2, 8, Token::Else),
            l(2, 9, 2, 16, Token::Identifier("recurse".into())),
            l(2, 17, 2, 18, Token::LeftParen),
            l(2, 18, 2, 19, Token::Identifier("n".into())),
            l(2, 20, 2, 21, Token::Minus),
            l(2, 22, 2, 23, Token::IntLiteral(1)),
            l(2, 23, 2, 24, Token::RightParen),
            l(2, 25, 2, 26, Token::Plus),
            l(2, 27, 2, 34, Token::Identifier("recurse".into())),
            l(2, 35, 2, 36, Token::LeftParen),
            l(2, 36, 2, 37, Token::Identifier("n".into())),
            l(2, 38, 2, 39, Token::Minus),
            l(2, 40, 2, 41, Token::IntLiteral(2)),
            l(2, 41, 2, 42, Token::RightParen),
            l(3, 0, 3, 2, Token::In),
            l(4, 0, 4, 3, Token::Identifier("fib".into())),
            l(4, 4, 4, 6, Token::IntLiteral(36)),
            l(4, 7, 4, 9, Token::EqualEqual),
            l(4, 10, 4, 18, Token::IntLiteral(14930352)),
        ];
        assert_eq!(
            tokens,
            expected,
        );
    }
}
