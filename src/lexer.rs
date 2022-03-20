//! Contains the Span, Token the Lexer types for lexing source code on demand.


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Default for Position {
    fn default() -> Self {
        Self {
            line: 1,
            column: 1,
        }
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

macro_rules! make_token_kind {
    (
        $($keyword:ident $keyword_ident:ident,)+
        $(-)+
        $($single_char_token:ident $single_char_token_char:expr,)+
        $(-)+
        $($multi_char_token:ident $multi_char_token_str:expr,)+
        $(-)+
        $($literal:ident $literal_type:ty,)+
    ) => {
        #[derive(Debug, Clone, PartialEq, Eq, Hash)]
        pub enum TokenKind {
            $($keyword,)*
            $($single_char_token,)*
            $($multi_char_token,)*
            $($literal($literal_type),)*
        }

        impl TokenKind {
            pub fn from_keyword(keyword: &str) -> Option<TokenKind> {
                match keyword {
                    $(stringify!($keyword_ident) => {
                        Some(TokenKind::$keyword)
                    })*
                    _ => None,
                }
            }

            pub fn from_single_char_token(token: char) -> Option<TokenKind> {
                match token {
                    $($single_char_token_char => {
                        Some(TokenKind::$single_char_token)
                    })*
                    _ => None,
                }
            }

            pub fn from_multi_char_token(token: &str) -> Option<(TokenKind, usize)> {
                // we only want to check the beginning of the str
                $(if token.starts_with($multi_char_token_str) {
                    return Some((
                            TokenKind::$multi_char_token,
                            $multi_char_token_str.len()
                        ));
                })*
                None
            }
        }
    };
}

make_token_kind! {
    Let let,
    In in,
    If if,
    Else else,
    -----
    LeftParen '(',
    RightParen ')',
    LeftBrace '{',
    RightBrace '}',
    LeftBracket '[',
    RightBracket ']',
    Comma ',',
    Semicolon ';',
    Colon ':',
    Dot '.',
    Plus '+',
    Star '*',
    Slash '/',
    Percent '%',
    Tilde '~',
    Question '?',
    And '&',
    Or '|',
    Caret '^',
    Ampersand '&',
    -----
    Arrow "->",
    Minus "-",
    BangEqual "!=",
    EqualEqual "==",
    Bang "!",
    Equal "=",
    GreaterEqual ">=",
    LesserEqual "<=",
    Greater ">",
    Lesser "<",
    -----
    Int i32,
    String String,
    Identifier Identifier,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum NumberLiteral {
    Int(i32),
    /// A float literal. Stored as a string because of precision loss
    Float(String),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
struct Identifier(String);

pub struct Lexer<'a> {
    source: &'a str,
    /// The current position of the lexer as an index.
    index: usize,
    /// The current position of the lexer as a Position.
    position: Position,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum LexError {
    InvalidCharacter(char),
    UnterminatedString,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            LexError::InvalidCharacter(c) => write!(f, "Invalid character {}", c),
            LexError::UnterminatedString => write!(f, "Unterminated string"),
        }
    }
}

impl std::error::Error for LexError {}


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

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str) -> Lexer<'a> {
        Lexer {
            source,
            index: 0,
            position: Position::default(),
        }
    }

    pub fn lex_token(&mut self) -> Result<Option<Token>, LexError> {
        self.skip_whitespace();

        let start = self.position;

        if let Some(c) = self.current_char() {
            if let Some(integer) = self.advance_int_literal()? {
                Ok(Some(self.make_token(start, TokenKind::Int(integer))))
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
                        TokenKind::Identifier(Identifier(identifier.to_string()))
                    })
                };
                Ok(Some(self.make_token(start, kind)))
            } else {
                Err(LexError::InvalidCharacter(c))
            }
        } else {
            Ok(None)
        }
    }

    fn make_token(&mut self, start: Position, kind: TokenKind) -> Token {
        let end = self.position;
        let span = Span { start, end };

        Token {
            kind,
            span,
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

    fn advance_string_literal(&mut self) -> Result<Option<&str>, LexError> {
        if let Some(quote @ ('"' | '\'')) = self.current_char() {
            self.advance_char();
            let start = self.index;
            while self.advance_char().ok_or(LexError::UnterminatedString)? != quote {}
            Ok(Some(&self.source[start..self.index-1]))
        } else {
            Ok(None)
        }
    }

    fn advance_int_literal(&mut self) -> Result<Option<i32>, LexError> {
        if let Some('0'..='9') = self.current_char() {
            let start = self.index;
            while let Some('0'..='9') = self.current_char() {
                self.advance_char();
            }
            let value = self.source[start..self.index].parse().unwrap();
            Ok(Some(value))
        } else {
            Ok(None)
        } 
    }

    fn advance_identifier(&mut self) -> Option<&str> {
        if is_ident_start(self.current_char()?) {
            let start = self.index;
            while self.current_char().map(is_ident_char).unwrap_or(false) {
                self.advance_char();
            }
            Some(&self.source[start..self.index])
        } else {
            None
        }
    }

    pub fn current_char(&self) -> Option<char> {
        self.source[self.index..].chars().next()

    }

    pub fn advance_char(&mut self) -> Option<char> {
        let c = self.current_char()?;

        // advance the indices
        self.index += c.len_utf8();
        if c == '\n' {
            self.position.line += 1;
            self.position.column = 1;
        } else {
            self.position.column += c.len_utf8();
        }

        Some(c)
    }

    /// Advances the lexer by the given amount of bytes.
    /// If not enough bytes are available, will return as many as possible.
    pub fn advance_bytes(&mut self, n: usize) -> &str {
        let slice = &self.source[self.index..self.index + n];
        
        let mut newlines = 0;
        let mut last_newline_index = None;
        for (i, c) in slice.char_indices() {
            if c == '\n' {
                newlines += 1;
                last_newline_index = Some(i);
            }
        }

        // note: using slice.len() here instead of n because it's possible that
        // the slice is shorter than n bytes.
        self.index += slice.len();
        self.position.line += newlines;
        if let Some(i) = last_newline_index {
            self.position.column = slice.len() - i;
        } else {
            self.position.column += slice.len();
        }

        slice
    }

    pub fn rest(&self) -> &str {
        &self.source[self.index..]
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
    fn advance_string_literal() {
        let mut lexer = Lexer::new("\"hello\"");
        assert_eq!(lexer.advance_string_literal().unwrap(), Some("hello"));
        assert_eq!(lexer.advance_string_literal().unwrap(), None);
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
}
