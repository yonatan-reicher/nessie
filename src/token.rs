use std::fmt::{self, Display, Formatter};
use std::rc::Rc;


/// A position is a byte offset from the start of a source file.
pub type Position = usize;

/// A line number is a line index from the start of a source file.
pub type Line = usize;

/// A span is a range of text within a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    /// Inclusive lower bound of the span.
    pub start: Position,
    /// Exclusive upper bound of the span.
    pub end: Position,
    /// The line number of the start of the span.
    pub line: Line,
}

impl Span {
    pub fn empty(at_position: Position, line: Line) -> Self {
        Span { start: at_position, end: at_position, line }
    }

    pub fn single_char(at_position: Position, line: Line) -> Self {
        Span { start: at_position, end: at_position + 1, line }
    }

    /// Returns the length of the span.
    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

/// A token is a lexical unit meaning in a text.
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

        impl Display for TokenKind {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                match self {
                    $(TokenKind::$keyword => {
                        write!(f, stringify!($keyword_ident))
                    })*
                    $(TokenKind::$single_char_token => {
                        write!(f, "{}", $single_char_token_char)
                    })*
                    $(TokenKind::$multi_char_token => {
                        write!(f, "{}", $multi_char_token_str)
                    })*
                    $(TokenKind::$literal(value) => {
                        write!(f, "{}", value)
                    })*
                }
            }
        }
    };
}

make_token_kind! {
    Let let,
    In in,
    If if,
    Else else,
    And and,
    Or or,
    Xor xor,
    Not not,
    Int int,
    Bool bool,
    True true,
    False false,
    ----------------
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
    Star '*',
    Slash '/',
    Percent '%',
    Tilde '~',
    Question '?',
    Pipe '|',
    Caret '^',
    Ampersand '&',
    ----------------
    Arrow "->",
    Minus "-",
    PlusPlus "++",
    Plus "+",
    BangEqual "!=",
    EqualEqual "==",
    Bang "!",
    Equal "=",
    GreaterEqual ">=",
    LesserEqual "<=",
    Greater ">",
    Lesser "<",
    ----------------
    IntLiteral i32,
    String Rc<str>,
    Identifier Rc<str>,
}


pub mod prelude {
    pub use super::{
        Token,
        TokenKind,
        Span,
        Position,
        Line,
    };
}

