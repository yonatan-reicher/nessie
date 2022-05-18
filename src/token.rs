use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

macro_rules! make_token_enum {
    (
        $($keyword:ident $keyword_ident:ident,)+
        $(-)+
        $($single_char_token:ident $single_char_token_char:expr,)+
        $(-)+
        $($multi_char_token:ident $multi_char_token_str:expr,)+
        $(-)+
        $($literal:ident $literal_type:ty,)+
    ) => {
        /// A token is a lexical unit meaning in a text.
        #[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub enum Token {
            $($keyword,)*
            $($single_char_token,)*
            $($multi_char_token,)*
            $($literal($literal_type),)*
        }

        impl Token {
            pub fn from_keyword(keyword: &str) -> Option<Token> {
                match keyword {
                    $(stringify!($keyword_ident) => {
                        Some(Token::$keyword)
                    })*
                    _ => None,
                }
            }

            pub fn from_single_char_token(token: char) -> Option<Token> {
                match token {
                    $($single_char_token_char => {
                        Some(Token::$single_char_token)
                    })*
                    _ => None,
                }
            }

            pub fn from_multi_char_token(token: &str) -> Option<(Token, usize)> {
                // we only want to check the beginning of the str
                $(if token.starts_with($multi_char_token_str) {
                    return Some((
                            Token::$multi_char_token,
                            $multi_char_token_str.len()
                        ));
                })*
                None
            }
        }

        impl Display for Token {
            fn fmt(&self, f: &mut Formatter) -> fmt::Result {
                match self {
                    $(Token::$keyword => {
                        write!(f, stringify!($keyword_ident))
                    })*
                    $(Token::$single_char_token => {
                        write!(f, "{}", $single_char_token_char)
                    })*
                    $(Token::$multi_char_token => {
                        write!(f, "{}", $multi_char_token_str)
                    })*
                    $(Token::$literal(value) => {
                        write!(f, "{}", value)
                    })*
                }
            }
        }
    };
}

make_token_enum! {
    Let let,
    In in,
    If if,
    Then then,
    Else else,
    And and,
    Or or,
    Xor xor,
    Not not,
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
    FatArrow "=>",
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
    IntLiteral i64,
    String Rc<str>,
    Identifier Rc<str>,
}

pub mod prelude {
    pub use super::Token;
}
