//! This module contains the parser for the language.

use crate::lexer::{Token, TokenKind, Position, Span};
use crate::ast::*;


#[derive(Debug)]
pub enum ParseErrorKind {
    ExpectedExpression,
    ExpectedExpressionAtom,
    ExpectedEndOfProgram,
    UnclosedDelimiter,
}

impl std::fmt::Display for ParseErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParseErrorKind::*;
        match self {
            ExpectedExpression => {
                write!(f, "Expected expression")
            }
            ExpectedExpressionAtom => {
                write!(f, "Expected expression atom")
            }
            ExpectedEndOfProgram => {
                write!(f, "Expected end of program")
            }
            UnclosedDelimiter => {
                write!(f, "Unclosed delimiter")
            }
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "line {} col {} to line {} col {}: {}",
            self.span.start.line, self.span.start.column,
            self.span.end.line, self.span.end.column,
            self.kind,
        )
    }
}

impl std::error::Error for ParseErrorKind {}

pub fn parse<'a>(tokens: &[Token]) -> Result<Program, Vec<ParseError>> {
    let mut parser = Parser {
        tokens,
        index: 0,
        errors: Vec::new(),
    };

    match parser.parse_program() {
        Ok(program) if parser.errors.is_empty() => Ok(program),
        _ => Err(parser.errors),
    }
}


struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    errors: Vec<ParseError>,
}

fn try_kind(token: Option<&Token>) -> Option<&TokenKind> {
    token.map(|t| &t.kind)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Precedence {
    // Precedence from lowest to highest.
    Or,             // ||
    And,            // &&
    Equality,       // == !=
    Comparison,     // < > <= >=
    Term,           // + -
    Factor,         // * / %
    Unary,          // ! -
    Call,           // . ()
    Atom,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Associativity {
    Left,
    Right,
}

impl Precedence {
    const LOWEST: Precedence = Precedence::Or;

    fn next(self) -> Self {
        use Precedence::*;

        match self {
            Or => And,
            And => Equality,
            Equality => Comparison,
            Comparison => Term,
            Term => Factor,
            Factor => Unary,
            Unary => Call,
            Call => Atom,
            Atom => Atom,
        }
    }

    fn associativity(self) -> Associativity {
        use Precedence::*;
        use Associativity::{Left, Right};

        match self {
            Or => Left,
            And => Left,
            Equality => Left,
            Comparison => Left,
            Term => Left,
            Factor => Left,
            Unary => Right,
            Call => Left,
            Atom => Left,
        }
    }
}

fn get_binary_operator(token: &Token)
-> Option<(Precedence, BinaryOp)> {
    use TokenKind::*;
    use BinaryOp::*;
    use Precedence::*;

    match token.kind {
        Plus => Some((Term, Add)),
        Minus => Some((Term, Sub)),
        Star => Some((Factor, Mul)),
        Slash => Some((Factor, Div)),
        Percent => Some((Factor, Mod)),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    fn make_expr(&self, start_index: usize, kind: ExprKind) -> Expr {
        let max_index = self.tokens.len() - 1;
        let start = self.tokens[start_index.clamp(0, max_index)].span.start;
        let end = self.tokens[(self.index - 1).clamp(0, max_index)].span.end;
        let span = Span { start, end };
        Expr { span, kind, ty: None }
    }

    fn make_error(&mut self, kind: ParseErrorKind, start: usize, end: usize) {
        let max_index = self.tokens.len() - 1;
        let start = self.tokens[start.clamp(0, max_index)].span.start;
        let end = self.tokens[end.clamp(0, max_index)].span.end;
        let span = Span { start, end };
        self.errors.push(ParseError {
            kind,
            span,
        });
    }

    pub fn parse_program(&mut self) -> Result<Program, ()> {
        let body = self.parse_expr()?;
        if self.index < self.tokens.len() {
            self.make_error(
                ParseErrorKind::ExpectedEndOfProgram, 
                self.index, self.tokens.len() - 1
            );
        }
        Ok(Program {
            body,
        })
    }

    fn parse_atom(&mut self) -> Result<Option<Expr>, ()> {
        let start = self.index;
        match try_kind(self.tokens.get(self.index)) {
            Some(&TokenKind::Int(i)) => {
                self.index += 1;
                Ok(Some(self.make_expr(start, ExprKind::Int(i))))
            }
            Some(&TokenKind::LeftParen) => {
                self.index += 1;
                let expr = self.parse_expr();
                if try_kind(self.tokens.get(self.index)) != Some(&TokenKind::RightParen) {
                    self.make_error(
                        ParseErrorKind::UnclosedDelimiter,
                        start, start,
                    );
                }
                self.index += 1;
                expr.map(Some)
            }
            Some(&TokenKind::Minus) => {
                self.index += 1;
                let expr = self.parse_atom();
                match expr {
                    Ok(Some(expr)) => {
                        let kind = ExprKind::Unary(UnaryOp::Neg, Box::new(expr));
                        Ok(Some(self.make_expr(start, kind)))
                    }
                    Ok(None) => {
                        self.make_error(
                            ParseErrorKind::ExpectedExpressionAtom,
                            start, self.index,
                        );
                        Err(())
                    }
                    Err(()) => Err(()),
                }
            }
            _ => Ok(None),
        }
    }

    fn parse_precedence(&mut self, lowest_precedence: Precedence) -> Result<Expr, ()> {
        if let Precedence::Atom = lowest_precedence {
            let atom = self.parse_atom()?;
            return match atom {
                Some(atom) => Ok(atom),
                None => {
                    self.make_error(
                        ParseErrorKind::ExpectedExpressionAtom,
                        self.index, self.index,
                    );
                    Err(())
                }
            };
        }

        let next_precedence = lowest_precedence.next();

        let start = self.index;
        let mut ret = self.parse_precedence(next_precedence)?;

        while let Some(token) = self.tokens.get(self.index) {
            let (precedence, op) = match get_binary_operator(token) {
                Some((precedence, op)) => (precedence, op),
                None => break,
            };
            if precedence < lowest_precedence {
                break;
            }
            self.index += 1;

            // for now only allow left associativity
            let right = self.parse_precedence(precedence.next())?;
            let kind = ExprKind::BinaryOp(op, Box::new(ret), Box::new(right));
            ret = self.make_expr(start, kind);
        }

        Ok(ret)
    }

    fn parse_expr(&mut self) -> Result<Expr, ()> {
        let start = self.index;
        let res = self.parse_precedence(Precedence::LOWEST);
        if res.is_err() {
            self.make_error(
                ParseErrorKind::ExpectedExpression,
                start, self.index,
            );
        }
        res
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;

    fn lex(input: &str) -> Vec<Token> {
        let mut ret = Vec::new();
        let mut lexer = Lexer::new(input);
        while let Some(t) = lexer.lex_token().unwrap() {
            ret.push(t);
        }
        ret
    }

    #[test]
    fn int() {
        let tokens = lex("123");
        let program = parse(&tokens).expect("failed to parse");
        assert_eq!(
            program.body,
            Expr {
                span: Span {
                    start: Position { line: 1, column: 1 },
                    end: Position { line: 1, column: 4 },
                },
                kind: ExprKind::Int(123),
                ty: None,
            }
        );
    }

    #[test]
    fn addition() {
        let tokens = lex("1 + 2");
        let program = parse(&tokens).expect("failed to parse");
        assert_eq!(
            program.body,
            Expr {
                span: Span {
                    start: Position { line: 1, column: 1 },
                    end: Position { line: 1, column: 6 },
                },
                kind: ExprKind::BinaryOp(
                    BinaryOp::Add,
                    Box::new(Expr {
                        span: Span {
                            start: Position { line: 1, column: 1 },
                            end: Position { line: 1, column: 2 },
                        },
                        kind: ExprKind::Int(1),
                        ty: None,
                    }),
                    Box::new(Expr {
                        span: Span {
                            start: Position { line: 1, column: 5 },
                            end: Position { line: 1, column: 6 },
                        },
                        kind: ExprKind::Int(2),
                        ty: None,
                    }),
                ),
                ty: None,
            }
        );
    }
}

