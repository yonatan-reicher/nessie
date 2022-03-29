//! This module contains the parser for the language.

use crate::token::prelude::*;
use crate::ast::*;
use crate::source_error::SourceError;
use std::fmt::{self, Display, Formatter};


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
            ExpectedExpression => write!(f, "Expected expression"),
            ExpectedExpressionAtom => write!(f, "Expected expression atom"),
            ExpectedEndOfProgram => write!(f, "Expected end of program"),
            UnclosedDelimiter => write!(f, "Unclosed delimiter"),
        }
    }
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl std::error::Error for ParseError {}

impl SourceError for ParseError {
    fn get_span(&self) -> Span {
        self.span
    }
}


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
    Or,             // or
    Xor,            // xor
    And,            // and
    Equality,       // == !=
    Comparison,     // < > <= >=
    Term,           // + - ++
    Factor,         // * / %
    Unary,          // ! -
    Call,           // . ()
    Atom,
}

impl Precedence {
    const LOWEST: Precedence = Precedence::Or;

    fn next(self) -> Self {
        use Precedence::*;

        match self {
            Or => Xor,
            Xor => And,
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
}

fn get_binary_operator(token: &Token)
-> Option<(Precedence, BinaryOp)> {
    use TokenKind::*;
    use BinaryOp::*;
    use Precedence::*;

    match &token.kind {
        Plus => Some((Term, Add)),
        Minus => Some((Term, Sub)),
        Star => Some((Factor, Mul)),
        Slash => Some((Factor, Div)),
        Percent => Some((Factor, Mod)),
        TokenKind::And => Some((Precedence::And, BinaryOp::And)),
        TokenKind::Or => Some((Precedence::Or, BinaryOp::Or)),
        TokenKind::Xor => Some((Precedence::Xor, BinaryOp::Xor)),
        EqualEqual => Some((Equality, BinaryOp::Eq)),
        BangEqual => Some((Equality, BinaryOp::Ne)),
        Lesser => Some((Comparison, BinaryOp::Lt)),
        Greater => Some((Comparison, BinaryOp::Gt)),
        LesserEqual => Some((Comparison, BinaryOp::Le)),
        GreaterEqual => Some((Comparison, BinaryOp::Ge)),
        PlusPlus => Some((Term, Concat)),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    fn span_from_token_indices(&self, start: usize, end: usize) -> Span {
        let max_index = self.tokens.len() - 1;
        let start = start.clamp(0, max_index);
        let end = end.clamp(0, max_index);
        let Span { start, line, .. } = self.tokens[start].span;
        let Span { end, .. } = self.tokens[end].span;
        Span { start, end, line }
    }

    fn make_expr(&self, start_index: usize, kind: ExprKind) -> Expr {
        let span = self.span_from_token_indices(start_index, self.index - 1);
        Expr { span, kind, ty: None }
    }

    fn make_error(&mut self, kind: ParseErrorKind, start: usize, end: usize) {
        let span = self.span_from_token_indices(start, end);
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
            Some(&TokenKind::IntLiteral(i)) => {
                self.index += 1;
                Ok(Some(self.make_expr(start, ExprKind::Int(i))))
            }
            Some(TokenKind::String(s)) => {
                self.index += 1;
                Ok(Some(self.make_expr(start, ExprKind::String(s.clone()))))
            }
            Some(TokenKind::True) => {
                self.index += 1;
                Ok(Some(self.make_expr(start, ExprKind::True)))
            }
            Some(TokenKind::False) => {
                self.index += 1;
                Ok(Some(self.make_expr(start, ExprKind::False)))
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
                expr.map(|expr| {
                    let kind = ExprKind::Paren(Box::new(expr));
                    Some(self.make_expr(start, kind))
                })
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
            let kind = ExprKind::Binary(op, Box::new(ret), Box::new(right));
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
    use crate::lexer::lex;
    use crate::token::Span;

    #[test]
    fn int() {
        let tokens = lex("123").unwrap();
        let program = parse(&tokens).expect("failed to parse");
        assert_eq!(
            program.body,
            Expr {
                span: Span {
                    start: 0,
                    end: 3,
                    line: 0,
                },
                kind: ExprKind::Int(123),
                ty: None,
            }
        );
    }

    #[test]
    fn addition() {
        let tokens = lex("1 + 2").unwrap();
        let program = parse(&tokens).expect("failed to parse");
        assert_eq!(
            program.body,
            Expr {
                span: Span {
                    start: 0,
                    end: 5,
                    line: 0,
                },
                kind: ExprKind::Binary(
                    BinaryOp::Add,
                    Box::new(Expr {
                        span: Span {
                            start: 0,
                            end: 1,
                            line: 0,
                        },
                        kind: ExprKind::Int(1),
                        ty: None,
                    }),
                    Box::new(Expr {
                        span: Span {
                            start: 4,
                            end: 5,
                            line: 0,
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

