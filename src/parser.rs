//! This module contains the parser for the language.

use crate::ast::*;
use crate::source_error::SourceError;
use crate::token::prelude::*;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

type TKind = TokenKind;

struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    errors: Vec<ParseError>,
}


#[derive(Debug)]
pub enum ParseErrorKind {
    ExpectedExpression,
    ExpectedExpressionAtom,
    LeftoverSource,
    UnclosedDelimiter,
    ExpectedIdentifier,
    ExpectedToken(TokenKind),
    EmptyCode,
}

#[derive(Debug)]
pub struct ParseError {
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl Display for ParseErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ParseErrorKind::*;
        match self {
            ExpectedExpression => write!(f, "Expected expression"),
            ExpectedExpressionAtom => write!(f, "Expected expression atom"),
            UnclosedDelimiter => write!(f, "Unclosed delimiter"),
            LeftoverSource => write!(f, "The source code contained leftover characters"),
            ExpectedIdentifier => write!(f, "Expected identifier"),
            ExpectedToken(tk) => write!(f, "Expected a {}", tk),
            EmptyCode => write!(f, "The source code is empty"),
        }
    }
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

    match parser.program() {
        Ok(program) if parser.errors.is_empty() => Ok(program),
        _ => Err(parser.errors),
    }
}

fn try_kind(token: Option<&Token>) -> Option<&TokenKind> {
    token.map(|t| &t.kind)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Precedence {
    // Precedence from lowest to highest.
    Or,         // or
    Xor,        // xor
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + - ++
    Factor,     // * / %
    Unary,      // ! -
    Call,       // . ()
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

fn get_binary_operator(token: &Token) -> Option<(Precedence, BinaryOp)> {
    use BinaryOp::*;
    use Precedence::*;
    use TokenKind::*;

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
    fn skip_until_kind(&mut self, kind: &TokenKind) {
        while let Some(token) = self.tokens.get(self.index) {
            if token.kind == *kind {
                break;
            }
            self.index += 1;
        }
    }

    fn in_range(&self) -> bool {
        self.index < self.tokens.len()
    }

    fn span_from_token_indices(&self, start: usize, end: usize) -> Span {
        // Edge case - no tokens at all
        if self.tokens.is_empty() { return Span::empty(0, 0); }

        let max_index = self.tokens.len() - 1;
        let start = start.clamp(0, max_index);
        let end = end.clamp(0, max_index);
        let Span { start, line, .. } = self.tokens[start].span;
        let Span { end, .. } = self.tokens[end].span;
        Span { start, end, line }
    }

    fn report_error(&mut self, kind: ParseErrorKind, start: usize) {
        let span = self.span_from_token_indices(start, self.index);
        self.errors.push(ParseError { kind, span });
    }

    fn report_error_here(&mut self, kind: ParseErrorKind) {
        self.report_error(kind, self.index)
    }

    fn make_expr(&self, start_index: usize, kind: ExprKind) -> Expr {
        let span = self.span_from_token_indices(start_index, self.index - 1);
        Expr {
            span,
            kind,
            ty: None,
        }
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
            Some(TokenKind::Identifier(name)) => {
                self.index += 1;
                Ok(Some(
                    self.make_expr(start, ExprKind::Var(name.clone(), None)),
                ))
            }
            Some(&TokenKind::LeftParen) => {
                self.index += 1;
                let expr = self.expr();
                if try_kind(self.tokens.get(self.index)) != Some(&TokenKind::RightParen) {
                    self.report_error_here(ParseErrorKind::UnclosedDelimiter);
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
                        self.report_error(ParseErrorKind::ExpectedExpressionAtom, start);
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
                    self.report_error_here(ParseErrorKind::ExpectedExpressionAtom);
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

    fn token_kind_eq(&mut self, kind: TokenKind) -> Result<(), ()> {
        if try_kind(self.tokens.get(self.index)) == Some(&kind) {
            self.index += 1;
            Ok(())
        } else {
            self.report_error_here(ParseErrorKind::ExpectedToken(kind));
            Err(())
        }
    }

    fn identifier(&mut self) -> Result<Rc<str>, ()> {
        if let Some(TKind::Identifier(name)) = try_kind(self.tokens.get(self.index)) {
            self.index += 1;
            Ok(name.clone())
        } else {
            self.report_error_here(ParseErrorKind::ExpectedIdentifier);
            Err(())
        }
    }

    fn let_expr(&mut self) -> Result<Expr, ()> {
        let start = self.index;
        self.token_kind_eq(TKind::Let);
        self.identifier();
        self.token_kind_eq(TKind::Equal);
        let expr = self.expr();
        self.token_kind_eq(TKind::In);
        let body = self.expr();
        
        todo!();
    }

    fn expr(&mut self) -> Result<Expr, ()> {
        let start = self.index;
        let res = match try_kind(self.tokens.get(self.index)) {
            Some(TKind::Let) => self.let_expr(),
            _ => self.parse_precedence(Precedence::LOWEST),
        };
        if res.is_err() {
            self.report_error(ParseErrorKind::ExpectedExpression, start);
        }
        res
    }

    pub fn program(&mut self) -> Result<Program, ()> {
        // Edge case - empty file
        if self.tokens.is_empty() {
            self.report_error_here(ParseErrorKind::EmptyCode);
            return Err(())
        }

        let body = self.expr();
        if self.in_range() {
            self.report_error_here(ParseErrorKind::LeftoverSource);
        }
        body.map(|body| Program { body })
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
