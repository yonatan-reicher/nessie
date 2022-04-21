//! This module contains the parser for the language.

use crate::ast::*;
use crate::source_error::SourceError;
use crate::token::prelude::*;
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

type EKind = ExprKind;

type TEKind = TypeExprKind;

pub fn parse<'a>(tokens: &'a [Token]) -> Result<Program, Vec<Error>> {
    let mut parser = Parser::new(tokens);

    match parser.program() {
        Ok(program) if parser.errors.is_empty() => Ok(program),
        _ => Err(parser.errors),
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ErrorKind {
    ExpectedExpression,
    ExpectedExpressionAtom,
    LeftoverSource,
    UnclosedDelimiter,
    ExpectedIdentifier,
    ExpectedToken(TokenKind),
    EmptyCode,
    ExpectedTypeExpr,
    UnaryOperatorMissingOperand,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use ErrorKind::*;
        match self {
            ExpectedExpression => write!(f, "Expected expression"),
            ExpectedExpressionAtom => write!(f, "Expected expression atom"),
            UnclosedDelimiter => write!(f, "Unclosed delimiter"),
            LeftoverSource => write!(f, "The source code contained leftover characters"),
            ExpectedIdentifier => write!(f, "Expected identifier"),
            ExpectedToken(tk) => write!(f, "Expected a {}", tk),
            EmptyCode => write!(f, "The source code is empty"),
            ExpectedTypeExpr => write!(f, "Expected type expression"),
            UnaryOperatorMissingOperand => write!(f, "Unary operator missing operand"),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl std::error::Error for Error {}

impl SourceError for Error {
    fn get_span(&self) -> Span {
        self.span
    }
}

type TKind = TokenKind;

#[derive(Debug)]
struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    errors: Vec<Error>,
}

fn try_kind(token: Option<&Token>) -> Option<&TokenKind> {
    token.map(|t| &t.kind)
}

/// Note: the meaning of precedence is not consistent. In this project, highest
/// precedence is the most tightly bound. For example, `*` is higher precedence
/// than `+`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum Precedence {
    // Precedence from lowest to highest.
    Or,          // or
    Xor,         // xor
    And,         // and
    Equality,    // == !=
    Comparison,  // < > <= >=
    Term,        // + - ++
    Factor,      // * / %
    Application, // x y, ! -
    Call,        // . ()
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
            Factor => Application,
            Application => Call,
            Call => Atom,
            Atom => Atom,
        }
    }
}

fn get_unary_operator(token: &Token) -> Option<UnaryOp> {
    type U = UnaryOp;
    match &token.kind {
        TKind::Minus => Some(U::Neg),
        TKind::Not => Some(U::Not),
        _ => None,
    }
}

fn get_binary_operator(token: &Token) -> Option<(Precedence, BinaryOp)> {
    type B = BinaryOp;
    type P = Precedence;
    match &token.kind {
        TKind::Plus => Some((P::Term, B::Add)),
        TKind::Minus => Some((P::Term, B::Sub)),
        TKind::Star => Some((P::Factor, B::Mul)),
        TKind::Slash => Some((P::Factor, B::Div)),
        TKind::Percent => Some((P::Factor, B::Mod)),
        TKind::And => Some((P::And, B::And)),
        TKind::Or => Some((P::Or, B::Or)),
        TKind::Xor => Some((P::Xor, B::Xor)),
        TKind::EqualEqual => Some((P::Equality, B::Eq)),
        TKind::BangEqual => Some((P::Equality, B::Ne)),
        TKind::Lesser => Some((P::Comparison, B::Lt)),
        TKind::Greater => Some((P::Comparison, B::Gt)),
        TKind::LesserEqual => Some((P::Comparison, B::Le)),
        TKind::GreaterEqual => Some((P::Comparison, B::Ge)),
        TKind::PlusPlus => Some((P::Term, B::Concat)),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            index: 0,
            errors: Vec::new(),
        }
    }

    fn in_range(&self) -> bool {
        self.index < self.tokens.len()
    }

    fn span_from_token_indices(&self, start: usize, end: usize) -> Span {
        // Edge case - no tokens at all
        if self.tokens.is_empty() {
            return Span::empty(0, 0);
        }

        let max_index = self.tokens.len() - 1;
        let start = start.clamp(0, max_index);
        let end = end.clamp(0, max_index);
        let Span { start, line, .. } = self.tokens[start].span;
        let Span { end, .. } = self.tokens[end].span;
        Span { start, end, line }
    }

    fn report_error(&mut self, kind: ErrorKind, start: usize) {
        let span = self.span_from_token_indices(start, self.index);
        self.errors.push(Error { kind, span });
    }

    fn report_error_here(&mut self, kind: ErrorKind) {
        self.report_error(kind, self.index)
    }

    fn make_expr_at(&self, start_index: usize, end_index: usize, kind: ExprKind) -> Expr {
        let span = self.span_from_token_indices(start_index, end_index - 1);
        Expr {
            span,
            kind,
            ty: None,
        }
    }

    fn make_expr(&self, start_index: usize, kind: ExprKind) -> Expr {
        self.make_expr_at(start_index, self.index, kind)
    }

    fn make_type_expr_at(
        &self,
        start_index: usize,
        end_index: usize,
        kind: TypeExprKind,
    ) -> TypeExpr {
        let span = self.span_from_token_indices(start_index, end_index - 1);
        TypeExpr { span, kind }
    }

    fn make_type_expr(&self, start_index: usize, kind: TypeExprKind) -> TypeExpr {
        self.make_type_expr_at(start_index, self.index, kind)
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
                    self.report_error_here(ErrorKind::UnclosedDelimiter);
                }
                self.index += 1;
                expr.map(|expr| {
                    let kind = ExprKind::Paren(Box::new(expr));
                    Some(self.make_expr(start, kind))
                })
            }
            _ => Ok(None),
        }
    }

    fn application_exprs(&mut self) -> Result<Option<Expr>, ()> {
        let start = self.index;
        // Collect the atoms
        let mut atoms = Some(Vec::new());
        loop {
            match self.parse_atom() {
                Ok(Some(atom)) => {
                    if let Some(atoms) = &mut atoms {
                        atoms.push((atom, self.index));
                    }
                }
                Err(()) => {
                    atoms = None;
                }
                // Stop condition
                // TODO: stop at terminator tokens like parentheses or operators
                Ok(None) => {
                    break;
                }
            }
        }
        // Reduce to an expression
        Ok(atoms
            .ok_or(())?
            .into_iter()
            .reduce(|(left, _), (right, right_index)| {
                let kind = EKind::App {
                    func: Box::new(left),
                    arg: Box::new(right),
                };
                (self.make_expr_at(start, right_index, kind), right_index)
            })
            .map(|(expr, _)| expr))
    }

    fn unary_expr(&mut self) -> Result<Option<Expr>, ()> {
        let start = self.index;
        if let Some(op) = self.tokens.get(self.index).and_then(get_unary_operator) {
            self.index += 1;
            self.parse_atom().and_then(|atom| {
                if let Some(atom) = atom {
                    let kind = EKind::Unary(op, Box::new(atom));
                    Ok(Some(self.make_expr(start, kind)))
                } else {
                    self.report_error(ErrorKind::UnaryOperatorMissingOperand, start);
                    Err(())
                }
            })
        } else {
            Ok(None)
        }
    }

    fn parse_precedence(&mut self, lowest_precedence: Precedence) -> Result<Expr, ()> {
        if let Precedence::Application = lowest_precedence {
            return if let Some(expr) = self.unary_expr()? {
                Ok(expr)
            } else if let Some(expr) = self.application_exprs()? {
                Ok(expr)
            } else {
                self.report_error_here(ErrorKind::ExpectedExpressionAtom);
                Err(())
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
            self.report_error_here(ErrorKind::ExpectedToken(kind));
            Err(())
        }
    }

    fn identifier(&mut self) -> Result<Rc<str>, ()> {
        if let Some(TKind::Identifier(name)) = try_kind(self.tokens.get(self.index)) {
            self.index += 1;
            Ok(name.clone())
        } else {
            self.report_error_here(ErrorKind::ExpectedIdentifier);
            Err(())
        }
    }

    fn let_expr(&mut self) -> Result<Expr, ()> {
        let start = self.index;
        let _ = self.token_kind_eq(TKind::Let);
        let name = self.identifier();
        let _ = self.token_kind_eq(TKind::Equal);
        let binding = self.expr();
        let _ = self.token_kind_eq(TKind::In);
        let body = self.expr();

        Ok(self.make_expr(
            start,
            ExprKind::Let {
                name: name?,
                unique_name: None,
                binding: Box::new(binding?),
                expr: Box::new(body?),
            },
        ))
    }

    fn if_expr(&mut self) -> Result<Expr, ()> {
        let start = self.index;
        let _ = self.token_kind_eq(TKind::If);
        let cond = self.expr();
        let _ = self.token_kind_eq(TKind::Then);
        let then = self.expr();
        let _ = self.token_kind_eq(TKind::Else);
        let else_ = self.expr();

        Ok(self.make_expr(
            start,
            ExprKind::If {
                cond: Box::new(cond?),
                then: Box::new(then?),
                else_: Box::new(else_?),
            },
        ))
    }

    fn type_expr_atom(&mut self) -> Result<TypeExpr, ()> {
        let start = self.index;
        let identifier = self.identifier();
        Ok(self.make_type_expr(start, TypeExprKind::Var(identifier?)))
    }

    fn function_type_expr(&mut self) -> Result<TypeExpr, ()> {
        let start = self.index;
        let atom = self.type_expr_atom();
        match try_kind(self.tokens.get(self.index)) {
            Some(TokenKind::Arrow) => {
                self.index += 1;
                let type_expr = self.type_expr();
                let kind = TEKind::Function(Box::new(atom?), Box::new(type_expr?));
                Ok(self.make_type_expr(start, kind))
            }
            _ => Ok(atom?),
        }
    }

    fn type_expr(&mut self) -> Result<TypeExpr, ()> {
        let start = self.index;
        let res = self.function_type_expr();
        if res.is_err() {
            self.report_error(ErrorKind::ExpectedTypeExpr, start);
            return Err(());
        }
        res
    }

    fn function_expr(&mut self) -> Result<Option<Expr>, ()> {
        let start = self.index;
        let errors = self.errors.len();
        let arg_name = self.identifier();
        let arg_type_expr = {
            if try_kind(self.tokens.get(self.index)) == Some(&TKind::Colon) {
                self.index += 1;
                self.type_expr().map(Some)
            } else {
                Ok(None)
            }
        };
        if self.token_kind_eq(TKind::FatArrow).is_ok() {
            let body = self.expr();
            Ok(Some(self.make_expr(
                start,
                ExprKind::Function {
                    arg: NameDeclaration::new(arg_name?, arg_type_expr?),
                    body: Box::new(body?),
                },
            )))
        } else {
            // rollback
            self.index = start;
            self.errors.truncate(errors);
            Ok(None)
        }
    }

    fn expr(&mut self) -> Result<Expr, ()> {
        let start = self.index;
        let res = match try_kind(self.tokens.get(self.index)) {
            Some(TKind::Let) => self.let_expr(),
            Some(TKind::If) => self.if_expr(),
            _ => self.function_expr().and_then(|function_expr| {
                if let Some(function_expr) = function_expr {
                    Ok(function_expr)
                } else {
                    self.parse_precedence(Precedence::LOWEST)
                }
            }),
        };
        if res.is_err() {
            self.report_error(ErrorKind::ExpectedExpression, start);
        }
        res
    }

    pub fn program(&mut self) -> Result<Program, ()> {
        // Edge case - empty file
        if self.tokens.is_empty() {
            self.report_error_here(ErrorKind::EmptyCode);
            return Err(());
        }

        let body = self.expr();
        if self.in_range() {
            self.report_error_here(ErrorKind::LeftoverSource);
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
