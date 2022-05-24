//! This module contains the parser for the language.

use crate::ast::*;
use crate::reporting::annotation::{Located, Region};
use crate::reporting::error::parsing::Error;
use crate::token::prelude::*;
use std::rc::Rc;

type EKind = ExprKind;

type L<T> = Located<T>;

pub fn parse<'a>(tokens: &'a [L<Token>]) -> Result<Program, Vec<L<Error>>> {
    let mut parser = Parser::new(tokens);

    match parser.program() {
        Ok(program) if parser.errors.is_empty() => Ok(program),
        _ => Err(parser.errors),
    }
}

#[derive(Debug)]
struct Parser<'a> {
    tokens: &'a [L<Token>],
    index: usize,
    errors: Vec<L<Error>>,
}

fn try_kind(token: Option<&L<Token>>) -> Option<&Token> {
    token.map(|t| &t.value)
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
    match token {
        Token::Minus => Some(U::Neg),
        Token::Not => Some(U::Not),
        _ => None,
    }
}

fn get_binary_operator(token: &Token) -> Option<(Precedence, BinaryOp)> {
    type B = BinaryOp;
    type P = Precedence;
    match &token {
        Token::Plus => Some((P::Term, B::Add)),
        Token::Minus => Some((P::Term, B::Sub)),
        Token::Star => Some((P::Factor, B::Mul)),
        Token::Slash => Some((P::Factor, B::Div)),
        Token::Percent => Some((P::Factor, B::Mod)),
        Token::And => Some((P::And, B::And)),
        Token::Or => Some((P::Or, B::Or)),
        Token::Xor => Some((P::Xor, B::Xor)),
        Token::EqualEqual => Some((P::Equality, B::Eq)),
        Token::BangEqual => Some((P::Equality, B::Ne)),
        Token::Lesser => Some((P::Comparison, B::Lt)),
        Token::Greater => Some((P::Comparison, B::Gt)),
        Token::LesserEqual => Some((P::Comparison, B::Le)),
        Token::GreaterEqual => Some((P::Comparison, B::Ge)),
        Token::PlusPlus => Some((P::Term, B::Concat)),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [L<Token>]) -> Self {
        Self {
            tokens,
            index: 0,
            errors: Vec::new(),
        }
    }

    fn in_range(&self) -> bool {
        self.index < self.tokens.len()
    }

    /// Get a range from a (inclusive) start and an (inclusive) range.
    fn region_from_index_range(&self, start: usize, end: usize) -> Region {
        // Edge case - no tokens at all
        if self.tokens.is_empty() {
            return Region::default();
        }

        let max_index = self.tokens.len() - 1;
        let start = start.clamp(0, max_index);
        let end = end.clamp(start, max_index);
        let start_region = self.tokens[start].region;
        let end_region = self.tokens[end].region;
        start_region.merge(&end_region)
    }

    fn report_error(&mut self, start: usize, error: Error) {
        let region = self.region_from_index_range(start, self.index - 1);
        self.errors.push(L {
            region,
            value: error,
        });
    }

    fn report_error_here(&mut self, error: Error) {
        self.report_error(self.index, error)
    }

    fn located_at_indicies<T>(&self, start_index: usize, end_index: usize, value: T) -> L<T> {
        let region = self.region_from_index_range(start_index, end_index - 1);
        L { region, value }
    }

    fn located<T>(&self, start_index: usize, value: T) -> L<T> {
        self.located_at_indicies(start_index, self.index, value)
    }

    fn parse_atom(&mut self) -> Result<Option<Expr>, ()> {
        let start = self.index;
        match try_kind(self.tokens.get(self.index)) {
            Some(&Token::IntLiteral(i)) => {
                self.index += 1;
                Ok(Some(self.located(start, ExprKind::Int(i)).into()))
            }
            Some(Token::String(s)) => {
                self.index += 1;
                Ok(Some(
                    self.located(start, ExprKind::String(s.clone())).into(),
                ))
            }
            Some(Token::True) => {
                self.index += 1;
                Ok(Some(self.located(start, ExprKind::True).into()))
            }
            Some(Token::False) => {
                self.index += 1;
                Ok(Some(self.located(start, ExprKind::False).into()))
            }
            Some(Token::Identifier(name)) => {
                self.index += 1;
                Ok(Some(
                    self.located(start, ExprKind::Var(name.clone(), None))
                        .into(),
                ))
            }
            Some(&Token::LeftParen) => {
                self.index += 1;
                let expr = self.expr();
                if try_kind(self.tokens.get(self.index)) != Some(&Token::RightParen) {
                    self.report_error_here(Error::UnclosedDelimiter);
                }
                self.index += 1;
                expr.map(|expr| {
                    let kind = ExprKind::Paren(Box::new(expr));
                    Some(self.located(start, kind).into())
                })
            }
            _ => Ok(None),
        }
    }

    fn application_exprs(&mut self) -> Result<Option<Expr>, ()> {
        let start = self.index;
        // Collect the atoms
        let mut atoms: Option<Vec<(Expr, usize)>> = Some(Vec::new());
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
                (
                    self.located_at_indicies(start, right_index, kind).into(),
                    right_index,
                )
            })
            .map(|(expr, _)| expr))
    }

    fn unary_expr(&mut self) -> Result<Option<Expr>, ()> {
        let start = self.index;
        if let Some(op) = try_kind(self.tokens.get(self.index)).and_then(get_unary_operator) {
            self.index += 1;
            self.parse_atom().and_then(|atom| {
                if let Some(atom) = atom {
                    let kind = EKind::Unary(op, Box::new(atom));
                    Ok(Some(self.located(start, kind).into()))
                } else {
                    self.report_error(start, Error::UnaryOperatorMissingOperand);
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
                self.report_error_here(Error::ExpectedExpressionAtom);
                Err(())
            };
        }

        let next_precedence = lowest_precedence.next();

        let start = self.index;
        let mut ret = self.parse_precedence(next_precedence)?;

        while let Some(token) = self.tokens.get(self.index) {
            let (precedence, op) = match get_binary_operator(&token.value) {
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
            ret = self.located(start, kind).into();
        }

        Ok(ret)
    }

    fn token_kind_eq(&mut self, kind: Token) -> Result<(), ()> {
        if try_kind(self.tokens.get(self.index)) == Some(&kind) {
            self.index += 1;
            Ok(())
        } else {
            self.report_error_here(Error::ExpectedToken(kind));
            Err(())
        }
    }

    fn identifier(&mut self) -> Result<Rc<str>, ()> {
        if let Some(Token::Identifier(name)) = try_kind(self.tokens.get(self.index)) {
            self.index += 1;
            Ok(name.clone())
        } else {
            self.index += 1;
            self.report_error_here(Error::ExpectedIdentifier);
            Err(())
        }
    }

    fn let_expr(&mut self) -> Result<Expr, ()> {
        let start = self.index;
        let _ = self.token_kind_eq(Token::Let);
        let name = self.identifier();
        let _ = self.token_kind_eq(Token::Equal);
        let binding = self.expr();
        let _ = self.token_kind_eq(Token::In);
        let body = self.expr();

        Ok(self
            .located(
                start,
                ExprKind::Let {
                    name: name?,
                    unique_name: None,
                    binding: Box::new(binding?),
                    expr: Box::new(body?),
                },
            )
            .into())
    }

    fn if_expr(&mut self) -> Result<Expr, ()> {
        let start = self.index;
        let _ = self.token_kind_eq(Token::If);
        let cond = self.expr();
        let _ = self.token_kind_eq(Token::Then);
        let then = self.expr();
        let _ = self.token_kind_eq(Token::Else);
        let else_ = self.expr();

        Ok(self
            .located(
                start,
                ExprKind::If {
                    cond: Box::new(cond?),
                    then: Box::new(then?),
                    else_: Box::new(else_?),
                },
            )
            .into())
    }

    fn type_expr_atom(&mut self) -> Result<Located<TypeExpr>, ()> {
        let start = self.index;
        if let Some(Token::LeftParen) = try_kind(self.tokens.get(self.index)) {
            self.index += 1;
            let expr = self.type_expr();
            self.token_kind_eq(Token::RightParen)?;
            return Ok(self.located(start, TypeExpr::Paren(Box::new(expr?))));
        }
        let identifier = self.identifier();
        Ok(self.located(start, TypeExpr::Var(identifier?)))
    }

    fn function_type_expr(&mut self) -> Result<Located<TypeExpr>, ()> {
        let start = self.index;
        let atom = self.type_expr_atom();
        match try_kind(self.tokens.get(self.index)) {
            Some(Token::Arrow) => {
                self.index += 1;
                let type_expr = self.type_expr();
                let kind = TypeExpr::Function(Box::new(atom?), Box::new(type_expr?));
                Ok(self.located(start, kind).into())
            }
            _ => Ok(atom?),
        }
    }

    fn type_expr(&mut self) -> Result<Located<TypeExpr>, ()> {
        let start = self.index;
        let res = self.function_type_expr();
        if res.is_err() {
            self.report_error(start, Error::ExpectedTypeExpr);
            return Err(());
        }
        res
    }

    fn function_expr(&mut self) -> Result<Option<Expr>, ()> {
        let start = self.index;
        let errors = self.errors.len();
        let arg_name = self.identifier();
        let arg_type_expr = {
            if try_kind(self.tokens.get(self.index)) == Some(&Token::Colon) {
                self.index += 1;
                self.type_expr().map(Some)
            } else {
                Ok(None)
            }
        };
        if self.token_kind_eq(Token::FatArrow).is_ok() {
            let body = self.expr();
            Ok(Some(
                self.located(
                    start,
                    ExprKind::Function {
                        arg: NameDeclaration::new(arg_name?, arg_type_expr?),
                        recursion_var: None,
                        body: Box::new(body?),
                    },
                )
                .into(),
            ))
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
            Some(Token::Let) => self.let_expr(),
            Some(Token::If) => self.if_expr(),
            _ => self.function_expr().and_then(|function_expr| {
                if let Some(function_expr) = function_expr {
                    Ok(function_expr)
                } else {
                    self.parse_precedence(Precedence::LOWEST)
                }
            }),
        };
        if res.is_err() {
            self.report_error(start, Error::ExpectedExpression);
        }
        res
    }

    pub fn program(&mut self) -> Result<Program, ()> {
        // Edge case - empty file
        if self.tokens.is_empty() {
            self.index += 1;
            self.report_error_here(Error::EmptyCode);
            return Err(());
        }

        let body = self.expr();
        if self.in_range() {
            self.report_error_here(Error::LeftoverSource);
        }
        body.map(|body| Program { body })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::lex;
    use crate::reporting::annotation::Position;

    #[test]
    fn int() {
        let tokens = lex("123").unwrap();
        let program = parse(&tokens).expect("failed to parse");
        assert_eq!(
            program.body,
            Expr {
                span: Region(
                    Position { line: 0, column: 0 },
                    Position { line: 0, column: 3 }
                ),
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
                span: Region(
                    Position { line: 0, column: 0 },
                    Position { line: 0, column: 5 }
                ),
                kind: ExprKind::Binary(
                    BinaryOp::Add,
                    Box::new(Expr {
                        span: Region(
                            Position { line: 0, column: 0 },
                            Position { line: 0, column: 1 },
                        ),
                        kind: ExprKind::Int(1),
                        ty: None,
                    }),
                    Box::new(Expr {
                        span: Region(
                            Position { line: 0, column: 4 },
                            Position { line: 0, column: 5 },
                        ),
                        kind: ExprKind::Int(2),
                        ty: None,
                    }),
                ),
                ty: None,
            }
        );
    }
}
