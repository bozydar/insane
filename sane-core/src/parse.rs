use pest::error::InputLocation;
use pest::iterators::Pair;
use pest::iterators::Pairs;
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use pest::Span;
use std::fmt::Debug;
use std::path;
use std::rc::Rc;

use crate::binary::Binary;
use crate::bind::Bind;
use crate::build_in::BuildIn;
use crate::const_expr::Const;
use crate::context::Context;
use crate::error::Error;
use crate::file::File;
use crate::fun::Fun;
use crate::ident::Ident;
use crate::if_then_else::IfThenElse;
use crate::let_in::LetIn;
use crate::list::List;
use crate::ns_ident::NSIdent;
use crate::pest::Parser;

pub trait ToSource {
    fn to_source(&self) -> String;
}

#[derive(Clone)]
pub struct Input<'s> {
    pub pair: Pair<'s, Rule>,
    pub path: &'s str,
    pub source: &'s str,
}

impl<'s> Input<'s> {
    pub fn new(pair: Pair<'s, Rule>, source: &'s str, path: &'s str) -> Self {
        Self { pair, path, source }
    }

    pub fn with_pair(&self, pair: &Pair<'s, Rule>) -> Self {
        Self {
            pair: pair.clone(),
            path: self.path,
            source: self.source,
        }
    }

    pub fn as_rule(&self) -> Rule {
        self.pair.as_rule()
    }

    pub fn into_inner(self) -> Pairs<'s, Rule> {
        self.pair.into_inner()
    }

    pub fn as_span(&self) -> Span {
        self.pair.as_span()
    }

    pub fn pair_as_str(&self) -> &str {
        self.pair.as_str()
    }

    pub fn source_name(&self) -> String {
        let path = path::Path::new(self.path);
        path.file_stem().unwrap().to_str().unwrap().to_string()
    }
}

pub trait FromInput {
    fn from_input(pair: Input<'_>, context: &mut Context) -> ExprResult;
}

pub trait ExprEq {
    fn expr_eq(&self, other: &Expr) -> bool;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Position {
    pub start: usize,
    pub end: usize,
    pub source: Rc<str>,
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Selection {
    pub start: (usize, usize),
    pub end: (usize, usize),
    pub source: Rc<str>,
}

impl Selection {
    pub fn new(start: (usize, usize), end: (usize, usize), source: &str) -> Selection {
        Selection {
            start,
            end,
            source: Rc::from(source),
        }
    }

    pub fn from_content(content: &str, position: &Position) -> Selection {
        fn count_nl_till_end(content: &str, end: usize) -> (usize, usize) {
            let mut count_nl = 1;
            let mut count_column = 1;
            for (index, ch) in content.chars().enumerate() {
                if index == end {
                    break;
                }
                count_column += 1;
                if ch == '\n' {
                    count_nl += 1;
                    count_column = 1;
                }
            }
            (count_nl, count_column)
        }

        let start = count_nl_till_end(content, position.start);
        let end = count_nl_till_end(content, position.end);
        Selection::new(start, end, &position.source)
    }
}

impl Position {
    pub fn new(start: usize, end: usize, source: &str) -> Position {
        Position {
            start,
            end,
            source: Rc::from(source),
        }
    }

    pub fn from_input(input: &Input) -> Position {
        let span = input.as_span();
        Position::new(span.start(), span.end(), &input.path)
    }
}

pub type ExprResult = Result<Rc<Expr>, Error>;

impl Into<ExprResult> for Error {
    fn into(self) -> ExprResult {
        Result::Err(self)
    }
}

#[derive(Parser)]
#[grammar = "sane.pest"]
struct SaneParser;

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
    LetIn(LetIn),
    Const(Const),
    Ident(Ident),
    NSIdent(NSIdent),
    Bind(Bind),
    Fun(Fun),
    List(List),
    IfThenElse(IfThenElse),
    Binary(Binary),
    BuildIn(BuildIn),
    // These are rather statesments and not 1st class citizens
    File(File),
}

impl ExprEq for Expr {
    fn expr_eq(&self, other: &Expr) -> bool {
        match self {
            Expr::Const(const_) => const_.expr_eq(other),
            Expr::List(list) => list.expr_eq(other),
            Expr::Fun(fun) => fun.expr_eq(other),
            _ => unreachable!(),
        }
    }
}

impl ToSource for Expr {
    fn to_source(&self) -> String {
        match self {
            Expr::LetIn(let_in) => let_in.to_source(),
            Expr::Const(const_) => const_.to_source(),
            Expr::Ident(ident) => ident.to_source(),
            Expr::NSIdent(ns_ident) => ns_ident.to_source(),
            Expr::Bind(bind) => bind.to_source(),
            Expr::List(list) => list.to_source(),
            Expr::IfThenElse(if_then_else) => if_then_else.to_source(),
            Expr::Fun(fun) => fun.to_source(),
            Expr::Binary(binary) => binary.to_source(),
            Expr::File(file) => file.to_source(),
            _ => format!("{:?}", self),
        }
    }
}

impl FromInput for Expr {
    fn from_input(input: Input<'_>, context: &mut Context) -> ExprResult {
        let rule = input.as_rule();
        match rule {
            Rule::file => File::from_input(input, context),
            Rule::infix => climb(input, context),
            Rule::constant => Const::from_input(input, context),
            Rule::let_in => LetIn::from_input(input, context),
            Rule::ident => Ident::from_input(input, context),
            Rule::bind => Bind::from_input(input, context),
            Rule::fun => Fun::from_input(input, context),
            Rule::list => List::from_input(input, context),
            Rule::ns_ident => NSIdent::from_input(input, context),
            Rule::if_then_else => IfThenElse::from_input(input, context),
            _ => {
                let position: Position = Position::from_input(&input);
                Error::new(&format!("Unknown rule `{:?}`", rule), &position).into()
            }
        }
    }
}

pub fn parse_file(source: &str, path: &str, context: &mut Context) -> ExprResult {
    let parse_result = SaneParser::parse(Rule::file, source);
    match parse_result {
        Ok(mut pairs) => {
            if let Some(pair) = pairs.next() {
                Expr::from_input(Input::new(pair, source, path), context)
            } else {
                Err(Error::new(
                    "Can't find any expression",
                    &Position::new(1, 1, source),
                ))
            }
        }
        Err(err) => {
            let position = match err.location {
                InputLocation::Pos(pos) => Position::new(pos, pos, source),
                InputLocation::Span(span) => Position::new(span.0, span.1, source),
            };
            Err(Error::new(&format!("Can't parse: {}", err), &position))
        }
    }
}

fn precedence_climber() -> PrecClimber<Rule> {
    PrecClimber::new(vec![
        Operator::new(Rule::op_left_pipe, Assoc::Left)
            | Operator::new(Rule::op_right_pipe, Assoc::Right),
        Operator::new(Rule::op_comma, Assoc::Left),
        Operator::new(Rule::op_dollar, Assoc::Right),
        Operator::new(Rule::op_eq, Assoc::Left) | Operator::new(Rule::op_neq, Assoc::Left),
        Operator::new(Rule::op_plus, Assoc::Left) | Operator::new(Rule::op_minus, Assoc::Left),
        Operator::new(Rule::op_star, Assoc::Left) | Operator::new(Rule::op_slash, Assoc::Left),
    ])
}

lazy_static! {
    pub static ref PREC_CLIMBER: PrecClimber<Rule> = precedence_climber();
}

pub fn climb(input: Input<'_>, context: &mut Context) -> ExprResult {
    let source = &*input.source.clone();
    let primary = |pair: Pair<'_, Rule>| Expr::from_input(input.with_pair(&pair), context);

    let binary_expr = |lhs, op, rhs| Binary::build_expr(op, lhs, rhs, source);

    PREC_CLIMBER.climb(input.clone().into_inner(), primary, binary_expr)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_let_in_0() {
        let context = &mut Context::new(vec![]);
        let result = parse_file("let a = 1 in a", "ADHOC", context)
            .unwrap()
            .to_source();
        assert_eq!(result, "let a = 1.0 in a");
    }

    #[test]
    fn parse_let_in_1() {
        let context = &mut Context::new(vec![]);
        let result = parse_file("let a = 1 and let b = 2 in [a; b]", "ADHOC", context)
            .unwrap()
            .to_source();
        assert_eq!(result, "let a = 1.0 and let b = 2.0 in [a; b]");
    }

    #[test]
    fn parse_bind_1() {
        let context = &mut Context::new(vec![]);
        let result = parse_file("`f(1)`(2)", "ADHOC", context)
            .unwrap()
            .to_source();
        assert_eq!(result, "`f(1.0)`(2.0)");
    }

    #[test]
    fn parse_fun() {
        let context = &mut Context::new(vec![]);
        let result = &*parse_file("fun a => a", "ADHOC", context)
            .unwrap()
            .to_source();
        assert_eq!(result, "fun a => a");
    }

    #[test]
    fn from_content_0() {
        let source = "a\nab\nabc\n";

        let result = Selection::from_content(source, &Position::new(0, 1, "ADHOC"));
        assert_eq!(
            result,
            Selection {
                start: (1, 1),
                end: (1, 2),
                source: Rc::from("ADHOC"),
            }
        );

        let result = Selection::from_content(source, &Position::new(2, 3, "ADHOC"));
        assert_eq!(
            result,
            Selection {
                start: (2, 1),
                end: (2, 2),
                source: Rc::from("ADHOC"),
            }
        );

        let result = Selection::from_content(source, &Position::new(0, 7, "ADHOC"));
        assert_eq!(
            result,
            Selection {
                start: (1, 1),
                end: (3, 3),
                source: Rc::from("ADHOC"),
            }
        );
    }
}
