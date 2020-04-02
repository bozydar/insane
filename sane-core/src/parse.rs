use pest::Span;
use pest::iterators::Pair;
use std::rc::Rc;


use std::fmt::{Debug};
use crate::let_in::LetIn;
use crate::ident::Ident;
use crate::const_expr::Const;
use crate::bind::Bind;
use crate::fun::Fun;
use crate::list::List;
use crate::pest::Parser;
use crate::if_then_else::IfThenElse;
use crate::build_in::BuildIn;

pub trait ToSource {
    fn to_source(&self) -> String;
}

pub trait FromPair {
    fn from_pair(pair: Pair<'_, Rule>) -> ExprResult;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq, Copy)]
pub struct Position {
    pub start: usize,
    pub end: usize,
}

impl Position {
    fn new(start: usize, end: usize) -> Position {
        Position {
            start,
            end,
        }
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Error {
    message: String,
    position: Position,
}

impl Error {
    pub fn new(message: &str, position: Position) -> Self {
        Self {
            message: message.to_string(),
            position,
        }
    }
}

pub type ExprResult = Result<Rc<Expr>, Error>;

impl Into<ExprResult> for Error {
    fn into(self) -> ExprResult {
        Result::Err(self)
    }
}

impl<'i> From<Span<'i>> for Position {
    fn from(span: Span<'i>) -> Self {
        Position { start: span.start(), end: span.end() }
    }
}

impl<'i, R> From<Pair<'i, R>> for Position where R: pest::RuleType {
    fn from(pair: Pair<'i, R>) -> Self {
        let span = pair.as_span();
        Position { start: span.start(), end: span.end() }
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
    Bind(Bind),
    Fun(Fun),
    List(List),
    IfThenElse(IfThenElse),
    BuildIn(BuildIn),
}

impl ToSource for Expr {
    fn to_source(&self) -> String {
        match self {
            Expr::LetIn(let_in) => let_in.to_source(),
            Expr::Const(const_) => const_.to_source(),
            Expr::Ident(ident) => ident.to_source(),
            Expr::Bind(bind) => bind.to_source(),
            Expr::List(list) => list.to_source(),
            Expr::IfThenElse(if_then_else) => if_then_else.to_source(),
            Expr::Fun(fun) => fun.to_source(),
            _ => format!("{:?}", self)
        }
    }
}

impl FromPair for Expr {
    fn from_pair(pair: Pair<'_, Rule>) -> ExprResult {
        let rule = pair.as_rule();
        match rule {
            Rule::file => parse_file(pair),
            Rule::constant => Const::from_pair(pair),
            Rule::let_in => LetIn::from_pair(pair),
            Rule::ident => Ident::from_pair(pair),
            Rule::bind => Bind::from_pair(pair),
            Rule::fun => Fun::from_pair(pair),
            Rule::list => List::from_pair(pair),
            Rule::if_then_else => IfThenElse::from_pair(pair),
            _ => {
                let position: Position = From::from(pair.as_span());
                Error::new(&format!("Unknown rule `{:?}`", rule), position).into()
            }
        }
    }
}

fn parse_file(pair: Pair<'_, Rule>) -> ExprResult {
    let pair = pair.into_inner().next().unwrap();
    Expr::from_pair(pair)
}

pub fn parse_sane(input: &str) -> ExprResult {
    let parsed = SaneParser::parse(Rule::file, input)
        .expect("Can't sane-core")
        .next()
        .unwrap();

    // let result: Option<pest::iterators::Pair<'_, Rule>> = parsed;
    // println!("-----------{:?}", parsed);

    Expr::from_pair(parsed)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::const_expr::ConstType;

    #[test]
    fn parse_number() {
        let result = &*parse_sane("-23.1").unwrap();
        assert_eq!(result, &Expr::Const(Const { value: ConstType::Numeric(-23.1), position: Position { start: 0, end: 5 } }));
    }

    #[test]
    fn parse_string() {
        let result = &*parse_sane("\"test\"").unwrap();
        assert_eq!(result, &Expr::Const(Const { value: ConstType::String("test".into()), position: Position { start: 1, end: 5 } }));
    }

    #[test]
    fn parse_let_in_0() {
        let result = parse_sane("let a = 1 in a").unwrap().to_source();
        assert_eq!(result, "let a = 1.0 in a");
    }

    #[test]
    fn parse_let_in_1() {
        let result = parse_sane("let a = 1 and let b = 2 in [a; b]").unwrap().to_source();
        assert_eq!(result, "let a = 1.0 and let b = 2.0 in [a; b]");
    }

    #[test]
    fn parse_bind_1() {
        let result = parse_sane("app 2 to app 1 to f").unwrap().to_source();
        assert_eq!(result, "app 2.0 to app 1.0 to f");
    }

    #[test]
    fn parse_fun() {
        let result = &*parse_sane("fun a => a").unwrap().to_source();
        assert_eq!(result, "fun a => a");
    }
}