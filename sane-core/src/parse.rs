use pest::Span;
use pest::iterators::Pair;
use pest::error::InputLocation;
use pest::prec_climber::{Assoc, Operator, PrecClimber};
use std::rc::Rc;
use std::fmt::{Debug};
use lazy_static;

use crate::error::Error;
use crate::let_in::LetIn;
use crate::ident::Ident;
use crate::const_expr::Const;
use crate::bind::Bind;
use crate::fun::Fun;
use crate::list::List;
use crate::pest::Parser;
use crate::if_then_else::IfThenElse;
use crate::build_in::BuildIn;
use crate::binary::Binary;
use crate::file::{File, NSpace, Def};

pub trait ToSource {
    fn to_source(&self) -> String;
}

pub trait FromPair {
    fn from_pair(pair: Pair<'_, Rule>, source: &str) -> ExprResult;
}

pub trait ExprEq {
    fn expr_eq(&self, other: &Expr) -> bool;
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Position {
    pub start: usize,
    pub end: usize,
    pub source: Rc<str>
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Selection {
    pub start: (usize, usize),
    pub end: (usize, usize),
    pub source: Rc<str>
}

impl Selection {
    pub fn new(start: (usize, usize), end: (usize, usize), source: &str) -> Selection {
        Selection { start, end, source: Rc::from(source) }
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
            source: Rc::from(source)
        }
    }

    pub fn from_span(span: Span, source: &str) -> Position {
        Position::new(span.start(), span.end(), source)
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
    Bind(Bind),
    Fun(Fun),
    List(List),
    IfThenElse(IfThenElse),
    Binary(Binary),
    BuildIn(BuildIn),
    // These are rather statesments and not 1st class citizens
    File(File),
    NSpace(NSpace),
    Def(Def),
}

impl ExprEq for Expr {
    fn expr_eq(&self, other: &Expr) -> bool {
        match self {
            Expr::Const(const_) => const_.expr_eq(other),
            Expr::List(list) => list.expr_eq(other),
            Expr::Fun(fun) => fun.expr_eq(other),
            _ => unreachable!()
        }
    }
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
            Expr::Binary(binary) => binary.to_source(),
            Expr::File(file) => file.to_source(),
            Expr::NSpace(n_space) => n_space.to_source(),
            Expr::Def(def) => def.to_source(),
            _ => format!("{:?}", self)
        }
    }
}

impl FromPair for Expr {
    fn from_pair(pair: Pair<'_, Rule>, source: &str) -> ExprResult {
        let rule = pair.as_rule();
        match rule {
            Rule::file => File::from_pair(pair, source),
            Rule::infix => climb(pair, source),
            Rule::constant => Const::from_pair(pair, source),
            Rule::let_in => LetIn::from_pair(pair, source),
            Rule::ident => Ident::from_pair(pair, source),
            Rule::bind => Bind::from_pair(pair, source),
            Rule::fun => Fun::from_pair(pair, source),
            Rule::list => List::from_pair(pair, source),
            // TODO Should be fixed when nspaces implemented
            Rule::nspaced_ident => Ident::from_pair(pair, source),
            Rule::if_then_else => IfThenElse::from_pair(pair, source),
            _ => {
                let position: Position = Position::from_span(pair.as_span(), source);
                Error::new(&format!("Unknown rule `{:?}`", rule), &position).into()
            }
        }
    }
}

pub fn parse_file(input: &str, source: &str) -> ExprResult {
    let parse_result = SaneParser::parse(Rule::file, input);
    match parse_result {
        Ok(mut pairs) => {
            if let Some(pair) = pairs.next() {
                Expr::from_pair(pair, source)
            } else {
                Err(Error::new("Cant't find any expression", &Position::new(1, 1, source)))
            }
        },
        Err(err) => {
            let position = match err.location {
                InputLocation::Pos(pos) => Position::new(pos, pos, source),
                InputLocation::Span(span) => Position::new(span.0, span.1, source),
            };
            Err(Error::new(&format!("Can't parse: {}", err), &position))
        }
    }
}

pub fn parse_sane(input: &str) -> ExprResult {
    parse_file(input, "ADHOC")
}

fn precedence_climber() -> PrecClimber<Rule> {
    PrecClimber::new(vec![
        Operator::new(Rule::op_left_pipe, Assoc::Left) | Operator::new(Rule::op_right_pipe, Assoc::Right),
        Operator::new(Rule::op_dollar, Assoc::Right),
    ])
}

lazy_static! {
    pub static ref PREC_CLIMBER: PrecClimber<Rule> = precedence_climber();
}

pub fn climb(pair: Pair<'_, Rule>, source: &str) -> ExprResult {
    let primary = |pair| {
        Expr::from_pair(pair, source)
    };

    let build_binary = |lhs, op, rhs| {
        Binary::new(op, lhs, rhs, source)
    };

    PREC_CLIMBER.climb(pair.into_inner(), primary, build_binary)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::const_expr::ConstType;

    #[test]
    fn parse_number() {
        let result = &*parse_sane("-23.1").unwrap();
        assert_eq!(result, &Expr::Const(Const { value: ConstType::Numeric(-23.1), position: Position::new(0, 5, "ADHOC") } ));
    }

    #[test]
    fn parse_string() {
        let result = &*parse_sane("\"test\"").unwrap();
        assert_eq!(result, &Expr::Const(Const { value: ConstType::String("test".into()), position: Position::new(1, 5, "ADHOC") }));
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
        let result = parse_sane("(f(1))(2)").unwrap().to_source();
        assert_eq!(result, "(f(1.0))(2.0)");
    }

    #[test]
    fn parse_fun() {
        let result = &*parse_sane("fun a => a").unwrap().to_source();
        assert_eq!(result, "fun a => a");
    }

    #[test]
    fn parse_file_0() {
        let result = &*parse_sane(r#"
        nspace A = a
          
        def a = fun b => a
          
        1 |> A.a  "#).unwrap().to_source();
        assert_eq!(result, "fun a => a");
    }

    #[test]
    fn from_content_0() {
        let source = "a\nab\nabc\n";
        
        let result = Selection::from_content(source, &Position::new(0, 1, "ADHOC"));
        assert_eq!(result, Selection { start: (1, 1), end: (1, 2), source: Rc::from("ADHOC") });

        let result = Selection::from_content(source, &Position::new(2, 3, "ADHOC"));
        assert_eq!(result, Selection { start: (2, 1), end: (2, 2), source: Rc::from("ADHOC") });

        let result = Selection::from_content(source, &Position::new(0, 7, "ADHOC"));
        assert_eq!(result, Selection { start: (1, 1), end: (3, 3), source: Rc::from("ADHOC") });
    }
}
