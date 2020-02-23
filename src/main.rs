extern crate pest;
#[macro_use]
extern crate pest_derive;


use pest::Parser;
use pest::iterators::{Pair, Pairs};
use std::borrow::Borrow;

fn main() {}

#[derive(Parser)]
#[grammar = "sane.pest"]
pub struct SaneParser;

#[derive(Debug, PartialEq)]
struct LetIn {
    pub var: String,
    pub value: Box<Expr>,
    pub in_part: Box<Expr>,
}

#[derive(Debug, PartialEq)]
struct Bind {
    pub arg: Box<Expr>,
    pub fun: Box<Expr>,
}

#[derive(Debug, PartialEq)]
struct Fun {
    pub param: String,
    pub body: Box<Expr>,
}

#[derive(Debug, PartialEq)]
enum Const {
    String(String),
    Numeric(f64),
}

#[derive(Debug, PartialEq)]
enum Expr {
    LetIn(LetIn),
    Const(Const),
    Ident(String),
    Bind(Bind),
    Fun(Fun),
}

fn parse_const(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::string => Ok(Box::new(Expr::Const(Const::String(pair.as_str().to_string())))),
        Rule::number => {
            let value = pair.as_str().parse::<f64>().unwrap();
            Ok(Box::new(Expr::Const(Const::Numeric(value))))
        }
        _ => Err(format!("Unknown const type: `{:?}`", pair))
    }
}

fn parse_let_in(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let mut inner: Pairs<'_, Rule> = pair.into_inner();
    let ident = inner.next().unwrap();
    let value = inner.next().unwrap();
    let in_part = inner.next().unwrap();

    Ok(Box::new(Expr::LetIn(LetIn {
        var: ident.as_str().to_string(),
        value: parse_pair(value)?,
        in_part: parse_pair(in_part)?,
    })))
}

fn parse_ident(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    Ok(Box::new(Expr::Ident(pair.as_str().to_string())))
}

fn parse_fun(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let mut inner: Pairs<'_, Rule> = pair.into_inner();
    let ident = inner.next().unwrap();
    let body = inner.next().unwrap();

    Ok(Box::new(Expr::Fun(Fun {
        param: ident.as_str().to_string(),
        body: parse_pair(body)?,
    })))
}

fn parse_bind(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let mut inner: Pairs<'_, Rule> = pair.into_inner();
    let arg = inner.next().unwrap();
    let fun = inner.next().unwrap();

    Ok(Box::new(Expr::Bind(Bind {
        arg: parse_pair(arg)?,
        fun: parse_pair(fun)?,
    })))
}

fn parse_pair(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let rule = pair.as_rule();
    match rule {
        Rule::constant => parse_const(pair),
        Rule::let_in => parse_let_in(pair),
        Rule::ident => parse_ident(pair),
        Rule::bind => parse_bind(pair),
        Rule::fun => parse_fun(pair),
        _ => {
            Err(format!("Unknown rule `{:?}`", rule))
        }
    }
}

fn parse_sane(input: &str) -> Result<Box<Expr>, String> {
    let parsed = SaneParser::parse(Rule::expr, input)
        .expect("Can't parse")
        .next()
        .unwrap();

    // let result: Option<pest::iterators::Pair<'_, Rule>> = parsed;
    // println!("-----------{:?}", parsed);

    parse_pair(parsed)
}

type Stack = Vec<(String, Box<Expr>)>;

fn execute_sane(input: &str) -> Result<Box<Expr>, String> {
    let expr = parse_sane(input)?;
    let stack = &mut vec![];
    execute(expr, stack)
}

fn execute(expr: Box<Expr>, stack: &mut Stack) -> Result<Box<Expr>, String> {
    match *expr {
        Expr::LetIn(let_in) => {
            stack.push((let_in.var.clone(), let_in.value));
            let result = execute(let_in.in_part, stack);
            stack.pop();
            result
        },
        // Expr::Ident(ident) => {
        //     if let Some(item) = stack.iter().rev().find(|item| { item.0 == ident}) {
        //         Ok(item.1)
        //     } else {
        //         Err(format!("Ident `{:?}` not found", ident))
        //     }
        // },
        _ => Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::ops::Deref;

    #[test]
    fn parse_number() {
        let result = *parse_sane("-23.1").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(-23.1)));
    }

    #[test]
    fn parse_string() {
        let result = *parse_sane("\"test\"").unwrap();
        assert_eq!(result, Expr::Const(Const::String("test".to_string())));
    }

    #[test]
    fn parse_let_in() {
        let result = *parse_sane("let a = 1 in a").unwrap();
        assert_eq!(result, Expr::LetIn(LetIn { var: "a".to_string(), value: Box::new(Expr::Const(Const::Numeric(1.0))), in_part: Box::new(Expr::Ident("a".to_string())) }));
    }

    #[test]
    fn parse_bind() {
        let result = *parse_sane("1 -> f").unwrap();
        assert_eq!(result, Expr::Bind(Bind { arg: Box::new(Expr::Const(Const::Numeric(1.0))), fun: Box::new(Expr::Ident("f".to_string())) }));
    }

    #[test]
    fn parse_fun() {
        let result = *parse_sane("fun a => a").unwrap();
        assert_eq!(result, Expr::Fun(Fun { param: "a".to_string(), body: Box::new(Expr::Ident("a".to_string())) }));
    }

    #[test]
    fn test_execute() {
        let result = *execute_sane("let a = 1 in a").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(-23.1)));
    }
}
