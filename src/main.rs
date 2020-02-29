extern crate pest;
#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;


use std::collections::HashMap;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use std::borrow::BorrowMut;

fn main() {}

#[derive(Parser)]
#[grammar = "sane.pest"]
pub struct SaneParser;

#[derive(Debug, PartialEq, Clone)]
struct LetIn {
    pub var: String,
    pub value: Box<Expr>,
    pub in_part: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
struct Bind {
    pub arg: Box<Expr>,
    pub fun: Box<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
struct Fun {
    pub param: String,
    pub body: Box<Expr>,
    pub env: Stack,
}

#[derive(Debug, PartialEq, Clone)]
struct List {
    pub items: Vec<Box<Expr>>
}

#[derive(Debug, PartialEq, Clone)]
enum BuildIn {
    Head,
    Tail,
    Eq,
}

#[derive(Debug, PartialEq, Clone)]
enum Const {
    String(String),
    Numeric(f64),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone)]
enum Expr {
    LetIn(LetIn),
    Const(Const),
    Ident(String),
    Bind(Bind),
    Fun(Fun),
    List(List),
    BuildIn(BuildIn),
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
        env: vec![],
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

fn parse_list(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let mut items: Vec<Box<Expr>> = vec![];

    for item in pair.into_inner() {
        items.push(parse_pair(item)?);
    }
    Ok(Box::new(Expr::List(List { items })))
}

fn sane_head(param: Box<Expr>) -> Result<Box<Expr>, String> {
    match *param {
        Expr::List(List { items }) => {
            if let Some(item) = items.first() {
                Ok(item.clone())
            } else {
                Err("The list is empty".to_string())
            }
        }
        _ => Err("Not a list".to_string())
    }
}

fn sane_tail(param: Box<Expr>) -> Result<Box<Expr>, String> {
    match *param {
        Expr::List(List { items }) => {
            let new_items =
                if items.is_empty() {
                    vec![]
                } else {
                    items[1..].to_vec()
                };
            Ok(Box::new(Expr::List(List { items: new_items })))
        }
        _ => Err("Not a list".to_string())
    }
}

fn parse_pair(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let rule = pair.as_rule();
    match rule {
        Rule::constant => parse_const(pair),
        Rule::let_in => parse_let_in(pair),
        Rule::ident => parse_ident(pair),
        Rule::bind => parse_bind(pair),
        Rule::fun => parse_fun(pair),
        Rule::list => parse_list(pair),
        _ => {
            Err(format!("Unknown rule `{:?}`", rule))
        }
    }
}
//
// fn sane_eq_2(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
//     // TODO
//     // 1. Introduce boolean
//     // 2. From this function return another function which returns true when its argument equal to this pair
//     // (autocurrying)
// }

fn sane_eq(param: Box<Expr>, stack: &mut Stack) -> Result<Box<Expr>, String> {
    match *param {
        Expr::List(List { items }) => {
            if let (Some(left), Some(right)) = (items.get(0), items.get(1)) {
                let left_value = execute(left.clone(), stack)?;
                let right_value = execute(right.clone(), stack)?;
                Ok(Box::new(Expr::Const(Const::Bool(left.eq(right)))))
            } else {
                Err("The list is empty".to_string())
            }
        }
        _ => Err("Not a list".to_string())
    }
}

fn generate_build_in_fun(build_in: BuildIn) {}

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
    let stack = &mut vec![
        ("head".to_string(), Box::new(Expr::BuildIn(BuildIn::Head))),
        ("tail".to_string(), Box::new(Expr::BuildIn(BuildIn::Tail))),
        ("eq".to_string(), Box::new(Expr::BuildIn(BuildIn::Eq))),
    ];
    execute(expr, stack)
}

fn execute(expr: Box<Expr>, stack: &mut Stack) -> Result<Box<Expr>, String> {
    match *expr {
        Expr::LetIn(let_in) => {
            stack.push((let_in.var.clone(), let_in.value));
            let result = execute(let_in.in_part, stack);
            stack.pop();
            result
        }
        Expr::Ident(ident) => {
            if let Some(item) = stack.iter().rev().find(|item| { item.0 == ident }) {
                execute(item.1.clone(), stack)  // TODO might be inefficient (optimize referencing)
            } else {
                Err(format!("Ident {:?} not found: {:?}", ident, stack))
            }
        }
        Expr::Bind(Bind { arg, fun }) => {
            match *execute(fun.clone(), stack)? {
                Expr::Fun(Fun { param, body, env }) => {
                    let mut env = env.clone();
                    let env: &mut Stack = env.borrow_mut();
                    env.push((param.clone(), arg));
                    let result = execute(body.clone(), env);
                    stack.pop();
                    result
                }
                Expr::BuildIn(build_in) => {
                    match build_in {
                        BuildIn::Head => sane_head(arg),
                        BuildIn::Tail => sane_tail(arg),
                        BuildIn::Eq => sane_eq(arg, stack)
                    }
                }
                _ => Err(format!("Expr {:?} is not a function", fun))
            }
        }
        Expr::Fun(mut fun) => {
            Ok(Box::new(Expr::Fun(Fun { env: stack.clone(), ..fun })))
        }
        _ => Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert_eq!(result, Expr::Fun(Fun { param: "a".to_string(), body: Box::new(Expr::Ident("a".to_string())), env: vec![] }));
    }

    #[test]
    fn test_execute_let_0() {
        let result = *execute_sane("let a = 1 in a").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(1.0)));
    }

    #[test]
    fn test_execute_let_1() {
        let result = *execute_sane("let a = let b = 1 in b in a").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(1.0)));
    }

    #[test]
    fn test_execute_bind_0() {
        let result = *execute_sane("2 -> fun a => a").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(2.0)));
    }

    #[test]
    fn test_execute_bind_1() {
        let result = *execute_sane("let f = fun a => a in 2 -> f").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(2.0)));
    }

    #[test]
    fn test_execute_bind_2() {
        // TODO Looks like arguments from upper scope don't appear in the stack
        // The taken idents should be copied to the function environment
        let result = *execute_sane("let f = fun a => fun b => a in 1 -> 2 -> f").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(2.0)));
    }

    #[test]
    fn test_execute_array_0() {
        let result = *execute_sane("[1; \"two\"; fun a => a]").unwrap();
        assert_eq!(result, Expr::List(
            List {
                items: vec![
                    Box::new(Expr::Const(Const::Numeric(1.0))),
                    Box::new(Expr::Const(Const::String("two".to_string()))),
                    Box::new(Expr::Fun(Fun { param: "a".to_string(), body: Box::new(Expr::Ident("a".to_string())), env: vec![] }))
                ]
            }
        ));
    }

    #[test]
    fn test_execute_array_1() {
        let result = *execute_sane("[1; [2; [3]]]").unwrap();
        assert_eq!(
            result,
            Expr::List(
                List {
                    items: vec![
                        Box::new(Expr::Const(Const::Numeric(1.0))),
                        Box::new(Expr::List(
                            List {
                                items: vec![
                                    Box::new(Expr::Const(Const::Numeric(2.0))),
                                    Box::new(Expr::List(
                                        List {
                                            items: vec![
                                                Box::new(Expr::Const(Const::Numeric(3.0)))
                                            ]
                                        }
                                    ))
                                ]
                            }
                        ))
                    ]
                }
            )
        );
    }

    #[test]
    fn test_execute_head_0() {
        let result = *execute_sane("head").unwrap();
        assert_eq!(result, Expr::BuildIn(BuildIn::Head));
    }

    #[test]
    fn test_execute_head_1() {
        let result = *execute_sane("[1] -> head").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(1.0)));
    }

    #[test]
    fn test_execute_tail_0() {
        let result = *execute_sane("[] -> tail").unwrap();
        assert_eq!(result, Expr::List(
            List {
                items: vec![]
            }
        ));
    }

    #[test]
    fn test_execute_tail_1() {
        let result = *execute_sane("[1] -> tail").unwrap();
        assert_eq!(result, Expr::List(
            List {
                items: vec![]
            }
        ));
    }

    #[test]
    fn test_execute_tail_2() {
        let result = *execute_sane("[1;2] -> tail").unwrap();
        assert_eq!(result, Expr::List(
            List {
                items: vec![
                    Box::new(Expr::Const(Const::Numeric(2.0)))
                ]
            }
        ));
    }

    #[test]
    fn test_execute_eq_0() {
        let result = *execute_sane("[1; 1] -> eq").unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_eq_1() {
        let result = *execute_sane("[fun a => b; fun a => b] -> eq").unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_eq_2() {
        let result = *execute_sane("[fun a => b; fun a => c] -> eq").unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(false)));
    }

    #[test]
    fn test_execute_eq_3() {
        let result = *execute_sane("[[]; []] -> eq").unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_curry_0() {
        let result = *execute_sane(
            "let c = fun a => \
                    fun b => \
                      [a; b] -> eq \
                  in\
                    let curr = 1 -> c \
                    in \
                      2 -> curr").unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(false)));
    }
}
