extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::collections::HashMap;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use std::borrow::{BorrowMut, Borrow};
use std::fmt::{Debug, Formatter};
use core::fmt;
use std::ops::{Add, Deref};
use std::rc::Rc;
use std::cell::RefCell;
use std::borrow::Cow::Borrowed;

fn main() {}

#[derive(Parser)]
#[grammar = "sane.pest"]
pub struct SaneParser;

#[derive(Debug, PartialEq, Clone)]
struct LetIn {
    pub lets: Vec<(String, Rc<Expr>)>,
    pub in_part: Rc<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
struct Bind {
    pub args: Vec<Rc<Expr>>,
    pub fun: Rc<Expr>,
}

#[derive(Debug, PartialEq, Clone)]
struct Fun {
    pub params: Vec<String>,
    pub body: Rc<Expr>,
    pub env: Rc<RefCell<Stack>>,
}

#[derive(Debug, PartialEq, Clone)]
struct List {
    pub items: Vec<Rc<Expr>>
}

#[derive(Debug, PartialEq, Clone)]
struct IfThenElse {
    pub cond: Rc<Expr>,
    pub then: Rc<Expr>,
    pub otherwise: Rc<Expr>,
}

type BuildInFun = fn(Vec<Rc<Expr>>) -> Result<Rc<Expr>, String>;

#[derive(Clone)]
struct BuildIn {
    pub fun: BuildInFun,
    pub name: String,
    pub arity: usize,
}

impl Debug for BuildIn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "BuildIn({})", self.name)
    }
}

impl PartialEq for BuildIn {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(&other.name)
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
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
    IfThenElse(IfThenElse),
    BuildIn(BuildIn),
}

impl ToSource for Expr {
    fn to_source(&self) -> String {
        match self {
            Expr::LetIn(let_in) => let_in.to_source(),
            Expr::Const(const_) => const_.to_source(),
            Expr::Ident(ident) => ident.clone(),
            Expr::Bind(bind) => bind.to_source(),
            Expr::List(list) => list.to_source(),
            Expr::IfThenElse(if_then_else) => if_then_else.to_source(),
            Expr::Fun(fun) => fun.to_source(),
            _ => format!("{:?}", self)
        }
    }
}

impl ToSource for LetIn {
    fn to_source(&self) -> String {
        let lets = self.lets.iter().map(|item| {
            format!("let {} = {}", item.0, item.1.to_source())
        }).collect::<Vec<String>>().join(" and ");
        format!("{} in {}", lets, self.in_part.to_source())
    }
}

impl ToSource for Fun {
    fn to_source(&self) -> String {
        format!("fun {} => {}", self.params.join(" "), self.body.to_source())
    }
}

impl ToSource for Const {
    fn to_source(&self) -> String {
        match self {
            Const::String(string) => format!("\"{}\"", string),
            Const::Numeric(f64) => format!("{:?}", f64),
            Const::Bool(bool) => if *bool {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
    }
}

impl ToSource for List {
    fn to_source(&self) -> String {
        let c = self.items.iter().map(|item| item.to_source()).collect::<Vec<String>>().join("; ");
        format!("[{}]", c)
    }
}

impl ToSource for Bind {
    fn to_source(&self) -> String {
        format!("app {} to {}", self.args.iter()
            .map(|arg| arg.to_source())
            .collect::<Vec<String>>()
            .join(", "), self.fun.to_source())
    }
}

impl ToSource for BuildIn {
    fn to_source(&self) -> String {
        format!("{:?}", self).to_lowercase()
    }
}

impl ToSource for IfThenElse {
    fn to_source(&self) -> String {
        format!("if {} then {} else {}", self.cond.to_source(), self.then.to_source(), self.otherwise.to_source()).to_lowercase()
    }
}

trait ToSource {
    fn to_source(&self) -> String;
}

fn parse_const(pair: Pair<'_, Rule>) -> Result<Rc<Expr>, String> {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::string => Ok(Rc::new(Expr::Const(Const::String(pair.as_str().to_string())))),
        Rule::number => {
            let value = pair.as_str().parse::<f64>().unwrap();
            Ok(Rc::new(Expr::Const(Const::Numeric(value))))
        }
        _ => Err(format!("Unknown const type: `{:?}`", pair))
    }
}

fn parse_file(pair: Pair<'_, Rule>) -> Result<Rc<Expr>, String> {
    let pair = pair.into_inner().next().unwrap();
    parse_pair(pair)
}

fn parse_let_in(pair: Pair<'_, Rule>) -> Result<Rc<Expr>, String> {
    let mut inner: Pairs<'_, Rule> = pair.into_inner();
    let mut lets: Vec<(String, Rc<Expr>)> = vec![];

    let mut pair = inner.next().unwrap();
    while Rule::ident_expr == pair.as_rule() {
        let mut ident_expr_inner = pair.into_inner();
        let ident = ident_expr_inner.next().unwrap().as_str().to_string();
        let value = ident_expr_inner.next().unwrap();
        lets.push((ident, parse_pair(value)?));
        pair = inner.next().unwrap();
    }

    let in_part = parse_pair(pair)?;
    Ok(Rc::new(Expr::LetIn(LetIn {
        lets,
        in_part,
    })))
}

fn parse_ident(pair: Pair<'_, Rule>) -> Result<Rc<Expr>, String> {
    Ok(Rc::new(Expr::Ident(pair.as_str().to_string())))
}

fn parse_fun(pair: Pair<Rule>) -> Result<Rc<Expr>, String> {
    let mut inner: Pairs<Rule> = pair.into_inner();
    let params = inner.next().unwrap().into_inner()
        .map(|item| item.as_str().to_string())
        .collect::<Vec<String>>();
    let body = inner.next().unwrap();

    Ok(Rc::new(Expr::Fun(Fun {
        params,
        body: parse_pair(body)?,
        env: Rc::new(RefCell::new(vec![]))
    })))
}

fn parse_bind(pair: Pair<Rule>) -> Result<Rc<Expr>, String> {
    let mut inner: Pairs<Rule> = pair.into_inner();
    let mut args = vec![];
    while let Some(next_pair) = inner.next() {
        args.push(parse_pair(next_pair)?);
    }

    let fun = args.pop().ok_or("There is no function")?;

    Ok(Rc::new(Expr::Bind(Bind { args, fun })))
}

fn parse_if_then_else(pair: Pair<'_, Rule>) -> Result<Rc<Expr>, String> {
    let mut inner: Pairs<'_, Rule> = pair.into_inner();
    let cond = inner.next().unwrap();
    let then = inner.next().unwrap();
    let otherwise = inner.next().unwrap();

    Ok(Rc::new(Expr::IfThenElse(IfThenElse {
        cond: parse_pair(cond)?,
        then: parse_pair(then)?,
        otherwise: parse_pair(otherwise)?,
    })))
}

fn parse_list(pair: Pair<'_, Rule>) -> Result<Rc<Expr>, String> {
    let mut items: Vec<Rc<Expr>> = vec![];

    for item in pair.into_inner() {
        items.push(parse_pair(item)?);
    }
    Ok(Rc::new(Expr::List(List { items })))
}

fn sane_head(params: Vec<Rc<Expr>>) -> Result<Rc<Expr>, String> {
    let param = params.first().ok_or("parameter error")?.deref();
    match param {
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

fn sane_count(params: Vec<Rc<Expr>>) -> Result<Rc<Expr>, String> {
    let param = params.get(0).ok_or("No arguments")?.clone();
    match &*param {
        Expr::List(List { items }) => {
            Ok(Rc::new(Expr::Const(Const::Numeric(items.len() as f64))))
        }
        _ => Err("Not a list".to_string())
    }
}

fn sane_concat(params: Vec<Rc<Expr>>) -> Result<Rc<Expr>, String> {
    if let (Some(left), Some(right)) = (params.get(0).cloned(), params.get(1).cloned()) {
        let mut pair = (&*left, &*right);
        match pair {
            (Expr::List(List { items: left_items }), Expr::List(List { items: right_items })) => {
                let mut left_items = left_items.clone();
                let mut right_items = right_items.clone();
                left_items.append(&mut right_items);
                Ok(Rc::new(Expr::List(List { items: left_items })))
            }
            _ => Err("It is not a list of lists".to_string())
        }
    } else {
        Err("The list is empty".to_string())
    }
}

fn sane_tail(params: Vec<Rc<Expr>>) -> Result<Rc<Expr>, String> {
    let param = params.first().ok_or("parameter error")?.deref();
    match param {
        Expr::List(List { items }) => {
            let new_items =
                if items.is_empty() {
                    vec![]
                } else {
                    items[1..].to_vec()
                };
            Ok(Rc::new(Expr::List(List { items: new_items })))
        }
        _ => Err("Not a list".to_string())
    }
}


fn sane_eq(params: Vec<Rc<Expr>>) -> Result<Rc<Expr>, String> {
    if let (Some(left), Some(right)) = (params.get(0), params.get(1)) {
        Ok(Rc::new(Expr::Const(Const::Bool(left.eq(&right)))))
    } else {
        Err("The list is empty".to_string())
    }
}

fn sane_add(params: Vec<Rc<Expr>>) -> Result<Rc<Expr>, String> {
    let left = &*params.get(0).ok_or("Can't find the first argument")?.clone();
    let right = &*params.get(1).ok_or("Can't find the second argument")?.clone();
    if let (Expr::Const(Const::Numeric(left)), Expr::Const(Const::Numeric(right))) = (left, right) {
        Ok(Rc::new(Expr::Const(Const::Numeric(left.add(*right)))))
    } else {
        Err("The list doesn't match".to_string())
    }
}

fn sane_inc(params: Vec<Rc<Expr>>) -> Result<Rc<Expr>, String> {
    let param = &*params.get(0).ok_or("Can't find the first argument")?.clone();
    match param {
        Expr::Const(Const::Numeric(num)) => {
            Ok(Rc::new(Expr::Const(Const::Numeric(num + 1.0))))
        }
        _ => Err(format!("Not a numeric. `{:?}`", param))
    }
}

fn sane_print(params: Vec<Rc<Expr>>) -> Result<Rc<Expr>, String> {
    let param = params.get(0).ok_or("Can't find the first argument")?.clone();
    println!("{:#?}", *param);
    Ok(param)
}
fn parse_pair(pair: Pair<'_, Rule>) -> Result<Rc<Expr>, String> {
    let rule = pair.as_rule();
    match rule {
        Rule::file => parse_file(pair),
        Rule::constant => parse_const(pair),
        Rule::let_in => parse_let_in(pair),
        Rule::ident => parse_ident(pair),
        Rule::bind => parse_bind(pair),
        Rule::fun => parse_fun(pair),
        Rule::list => parse_list(pair),
        Rule::if_then_else => parse_if_then_else(pair),
        _ => {
            Err(format!("Unknown rule `{:?}`", rule))
        }
    }
}

fn parse_sane(input: &str) -> Result<Rc<Expr>, String> {
    let parsed = SaneParser::parse(Rule::file, input)
        .expect("Can't parse")
        .next()
        .unwrap();

    // let result: Option<pest::iterators::Pair<'_, Rule>> = parsed;
    // println!("-----------{:?}", parsed);

    parse_pair(parsed)
}

type Stack = Vec<(String, Rc<Expr>)>;

fn execute_sane(input: &str) -> Result<Rc<Expr>, String> {
    let expr = parse_sane(input)?;
    let stack = &mut vec![
        create_build_in("head".to_string(), sane_head, 1),
        create_build_in("tail".to_string(), sane_tail, 1),
        create_build_in("eq".to_string(), sane_eq, 2),
        create_build_in("inc".to_string(), sane_inc, 1),
        create_build_in("count".to_string(), sane_count, 1),
        create_build_in("concat".to_string(), sane_concat, 2),
        create_build_in("add".to_string(), sane_add, 2),
        create_build_in("print".to_string(), sane_print, 1),
        ("true".to_string(), Rc::new(Expr::Const(Const::Bool(true)))),
        ("false".to_string(), Rc::new(Expr::Const(Const::Bool(false)))),
    ];
    // println!("{:#?}", create_build_in("count".to_string(), sane_count, 1));
    let result = execute(Rc::from(expr), stack)?;
    Ok(result)
}

fn create_build_in(name: String, fun: BuildInFun, arity: usize) -> (String, Rc<Expr>) {
    (name.to_string(), Rc::new(Expr::BuildIn(BuildIn { fun, name, arity })))
}

fn stack_to_string(stack: &Stack) -> String {
    stack.iter().map(|item| {
        format!("{} = {}", item.0, item.1.to_source())
    }).collect::<Vec<String>>().join("\n")
}

fn execute(expr: Rc<Expr>, stack: &mut Stack) -> Result<Rc<Expr>, String> {
    // println!("Executing: {:?}", expr);
    // println!("Stack: {}", stack_to_string(stack));
    match &*expr {
        Expr::LetIn(let_in) => {
            let let_in = let_in.clone();
            let mut pushed = 0; // TODO execute lets part.
            for (ident, expr) in let_in.lets.iter().cloned() {
                let expr = execute(expr, stack)?;
                // TODO For let_and propagate all the functions around all the environments
                if let Expr::Fun(fun) = &*expr {
                    RefCell::replace_with(&fun.env, |env| {
                        env.push((ident.clone(), expr.clone()));
                        env.clone()
                    });
                }
                stack.push((ident, expr));
                pushed += 1;
            }
            let result = execute(let_in.in_part, stack);
            for _ in 0..pushed {
                stack.pop().unwrap();
            }
            // println!(">>> LetIn Pop {}", popped.0);
            result
        }
        Expr::Ident(ident) => {
            if let Some((_, expr)) = stack.iter().rev().find(|item| { &item.0 == ident }) {
                Ok(expr.clone())
            } else {
                Err(format!("Ident `{}` not found: {}", ident, stack_to_string(stack)))
            }
        }
        Expr::Bind(Bind { args, fun }) => {
            // TODO
            // If a regular function check how many params it has and build a function according to it
            // If a BuiltIn check arity and do the same

            fn build_curry_or_execute(current_args: Vec<Rc<Expr>>, current_fun: Rc<Expr>, stack: &mut Stack) -> Result<Rc<Expr>, String> {
                let arity = match &*current_fun {
                    Expr::Fun(fun) => fun.params.len(),
                    Expr::BuildIn(build_in) => build_in.arity,
                    _ => unreachable!()
                };
                let diff = arity - current_args.len();
                if diff < 0 {
                    return Err("Too many arguments".to_string());
                }
                if diff == 0 {
                    return match &*current_fun {
                        Expr::Fun(Fun { params, body, env }) => {
                            // TODO: it is actually bad
                            let env = &mut RefCell::borrow(env).clone();
                            // println!(">>>>> Push {:?}", param);
                            // Zip params and args and push to stack
                            //  let current_args_ = current_args.clone();
                            for i in 0..params.len() {
                                env.push((params[i].clone(), current_args[i].clone()));
                            }
                            execute(body.clone(), env)
                        }
                        Expr::BuildIn(build_in) => {
                            let f: BuildInFun = build_in.fun;
                            f(current_args)
                        }
                        _ => Err(format!("Expr {:?} is not a function", current_fun))
                    };
                }

                let mut params = vec![];
                for i in 0..diff {
                    params.push(format!("$param_#{}", i));
                }

                let params_as_idents = &mut params.iter()
                    .map(|param| {
                        Rc::new(Expr::Ident(param.clone()))
                    })
                    .collect::<Vec<Rc<Expr>>>();
                let mut params_to_bind = current_args.clone();
                params_to_bind.append(params_as_idents);

                let body = Rc::new(
                    Expr::Bind(
                        Bind {
                            args: params_to_bind,
                            fun: current_fun,
                        }
                    )
                );
                return Ok(
                    Rc::new(Expr::Fun(
                        Fun {
                            params,
                            body,
                            env: Rc::new(RefCell::new(stack.clone())),
                        }))
                );
            }

            // evaluate arguments
            let mut arg_results = vec![];
            for arg in args.into_iter() {
                arg_results.push(execute(arg.clone(), stack)?);
            }
            let fun = execute(fun.clone(), stack)?;
            build_curry_or_execute(arg_results, fun, stack)
        }
        Expr::IfThenElse(IfThenElse { cond, then, otherwise }) => {
            let result = execute(cond.clone(), stack)?;
            if let Expr::Const(Const::Bool(true)) = *result {
                execute(then.clone(), stack)
            } else {
                execute(otherwise.clone(), stack)
            }
        }
        Expr::List(List { items }) => {
            let mut result: Vec<Rc<Expr>> = vec![];
            // let items = items.clone();
            for item in items.into_iter() {
                let item = execute(item.clone(), stack)?;
                result.push(item);
            }
            Ok(Rc::new(Expr::List(List { items: result })))
        }
        Expr::Fun(fun) => {
            RefCell::replace(&fun.env, stack.clone());
            Ok(expr)
        }
        _ => Ok(expr)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_number() {
        let result = &*parse_sane("-23.1").unwrap();
        assert_eq!(result, &Expr::Const(Const::Numeric(-23.1)));
    }

    #[test]
    fn parse_string() {
        let result = &*parse_sane("\"test\"").unwrap();
        assert_eq!(result, &Expr::Const(Const::String("test".to_string())));
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
    fn parse_bind_0() {
        let result = &*parse_sane("app 1 to f").unwrap();
        assert_eq!(result, &Expr::Bind(Bind { args: vec![Rc::new(Expr::Const(Const::Numeric(1.0)))], fun: Rc::new(Expr::Ident("f".to_string())) }));
    }

    #[test]
    fn parse_bind_1() {
        let result = parse_sane("app 2 to app 1 to f").unwrap().to_source();
        assert_eq!(result, "app 2.0 to app 1.0 to f");
    }

    #[test]
    fn parse_fun() {
        let result = &*parse_sane("fun a => a").unwrap();
        assert_eq!(result, &Expr::Fun(Fun { params: vec!["a".to_string()], body: Rc::new(Expr::Ident("a".to_string())), env: Rc::new(RefCell::new(vec![])) }));
    }

    #[test]
    fn test_execute_let_0() {
        let result = &*execute_sane("let a = 1 in a").unwrap();
        assert_eq!(result, &Expr::Const(Const::Numeric(1.0)));
    }

    #[test]
    fn test_execute_let_1() {
        let result = &*execute_sane("let a = let b = 1 in b in a").unwrap();
        assert_eq!(result, &Expr::Const(Const::Numeric(1.0)));
    }

    #[test]
    fn test_execute_bind_0() {
        let result = &*execute_sane("app 2 to (fun a => a)").unwrap();
        assert_eq!(result, &Expr::Const(Const::Numeric(2.0)));
    }

    #[test]
    fn test_execute_bind_1() {
        let result = &*execute_sane("let f = fun a => a in app  2 to f").unwrap();
        assert_eq!(result, &Expr::Const(Const::Numeric(2.0)));
    }

    #[test]
    fn test_execute_bind_2() {
        let result = &*execute_sane("let f = fun a => fun b => a in app 1 to app 2 to f").unwrap();
        assert_eq!(result, &Expr::Const(Const::Numeric(2.0)));
    }

    #[test]
    fn test_execute_bind_3() {
        let result = &*execute_sane("let f = fun a => fun b => b in app 1 to app 2 to f").unwrap();
        assert_eq!(result, &Expr::Const(Const::Numeric(1.0)));
    }

    #[test]
    fn test_execute_array_0() {
        let result = execute_sane("[1; \"two\"; fun a => a]").unwrap().to_source();
        assert_eq!(result, r#"[1.0; "two"; fun a => a]"#);
    }

    #[test]
    fn test_execute_array_1() {
        let result = execute_sane("[1; [2; [3]]]").unwrap().to_source();
        assert_eq!(result, "[1.0; [2.0; [3.0]]]");
    }

    #[test]
    fn test_execute_array_2() {
        let result = execute_sane(r#"[1; "two"; fun a => a]"#).unwrap().to_source();
        assert_eq!(result, r#"[1.0; "two"; fun a => a]"#);
    }

    #[test]
    fn test_execute_head_1() {
        let result = &*execute_sane("app [1] to  head").unwrap();
        assert_eq!(result, &Expr::Const(Const::Numeric(1.0)));
    }

    #[test]
    fn test_execute_tail_0() {
        let result = &*execute_sane("app [] to tail").unwrap();
        assert_eq!(result, &Expr::List(
            List {
                items: vec![]
            }
        ));
    }

    #[test]
    fn test_execute_tail_1() {
        let result = &*execute_sane("app [1] to tail").unwrap();
        assert_eq!(result, &Expr::List(
            List {
                items: vec![]
            }
        ));
    }

    #[test]
    fn test_execute_tail_2() {
        let result = &*execute_sane("app [1;2] to tail").unwrap();
        assert_eq!(result, &Expr::List(
            List {
                items: vec![
                    Rc::new(Expr::Const(Const::Numeric(2.0)))
                ]
            }
        ));
    }

    #[test]
    fn test_execute_eq_0() {
        let result = &*execute_sane("app 1, 1 to eq").unwrap();
        assert_eq!(result, &Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_eq_1() {
        let result = &*execute_sane("app fun a => b, fun a => b to eq").unwrap();
        assert_eq!(result, &Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_eq_2() {
        let result = &*execute_sane("app fun a => b, fun a => c to eq").unwrap();
        assert_eq!(result, &Expr::Const(Const::Bool(false)));
    }

    #[test]
    fn test_execute_eq_3() {
        let result = &*execute_sane("app [], [] to eq").unwrap();
        assert_eq!(result, &Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_eq_4() {
        let result = &*execute_sane(
            r#"let a = fun b => b in
               let c = fun d => d in
                 app app 1 to c, app 1 to a to eq"#).unwrap();
        assert_eq!(result, &Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_eq_5() {
        let result = &*execute_sane(
            r#"let eqa = fun left =>
                  let eqa_ = fun right =>
                    app left, right to eq
                  in eqa_
               in app 1 to app 1 to eqa"#).unwrap();
        assert_eq!(result, &Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_add_0() {
        let result = &*execute_sane(
            r#"app 1, 2 to add"#).unwrap();
        assert_eq!(result, &Expr::Const(Const::Numeric(3.0)));
    }

    #[test]
    fn test_execute_curry_0() {
        let result = &*execute_sane(
            r#"let c = fun a =>
                 fun b =>
                   app a, b to eq
               in
                 let curr = app 1 to c
                 in
                   app 2 to curr"#).unwrap();
        assert_eq!(result, &Expr::Const(Const::Bool(false)));
    }

    #[test]
    fn test_map_0() {
        let result = execute_sane(
            r#"let map = fun fn =>
                 let map_ = fun list =>
                   if app (app list to count), 0 to eq then
                     []
                   else
                     let h = app (app list to head) to fn in
                     let t = app list to tail in
                     app [h], (app t to map_) to concat
                 in map_
               in
                 app [1; 2; 3], (fun a => app a to inc) to map
            "#
        ).unwrap().to_source();
        assert_eq!(result, "[2.0; 3.0; 4.0]");
    }

    #[test]
    fn test_reduce_0() {
        let result = execute_sane(
            r#"let reduce = fun fn =>
                 let reduce_0 = fun acc list =>
                     if app (app (app list to print) to count), 0 to eq then
                       acc
                     else
                       let h = app list to head in
                       let t = app list to tail in
                       let new_acc = app h, acc to fn in
                         app new_acc, t to reduce_0
                 in reduce_0
               in
                 let sum = add
                 in
                   app 0, [1; 2; 3] to app sum to reduce
            "#
        ).unwrap().to_source();
        assert_eq!(result, "6.0");
    }

    #[test]
    fn test_recursive_0() {
        let result = execute_sane(
            r#"let add_till_10 = fun a =>
                 if app a, 10.0 to eq then a else app (app a to inc) to add_till_10
               in app 0 to add_till_10"#).unwrap().to_source();
        assert_eq!(result, "10.0");
    }

    #[test]
    fn test_recursive_1() {
        let result = execute_sane(
            r#"let flip = fun a =>
                 if (app a, 10.0 to eq) then a else (app (app a to inc) to flop)
               in let flop = fun b =>
                 app b to flip
               in app 0 to flip"#).unwrap().to_source();
        assert_eq!(result, "Err");
    }

    #[test]
    fn test_execute_inc_0() {
        let result = execute_sane(
            r#"let a = 1 in
                 app a to inc"#).unwrap().to_source();
        assert_eq!(result, "2.0");
    }

    #[test]
    fn test_execute_if_0() {
        let result = execute_sane(
            r#"if true then 1 else 2"#).unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_execute_if_1() {
        let result = execute_sane(
            r#"let plus_one = fun a =>
                 app a to inc
               in
               let b = app 1 to plus_one in
               if app b, 2 to eq then 1 else 2"#).unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_count_0() {
        let result = execute_sane(
            r#"app [] to count"#).unwrap().to_source();
        assert_eq!(result, "0.0");
    }

    #[test]
    fn test_count_1() {
        let result = execute_sane(
            r#"app [1] to count"#).unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_concat_0() {
        let result = execute_sane(
            r#"app [1], [2] to concat"#).unwrap().to_source();
        assert_eq!(result, "[1.0; 2.0]");
    }

    #[test]
    fn test_auto_curr_0() {
        let result = parse_sane(
            r#"fun a b c => a"#).unwrap().to_source();
        assert_eq!(result, "fun a b c => a");
    }

    #[test]
    fn test_auto_curr_1() {
        let result = execute_sane(
            r#"let f = fun a b => app a, b to add in
               let my_inc = app 1 to f in
               app 2 to my_inc"#).unwrap().to_source();
        assert_eq!(result, "3.0");
    }

    #[test]
    fn test_auto_curr_2() {
        let result = execute_sane(
            r#"let f = add in
               let my_inc = app 1 to f in
               app 2 to my_inc"#).unwrap().to_source();
        assert_eq!(result, "3.0");
    }

    #[test]
    fn test_auto_curr_3() {
        let result = execute_sane(
            r#"let my_inc = app 1 to add in
               app 2 to my_inc"#).unwrap().to_source();
        assert_eq!(result, "3.0");
    }
}
