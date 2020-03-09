extern crate pest;
#[macro_use]
extern crate pest_derive;

#[macro_use]
extern crate lazy_static;


use std::collections::HashMap;

use pest::Parser;
use pest::iterators::{Pair, Pairs};
use std::borrow::BorrowMut;
use std::fmt::{Debug, Formatter};
use core::fmt;
use std::ops::{Add, Deref};

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
struct IfThenElse {
    pub cond: Box<Expr>,
    pub then: Box<Expr>,
    pub otherwise: Box<Expr>,
}

type BuildInFun = fn(param: Box<Expr>) -> Result<Box<Expr>, String>;

#[derive(Clone)]
struct BuildIn {
    pub fun: BuildInFun,
    pub name: String,
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
        format!("let {} = {} in {}", self.var, self.value.to_source(), self.in_part.to_source())
    }
}

impl ToSource for Fun {
    fn to_source(&self) -> String {
        format!("fun {} => {}", self.param, self.body.to_source())
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
        format!("{} > {}", self.arg.to_source(), self.fun.to_source())
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

fn parse_file(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let pair = pair.into_inner().next().unwrap();
    parse_pair(pair)
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

fn parse_fun(pair: Pair<Rule>) -> Result<Box<Expr>, String> {
    let mut inner: Pairs<Rule> = pair.into_inner();
    let params = &mut inner.next().unwrap().into_inner().clone();
    let body = inner.next().unwrap();

    fn build_inner_functions(params: &mut Pairs<Rule>, body: Pair<Rule>) -> Result<Box<Expr>, String> {
        if let Some(param) = params.next() {
            Ok(Box::new(Expr::Fun(Fun {
                param: param.as_str().to_string(),
                body: build_inner_functions(params, body)?,
                env: vec![]
            })))
        } else {
            parse_pair(body)
        }
    }

    Ok(build_inner_functions(params, body)?)
}

fn parse_bind_to_left(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let mut inner: Pairs<'_, Rule> = pair.into_inner();
    let fun = inner.next().unwrap();
    let arg = inner.next().unwrap();

    Ok(Box::new(Expr::Bind(Bind {
        arg: parse_pair(arg)?,
        fun: parse_pair(fun)?,
    })))
}

fn parse_bind_to_right(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let mut inner: Pairs<'_, Rule> = pair.into_inner();
    let arg = inner.next().unwrap();
    let fun = inner.next().unwrap();

    Ok(Box::new(Expr::Bind(Bind {
        arg: parse_pair(arg)?,
        fun: parse_pair(fun)?,
    })))
}

fn parse_if_then_else(pair: Pair<'_, Rule>) -> Result<Box<Expr>, String> {
    let mut inner: Pairs<'_, Rule> = pair.into_inner();
    let cond = inner.next().unwrap();
    let then = inner.next().unwrap();
    let otherwise = inner.next().unwrap();

    Ok(Box::new(Expr::IfThenElse(IfThenElse {
        cond: parse_pair(cond)?,
        then: parse_pair(then)?,
        otherwise: parse_pair(otherwise)?,
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

fn sane_count(param: Box<Expr>) -> Result<Box<Expr>, String> {
    match *param {
        Expr::List(List { items }) => {
            Ok(Box::new(Expr::Const(Const::Numeric(items.len() as f64))))
        }
        _ => Err("Not a list".to_string())
    }
}

fn sane_concat(param: Box<Expr>) -> Result<Box<Expr>, String> {
    match *param {
        Expr::List(List { items }) => {
            if let (Some(left), Some(right)) = (items.get(0), items.get(1)) {
                let mut pair = (*left.clone(), *right.clone());
                match pair {
                    (Expr::List(List { items: mut left_items }), Expr::List(List { items: mut right_items })) => {
                        ;
                        left_items.append(&mut right_items);
                        Ok(Box::new(Expr::List(List { items: left_items })))
                    }
                    _ => Err("It is not a list of lists".to_string())
                }
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
        Rule::file => parse_file(pair),
        Rule::constant => parse_const(pair),
        Rule::let_in => parse_let_in(pair),
        Rule::ident => parse_ident(pair),
        Rule::bind_to_left => parse_bind_to_left(pair),
        Rule::bind_to_right => parse_bind_to_right(pair),
        Rule::fun => parse_fun(pair),
        Rule::list => parse_list(pair),
        Rule::if_then_else => parse_if_then_else(pair),
        _ => {
            Err(format!("Unknown rule `{:?}`", rule))
        }
    }
}

fn sane_eq(param: Box<Expr>) -> Result<Box<Expr>, String> {
    match *param {
        Expr::List(List { items }) => {
            if let (Some(left), Some(right)) = (items.get(0), items.get(1)) {
                Ok(Box::new(Expr::Const(Const::Bool(left.eq(&right)))))
            } else {
                Err("The list is empty".to_string())
            }
        }
        _ => Err("Not a list".to_string())
    }
}

fn sane_add(param: Box<Expr>) -> Result<Box<Expr>, String> {
    match *param {
        Expr::List(List { items }) => {
            let left = *items.get(0).ok_or("Can't find the first argument")?.clone();
            let right = *items.get(1).ok_or("Can't find the second argument")?.clone();
            if let (Expr::Const(Const::Numeric(left)), Expr::Const(Const::Numeric(right))) = (left, right) {
                Ok(Box::new(Expr::Const(Const::Numeric(left.add(right)))))
            } else {
                Err("The list doesn't match".to_string())
            }
        }
        _ => Err("Not a list (sane_add)".to_string())
    }
}

fn sane_inc(param: Box<Expr>) -> Result<Box<Expr>, String> {
    match *param {
        Expr::Const(Const::Numeric(num)) => {
            Ok(Box::new(Expr::Const(Const::Numeric(num + 1.0))))
        }
        _ => Err(format!("Not a numeric. `{:?}`", param))
    }
}

fn sane_print(param: Box<Expr>) -> Result<Box<Expr>, String> {
    println!("{:#?}", param);
    Ok(param)
}

fn parse_sane(input: &str) -> Result<Box<Expr>, String> {
    let parsed = SaneParser::parse(Rule::file, input)
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
        create_build_in("head".to_string(), sane_head, 1),
        create_build_in("tail".to_string(), sane_tail, 1),
        create_build_in("eq".to_string(), sane_eq, 2),
        create_build_in("inc".to_string(), sane_inc, 1),
        create_build_in("count".to_string(), sane_count, 1),
        create_build_in("concat".to_string(), sane_concat, 2),
        create_build_in("add".to_string(), sane_add, 2),
        create_build_in("print".to_string(), sane_print, 1),
        ("true".to_string(), Box::new(Expr::Const(Const::Bool(true)))),
        ("false".to_string(), Box::new(Expr::Const(Const::Bool(false)))),
    ];
    println!("{:#?}", create_build_in("count".to_string(), sane_count, 1));
    execute(expr, stack)
}

fn create_build_in(name: String, fun: BuildInFun, nry: i32) -> (String, Box<Expr>) {
    fn build_arg(n: i32) -> Box<Expr> {
        let mut items = vec![];
        for i in 0..n {
            items.push(Box::new(Expr::Ident(format!("$param_{}", i))))
        }
        Box::new(Expr::List(List { items }))
    };

    fn create_build_in_(name: String, fun: BuildInFun, nry: i32, n: i32) -> Box<Expr> {
        if n == 0 {
            // TODO all build in should work with List{} as an argument
            let arg = build_arg(nry);
            let fun = Box::new(Expr::BuildIn(BuildIn { fun, name: name.clone() }));
            Box::new(Expr::Bind(Bind { arg, fun }))
        } else {
            let param = format!("$param_{}", n - 1);
            let body = create_build_in_(name, fun, nry, n - 1);
            Box::new(Expr::Fun(Fun { param, body, env: vec![] }))
        }
    };
    if nry > 1 {
        (name.to_string(), create_build_in_(name.to_string(), fun, nry, nry))
    } else {
        (name.to_string(), Box::new(Expr::BuildIn(BuildIn { fun, name: name.clone() })))
    }
}

fn stack_to_string(stack: &Stack) -> String {
    stack.iter().map(|item| {
        format!("{} = {}", item.0, item.1.to_source())
    }).collect::<Vec<String>>().join("\n")
}

fn execute(expr: Box<Expr>, stack: &mut Stack) -> Result<Box<Expr>, String> {
    // println!("Executing: {:?}", expr);
    // println!("Stack: {}", stack_to_string(stack));
    match *expr {
        Expr::LetIn(let_in) => {
            stack.push((let_in.var.clone(), let_in.value));
            // println!(">>> LetIn Push: {}", let_in.var);
            let result = execute(let_in.in_part, stack);
            let popped = stack.pop().unwrap();
            // println!(">>> LetIn Pop {}", popped.0);
            result
        }
        Expr::Ident(ident) => {
            if let Some(item) = stack.iter().rev().find(|item| { item.0 == ident }) {
                execute(item.1.clone(), stack)  // TODO might be inefficient (optimize referencing)
            } else {
                Err(format!("Ident `{}` not found: {}", ident, stack_to_string(stack)))
            }
        }
        Expr::Bind(Bind { arg, fun }) => {
            let arg_result = execute(arg, stack)?;
            match *execute(fun.clone(), stack)? {
                Expr::Fun(Fun { param, body, env }) => {
                    let mut env = env.clone();
                    let env: &mut Stack = env.borrow_mut();
                    // println!(">>>>> Push {:?}", param);
                    env.push((param.clone(), arg_result));
                    let result = execute(body.clone(), env);
                    result
                }
                Expr::BuildIn(build_in) => {
                    let f: BuildInFun = build_in.fun;
                    f(arg_result)
                }
                _ => Err(format!("Expr {:?} is not a function", fun))
            }
        }
        Expr::IfThenElse(IfThenElse { cond, then, otherwise }) => {
            let result = execute(cond, stack)?;
            if let Expr::Const(Const::Bool(true)) = *result {
                execute(then, stack)
            } else {
                execute(otherwise, stack)
            }
        }
        Expr::List(List { items }) => {
            let mut result: Vec<Box<Expr>> = vec![];
            for item in items.into_iter() {
                result.push(execute(item, stack)?);
            }
            Ok(Box::new(Expr::List(List { items: result })))
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
    fn parse_bind_left() {
        let result = *parse_sane("f < 1").unwrap();
        assert_eq!(result, Expr::Bind(Bind { arg: Box::new(Expr::Const(Const::Numeric(1.0))), fun: Box::new(Expr::Ident("f".to_string())) }));
    }

    #[test]
    fn parse_bind_right() {
        let result = *parse_sane("1 > f").unwrap();
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
        let result = *execute_sane("(fun a => a) < 2").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(2.0)));
    }

    #[test]
    fn test_execute_bind_1() {
        let result = *execute_sane("let f = fun a => a in f < 2").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(2.0)));
    }

    #[test]
    fn test_execute_bind_2() {
        let result = *execute_sane("let f = fun a => fun b => a in 1 > 2 > f").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(2.0)));
    }

    #[test]
    fn test_execute_bind_3() {
        let result = *execute_sane("let f = fun a => fun b => b in 1 > 2 > f").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(1.0)));
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
        let result = *execute_sane(r#"[1; "two"; fun a => a]"#).unwrap();
        assert_eq!(result.to_source(), r#"[1.0; "two"; fun a => a]"#);
    }

    #[test]
    fn test_execute_head_1() {
        let result = *execute_sane("[1] > head").unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(1.0)));
    }

    #[test]
    fn test_execute_tail_0() {
        let result = *execute_sane("[] > tail").unwrap();
        assert_eq!(result, Expr::List(
            List {
                items: vec![]
            }
        ));
    }

    #[test]
    fn test_execute_tail_1() {
        let result = *execute_sane("[1] > tail").unwrap();
        assert_eq!(result, Expr::List(
            List {
                items: vec![]
            }
        ));
    }

    #[test]
    fn test_execute_tail_2() {
        let result = *execute_sane("[1;2] > tail").unwrap();
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
        let result = *execute_sane("1 > 1 > eq").unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_eq_1() {
        let result = *execute_sane("(fun a => b) > (fun a => b) > eq").unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_eq_2() {
        let result = *execute_sane("(fun a => b) > (fun a => c) > eq").unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(false)));
    }

    #[test]
    fn test_execute_eq_3() {
        let result = *execute_sane("[] > [] > eq").unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_eq_4() {
        let result = *execute_sane(
            r#"let a = fun b => b in
               let c = fun d => d in
                 (1 > c) > (1 > a) > eq"#).unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_eq_5() {
        let result = *execute_sane(
            r#"let eqa = fun left =>
                  let eqa_ = fun right =>
                    left > right > eq
                  in eqa_
               in 1 > 1 > eqa "#).unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(true)));
    }

    #[test]
    fn test_execute_add_0() {
        let result = *execute_sane(
            r#"1 > 2 > add"#).unwrap();
        assert_eq!(result, Expr::Const(Const::Numeric(3.0)));
    }

    #[test]
    fn test_execute_curry_0() {
        let result = *execute_sane(
            r#"let c = fun a =>
                 fun b =>
                   a > b > eq
               in
                 let curr = 1 > c
                 in
                   2 > curr"#).unwrap();
        assert_eq!(result, Expr::Const(Const::Bool(false)));
    }

    #[test]
    fn test_map_0() {
        let result = execute_sane(
            r#"let map = fun fn =>
                 let map_ = fun list =>
                   if (list > count) > 0 > eq then
                     []
                   else
                     let h = fn < head < list in
                     let t = list > tail in
                     [h] > (t > map_) > concat
                 in map_
               in
                 [1; 2; 3] > (fun a => a > inc) > map
            "#
        ).unwrap().to_source();
        assert_eq!(result, "[2.0; 3.0; 4.0]");
    }

    #[test]
    fn test_reduce_0() {
        let result = execute_sane(
            r#"let reduce = fun fn =>
                 let reduce_0 = fun acc => fun list =>
                     if (count < print < list) > 0 > eq then
                       acc
                     else
                       let h = list > head in
                       let t = list > tail in
                       let new_acc = h > acc > fn in
                       t > new_acc > reduce_0
                 in reduce_0
               in
                 let sum = fun acc => fun item => acc > item > add
                 in
                   [1; 2; 3] > 0 > sum > reduce
            "#
        ).unwrap().to_source();
        assert_eq!(result, "6.0");
    }

    #[test]
    fn test_recursive_0() {
        let result = execute_sane(
            r#"let add_till_10 = fun a =>
                 if a > 10.0 > eq then a else add_till_10 < inc < a
               in 0 > add_till_10"#).unwrap().to_source();
        assert_eq!(result, "10.0");
    }

    #[test]
    fn test_execute_inc_0() {
        let result = execute_sane(
            r#"let a = 1 in
                 a > inc"#).unwrap().to_source();
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
                 a > inc
               in
               let b = 1 > plus_one in
               if b > 2 > eq then 1 else 2"#).unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_count_0() {
        let result = execute_sane(
            r#"[] > count"#).unwrap().to_source();
        assert_eq!(result, "0.0");
    }

    #[test]
    fn test_count_1() {
        let result = execute_sane(
            r#"[1] > count"#).unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_concat_0() {
        let result = execute_sane(
            r#"[1] > [2] > concat"#).unwrap().to_source();
        assert_eq!(result, "[1.0; 2.0]");
    }

    #[test]
    fn test_auto_curr_0() {
        let result = parse_sane(
            r#"fun a b c => a"#).unwrap().to_source();
        assert_eq!(result, "fun a => fun b => fun c => a");
    }

    #[test]
    fn test_auto_curr_1() {
        let result = execute_sane(
            r#"let f = fun a b => a > b > add in
               let my_inc = 1 > f in
               2 > my_inc"#).unwrap().to_source();
        assert_eq!(result, "3.0");
    }

    #[test]
    fn test_auto_curr_2() {
        let result = execute_sane(
            r#"let f = add in
               let my_inc = 1 > f in
               2 > my_inc"#).unwrap().to_source();
        assert_eq!(result, "3.0");
    }

    #[test]
    fn test_auto_curr_3() {
        let result = execute_sane(
            r#"let my_inc = 1 > add in
               2 > my_inc"#).unwrap().to_source();
        assert_eq!(result, "3.0");
    }
}
