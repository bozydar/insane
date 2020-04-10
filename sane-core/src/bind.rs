use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, ToSource, FromPair, Rule};
use crate::error::Error;
use crate::execute::{Stack, Execute, execute};
use pest::iterators::{Pair, Pairs};
use crate::fun::Fun;
use std::cell::RefCell;
use crate::build_in::BuildInFun;
use crate::ident::Ident;

#[derive(Debug, PartialEq, Clone)]
pub struct Bind {
    pub args: Vec<Rc<Expr>>,
    pub fun: Rc<Expr>,
    pub position: Position,
}

impl ToSource for Bind {
    fn to_source(&self) -> String {
        format!("app {} to {}", self.args.iter()
            .map(|arg| arg.to_source())
            .collect::<Vec<String>>()
            .join(", "), self.fun.to_source())
    }
}

impl FromPair for Bind {
    fn from_pair(pair: Pair<'_, Rule>, source: &str) -> ExprResult {
        let position = Position::from_span(pair.as_span(), source);
        let rule = pair.as_rule();
        let mut inner: Pairs<Rule> = pair.into_inner();
        let mut args = vec![];
        for next_pair in inner {
            args.push(Expr::from_pair(next_pair, source)?);
        }

        let fun = args
            .pop()
            .ok_or(
                Error::new(
                    &format!("Unknown rule `{:?}`", rule),
                    &position,
                )
            )?;

        Ok(Rc::new(Expr::Bind(Bind { args, fun, position })))
    }
}

impl Execute for Bind {
    fn execute(&self, stack: &mut Stack) -> ExprResult {
        fn build_curry_or_execute(current_args: Vec<Rc<Expr>>, current_fun: Rc<Expr>, position: &Position, stack: &mut Stack) -> ExprResult {
            let (arity, name) = match &*current_fun {
                Expr::Fun(fun) => (fun.params.len(), "__custom__".to_string()),
                Expr::BuildIn(build_in) => (build_in.arity, build_in.name.clone()),
                _ => {
                    // println!("{:?}", stack);
                    unreachable!("{:?}", current_fun)
                }
            };
            if current_args.len() > arity {
                return Error::new(
                    &format!("Function `{}` requires {} params but {} provided", name, arity, current_args.len()),
                    position
                ).into()
            }

            let diff = arity - current_args.len();
            if diff == 0 {
                return match &*current_fun {
                    Expr::Fun(Fun { params, body, env, .. }) => {
                        // TODO: it is actually bad
                        let env = &mut RefCell::borrow(env).clone();
                        for i in 0..params.len() {
                            env.push((params[i].clone(), current_args[i].clone()));
                        }
                        execute(body.clone(), env)
                    }
                    Expr::BuildIn(build_in) => {
                        let f: BuildInFun = build_in.fun;
                        f(current_args, &position)
                    }
                    _ => Error::new(&format!("Expr {:?} is not a function", current_fun), position).into()
                };
            }

            let mut params = vec![];
            for i in 0..diff {
                params.push(format!("$param_{}", i));
            }

            let params_as_idents = &mut params.iter()
                .map(|param| {
                    Rc::new(Expr::Ident(Ident { label: param.clone(), position: position.clone() }))
                })
                .collect::<Vec<Rc<Expr>>>();
            let mut params_to_bind = current_args;
            params_to_bind.append(params_as_idents);

            let body = Rc::new(
                Expr::Bind(
                    Bind {
                        args: params_to_bind,
                        fun: current_fun,
                        position: position.clone(),
                    }
                )
            );
            Ok(
                Rc::new(Expr::Fun(
                    Fun {
                        closure: RefCell::new(true),
                        rec_decorated: RefCell::new(true),
                        params,
                        body,
                        env: Rc::new(RefCell::new(stack.clone())),
                        position: position.clone(),
                    }))
            )
        }

        // evaluate arguments
        let mut arg_results = vec![];
        for arg in self.args.iter() {
            arg_results.push(execute(arg.clone(), stack)?);
        }
        // println!("{:?}", fun.clone());
        let fun = execute(self.fun.clone(), stack)?;
        build_curry_or_execute(arg_results, fun, &self.position, stack)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute::execute_sane;

    #[test]
    fn test_execute_bind_0() {
        let result = &*execute_sane("app 2 to (fun a => a)").unwrap().to_source();
        assert_eq!(result, "2.0");
    }

    #[test]
    fn test_execute_bind_1() {
        let result = &*execute_sane("let f = fun a => a in app  2 to f").unwrap().to_source();
        assert_eq!(result, "2.0");
    }

    #[test]
    fn test_execute_bind_2() {
        let result = &*execute_sane("let f = fun a => fun b => a in app 1 to app 2 to f").unwrap().to_source();
        assert_eq!(result, "2.0");
    }

    #[test]
    fn test_execute_bind_3() {
        let result = &*execute_sane("let f = fun a => fun b => b in app 1 to app 2 to f").unwrap().to_source();
        assert_eq!(result, "1.0");
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
                   app 2 to curr"#).unwrap().to_source();
        assert_eq!(result, "false");
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
               and let flop = fun b =>
                 app b to flip
               in app 0 to flip"#).unwrap().to_source();
        assert_eq!(result, "10.0");
    }

    #[test]
    fn test_recursive_2() {
        let result = execute_sane(
            r#"let flip = fun a =>
                 if (app a, 10.0 to eq) then a else (app (app a to inc) to flop)
               in let flop = fun b =>
                 app b to flip
               in app 0 to flip"#);
        assert_eq!(result, Err(Error { message: "Ident `flop` not found".to_string(), 
            backtrace: vec![
                Position { start: 95, end: 99, source: Rc::from("ADHOC") }, 
                Position { start: 95, end: 99, source: Rc::from("ADHOC") }, 
                Position { start: 73, end: 99, source: Rc::from("ADHOC") }, 
                Position { start: 37, end: 100, source: Rc::from("ADHOC") }, 
                Position { start: 188, end: 201, source: Rc::from("ADHOC") }, 
                Position { start: 188, end: 201, source: Rc::from("ADHOC") }, 
                Position { start: 119, end: 201, source: Rc::from("ADHOC") }
            ]
        }));
    }

    #[test]
    fn test_auto_curr_1() {
        let result = execute_sane(
            r#"let f = fun a b c => a
               in app 1 to f"#).unwrap().to_source();
        assert_eq!(result, "fun $param_0 $param_1 => app 1.0, $param_0, $param_1 to fun a b c => a");
    }

    #[test]
    fn test_auto_curr_2() {
        let result = execute_sane(
            r#"let f = fun a b c => [a; b; c]
               in let g = app 1 to f
               in app 2, 3 to g"#).unwrap().to_source();
        assert_eq!(result, "[1.0; 2.0; 3.0]");
    }

    #[test]
    fn test_auto_curr_3() {
        let result = execute_sane(
            r#"let f = fun a b => app a, b to add in
               let my_inc = app 1 to f in
               app 2 to my_inc"#).unwrap().to_source();
        assert_eq!(result, "3.0");
    }

    #[test]
    fn test_auto_curr_4() {
        let result = execute_sane(
            r#"let f = add in
               let my_inc = app 1 to f in
               app 2 to my_inc"#).unwrap().to_source();
        assert_eq!(result, "3.0");
    }

    #[test]
    fn test_auto_curr_5() {
        let result = execute_sane(
            r#"let my_inc = app 1 to add in
               app 2 to my_inc"#).unwrap().to_source();
        assert_eq!(result, "3.0");
    }
}