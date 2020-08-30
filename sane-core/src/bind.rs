use crate::build_in::BuildInFun;
use crate::context::Context;
use crate::error::Error;
use crate::execute::{execute, Execute, Scope};
use crate::fun::Fun;
use crate::ident::Ident;
use crate::parse::Input;
use crate::parse::{Expr, ExprResult, FromInput, Position, Rule, ToSource};
use pest::iterators::Pairs;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub struct Bind {
    pub args: Vec<Rc<Expr>>,
    pub fun: Rc<Expr>,
    pub position: Position,
}

impl ToSource for Bind {
    fn to_source(&self) -> String {
        match &*self.fun {
            Expr::Ident(_)|Expr::NSIdent(_) => format!(
                "{}({})",
                self.fun.to_source(),
                self.args
                    .iter()
                    .map(|arg| arg.to_source())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
            _ => format!(
                "`{}`({})",
                self.fun.to_source(),
                self.args
                    .iter()
                    .map(|arg| arg.to_source())
                    .collect::<Vec<String>>()
                    .join(" ")
            ),
        }
    }
}

impl FromInput for Bind {
    fn from_input(input: Input<'_>, context: &mut Context) -> ExprResult {
        // TODO the order has changed
        let position = Position::from_input(&input);
        let input_ = input.clone();
        let mut inner: Pairs<Rule> = input.into_inner();
        let fun = Expr::from_input(input_.with_pair(&inner.next().unwrap()), context)?;
        let mut args = vec![];
        for next_pair in inner {
            args.push(Expr::from_input(input_.with_pair(&next_pair), context)?);
        }

        Ok(Rc::new(Expr::Bind(Bind {
            args,
            fun,
            position,
        })))
    }
}

impl Execute for Bind {
    fn execute(&self, stack: &mut Scope, context: &Context) -> ExprResult {
        fn build_curry_or_execute(
            current_args: Vec<Rc<Expr>>,
            current_fun: Rc<Expr>,
            position: &Position,
            stack: &mut Scope,
            context: &Context,
        ) -> ExprResult {
            let (arity, name) = match &*current_fun {
                Expr::Fun(fun) => (fun.params.len(), "__custom__".to_string()),
                Expr::BuildIn(build_in) => (build_in.arity, build_in.name.clone()),
                _ => {
                    // println!("{:?}", stack);
                    return Error::new(
                        &format!("It is not a function `{:?}`", current_fun),
                        position
                    ).into()
                }
            };
            if current_args.len() > arity {
                return Error::new(
                    &format!(
                        "Function `{}` requires {} params but {} provided",
                        name,
                        arity,
                        current_args.len()
                    ),
                    position,
                )
                .into();
            }

            let diff = arity - current_args.len();
            if diff == 0 {
                return match &*current_fun {
                    Expr::Fun(Fun {
                        params, body, env, ..
                    }) => {
                        // TODO: it is actually bad
                        let env = &mut RefCell::borrow(env).clone();
                        for i in 0..params.len() {
                            env.push((params[i].clone(), current_args[i].clone()));
                        }
                        execute(body.clone(), env, context)
                    }
                    Expr::BuildIn(build_in) => {
                        let f: BuildInFun = build_in.fun;
                        f(current_args, &position)
                    }
                    _ => Error::new(
                        &format!("Expr {:?} is not a function", current_fun),
                        position,
                    )
                    .into(),
                };
            }

            let mut params = vec![];
            for i in 0..diff {
                params.push(format!("$param_{}", i));
            }

            let params_as_idents = &mut params
                .iter()
                .map(|param| {
                    Rc::new(Expr::Ident(Ident {
                        label: param.clone(),
                        position: position.clone(),
                    }))
                })
                .collect::<Vec<Rc<Expr>>>();
            let mut params_to_bind = current_args;
            params_to_bind.append(params_as_idents);

            let body = Rc::new(Expr::Bind(Bind {
                args: params_to_bind,
                fun: current_fun,
                position: position.clone(),
            }));
            Ok(Rc::new(Expr::Fun(Fun {
                closure: RefCell::new(true),
                rec_decorated: RefCell::new(true),
                params,
                body,
                env: Rc::new(RefCell::new(stack.clone())),
                position: position.clone(),
            })))
        }

        // evaluate arguments
        let mut arg_results = vec![];
        for arg in self.args.iter() {
            arg_results.push(execute(arg.clone(), stack, context)?);
        }
        // println!("{:?}", fun.clone());
        let fun = execute(self.fun.clone(), stack, context)?;
        build_curry_or_execute(arg_results, fun, &self.position, stack, context)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute::execute_sane;

    #[test]
    fn test_execute_bind_0() {
        let result = &*execute_sane("`fun a => a`(2)").unwrap().to_source();
        assert_eq!(result, "2.0");
    }

    #[test]
    fn test_execute_bind_1() {
        let result = &*execute_sane("let f = fun a => a in f(2)")
            .unwrap()
            .to_source();
        assert_eq!(result, "2.0");
    }

    #[test]
    fn test_execute_bind_2() {
        // TODO if we tread parenthesis as another operator it will possible to: f (2) (1)
        let result = &*execute_sane("let f = fun a => fun b => a in `f(2)`(1)")
            .unwrap()
            .to_source();
        assert_eq!(result, "2.0");
    }

    #[test]
    fn test_execute_bind_3() {
        let result = &*execute_sane("let f = fun a b => [a; b] in f(1 2)")
            .unwrap()
            .to_source();
        assert_eq!(result, "[1.0; 2.0]");
    }

    #[test]
    fn test_execute_curry_0() {
        let result = &*execute_sane(
            r#"let c = fun a =>
                 fun b =>
                   eq(a b)
               in
                 let curr = c(1)
                 in
                   curr(2)"#,
        )
        .unwrap()
        .to_source();
        assert_eq!(result, "false");
    }

    #[test]
    fn test_recursive_0() {
        let result = execute_sane(
            r#"let add_till_10 = fun a =>
                 if eq(a 10.0) then a else add_till_10(inc(a))
               in add_till_10(0)"#,
        )
        .unwrap()
        .to_source();
        assert_eq!(result, "10.0");
    }

    #[test]
    fn test_recursive_1() {
        let result = execute_sane(
            r#"let flip = fun a =>
                 if eq(a 10) then a else flop(inc(a))
               and let flop = fun b =>
                 flip(b)
               in flip(0)"#,
        )
        .unwrap()
        .to_source();
        assert_eq!(result, "10.0");
    }

    #[test]
    fn test_recursive_2() {
        let result = execute_sane(
            r#"let flip = fun a =>
                 if eq(a 10) then a else flop(inc(a))
               in let flop = fun b =>
                 flip(b)
               in flip(0)"#,
        );
        assert_eq!(
            result,
            Err(Error {
                message: "Ident `flop` not found".to_string(),
                backtrace: vec![
                    Position {
                        start: 61,
                        end: 65,
                        source: Rc::from("ADHOC")
                    },
                    Position {
                        start: 61,
                        end: 65,
                        source: Rc::from("ADHOC")
                    },
                    Position {
                        start: 61,
                        end: 73,
                        source: Rc::from("ADHOC")
                    },
                    Position {
                        start: 37,
                        end: 73,
                        source: Rc::from("ADHOC")
                    },
                    Position {
                        start: 155,
                        end: 162,
                        source: Rc::from("ADHOC")
                    },
                    Position {
                        start: 155,
                        end: 162,
                        source: Rc::from("ADHOC")
                    },
                    Position {
                        start: 92,
                        end: 162,
                        source: Rc::from("ADHOC")
                    },
                    Position {
                        start: 0,
                        end: 162,
                        source: Rc::from("ADHOC")
                    }
                ]
            })
        );
    }

    #[test]
    fn test_auto_curr_1() {
        let result = execute_sane(
            r#"let f = fun a b c => a
               in f(1)"#,
        )
        .unwrap()
        .to_source();
        assert_eq!(
            result,
            "fun $param_0 $param_1 => `fun a b c => a`(1.0 $param_0 $param_1)"
        );
    }

    #[test]
    fn test_auto_curr_2() {
        let result = execute_sane(
            r#"let f = fun a b c => [a; b; c]
               in let g = f(1)
               in g(2 3)"#,
        )
        .unwrap()
        .to_source();
        assert_eq!(result, "[1.0; 2.0; 3.0]");
    }

    #[test]
    fn test_auto_curr_3() {
        let result = execute_sane(
            r#"let f = fun a b => add(a b) in
               let my_inc = f(1) in
               my_inc(2)"#,
        )
        .unwrap()
        .to_source();
        assert_eq!(result, "3.0");
    }

    #[test]
    fn test_auto_curr_4() {
        let result = execute_sane(
            r#"let f = add in
               let my_inc = f(1) in
               my_inc(2)"#,
        )
        .unwrap()
        .to_source();
        assert_eq!(result, "3.0");
    }

    #[test]
    fn test_auto_curr_5() {
        let result = execute_sane(
            r#"let my_inc = add(1) in
               my_inc(2)"#,
        )
        .unwrap()
        .to_source();
        assert_eq!(result, "3.0");
    }
}
