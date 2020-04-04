use std::rc::Rc;
use crate::parse::{Expr, ExprResult, parse_sane, parse_file, ToSource, Position};

use std::cell::RefCell;
use crate::build_in_functions::build_in_functions;

pub trait Execute {
    fn execute(&self, stack: &mut Stack) -> ExprResult;
}

pub type Stack = Vec<(String, Rc<Expr>)>;

pub fn execute_sane(input: &str) -> ExprResult {
    let expr = parse_sane(input)?;
    let stack = &mut build_in_functions();
    execute(expr, stack)
}

pub fn execute_file(input: &str, source: &str) -> ExprResult {
    let expr = parse_file(input, source)?;
    let stack = &mut build_in_functions();
    // println!("{:#?}", create_build_in("count".to_string(), sane_count, 1));
    let result = execute(expr, stack)?;
    Ok(result)
}

// TODO: Check if it possible to impl Execute for Rc<T> where T: Expr + Execute
pub(crate) fn execute(expr: Rc<Expr>, stack: &mut Stack) -> ExprResult {
    // println!("Executing: {:?}", expr);
    // println!("Stack: {}", stack_to_string(stack));
    let pre_result = 
        match &*expr {
            Expr::LetIn(let_in) => {
                (Some(let_in.position.clone()), let_in.execute(stack))
            }
            Expr::Ident(ident) => {
                (Some(ident.position.clone()), ident.execute(stack))
            }
            Expr::Bind(bind) => {
                (Some(bind.position.clone()), bind.execute(stack))
            }
            Expr::IfThenElse(if_then_else) => {
                (Some(if_then_else.position.clone()), if_then_else.execute(stack))
            }
            Expr::List(list) => {
                (Some(list.position.clone()), list.execute(stack))
            }
            Expr::Fun(fun) => {
                // Can't implement it inside impl Execute for Fun because I would need to clone
                // the Fun struct.
                if !*RefCell::borrow(&fun.closure) {
                    RefCell::replace(&fun.env, stack.clone());
                    RefCell::replace(&fun.closure, true);
                }
                (None, Ok(expr))
            }
            _ => (None, Ok(expr))
        };
    match pre_result {
        (Some(position), Err(mut error)) => {
            error.push_backtrace_item(&position);
            Err(error.clone())
        },
        (_, result) => result
    }
}

fn stack_to_string(stack: &Stack) -> String {
    stack.iter().map(|item| {
        format!("{} = {}", item.0, item.1.to_source())
    }).collect::<Vec<String>>().join("\n")
}


#[cfg(test)]
mod tests {
    use super::*;

        
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
}