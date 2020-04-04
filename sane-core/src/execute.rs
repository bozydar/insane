use std::rc::Rc;
use crate::parse::{Expr, ExprResult, parse_sane, parse_file, ToSource};

use std::cell::RefCell;
use crate::build_in_functions::build_in_functions;

pub trait Execute {
    fn execute(&self, stack: &mut Stack) -> ExprResult;
}

pub type Stack = Vec<(String, Rc<Expr>)>;

pub fn execute_sane(input: &str) -> ExprResult {
    let expr = parse_sane(input)?;
    let stack = &mut build_in_functions();
    // println!("{:#?}", create_build_in("count".to_string(), sane_count, 1));
    let result = execute(expr, stack)?;
    Ok(result)
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
    match &*expr {
        Expr::LetIn(let_in) => {
            let_in.execute(stack)
        }
        Expr::Ident(ident) => {
            ident.execute(stack)
        }
        Expr::Bind(bind) => {
            bind.execute(stack)
        }
        Expr::IfThenElse(if_then_else) => {
            if_then_else.execute(stack)
        }
        Expr::List(list) => {
            list.execute(stack)
        }
        Expr::Fun(fun) => {
            // Can't implement it inside impl Execute for Fun because I would need to clone
            // the Fun struct.
            if !*RefCell::borrow(&fun.closure) {
                RefCell::replace(&fun.env, stack.clone());
                RefCell::replace(&fun.closure, true);
            }
            Ok(expr)
        }
        _ => Ok(expr)
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