use crate::build_in_functions::build_in_functions;
use crate::context::Context;
use crate::parse::{parse_file, Expr, ExprResult, ToSource};
use std::cell::RefCell;
use std::env;
use std::rc::Rc;

pub trait Execute {
    fn execute(&self, stack: &mut Scope, context: &Context) -> ExprResult;
}

type Variable = (String, Rc<Expr>);
pub type Scope = Vec<Variable>;

pub fn execute_sane(input: &str) -> ExprResult {
    let look_path = look_path();
    // dbg!(&look_path);
    let mut context = Context::new(look_path);

    let expr = parse_file(input, "ADHOC", &mut context)?;
    let stack = &mut build_in_functions();
    execute(expr, stack, &context)
}

pub fn look_path() -> Vec<String> {
    // :facepalm:
    let mut result: Vec<String> = vec![];
    if let Some(val) = env::var_os("SANE_PATH") {
        if let Some(val) = val.to_str() {
            for path in env::split_paths(val) {
                if let Some(path) = path.to_str() {
                    result.push(String::from(path))
                }
            }
        }
    }
    result
}

pub fn execute_file(
    input: &str,
    path: &str,
    context: &mut Context,
    stack: &mut Scope,
) -> ExprResult {
    let expr = parse_file(input, path, context)?;
    // println!("{:#?}", create_build_in("count".to_string(), sane_count, 1));
    let result = execute(expr, stack, context)?;
    Ok(result)
}

// TODO: Check if it possible to impl Execute for Rc<T> where T: Expr + Execute
pub(crate) fn execute(expr: Rc<Expr>, stack: &mut Scope, context: &Context) -> ExprResult {
    // println!("Executing: {:?}", expr);
    // println!("Stack: {}", stack_to_string(stack));
    let pre_result = match &*expr {
        Expr::File(file) => (Some(file.position.clone()), file.execute(stack, context)),
        Expr::LetIn(let_in) => (
            Some(let_in.position.clone()),
            let_in.execute(stack, context),
        ),
        Expr::Ident(ident) => (Some(ident.position.clone()), ident.execute(stack, context)),
        Expr::NSIdent(ns_ident) => (
            Some(ns_ident.position.clone()),
            ns_ident.execute(stack, context),
        ),
        Expr::Bind(bind) => (Some(bind.position.clone()), bind.execute(stack, context)),
        Expr::Binary(binary) => (
            Some(binary.position.clone()),
            binary.execute(stack, context),
        ),
        Expr::IfThenElse(if_then_else) => (
            Some(if_then_else.position.clone()),
            if_then_else.execute(stack, context),
        ),
        Expr::List(list) => (Some(list.position.clone()), list.execute(stack, context)),
        Expr::Fun(fun) => {
            // Can't implement it inside impl Execute for Fun because I would need to clone
            // the Fun struct.
            if !*RefCell::borrow(&fun.closure) {
                RefCell::replace(&fun.env, stack.clone());
                RefCell::replace(&fun.closure, true);
            }
            (None, Ok(expr))
        }
        _ => (None, Ok(expr)),
    };
    match pre_result {
        (Some(position), Err(mut error)) => {
            error.push_backtrace_item(&position);
            Err(error)
        }
        (_, result) => result,
    }
}

#[allow(dead_code)]
fn stack_to_string(stack: &[(String, Rc<Expr>)]) -> String {
    stack
        .iter()
        .map(|item| format!("{} = {}", item.0, item.1.to_source()))
        .collect::<Vec<String>>()
        .join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_execute_array_0() {
        let result = execute_sane("[1; \"two\"; fun a => a]")
            .unwrap()
            .to_source();
        assert_eq!(result, r#"[1.0; "two"; fun a => a]"#);
    }

    #[test]
    fn test_execute_array_1() {
        let result = execute_sane("[1; [2; [3]]]").unwrap().to_source();
        assert_eq!(result, "[1.0; [2.0; [3.0]]]");
    }

    #[test]
    fn test_execute_array_2() {
        let result = execute_sane(r#"[1; "two"; fun a => a]"#)
            .unwrap()
            .to_source();
        assert_eq!(result, r#"[1.0; "two"; fun a => a]"#);
    }
}
