use crate::parse::Input;
use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, ToSource, FromInput, Rule};
use crate::context::Context;
use pest::iterators::{Pair, Pairs};


use crate::execute::{execute, Execute, Scope};

use crate::const_expr::{Const, ConstType};

#[derive(Debug, PartialEq, Clone)]
pub struct IfThenElse {
    pub cond: Rc<Expr>,
    pub then: Rc<Expr>,
    pub otherwise: Rc<Expr>,
    pub position: Position,
}


impl ToSource for IfThenElse {
    fn to_source(&self) -> String {
        format!("if {} then {} else {}", self.cond.to_source(), self.then.to_source(), self.otherwise.to_source()).to_lowercase()
    }
}

impl FromInput for IfThenElse {
    fn from_input(input: Input<'_>, context: &mut Context) -> ExprResult {
        let position = Position::from_input(input);
        let mut inner: Pairs<'_, Rule> = input.into_inner();
        let cond = input.with_pair(inner.next().unwrap());
        let then = input.with_pair(inner.next().unwrap());
        let otherwise = input.with_pair(inner.next().unwrap());

        Ok(Rc::new(Expr::IfThenElse(IfThenElse {
            cond: Expr::from_input(cond, context)?,
            then: Expr::from_input(then, context)?,
            otherwise: Expr::from_input(otherwise, context)?,
            position,
        })))
    }
}

impl Execute for IfThenElse {
    fn execute(&self, stack: &mut Scope, context: &Context) -> ExprResult {
        let result = execute(self.cond.clone(), stack, context)?;
        if let Expr::Const(Const { value: ConstType::Bool(true), .. }) = *result {
            execute(self.then.clone(), stack, context)
        } else {
            execute(self.otherwise.clone(), stack, context)
        }
    }
}



#[cfg(test)]
mod tests {
    use crate::execute::execute_sane;
    use crate::parse::ToSource;

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
                    inc(a)
                in
                let b = 1 |> plus_one in
                if b |> eq <| 2 then 1 else 2"#).unwrap().to_source();
        assert_eq!(result, "1.0");
    }

}
