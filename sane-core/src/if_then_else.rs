use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, ToSource, FromPair, Rule};
use pest::iterators::{Pair, Pairs};


use crate::execute::{execute, Execute, Stack};

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

impl FromPair for IfThenElse {
    fn from_pair(pair: Pair<'_, Rule>, source: &str) -> ExprResult {
        let position = Position::from_span(pair.as_span(), source);
        let mut inner: Pairs<'_, Rule> = pair.into_inner();
        let cond = inner.next().unwrap();
        let then = inner.next().unwrap();
        let otherwise = inner.next().unwrap();

        Ok(Rc::new(Expr::IfThenElse(IfThenElse {
            cond: Expr::from_pair(cond, source)?,
            then: Expr::from_pair(then, source)?,
            otherwise: Expr::from_pair(otherwise, source)?,
            position,
        })))
    }
}

impl Execute for IfThenElse {
    fn execute(&self, stack: &mut Stack) -> ExprResult {
        let result = execute(self.cond.clone(), stack)?;
        if let Expr::Const(Const { value: ConstType::Bool(true), .. }) = *result {
            execute(self.then.clone(), stack)
        } else {
            execute(self.otherwise.clone(), stack)
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
                    app a to inc
                in
                let b = app 1 to plus_one in
                if app b, 2 to eq then 1 else 2"#).unwrap().to_source();
        assert_eq!(result, "1.0");
    }

}
