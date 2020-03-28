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
    fn from_pair(pair: Pair<'_, Rule>) -> ExprResult {
        let position = pair.as_span().into();
        let mut inner: Pairs<'_, Rule> = pair.into_inner();
        let cond = inner.next().unwrap();
        let then = inner.next().unwrap();
        let otherwise = inner.next().unwrap();

        Ok(Rc::new(Expr::IfThenElse(IfThenElse {
            cond: Expr::from_pair(cond)?,
            then: Expr::from_pair(then)?,
            otherwise: Expr::from_pair(otherwise)?,
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
