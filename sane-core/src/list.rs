use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, ToSource, FromPair, Rule};
use pest::iterators::{Pair};


use crate::execute::{Execute, Stack, execute};


#[derive(Debug, PartialEq, Clone)]
pub struct List {
    pub items: Vec<Rc<Expr>>,
    pub position: Position,
}

impl ToSource for List {
    fn to_source(&self) -> String {
        let c = self.items.iter().map(|item| item.to_source()).collect::<Vec<String>>().join("; ");
        format!("[{}]", c)
    }
}

impl FromPair for List {
    fn from_pair(pair: Pair<'_, Rule>) -> ExprResult {
        let mut items: Vec<Rc<Expr>> = vec![];
        let position = pair.as_span().into();

        for item in pair.into_inner() {
            items.push(Expr::from_pair(item)?);
        }
        Ok(Rc::new(Expr::List(List { items, position })))
    }
}

impl Execute for List {
    fn execute(&self, stack: &mut Stack) -> ExprResult {
        let mut result: Vec<Rc<Expr>> = vec![];
        // let items = items.clone();
        for item in self.items.iter() {
            let item = execute(item.clone(), stack)?;
            result.push(item);
        }
        Ok(Rc::new(Expr::List(List { items: result, position: self.position })))
    }
}
