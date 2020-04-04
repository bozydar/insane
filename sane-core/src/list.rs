use std::rc::Rc;
use crate::parse::{Expr, Position, ExprEq, ExprResult, ToSource, FromPair, Rule};
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
    fn from_pair(pair: Pair<'_, Rule>, source: &str) -> ExprResult {
        let mut items: Vec<Rc<Expr>> = vec![];
        let position = Position::from_span(pair.as_span(), source);

        for item in pair.into_inner() {
            items.push(Expr::from_pair(item, source)?);
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
        Ok(Rc::new(Expr::List(List { items: result, position: self.position.clone() })))
    }
}

impl ExprEq for List {
    fn expr_eq(&self, other: &Expr) -> bool {
        match other {
            Expr::List(other) => self.items.iter()
                .zip(other.items.iter())
                .all(|(left, right)| left.expr_eq(right)),
            _ => false

        }
       
    }
}