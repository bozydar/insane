use crate::context::Context;
use crate::parse::Input;
use crate::parse::{Expr, ExprEq, ExprResult, FromInput, Position, ToSource};
use std::rc::Rc;

use crate::execute::{execute, Execute, Scope};

#[derive(Debug, PartialEq, Clone)]
pub struct List {
    pub items: Vec<Rc<Expr>>,
    pub position: Position,
}

impl ToSource for List {
    fn to_source(&self) -> String {
        let c = self
            .items
            .iter()
            .map(|item| item.to_source())
            .collect::<Vec<String>>()
            .join("; ");
        format!("[{}]", c)
    }
}

impl FromInput for List {
    fn from_input(input: Input<'_>, context: &mut Context) -> ExprResult {
        let mut items: Vec<Rc<Expr>> = vec![];
        let position = Position::from_input(&input);

        let input_ = input.clone();
        for pair in input.into_inner() {
            items.push(Expr::from_input(input_.with_pair(&pair), context)?);
        }
        Ok(Rc::new(Expr::List(List { items, position })))
    }
}

impl Execute for List {
    fn execute(&self, stack: &mut Scope, context: &Context) -> ExprResult {
        let mut result: Vec<Rc<Expr>> = vec![];
        // let items = items.clone();
        for item in self.items.iter() {
            let item = execute(item.clone(), stack, context)?;
            result.push(item);
        }
        Ok(Rc::new(Expr::List(List {
            items: result,
            position: self.position.clone(),
        })))
    }
}

impl ExprEq for List {
    fn expr_eq(&self, other: &Expr) -> bool {
        match other {
            Expr::List(other) => self
                .items
                .iter()
                .zip(other.items.iter())
                .all(|(left, right)| left.expr_eq(right)),
            _ => false,
        }
    }
}
