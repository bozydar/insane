use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, ToSource, FromPair, Rule};
use crate::error::Error;
use crate::execute::{Stack, Execute};
use pest::iterators::{Pair};


#[derive(Debug, PartialEq, Clone)]
pub struct Ident {
    pub label: String,
    pub position: Position,
}

impl ToSource for Ident {
    fn to_source(&self) -> String {
        self.label.clone()
    }
}


impl FromPair for Ident {
    fn from_pair(pair: Pair<'_, Rule>, source: &str) -> ExprResult {
        let position = Position::from_span(pair.as_span(), source);
        Ok(Rc::new(Expr::Ident(Ident { label: pair.as_str().to_string(), position })))
    }
}

impl Execute for Ident {
    fn execute(&self, stack: &mut Stack) -> ExprResult {
        if let Some((_, expr)) = stack.iter().rev().find(|item| { &item.0 == &self.label }) {
            Ok(expr.clone())
        } else {
            // Err(format!("Ident `{}` not found: {}", ident, stack_to_string(stack)))
            Error::new(&format!("Ident `{}` not found", self.label), &self.position).into()
        }
    }
}