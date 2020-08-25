use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, ToSource, FromPair, Rule};
use crate::context::Context;
use crate::error::Error;
use crate::execute::{Scope, Execute, execute};
use pest::iterators::{Pair, Pairs};


#[derive(Debug, PartialEq, Clone)]
pub struct NSIdent {
    pub nspace: String,
    pub label: String,
    pub position: Position,
}

impl ToSource for NSIdent {
    fn to_source(&self) -> String {
        format!("{}.{}", self.nspace.clone(), self.label.clone())
    }
}

impl NSIdent {
    pub fn try_from_pair(pair: Pair<'_, Rule>, context: &mut Context) -> Result<NSIdent, Error> {
        let position = Position::from_span(pair.as_span(), context);
        let mut inner: Pairs<'_, Rule> = pair.into_inner();
        let nspace = inner.next().unwrap().as_str();
        let label = inner.next().unwrap().as_str();

        Ok(NSIdent {
            nspace: nspace.to_string(),
            label: label.to_string(),
            position
        })
    }
}


impl FromPair for NSIdent {
    fn from_pair(pair: Pair<'_, Rule>, context: &mut Context) -> ExprResult {
        let _position = Position::from_span(pair.as_span(), context);
        Ok(Rc::new(Expr::NSIdent(NSIdent::try_from_pair(pair, context)?)))
    }
}

impl Execute for NSIdent {
    fn execute(&self, scope: &mut Scope, context: &Context) -> ExprResult {
        dbg!(scope.clone());
        // TODO Introduce to Context execute method which will find the file by nspace
        // and return the expression designed by identifier

        execute(context.expr_by_ns_ident(self)?, scope, context)
        // context
        // if let Some((_, expr)) = stack.iter().rev().find(|item| { &item.0 == &self.label }) {
        //     Ok(expr.clone())
        // } else {
        //     // Err(format!("Ident `{}` not found: {}", ident, stack_to_string(stack)))
        //     Error::new(&format!("NSIdent `{}` not found", self.to_source()), &self.position).into()
        // }
    }
}
