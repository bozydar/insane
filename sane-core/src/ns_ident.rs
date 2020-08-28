use crate::parse::Input;
use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, ToSource, FromInput, Rule};
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
    pub fn try_from_input(input: Input<'_>) -> Result<NSIdent, Error> {
        let position = Position::from_input(&input);
        let mut inner: Pairs<'_, Rule> = input.into_inner();
        let nspace = inner.next().unwrap().as_str();
        let label = inner.next().unwrap().as_str();

        Ok(NSIdent {
            nspace: nspace.to_string(),
            label: label.to_string(),
            position
        })
    }
}


impl FromInput for NSIdent {
    fn from_input(input: Input<'_>, context: &mut Context) -> ExprResult {
        let _position = Position::from_input(&input);
        Ok(Rc::new(Expr::NSIdent(NSIdent::try_from_input(input)?)))
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
