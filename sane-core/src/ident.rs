use crate::context::Context;
use crate::error::Error;
use crate::execute::{Execute, Scope};
use crate::parse::{Input, Typed};
use crate::parse::{Expr, ExprResult, FromInput, Position, ToSource};
use std::rc::Rc;
use crate::type_expr::{TypeExpr, Literal, LiteralType, Variable};
use crate::let_in::LetIn;

#[derive(Debug, PartialEq, Clone)]
pub struct Ident {
    pub label: String,
    pub position: Position,
    pub ttype: Rc<TypeExpr>
}

impl ToSource for Ident {
    fn to_source(&self) -> String {
        self.label.clone()
    }
}

// impl Typed for LetIn {
//     fn get_type(&self) -> Rc<TypeExpr> {
//         self.ttype.clone()
//     }
// }

impl Ident {
    pub fn try_from_input(input: Input<'_>) -> Result<Ident, Error> {
        let position = Position::from_input(&input);
        let ttype = Rc::new(TypeExpr::Variable(Variable { label: input.pair_as_str().to_string() }));
        Ok(Ident {
            label: input.pair_as_str().to_string(),
            position,
            ttype
        })
    }
}

impl FromInput for Ident {
    fn from_input(input: Input<'_>, _context: &mut Context) -> ExprResult {
        let _position = Position::from_input(&input);
        Ok(Rc::new(Expr::Ident(Ident::try_from_input(input)?)))
    }
}

impl Execute for Ident {
    fn execute(&self, stack: &mut Scope, _context: &Context) -> ExprResult {
        if let Some((_, expr)) = stack.iter().rev().find(|item| item.0 == self.label) {
            Ok(expr.clone())
        } else {
            // Err(format!("Ident `{}` not found: {}", ident, stack_to_string(stack)))
            Error::new(&format!("Ident `{}` not found", self.label), &self.position).into()
        }
    }
}
