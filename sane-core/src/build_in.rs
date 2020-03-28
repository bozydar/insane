use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, Error, ToSource};

use std::fmt::{Debug, Formatter};
use std::fmt;
use crate::const_expr::{Const, ConstType};
use crate::list::List;

pub type BuildInFun = fn(Vec<Rc<Expr>>, Position) -> ExprResult;

#[derive(Clone)]
pub struct BuildIn {
    pub fun: BuildInFun,
    pub name: String,
    pub arity: usize,
}

impl Debug for BuildIn {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "BuildIn({})", self.name)
    }
}

impl PartialEq for BuildIn {
    fn eq(&self, other: &Self) -> bool {
        self.name.eq(&other.name)
    }

    fn ne(&self, other: &Self) -> bool {
        !self.eq(other)
    }
}

impl ToSource for BuildIn {
    fn to_source(&self) -> String {
        format!("{:?}", self).to_lowercase()
    }
}




