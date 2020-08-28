use crate::parse::Input;
use std::rc::Rc;
use crate::parse::{Expr, ExprEq, Position, ExprResult, ToSource, FromInput, Rule};
use crate::context::Context;
use pest::iterators::{Pair, Pairs};
use core::fmt;
use std::cell::RefCell;
use crate::execute::{Scope};

#[derive(Debug, PartialEq, Clone)]
pub struct Fun {
    pub params: Vec<String>,
    pub body: Rc<Expr>,
    pub closure: RefCell<bool>,
    pub rec_decorated: RefCell<bool>,
    pub env: Rc<RefCell<Scope>>,
    pub position: Position,
}

impl fmt::Display for Fun {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self.params)
    }
}

impl ToSource for Fun {
    fn to_source(&self) -> String {
        format!("fun {} => {}", self.params.join(" "), self.body.to_source())
    }
}

impl FromInput for Fun {
    fn from_input(input: Input<'_>, context: &mut Context) -> ExprResult {
        let position = Position::from_input(&input);
        let input_ = input.clone();
        let mut inner: Pairs<Rule> = input.into_inner();
        let params = inner.next().unwrap().into_inner()
            .map(|item| item.as_str().to_string())
            .collect::<Vec<String>>();
        let body = inner.next().unwrap();

        Ok(Rc::new(Expr::Fun(Fun {
            params,
            closure: RefCell::new(false),
            rec_decorated: RefCell::new(false),
            body: Expr::from_input(input_.with_pair(&body), context)?,
            env: Rc::new(RefCell::new(vec![])),
            position,
        })))
    }
}

impl ExprEq for Fun {
    fn expr_eq(&self, _other: &Expr) -> bool {
        false
    }
}