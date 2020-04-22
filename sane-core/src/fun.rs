use std::rc::Rc;
use crate::parse::{Expr, ExprEq, Position, ExprResult, ToSource, FromPair, Rule, Context};
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

impl FromPair for Fun {
    fn from_pair(pair: Pair<'_, Rule>, context: &mut Context) -> ExprResult {
        let position = Position::from_span(pair.as_span(), context);
        let mut inner: Pairs<Rule> = pair.into_inner();
        let params = inner.next().unwrap().into_inner()
            .map(|item| item.as_str().to_string())
            .collect::<Vec<String>>();
        let body = inner.next().unwrap();

        Ok(Rc::new(Expr::Fun(Fun {
            params,
            closure: RefCell::new(false),
            rec_decorated: RefCell::new(false),
            body: Expr::from_pair(body, context)?,
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