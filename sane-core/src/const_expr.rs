use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, Error, ToSource, FromPair, Rule};
use pest::iterators::{Pair};

#[derive(Debug, PartialEq, Clone)]
pub enum ConstType {
    String(String),
    Numeric(f64),
    Bool(bool),
}

#[derive(Debug, PartialEq, Clone)]
pub struct Const {
    pub value: ConstType,
    pub position: Position,
}

impl Const {
    pub fn string(value: &str, position: Position) -> Const {
        Const {
            value: ConstType::String(value.to_string()),
            position,
        }
    }

    pub fn numeric(value: f64, position: Position) -> Const {
        Const {
            value: ConstType::Numeric(value),
            position,
        }
    }

    pub fn bool(value: bool, position: Position) -> Const {
        Const {
            value: ConstType::Bool(value),
            position,
        }
    }
}

impl ToSource for Const {
    fn to_source(&self) -> String {
        match &self.value {
            ConstType::String(string) => format!("\"{}\"", string),
            ConstType::Numeric(f64) => format!("{:?}", f64),
            ConstType::Bool(bool) => if *bool {
                "true".to_string()
            } else {
                "false".to_string()
            }
        }
    }
}

impl FromPair for Const {
    fn from_pair(pair: Pair<'_, Rule>) -> ExprResult {
        let pair = pair.into_inner().next().unwrap();
        let rule = pair.as_rule();
        let position: Position = From::from(pair.as_span());
        match rule {
            Rule::string => {
                Ok(Rc::new(Expr::Const(Const::string(pair.as_str(), position))))
            }
            Rule::number => {
                let value = pair.as_str().parse::<f64>().unwrap();
                Ok(Rc::new(Expr::Const(Const::numeric(value, position))))
            }
            _ => {
                let position: Position = From::from(pair.as_span());
                Error::new(&format!("Unknown const type `{:?}`", rule), position).into()
            }
        }
    }
}