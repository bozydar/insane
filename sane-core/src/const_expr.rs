use std::rc::Rc;
use crate::parse::{Expr, ExprEq, Position, ExprResult, Error, ToSource, FromPair, Rule};
use pest::iterators::{Pair};

#[derive(Debug, PartialEq, Clone)]
pub enum ConstType {
    String(String),
    Numeric(f64),
    Bool(bool),
}
                                            
#[derive(Debug, Clone)]
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

impl PartialEq for Const {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
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
    fn from_pair(pair: Pair<'_, Rule>, source: &str) -> ExprResult {
        let pair = pair.into_inner().next().unwrap();
        let rule = pair.as_rule();
        let position: Position = Position::from_span(pair.as_span(), source);
        match rule {
            Rule::string => {
                Ok(Rc::new(Expr::Const(Const::string(pair.as_str(), position))))
            }
            Rule::number => {
                let value = pair.as_str().parse::<f64>().unwrap();
                Ok(Rc::new(Expr::Const(Const::numeric(value, position))))
            }
            _ => {
                let position: Position = Position::from_span(pair.as_span(), source);
                Error::new(&format!("Unknown const type `{:?}`", rule), &position).into()
            }
        }
    }
}

impl ExprEq for Const {
    fn expr_eq(&self, other: &Expr) -> bool {
        match other {
            Expr::Const(other) => self.value == other.value,
            _ => false
        }
        
    }
}