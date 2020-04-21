use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, ToSource, Rule, Context};
use crate::execute::{Scope, Execute};
use pest::iterators::{Pair};
use crate::ident::Ident;
use crate::bind::Bind;

// The binary Expr is done only to allow ToSource() working fine. I could use just Bind expr
// to make too. Anyway it is important to have: source -> Parser -> source flow working
// fine
#[derive(Debug, PartialEq, Clone)]
pub struct Binary {
    pub left: Rc<Expr>,
    pub right: Rc<Expr>,
    pub operator: Operator,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Operator {
    LeftPipe,
    RightPipe,
    Dollar
}

pub(crate) const LEFT_PIPE: &str= "|>";
pub(crate) const RIGHT_PIPE: &str= "<|";
pub(crate) const DOLLAR: &str = "$";

impl From<&str> for Operator {
    fn from(s: &str) -> Operator {
        match s {
            LEFT_PIPE => Operator::LeftPipe,
            RIGHT_PIPE => Operator::RightPipe,
            DOLLAR => Operator::Dollar,
            _ => unreachable!("Don't know symbol `{}`", s)
        }
    }
}

impl ToSource for Operator {
    fn to_source(&self) -> String {
        match self {
            Operator::LeftPipe => LEFT_PIPE.to_string(),
            Operator::RightPipe => RIGHT_PIPE.to_string(),
            Operator::Dollar => DOLLAR.to_string(),
        }
    }
}

impl ToSource for Binary {
    fn to_source(&self) -> String {
        format!("({} {} {})", self.left.to_source(), 
            self.operator.to_source(), self.right.to_source())
    }
}

impl Binary {
    // TODO it is not a constructor of binary so it should have different name
    pub fn new(operator: Pair<'_, Rule>, left: ExprResult, right: ExprResult, context: &mut Context) -> ExprResult {
        let position = Position::from_span(operator.as_span(), context);
        Ok(
            Rc::new(
                Expr::Binary(
                    Binary { 
                        left: left.unwrap(), 
                        right: right.unwrap(), 
                        operator: operator.as_str().into(),
                        position
                    }
                )
            )
        )
    }
}

impl Execute for Binary {
    fn execute(&self, stack: &mut Scope) -> ExprResult {
        // TODO it is rather ineficient but need a "postparsing" phase to optimize it
        // In such phase all the Binary expression could be replaced with Bind so I couldn't
        // do it every time a binary is called

        let bind =
            match self {
                // Binding-like operators cannot be defined as build-ins because these functions has no access
                // stack. They can return Bind expressions though
                Binary {left, right, operator: Operator::LeftPipe, position} => {
                    let args = vec![left.clone()];
                    let fun = right.clone();
                    Bind {
                        args,
                        fun,
                        position: position.clone()
                    }
                }
                Binary {left, right, operator: Operator::RightPipe, position} => {
                    let args = vec![right.clone()];
                    let fun = left.clone();
                    Bind {
                        args,
                        fun,
                        position: position.clone()
                    }
                }
                Binary {left, right, operator, position} => {
                    let args = vec![
                        left.clone(),
                        right.clone()
                    ];
                    let fun = Rc::new(Expr::Ident(Ident {
                        label: operator.to_source(),
                        position: position.clone()
                    }));
                    Bind {
                        args,
                        fun,
                        position: self.position.clone()
                    }
                }
            };

        bind.execute(stack)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute::execute_sane;
    use crate::parse::parse_sane;

    #[test]
    fn test_parse_binary() {
        let result = &*parse_sane("a |> b |> c").unwrap().to_source();
        assert_eq!(result, "((a |> b) |> c)");
    }

    #[test]
    fn test_execute_binary_0() {
        let result = &*execute_sane("1 |> inc").unwrap().to_source();
        assert_eq!(result, "2.0");
    }

    #[test]
    fn test_execute_binary_1() {
        let result = &*execute_sane("1 |> inc |> inc |> inc").unwrap().to_source();
        assert_eq!(result, "4.0");
    }

    #[test]
    fn test_parse_binary_1() {
        let result = &*execute_sane("1 |> inc |> inc |> inc").unwrap().to_source();
        assert_eq!(result, "4.0");
    }


    #[test]
    fn test_execute_binary_2() {
        let result = &*execute_sane("inc <| print <| inc <| inc <| 1").unwrap().to_source();
        assert_eq!(result, "4.0");
    }

    #[test]
    fn test_execute_binary_curry_0() {
        let result = &*execute_sane("inc <| print <| inc <| inc <| 1").unwrap().to_source();
        assert_eq!(result, "4.0");
    }
}
