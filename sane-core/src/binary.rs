use crate::bind::Bind;
use crate::context::Context;
use crate::execute::{Execute, Scope};
use crate::ident::Ident;
use crate::parse::{Expr, ExprResult, Position, Rule, ToSource};
use pest::iterators::Pair;
use std::rc::Rc;

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
    Comma,
    Dollar,
    Plus,
    Minus,
    Star,
    Slash,
    Eq,
    Neq
}

pub(crate) const LEFT_PIPE: &str = "|>";
pub(crate) const RIGHT_PIPE: &str = "<|";
pub(crate) const COMMA: &str = ",";
pub(crate) const DOLLAR: &str = "$";
pub(crate) const PLUS: &str = "+";
pub(crate) const MINUS: &str = "-";
pub(crate) const STAR: &str = "*";
pub(crate) const SLASH: &str = "/";
pub(crate) const EQ: &str = "==";
pub(crate) const NEQ: &str = "!=";

impl From<&str> for Operator {
    fn from(s: &str) -> Operator {
        match s {
            LEFT_PIPE => Operator::LeftPipe,
            RIGHT_PIPE => Operator::RightPipe,
            COMMA => Operator::Comma,
            DOLLAR => Operator::Dollar,
            PLUS => Operator::Plus,
            MINUS => Operator::Minus,
            STAR => Operator::Star,
            SLASH => Operator::Slash,
            EQ => Operator::Eq,
            NEQ => Operator::Neq,
            _ => unreachable!("Don't know symbol `{}`", s),
        }
    }
}

impl ToSource for Operator {
    fn to_source(&self) -> String {
        match self {
            Operator::LeftPipe => LEFT_PIPE.to_string(),
            Operator::RightPipe => RIGHT_PIPE.to_string(),
            Operator::Comma => COMMA.to_string(),
            Operator::Dollar => DOLLAR.to_string(),
            Operator::Plus => PLUS.to_string(),
            Operator::Minus => MINUS.to_string(),
            Operator::Star => STAR.to_string(),
            Operator::Slash => SLASH.to_string(),
            Operator::Eq => EQ.to_string(),
            Operator::Neq => NEQ.to_string(),
        }
    }
}

impl ToSource for Binary {
    fn to_source(&self) -> String {
        format!(
            "({} {} {})",
            self.left.to_source(),
            self.operator.to_source(),
            self.right.to_source()
        )
    }
}

impl Binary {
    // TODO it is not a constructor of binary so it should have different name
    pub fn build_expr(
        operator: Pair<'_, Rule>,
        left: ExprResult,
        right: ExprResult,
        source: &str,
    ) -> ExprResult {
        let span = operator.as_span();
        let position = Position::new(span.start(), span.end(), source);
        Ok(Rc::new(Expr::Binary(Binary {
            left: left.unwrap(),
            right: right.unwrap(),
            operator: operator.as_str().into(),
            position,
        })))
    }
}

impl Execute for Binary {
    fn execute(&self, stack: &mut Scope, context: &Context) -> ExprResult {
        // TODO it is rather ineficient but need a "postparsing" phase to optimize it
        // In such phase all the Binary expression could be replaced with Bind so I couldn't
        // do it every time a binary is called

        let bind = match self {
            // Binding-like operators cannot be defined as build-ins because these functions has no access
            // stack. They can return Bind expressions though
            Binary {
                left,
                right,
                operator: Operator::LeftPipe,
                position,
            } => {
                let args = vec![left.clone()];
                let fun = right.clone();
                Bind {
                    args,
                    fun,
                    position: position.clone(),
                }
            }
            Binary {
                left,
                right,
                operator: Operator::RightPipe,
                position,
            } => {
                let args = vec![right.clone()];
                let fun = left.clone();
                Bind {
                    args,
                    fun,
                    position: position.clone(),
                }
            },
            Binary {
                left,
                right,
                operator: Operator::Comma,
                position,
            } => {
                let args = vec![right.clone()];
                let fun = left.clone();
                Bind {
                    args,
                    fun,
                    position: position.clone(),
                }
            }
            Binary {
                left,
                right,
                operator,
                position,
            } => {
                let args = vec![left.clone(), right.clone()];
                let fun = Rc::new(Expr::Ident(Ident {
                    label: operator.to_source(),
                    position: position.clone(),
                }));
                Bind {
                    args,
                    fun,
                    position: self.position.clone(),
                }
            }
        };

        bind.execute(stack, context)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute::execute_sane;
    use crate::parse::parse_file;

    #[test]
    fn test_parse_binary() {
        let context = &mut Context::new(vec![]);
        let result = &*parse_file("a |> b |> c", "ADHOC", context)
            .unwrap()
            .to_source();
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
        let result = &*execute_sane("inc <| print <| inc <| inc <| 1")
            .unwrap()
            .to_source();
        assert_eq!(result, "4.0");
    }

    #[test]
    fn test_execute_binary_3() {
        let result = &*execute_sane("1 |> inc |> add <| inc <| inc <| 1")
            .unwrap()
            .to_source();
        assert_eq!(result, "5.0");
    }

    #[test]
    fn test_execute_binary_curry_0() {
        let result = &*execute_sane("inc <| print <| inc <| inc <| 1")
            .unwrap()
            .to_source();
        assert_eq!(result, "4.0");
    }

    #[test]
    fn test_execute_binary_curry_1() {
        let result = &*execute_sane(r#"
        let f = fun a b c => a + b + c
        in f, 1, 2, 3"#)
            .unwrap()
            .to_source();
        assert_eq!(result, "6.0");
    }

    #[test]
    fn test_execute_binary_operations_0() {
        let result = &*execute_sane("2 + 3 * 4").unwrap().to_source();
        assert_eq!(result, "14.0");
    }

    #[test]
    fn test_execute_binary_operations_1() {
        let result = &*execute_sane("2 * 2 + 3").unwrap().to_source();
        assert_eq!(result, "7.0");
    }

    #[test]
    fn test_execute_binary_operations_2() {
        let result = &*execute_sane("3 + 2 / 2").unwrap().to_source();
        assert_eq!(result, "4.0");
    }

    #[test]
    fn test_execute_binary_operations_3() {
        let result = &*execute_sane("(2 + 3) * 4").unwrap().to_source();
        assert_eq!(result, "20.0");
    }

    #[test]
    fn test_execute_binary_operations_4() {
        let result = &*execute_sane("(3 + 2) / 2").unwrap().to_source();
        assert_eq!(result, "2.5");
    }
}
