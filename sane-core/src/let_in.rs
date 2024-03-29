use crate::context::Context;
use crate::execute::{execute, Execute, Scope};
use crate::parse::{Input, Typed};
use crate::parse::{Expr, ExprResult, FromInput, Position, Rule, ToSource};
use pest::iterators::Pairs;
use std::rc::Rc;
use crate::type_expr::TypeExpr;

#[derive(Debug, PartialEq, Clone)]
pub struct LetIn {
    pub lets: Vec<(String, Rc<Expr>)>,
    pub in_part: Rc<Expr>,
    pub position: Position,
    // pub ttype: Rc<TypeExpr>
}

impl ToSource for LetIn {
    fn to_source(&self) -> String {
        let lets = self
            .lets
            .iter()
            .map(|item| format!("let {} = {}", item.0, item.1.to_source()))
            .collect::<Vec<String>>()
            .join(" and ");
        format!("{} in {}", lets, self.in_part.to_source())
    }
}

// impl Typed for LetIn {
//     fn get_type(&self) -> Rc<TypeExpr> {
//         self.ttype.clone()
//     }
// }

impl FromInput for LetIn {
    fn from_input(input: Input<'_>, context: &mut Context) -> ExprResult {
        let input_ = input.clone();
        let mut inner: Pairs<'_, Rule> = input.into_inner();
        let mut lets: Vec<(String, Rc<Expr>)> = vec![];

        let mut pair = inner.next().unwrap();
        while Rule::ident_expr == pair.as_rule() {
            let mut ident_expr_inner = pair.into_inner();
            let ident = ident_expr_inner.next().unwrap().as_str().to_string();
            let value = ident_expr_inner.next().unwrap();
            lets.push((ident, Expr::from_input(input_.with_pair(&value), context)?));
            pair = inner.next().unwrap();
        }

        let in_ = input_.with_pair(&pair);
        let position = Position::from_input(&in_);
        let in_part = Expr::from_input(input_.with_pair(&pair), context)?;
        Ok(Rc::new(Expr::LetIn(LetIn {
            lets,
            in_part,
            position,
        })))
    }
}

impl Execute for LetIn {
    fn execute(&self, stack: &mut Scope, context: &Context) -> ExprResult {
        let let_in = self.clone();

        let mut pushed = 0;
        let mut functions: Scope = vec![];
        let mut to_append: Scope = vec![];

        // execute all the R-expressions
        // and put functions on the side for further analysis
        for (ident, expr) in let_in.lets.iter().cloned() {
            let expr = execute(expr, stack, context)?;

            if let Expr::Fun(_) = *expr {
                functions.push((ident.clone(), expr.clone()))
            }
            to_append.push((ident.clone(), expr));
        }

        // decorate each env of the function with the access to all the current stack
        // but decorate only these functions which haven't been decorated so far
        for (_ident, expr) in functions {
            if let Expr::Fun(fun) = &*expr {
                let mut rec_decorated = fun.rec_decorated.borrow_mut();
                if !*rec_decorated {
                    fun.env.replace_with(|env| {
                        // TODO
                        // 1. Introduce global function stack to not copy them every time
                        // 2. add only these variables which will be used inside
                        env.append(&mut let_in.lets.clone());
                        env.clone()
                    });
                    *rec_decorated = true;
                }
            }
        }

        // put all the declaration on the stack
        pushed += to_append.len();
        stack.append(&mut to_append);
        let result = execute(let_in.in_part, stack, context);
        stack.truncate(stack.len() - pushed);
        result
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute::execute_sane;

    #[test]
    fn execute_let_in_1() {
        let result = execute_sane("let a = 1 and let b = 2 in [a; b]")
            .unwrap()
            .to_source();
        assert_eq!(result, "[1.0; 2.0]");
    }

    #[test]
    fn check_type_1() {
        let result = execute_sane("type_of, (let a: Num = 1 in a)")
            .unwrap()
            .to_source();
        assert_eq!(result, "Num");
    }

    #[test]
    fn test_execute_let_0() {
        let result = &*execute_sane("let a = 1 in a").unwrap().to_source();
        assert_eq!(result, "1.0");
    }

    #[test]
    fn test_execute_let_1() {
        let result = &*execute_sane("let a = let b = 1 in b in a")
            .unwrap()
            .to_source();
        assert_eq!(result, "1.0");
    }
}
