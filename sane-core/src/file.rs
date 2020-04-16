use std::rc::Rc;
use crate::parse::{Expr, Position, ExprEq, ExprResult, ToSource, FromPair, Rule};
use crate::const_expr::{Const, ConstType};
use crate::ident::Ident;
use crate::error::Error;
use pest::iterators::Pair;


use crate::execute::{Execute, Stack, execute};

// TODO: Maybe better put everything into Expr because otherwise problems with converting
// Results


#[derive(Debug, PartialEq, Clone)]
pub struct File {
    pub n_space: Option<Rc<NSpace>>,
    pub uses: Vec<Ident>,
    pub expr: Rc<Expr>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct NSpace {
    pub name: String,
    pub exposed: Vec<Rc<Ident>>,
    pub defs: Vec<Rc<Def>>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Def {
    pub defs: Vec<(String, Rc<Expr>)>,
    pub position: Position,
}

impl ToSource for File {
    fn to_source(&self) -> String {
        let uses = 
            if !self.uses.is_empty() {
                format!("use {}\n", self.uses.iter()
                    .map(Ident::to_source)
                    .collect::<Vec<String>>()
                    .join(" "))
            } else {
                "".to_string()
            };
        let n_space = 
            if self.n_space.is_some() {
                format!("{}\n", self.n_space.clone().unwrap().to_source())
            } else {
                "".to_string()
            };
        let expr = self.expr.to_source();

        format!("{}{}{}", uses, expr, n_space)
    }
}

impl FromPair for File {
    fn from_pair(pair: Pair<'_, Rule>, source: &str) -> ExprResult {
        let position = Position::from_span(pair.as_span(), source);
        let mut inner = pair.into_inner();

        let uses = idents_from_use(inner.next().unwrap(), source)?;
        let expr = expr_else_unit(inner.next().unwrap(), source)?;
        let n_space = n_space_from_pair(inner.next().unwrap(), source)?;

        Ok(
            Rc::new(
                Expr::File(
                    File { n_space, uses, expr, position }
                )
            )
        )
    }
}

impl Execute for File {
    fn execute(&self, stack: &mut Stack) -> ExprResult {
        execute(self.expr.clone(), stack)
    }
}

fn idents_from_use(pair: Pair<'_, Rule>, source: &str) -> Result<Vec<Ident>, Error> {
    let mut idents = vec![];

    for ident in pair.into_inner() {
        if let Expr::Ident(ident) = &*Ident::from_pair(ident, source)? {
            idents.push(ident.clone())
        } else {
            unreachable!()
        }
    }

    Ok(idents)
}

fn expr_else_unit(pair: Pair<'_, Rule>, source: &str) -> ExprResult {
    let position = Position::from_span(pair.as_span(), source);

    if let Some(ident) = pair.into_inner().next() {
        Expr::from_pair(ident, source)
    } else {
        Ok(Rc::new(Expr::Const(Const { value: ConstType::Unit, position })))
    }
}

impl NSpace {
    fn autogenerate() -> Self {
        NSpace {
            name: "Auto".to_string(),
            exposed: vec![],
            defs: vec![],
            position: Position::new(0, 0, "AUTO"),
        }
    }
}

fn n_space_from_pair(pair: Pair<'_, Rule>, source: &str) -> Result<Option<Rc<NSpace>>, Error> {
    let position = Position::from_span(pair.as_span(), source);
    let mut inner = pair.into_inner();

    // generate default namespace if not found
    if inner.peek().is_none() {
        return Ok(None);
    }

    let name =
        if let Expr::Ident(ident) = &*Ident::from_pair(inner.next().unwrap(), source)? {
            ident.label.clone()
        } else {
            unreachable!()
        };
    let exposed_pair = inner.next().unwrap().into_inner();
    let mut exposed = vec![];
    for ident_pair in exposed_pair {
        if let Expr::Ident(ident) = &*Ident::from_pair(ident_pair, source)? {
            exposed.push(Rc::new(ident.clone()))
        } else {
            unreachable!()
        };
    }

    let defs_pair = inner.next().unwrap().into_inner();
    let mut defs = vec![];
    for def_pair in defs_pair {
        if let Expr::Def(def) = &*Def::from_pair(def_pair, source)? {
            defs.push(Rc::new(def.clone()))
        } else {
            unreachable!()
        };
    }

    Ok(
        Some(
            Rc::new(
                NSpace { name, exposed, defs, position }
            )
        )
    )
}


impl FromPair for Def {
    fn from_pair(pair: Pair<'_, Rule>, source: &str) -> ExprResult {
        let mut inner = pair.into_inner();
        let mut defs: Vec<(String, Rc<Expr>)> = vec![];

        let mut pair = inner.next().unwrap();
        while Rule::ident_expr == pair.as_rule() {
            let mut ident_expr_inner = pair.into_inner();
            let ident = ident_expr_inner.next().unwrap().as_str().to_string();
            let value = ident_expr_inner.next().unwrap();
            defs.push((ident, Expr::from_pair(value, source)?));
            pair = inner.next().unwrap();
        }

        let position = Position::from_span(pair.as_span(), source);
        let in_part = Expr::from_pair(pair, source)?;
        Ok(Rc::new(Expr::Def(Def {
            defs,
            position,
        })))
    }
}

impl ToSource for NSpace {
    fn to_source(&self) -> String {
        let exposed = self.exposed.iter()
            .map(|item| {
                item.to_source()
            })
            .collect::<Vec<String>>()
            .join(" ");

        let defs = self.exposed.iter()
            .map(|item| {
                item.to_source()
            })
            .collect::<Vec<String>>()
            .join("\n");

        format!("nspace {} give {}\n\n{}", &self.name, exposed, defs)
    }
}

impl ToSource for Def {
    fn to_source(&self) -> String {
        let lets = self.defs.iter()
            .map(|item| {
                format!("{} = {}", item.0, item.1.to_source())
            })
            .collect::<Vec<String>>()
            .join("\nand ");
        format!("def {}", lets)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::execute::execute_sane;
    use crate::parse::parse_sane;

    #[test]
    fn execute_let_in_2() {
        let result = execute_sane("let a = 1 and let b = 2 in [a; b]").unwrap().to_source();
        assert_eq!(result, "[1.0; 2.0]");
    }
}