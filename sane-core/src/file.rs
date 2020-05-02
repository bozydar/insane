use std::rc::Rc;
use std::cell::RefCell;
use crate::parse::{Expr, Position, ExprResult, ToSource, FromPair, Rule};
use crate::context::Context;
use crate::const_expr::{Const, ConstType};
use crate::ident::Ident;
use crate::error::Error;
use pest::iterators::Pair;


use crate::execute::{Execute, Scope, execute};

// TODO: Maybe better put everything into Expr because otherwise problems with converting
// Results

#[derive(Debug, PartialEq, Clone)]
pub struct Import {
    pub idents: Vec<Rc<Ident>>,
    pub position: Position,
}

impl ToSource for Import {
    fn to_source(&self) -> String {
        if !self.idents.is_empty() {
            format!("use {}\n", self.idents.iter()
                .map(|ident| ident.to_source() )
                .collect::<Vec<String>>()
                .join(" "))
        } else {
            "".to_string()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Exposition {
    pub idents: Vec<Rc<Ident>>,
    pub position: Position,
}

impl ToSource for Exposition {
    fn to_source(&self) -> String {
        if !self.idents.is_empty() {
            format!("use {}\n", self.idents.iter()
                .map(|ident| ident.to_source() )
                .collect::<Vec<String>>()
                .join(" "))
        } else {
            "".to_string()
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct File {
    pub name: String,
    pub import: Rc<Import>,
    pub exposition: Rc<Exposition>,
    pub expr: Rc<Expr>,
    pub definitions: Vec<Rc<Definition>>,
    pub exposed_module: Rc<Module>,
    // pub imported_modules: Vec<Rc<Module>>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Definition {
    pub def: (String, Rc<Expr>),
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Module {
    pub env: Rc<Scope>
}

impl ToSource for File {
    fn to_source(&self) -> String {
        let mut import = self.import.to_source();
        if !import.is_empty() {
            import = format!("{}\n", import);
        }

        let mut exposition = self.exposition.to_source();
        if !exposition.is_empty() {
            exposition = format!("{}\n", exposition);
        }

        let mut definitions = self.definitions.iter()
            .map(|item| item.to_source())
            .collect::<Vec<String>>()
            .join("\n");
        if !definitions.is_empty() {
            definitions = format!("{}\n", definitions);
        }

        let mut expr = self.expr.to_source();
        if !expr.is_empty() {
            expr = if exposition.is_empty() && definitions.is_empty() {
                expr
            } else {
                format!("{}\n", expr)
            }
        };

        format!("{}{}{}{}", import, expr, exposition, definitions)
    }
}

impl FromPair for File {
    fn from_pair(pair: Pair<'_, Rule>, context: &mut Context) -> ExprResult {
        let position = Position::from_span(pair.as_span(), context);
        let name = context.source_name();
        let mut inner = pair.into_inner();

        // TODO Ask module loader to parse the file and keep its AST there
        let import = Rc::new(Import::try_from_pair(inner.next().unwrap(), context)?);
        let expr = expr_else_unit(inner.next().unwrap(), context)?;
        let exposition = Rc::new(Exposition::try_from_pair(inner.next().unwrap(), context)?);
        let definitions = Definition::try_many_from_pair(inner.next().unwrap(), context)?;
        let exposed_module = Rc::new(build_module(&exposition.idents, &definitions)?);
        // let imported_modules = fetch_modules(&import.idents)?;

        Ok(
            Rc::new(
                Expr::File(
                    File { import, exposition, expr, position, definitions, exposed_module, name }
                )
            )
        )
    }
}

fn build_module(expositions: &[Rc<Ident>], definitions: &[Rc<Definition>]) -> Result<Module, Error> {
    let mut env = vec![];
    for exposed in expositions {
        if let Some(found_pair) = definitions.iter().find(|pair| pair.def.0 == exposed.label)  {
            env.push((exposed.label.clone(), found_pair.def.1.clone()));
        }
    }
    Ok(Module { env: Rc::new(env) })
}

fn fetch_modules(imports: &[Rc<Ident>]) -> Result<Vec<Rc<Module>>, Error> {
    unimplemented!()
}

impl Import {
    fn try_from_pair(pair: Pair<'_, Rule>, context: &mut Context) -> Result<Import, Error> {
        let position = Position::from_span(pair.as_span(), context);
        let mut idents = vec![];

        for ident in pair.into_inner() {
            let ident = Ident::try_from_pair(ident, context)?;
            let _ = context.load_file(&ident)?;
            idents.push(Rc::new(ident.clone()))
        }

        Ok(Import { idents, position  })
    }
}

impl Exposition {
    fn try_from_pair(pair: Pair<'_, Rule>, context: &mut Context) -> Result<Exposition, Error> {
        let position = Position::from_span(pair.as_span(), context);
        let mut idents = vec![];

        for ident in pair.into_inner() {
            let ident = Ident::try_from_pair(ident, context)?;
            idents.push(Rc::new(ident.clone()))
        }

        Ok(Exposition { idents, position  })
    }
}

impl Definition {
    fn try_from_pair(pair: Pair<'_, Rule>, context: &mut Context) -> Result<Definition, Error> {
        let position = Position::from_span(pair.as_span(), context);
        let mut ident_expr_inner = pair.into_inner().next().unwrap().into_inner();

        let ident = ident_expr_inner.next().unwrap().as_str().to_string();
        let value = ident_expr_inner.next().unwrap();

        let def = (ident, Expr::from_pair(value, context)?);

        Ok(Definition { def, position })
    }

    fn try_many_from_pair(pair: Pair<'_, Rule>, context: &mut Context) -> Result<Vec<Rc<Definition>>, Error> {
        let mut defintions = vec![];

        for def in pair.into_inner() {
            let def = Definition::try_from_pair(def, context)?;
            defintions.push(Rc::new(def));
        }

        Ok(defintions)
    }
}

impl ToSource for Definition {
    fn to_source(&self) -> String {
        format!("def {} = {}", self.def.0, self.def.1.to_source())
    }
}

impl Execute for File {
    fn execute(&self, stack: &mut Scope) -> ExprResult {
        execute(self.expr.clone(), stack)
    }
}

fn expr_else_unit(pair: Pair<'_, Rule>, context: &mut Context) -> ExprResult {
    let position = Position::from_span(pair.as_span(), context);

    if let Some(ident) = pair.into_inner().next() {
        print!("HERE");
        Expr::from_pair(ident, context)
    } else {
        print!("unit!!!!!!!");
        Ok(Rc::new(Expr::Const(Const { value: ConstType::Unit, position })))
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

    #[test]
    fn execute_use_0() {
        let result = execute_sane(r#"
        use Prelude

        let a = 1 and let b = 2 in [a; b]
        
        "#).unwrap().to_source();
        assert_eq!(result, "[1.0; 2.0]");
    }

    #[test]
    fn execute_namespace_0() {
        let result = execute_sane(r#"
        use Prelude
        
        let ......

        nspace A give a
            def a = 1

        
        "#).unwrap().to_source();
        assert_eq!(result, "[1.0; 2.0]");
    }
}