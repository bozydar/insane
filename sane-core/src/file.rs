use crate::const_expr::{Const, ConstType};
use crate::context::Context;
use crate::error::Error;
use crate::ident::Ident;
use crate::parse::Input;
use crate::parse::{Expr, ExprResult, FromInput, Position, ToSource};

use std::rc::Rc;

use crate::execute::{execute, Execute, Scope};

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
            format!(
                "use ({})\n",
                self.idents
                    .iter()
                    .map(|ident| ident.to_source())
                    .collect::<Vec<String>>()
                    .join(" ")
            )
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

impl Exposition {
    pub fn is_exposed(&self, label: &str) -> bool {
        self.idents.iter().any(|ident| ident.label == label)
    }
}

impl ToSource for Exposition {
    fn to_source(&self) -> String {
        if !self.idents.is_empty() {
            format!(
                "give ({})\n",
                self.idents
                    .iter()
                    .map(|ident| ident.to_source())
                    .collect::<Vec<String>>()
                    .join(" ")
            )
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
    pub env: Rc<Scope>,
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

        let mut definitions = self
            .definitions
            .iter()
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

impl FromInput for File {
    fn from_input(input: Input<'_>, context: &mut Context) -> ExprResult {
        let position = Position::from_input(&input);
        let name = input.source_name();
        let input_ = input.clone();
        let mut inner = input.into_inner();

        // TODO Ask module loader to parse the file and keep its AST there
        let import = Rc::new(Import::try_from_input(
            input_.with_pair(&inner.next().unwrap()),
            context,
        )?);
        let expr = expr_else_unit(input_.with_pair(&inner.next().unwrap()), context)?;
        let exposition = Rc::new(Exposition::try_from_input(
            input_.with_pair(&inner.next().unwrap()),
        )?);
        let definitions =
            Definition::try_many_from_input(input_.with_pair(&inner.next().unwrap()), context)?;
        let exposed_module = Rc::new(build_module(&exposition.idents, &definitions)?);
        // let imported_modules = fetch_modules(&import.idents)?;

        Ok(Rc::new(Expr::File(File {
            import,
            exposition,
            expr,
            position,
            definitions,
            exposed_module,
            name,
        })))
    }
}

fn build_module(
    expositions: &[Rc<Ident>],
    definitions: &[Rc<Definition>],
) -> Result<Module, Error> {
    let mut env = vec![];
    for exposed in expositions {
        if let Some(found_pair) = definitions.iter().find(|pair| pair.def.0 == exposed.label) {
            env.push((exposed.label.clone(), found_pair.def.1.clone()));
        }
    }
    Ok(Module { env: Rc::new(env) })
}

impl Import {
    fn try_from_input(input: Input<'_>, context: &mut Context) -> Result<Import, Error> {
        let position = Position::from_input(&input);
        let mut idents = vec![];

        let input_ = input.clone();
        for ident in input.into_inner() {
            let ident = Ident::try_from_input(input_.with_pair(&ident))?;
            let _ = context.load_file(&ident)?;
            idents.push(Rc::new(ident.clone()))
        }

        Ok(Import { idents, position })
    }
}

impl Exposition {
    fn try_from_input(input: Input<'_>) -> Result<Exposition, Error> {
        let position = Position::from_input(&input);
        let mut idents = vec![];

        let input_ = input.clone();
        for ident in input.into_inner() {
            let ident = Ident::try_from_input(input_.with_pair(&ident))?;
            idents.push(Rc::new(ident.clone()))
        }

        Ok(Exposition { idents, position })
    }
}

impl Definition {
    fn try_from_input(input: Input<'_>, context: &mut Context) -> Result<Definition, Error> {
        let position = Position::from_input(&input);
        let input_ = input.clone();
        let mut ident_expr_inner = input.into_inner().next().unwrap().into_inner();

        let ident = ident_expr_inner.next().unwrap().as_str().to_string();
        let value = ident_expr_inner.next().unwrap();

        let def = (ident, Expr::from_input(input_.with_pair(&value), context)?);

        Ok(Definition { def, position })
    }

    fn try_many_from_input(
        input: Input<'_>,
        context: &mut Context,
    ) -> Result<Vec<Rc<Definition>>, Error> {
        let mut defintions = vec![];

        let input_ = input.clone();
        for def in input.into_inner() {
            let def = Definition::try_from_input(input_.with_pair(&def), context)?;
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
    fn execute(&self, stack: &mut Scope, context: &Context) -> ExprResult {
        execute(self.expr.clone(), stack, context)
    }
}

impl File {
    pub fn find_exposed(&self, ident: &Ident) -> ExprResult {
        if let Some(definition) = self
            .definitions
            .iter()
            .cloned()
            .find(|definition| *definition.def.0 == ident.label)
        {
            Ok(definition.def.1.clone())
        } else {
            Error::new(
                &format!("Can't find definition of `{}`", ident.to_source()),
                &ident.position,
            )
            .into()
        }
    }
}

fn expr_else_unit(input: Input<'_>, context: &mut Context) -> ExprResult {
    let position = Position::from_input(&input);

    let input_ = input.clone();
    if let Some(ident) = input.into_inner().next() {
        Expr::from_input(input_.with_pair(&ident), context)
    } else {
        Ok(Rc::new(Expr::Const(Const {
            value: ConstType::Unit,
            position,
        })))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::build_in_functions::build_in_functions;
    use crate::execute::{execute_file, execute_sane};
    use crate::parse::parse_file;
    use std::borrow::Borrow;

    #[test]
    fn execute_let_in_2() {
        let result = execute_sane("let a = 1 and let b = 2 in [a; b]")
            .unwrap()
            .to_source();
        assert_eq!(result, "[1.0; 2.0]");
    }

    // Define Prelude and enable dotted names

    #[test]
    fn execute_use_0() {
        let context = &mut Context::new(vec![String::from("./src")]);
        let scope = &mut Scope::new();
        let result = execute_file(
            r#"
        use (module_0)

        let a = 1 and let b = 2 in [a; b]
        
        "#,
            "ADHOC",
            context,
            scope,
        )
        .unwrap()
        .to_source();
        assert_eq!(result, "[1.0; 2.0]");
    }

    #[test]
    fn execute_use_1() {
        let context = &mut Context::new(vec![String::from("./src")]);
        let scope = &mut build_in_functions();
        let result = execute_file(
            r#"
        use (module_0)

        [module_0.a; module_0.b]
        "#,
            "ADHOC",
            context,
            scope,
        )
        .unwrap()
        .to_source();
        assert_eq!(result, "[3.0; 5.0]");
    }

    #[test]
    fn execute_use_2() {
        let context = &mut Context::new(vec![String::from("./src")]);
        let result = parse_file(
            r#"
        use (module_0)

        module_0.a

        "#,
            "ADHOC",
            context,
        )
        .unwrap();
        // dbg!(result);
        // assert_eq!(result, "[1.0; 2.0]");
    }

    #[test]
    fn find_exposed_0() {
        let context = &mut Context::new(vec![]);
        let expr = parse_file(
            r#"
            give (a)

            def a = 1
            def b = fun x => x
        "#,
            "ADHOC",
            context,
        )
        .unwrap();
        let ident = Ident {
            label: String::from("a"),
            position: Position::new(0, 0, "a"),
        };
        let result = match expr.borrow() {
            Expr::File(file) => file.find_exposed(&ident),
            _ => panic!("{:?} is not a File", expr),
        }
        .unwrap();
        assert_eq!(result.to_source(), "1.0");
    }
}
