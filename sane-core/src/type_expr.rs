use std::rc::Rc;
use std::collections::HashMap;
use crate::parse::{Position, Selection};

#[derive(Debug, PartialEq, Clone)]
pub enum LiteralType {
    String,
    Numeric,
    Bool,
    Unit,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
    Variable(Variable), // Ident
    Literal(Literal), // Const
    Abstraction(Abstraction), // Fun
    Application(Application), // Bind
}

#[derive(Debug, PartialEq, Clone)]
pub struct Variable {
    pub label: String,
}

impl Variable {
    pub fn new(label: String) -> Self {
        Variable { label }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Literal {
    pub literal_type: LiteralType,  // label can be String, Numeric, Bool, Unit
}

#[derive(Debug, PartialEq, Clone)]
pub struct Abstraction {
    pub param_name: String,
    pub result: Rc<TypeExpr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Application {
    pub function: Rc<TypeExpr>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Tuple {
    pub items: Vec<Rc<TypeExpr>>,
    pub position: Selection,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Record {
    pub fields: HashMap<String, Rc<TypeExpr>>,
    pub position: Selection,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Union {
    pub fields: HashMap<String, Rc<TypeExpr>>,
    pub position: Selection,
}
