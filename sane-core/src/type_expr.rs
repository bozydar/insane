use std::rc::Rc;
use std::collections::HashMap;
use crate::parse::Position;

#[derive(Debug, PartialEq, Clone)]
pub enum TypeExpr {
    Ident(Ident),
    Bind(Bind),
    Tuple(Tuple),
    Record(Record),
    Union(Union)
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ident {
    pub label: String,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Bind {
    pub arg: Rc<TypeExpr>,
    pub fun: Rc<TypeExpr>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Tuple {
    pub items: Vec<Rc<TypeExpr>>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Record {
    pub fields: HashMap<String, Rc<TypeExpr>>,
    pub position: Position,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Union {
    pub fields: HashMap<String, Rc<TypeExpr>>,
    pub position: Position,
}
