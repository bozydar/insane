use std::rc::Rc;
use crate::parse::{Expr, Position, ExprResult, Error};
use crate::list::List;
use crate::const_expr::{Const, ConstType};

use crate::execute::Stack;
use crate::build_in::{BuildInFun, BuildIn};

pub fn build_in_functions() -> Stack {
    vec![
        create_build_in("head".to_string(), sane_head, 1),
        create_build_in("tail".to_string(), sane_tail, 1),
        create_build_in("eq".to_string(), sane_eq, 2),
        create_build_in("inc".to_string(), sane_inc, 1),
        create_build_in("count".to_string(), sane_count, 1),
        create_build_in("concat".to_string(), sane_concat, 2),
        create_build_in("add".to_string(), sane_add, 2),
        create_build_in("print".to_string(), sane_print, 1),
        ("true".to_string(), Rc::new(Expr::Const(Const::Bool(true)))),
        ("false".to_string(), Rc::new(Expr::Const(Const::Bool(false)))),
    ]
}

fn create_build_in(name: String, fun: BuildInFun, arity: usize) -> (String, Rc<Expr>) {
    (name.to_string(), Rc::new(Expr::BuildIn(BuildIn { fun, name, arity })))
}

fn sane_head(params: Vec<Rc<Expr>>, position: Position) -> ExprResult {
    let params = validate("head", params, position, vec!["List"])?;
    match &**params.first().unwrap() {
        Expr::List(List { items, position }) => {
            if let Some(item) = items.first() {
                Ok(item.clone())
            } else {
                Error::new("The list is empty", *position).into()
            }
        }
        _ => unreachable!()
    }
}


fn sane_count(params: Vec<Rc<Expr>>, position: Position) -> ExprResult {
    let params = validate("head", params, position, vec!["List"])?;

    match &**params.first().unwrap() {
        Expr::List(List { items, position }) => {
            Ok(Rc::new(Expr::Const(Const { value: ConstType::Numeric(items.len() as f64), position: *position })))
        }
        _ => unreachable!()
    }
}

fn sane_concat(params: Vec<Rc<Expr>>, position: Position) -> ExprResult {
    let params = validate("head", params, position, vec!["List", "List"])?;

    if let (Some(left), Some(right)) = (params.get(0).cloned(), params.get(1).cloned()) {
        let pair = (&*left, &*right);
        match pair {
            (Expr::List(List { items: left_items, .. }), Expr::List(List { items: right_items, .. })) => {
                let mut left_items = left_items.clone();
                let mut right_items = right_items.clone();
                left_items.append(&mut right_items);
                Ok(Rc::new(Expr::List(List { items: left_items, position })))
            }
            _ => unreachable!()
        }
    } else {
        unreachable!()
    }
}

fn sane_tail(params: Vec<Rc<Expr>>, position: Position) -> ExprResult {
    let params = validate("tail", params, position, vec!["List"])?;

    match &**params.first().unwrap() {
        Expr::List(List { items, .. }) => {
            let new_items =
                if items.is_empty() {
                    vec![]
                } else {
                    items[1..].to_vec()
                };
            Ok(Rc::new(Expr::List(List { items: new_items, position })))
        }
        _ => unreachable!()
    }
}


fn sane_eq(params: Vec<Rc<Expr>>, position: Position) -> ExprResult {
    let params = validate("eq", params, position, vec!["Any"])?;

    if let (Some(left), Some(right)) = (params.get(0), params.get(1)) {
        Ok(Rc::new(Expr::Const(Const{ value: ConstType::Bool(left.eq(&right)), position})))
    } else {
        unreachable!()
    }
}

fn sane_add(params: Vec<Rc<Expr>>, position: Position) -> ExprResult {
    let params = validate("add", params, position, vec!["Numeric", "Numeric"])?;

    let left = &*params.get(0).unwrap().clone();
    let right = &*params.get(1).unwrap().clone();
    if let (Expr::Const(Const {value: ConstType::Numeric(left), ..}), Expr::Const(Const {value: ConstType::Numeric(right), ..})) = (left, right) {
        Ok(Rc::new(Expr::Const(Const{ value: ConstType::Numeric(left + right), position: position})))
    } else {
        unreachable!()
    }
}

fn sane_inc(params: Vec<Rc<Expr>>, position: Position) -> ExprResult {
    let params = validate("add", params, position, vec!["Numeric"])?;

    match &**params.first().unwrap() {
        Expr::Const(Const {value: ConstType::Numeric(num), ..}) => {
            Ok(Rc::new(Expr::Const(Const{value: ConstType::Numeric(num + 1.0), position})))
        }
        _ => unreachable!()
    }
}

fn sane_print(params: Vec<Rc<Expr>>, position: Position) -> ExprResult {
    let params = validate("eq", params, position, vec!["Any"])?;

    let param = params.get(0).unwrap().clone();
    println!("{:#?}", *param);
    Ok(param)
}

fn validate(name: &str, params: Vec<Rc<Expr>>, position: Position, types: Vec<&str>) -> Result<Vec<Rc<Expr>>, Error> {
    if params.len() != types.len() {
        return Err(
            Error::new(
                &format!("Function `{}` requires {} params but `{}` provided",
                         name, types.len(), params.len()
                ),
                position,
            )
        );
    }

    for (index, (param, type_)) in params.iter().zip(types).enumerate() {
        let index = index + 1;
        if type_ == "Any" {
            match **param {
                Expr::List(_) | Expr::Const(_) | Expr::Fun(_) => continue,
                _ => return Err(
                    Error::new(
                        &format!("Function `{}` needs `{}` in the {} argument",
                                 name, "Any", index
                        ),
                        position,
                    )
                )
            };
        }
        if type_ == "List" {
            if let Expr::List(_) = **param {
                continue;
            } else {
                return Err(
                    Error::new(
                        &format!("Function `{}` needs `{}` in the {} argument",
                                 name, "List", index
                        ),
                        position,
                    )
                );
            }
        }
        if type_ == "String" {
            if let Expr::Const(Const { value: ConstType::String(_), .. }) = **param {
                continue;
            } else {
                return Err(
                    Error::new(
                        &format!("Function `{}` needs `{}` in the {} argument",
                                 name, "String", index
                        ),
                        position,
                    )
                );
            }
        }
        if type_ == "Numeric" {
            if let Expr::Const(Const { value: ConstType::Numeric(_), .. }) = **param {
                continue;
            } else {
                return Err(
                    Error::new(
                        &format!("Function `{}` needs `{}` in the {} argument",
                                 name, "Numeric", index
                        ),
                        position,
                    )
                );
            }
        }
        if type_ == "Bool" {
            if let Expr::Const(Const { value: ConstType::Bool(_), .. }) = **param {
                continue;
            } else {
                return Err(
                    Error::new(
                        &format!("Function `{}` needs `{}` in the {} argument",
                                 name, "Bool", index
                        ),
                        position,
                    )
                );
            }
        }
        if type_ == "Fun" {
            if let Expr::Fun(_) = **param {
                continue;
            } else {
                return Err(
                    Error::new(
                        &format!("Function `{}` needs `{}` in the {} argument",
                                 name, "Fun", index
                        ),
                        position,
                    )
                );
            }
        }
    }

    Ok(params)
}