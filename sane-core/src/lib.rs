extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

pub mod build_in_functions;
pub mod execute;
pub mod parse;

mod binary;
mod bind;
mod build_in;
mod const_expr;
pub mod context;
mod error;
mod file;
mod fun;
mod ident;
mod if_then_else;
mod let_in;
mod list;
mod ns_ident;
mod type_expr;
