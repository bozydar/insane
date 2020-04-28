extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

pub mod parse;
pub mod execute;
pub mod build_in_functions;

mod error;
mod let_in;
mod ident;
mod const_expr;
mod bind;
mod fun;
mod list;
mod if_then_else;
mod build_in;
mod binary;
mod file;
pub mod context;
