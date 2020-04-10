extern crate pest;
#[macro_use]
extern crate pest_derive;
#[macro_use]
extern crate lazy_static;

mod error;
pub mod parse;
mod let_in;
mod ident;
mod const_expr;
mod bind;
mod fun;
mod list;
mod if_then_else;
mod build_in;
mod binary;
pub mod execute;
pub mod build_in_functions;
