extern crate pest;
#[macro_use]
extern crate pest_derive;


use pest::Parser;
use pest::iterators::{Pair, Pairs};

fn main() {

}

#[derive(Parser)]
#[grammar = "sane.pest"]
pub struct SaneParser;

#[derive(Debug, PartialEq)]
struct LetIn {
    pub var: String,
    pub value: Box<Expr>,
    pub in_part: Box<Expr>,
}

#[derive(Debug, PartialEq)]
enum Const {
    String(String),
    Numeric(f64)
}

#[derive(Debug, PartialEq)]
enum Expr {
    LetIn(LetIn),
    Const(Const),
    Ident(String)
}

fn parse_const(pair: Pair<'_, Rule>) -> Result<Expr, &str> {
    let pair = pair.into_inner().next().unwrap();
    match pair.as_rule() {
        Rule::string => Ok(Expr::Const(Const::String(pair.as_str().to_string()))),
        Rule::number => {
            let value = pair.as_str().parse::<f64>().unwrap();
            Ok(Expr::Const(Const::Numeric(value)))
        }
        _ => Err("Unknown const type")
    }
}

fn parse_let_in(pair: Pair<'_, Rule>) -> Result<Expr, &str> {
    let mut inner: Pairs<'_, Rule> = pair.into_inner();
    let ident = inner.next().unwrap();
    let value = inner.next().unwrap();
    let in_part = inner.next().unwrap();

    Ok(Expr::LetIn(LetIn{
        var: ident.as_str().to_string(),
        value: Box::new(parse_pair(value)?),
        in_part: Box::new(parse_pair(in_part)?)
    }))
}

fn parse_ident(pair: Pair<'_, Rule>) -> Result<Expr, &str> {
    Ok(Expr::Ident(pair.as_str().to_string()))
}

fn parse_pair(pair: Pair<'_, Rule>) -> Result<Expr, &str> {
    let rule = pair.as_rule();
    match rule {
        Rule::constant => parse_const(pair),
        Rule::let_in => parse_let_in(pair),
        Rule::ident => parse_ident(pair),
        _ => {
            println!("--------------------Unknown rule {:?}", rule);
            Err("Unknown rule")
        }
    }
}

fn parse_sane(input: &str) -> Result<Expr, &str> {
    let parsed = SaneParser::parse(Rule::expr, input)
        .expect("Can't parse")
        .next()
        .unwrap();

    // let result: Option<pest::iterators::Pair<'_, Rule>> = parsed;
    println!("-----------{:?}", parsed);

    parse_pair(parsed)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_number() {
        let result = parse_sane("-23.1");
        assert_eq!(result, Ok(Expr::Const(Const::Numeric(-23.1))));
    }

    #[test]
    fn test_parse_string() {
        let result = parse_sane("\"test\"").unwrap();
        assert_eq!(result, Expr::Const(Const::String("test".to_string())));
    }

    #[test]
    fn parse_let_in() {
        let result = parse_sane("let a = 1 in a").unwrap();
        assert_eq!(result, Expr::LetIn(LetIn { var: "a".to_string(), value: Box::new(Expr::Const(Const::Numeric(1.0))), in_part: Box::new(Expr::Ident("a".to_string())) }));
    }
}
