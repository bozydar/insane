extern crate pest;
#[macro_use]
extern crate pest_derive;

use std::fs;
use std::env;

use pest::Parser;

#[derive(Parser)]
#[grammar = "csv.pest"]
pub struct CSVParser;

fn main() {
    let args: Vec<String> = env::args().collect();
    let file_name = args.get(1).unwrap();

    let unparsed_file = fs::read_to_string(&file_name).expect(format!("Can't read {}", file_name).as_str());
    let file = CSVParser::parse(Rule::file, &unparsed_file)
        .expect("Can't parse")
        .next()
        .unwrap();

    let mut field_sum: f64 = 0.0;
    let mut record_count: u64 = 0;
    let mut strings: Vec<String> = vec![];

    for record in file.into_inner() {
        match record.as_rule() {
            Rule::record => {
                record_count += 1;

                for field in record.into_inner() {
                    match field.as_rule() {
                        Rule::field => {
                            for field_type in field.into_inner() {
                                match field_type.as_rule() {
                                    Rule::quoted_field => {
                                        let inner = field_type.into_inner().nth(0).expect("Problem with the first element");
                                        strings.push(inner.as_str().to_string());
                                    }
                                    Rule::unquoted_field => {
                                        field_sum += field_type.as_str().parse::<f64>().unwrap();
                                    }
                                    _ => ()
                                }
                            }
                        }
                        _ => {
                            ()
                        }
                    }
                }
            }
            Rule::EOI => (),
            _ => unreachable!(),
        }
    }

    println!("strings: {:?}", strings);
    println!("Sum: {}", field_sum);
    println!("Count: {}", record_count);

    let successful_parse = CSVParser::parse(Rule::field, "\"-273.15\"");
    println!("{:?}", successful_parse);

    let unsuccessful_parse = CSVParser::parse(Rule::field, "this is not a number");
    println!("{:?}", unsuccessful_parse);
}
