extern crate sane_core;
extern crate clap;

use clap::{Arg, App};
use std::fs::File;
use std::io::prelude::*;
use std::result::Result;

use sane_core::execute;
use sane_core::parse::{ToSource, Selection, Position};

fn main() {
    let matches = App::new("sane programming language")
        .version("0.0.1")
        .author("Bozydar Sobczak <bozysob@gmail.com>")
        .about("Very simple functional programming language")
        .arg(
            Arg::with_name("input_file")
            .short('r')
            .takes_value(true)
            .help("File to interpret")
        )
        .get_matches();
    
    
    if let Some(o) = matches.value_of("input_file") {
        match run(o) {
            Ok(result) => println!("{}", result),
            Err(err) => {
                eprintln!("{}", err);
                std::process::exit(1);
            }
        }
    }
}

fn run(file_path: &str) -> Result<String, String> {
    execute_string(&read_content(file_path)?, file_path)
}

fn execute_string(content: &str, source: &str) -> Result<String, String> {
    let result = execute::execute_file(&content, source);
    match result {
        Ok(expr) => Ok(expr.to_source()),
        Err(err) => {
            if let Some(position) = err.backtrace.last() {
                let selection = Selection::from_content(content, position);
                let backtrace = err.backtrace.iter()
                    .map(|position| {
                        let selection = Selection::from_content(content, position);
                        format!("{}:{}:{}", selection.source, selection.start.0, selection.start.1)
                    })
                    .collect::<Vec<String>>()
                    .join("\n");
                Err(format!("Error: {} at {}:{}:{}\n{}", err.message, selection.source, selection.start.0, 
                selection.start.1, backtrace))
            } else {
                Err(format!("Error: {}", err.message))
            }
        }
    }
}

fn read_content(file_path: &str) -> Result<String, String> {
    match File::open(file_path) {
        Ok(mut file) => {
            let mut content = String::new();
            match file.read_to_string(&mut content) {
                Ok(_) => Ok(content),
                Err(error) => Err(format!("{}", error))
            }
        },
        Err(error) => Err(format!("{}", error))
    }
}
