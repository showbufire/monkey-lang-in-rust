mod lexer;
mod parser;
mod ast;
mod evaluator;

use crate::lexer::Lexer;
use crate::parser::Parser;
use std::io::{self, Write};

const PROMPT: &str = ">> ";

fn main() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let lexer = Lexer::new(input.clone());
                let mut parser = Parser::new(lexer);
                match parser.parse_program() {
                    Ok(program) => println!("{:?}", program),
                    Err(err) => println!("{:?}", err),
                }
            },
            Err(error) => println!("error reading: {}", error),
        }
    }
}
