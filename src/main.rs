mod lexer;
mod parser;
mod ast;
mod evaluator;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::evaluator::{eval_with_env,Env};
use std::io::{self, Write};

const PROMPT: &str = ">> ";

fn main() {
    let env = Env::new();
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let lexer = Lexer::new(input);
                let mut parser = Parser::new(lexer);
                match parser.parse_program() {
                    Ok(program) => match eval_with_env(&program, &env) {
                        Ok(obj) => println!("{:?}", obj),
                        Err(err) => println!("Eval error: {:?}", err),
                    },
                    Err(err) => println!("Parsing error: {:?}", err),
                }
            },
            Err(error) => println!("error reading: {}", error),
        }
    }
}
