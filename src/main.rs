mod lexer;
mod parser;
mod ast;

use crate::lexer::Lexer;
use crate::lexer::Token;
use std::io::{self, Write};

const PROMPT: &str = ">> ";

fn main() {
    loop {
        print!("{}", PROMPT);
        io::stdout().flush().unwrap();
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_) => {
                let mut lexer = Lexer::new(input.clone());
                loop {
                    let token = lexer.next_token();
                    if token == Token::EOF {
                        break;
                    }
                    println!("{:?}", token);
                }
            },
            Err(error) => println!("error reading: {}", error),
        }
    }
}
