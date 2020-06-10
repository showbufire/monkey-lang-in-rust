mod lexer;

use crate::lexer::Token;

fn main() {
    let t = Token::new("foo".to_string(), "bar".to_string());

    println!("{:?}", t);
}
