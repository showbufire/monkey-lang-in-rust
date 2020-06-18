mod lexer;

use crate::lexer::Token;

fn main() {
    let t = Token::new("foo", "bar".to_string());

    println!("{:?}", t);
}
