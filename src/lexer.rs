#[derive(Debug)]
pub struct Token {
    token_type: String,
    literal: String,
}

impl Token {
    pub fn new(token_type: String, literal: String) -> Token {
        Token { 
            token_type,
            literal,
        }
    }
}

