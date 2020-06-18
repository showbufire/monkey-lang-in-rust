const ASSIGN: &'static str = "ASSIGN";
const PLUS: &'static str = "PLUS";
const EOF: &'static str = "EOF";

#[derive(Debug, PartialEq)]
pub struct Token {
    token_type: &'static str,
    literal: String,
}

impl Token {
    pub fn new(token_type: &'static str, literal: String) -> Token {
        Token { 
            token_type,
            literal,
        }
    }
}

pub struct Lexer {
    input: String,    
    pos: usize,
    read_pos: usize,
    ch: u8,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let mut lexer = Lexer {
            input: input,
            pos: 0,
            read_pos: 0,
            ch: 0,
        };
        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) -> u8 {
        if self.read_pos >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_pos];
            self.pos = self.read_pos;
            self.read_pos += 1;
        }
        self.ch
    }

    pub fn next_token(&mut self) -> Token {
        let token = match self.ch {
            b'=' => Token::new(ASSIGN, String::from("=")),
            b'+' => Token::new(PLUS, String::from("+")),
            0 => Token::new(EOF, String::from("")),
            _ => panic!("unknown char {}", self.ch),
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    #[test]
    fn test_lexer() {
        let input = String::from("=+");
        let mut lexer = Lexer::new(input);

        let expected = vec![
            Token::new(ASSIGN, String::from("=")),
            Token::new(PLUS, String::from("+")),
            Token::new(EOF, String::from("")),
        ];

        for expected_token in expected {
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }
    }
}
