#[derive(Debug, PartialEq)]
pub enum Token {
    ASSIGN,
    PLUS,
    EOF,
    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,
    COMMA,
    SEMICOLON,
    LET,
    FUNCTION,
    IDENT(String),
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
            b'=' => Token::ASSIGN,
            b'+' => Token::PLUS,
            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,
            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,
            b',' => Token::COMMA,
            b';' => Token::SEMICOLON,
            0 => Token::EOF,
            _ => panic!("unknown char {}", String::from_utf8(vec![self.ch]).unwrap()),
        };
        self.read_char();
        token
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::*;

    #[test]
    fn test_next_token() {
        let input = String::from("=+(){},;");
        let mut lexer = Lexer::new(input);

        let expected = vec![
            Token::ASSIGN,
            Token::PLUS,
            Token::LPAREN,
            Token::RPAREN,
            Token::LBRACE,
            Token::RBRACE,
            Token::COMMA,
            Token::SEMICOLON,
            Token::EOF,
        ];

        for expected_token in expected {
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }
    }

    #[test]
    #[ignore]
    fn test_next_token_with_ident() {
        let input = String::from("let five = 5;
        let ten = 10;
        let add = fn(x, y) { 
            x + y;
        };
        let result = add(five, ten);");
        let mut lexer = Lexer::new(input);

        let expected = vec![
        ];

        for expected_token in expected {
            let token = lexer.next_token();
            assert_eq!(token, expected_token);
        }
    }
}
