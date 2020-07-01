const LET: &str = "let";
const FUNCTION: &str = "fn";
const IF: &str = "if";
const ELSE: &str = "else";
const TRUE: &str = "true";
const FALSE: &str = "false";
const RETURN: &str = "return";

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    ASSIGN,
    ASTERISK,
    BANG,
    COMMA,
    ELSE,
    EOF,
    EQ,
    FALSE,
    FUNCTION,
    GT,
    IDENT(String),
    IF,
    INT(i64),
    LBRACE,
    LET,
    LPAREN,
    LT,
    MINUS,
    NE,
    PLUS,
    RBRACE,
    RETURN,
    RPAREN,
    SEMICOLON,
    SLASH,
    TRUE,
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

    fn peek_char(&self) -> u8 {
        if self.read_pos >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_pos]
        }
    }

    fn next_identifier_or_keyword(&mut self) -> Token {
        let mut bytes = vec![];
        loop {
            bytes.push(self.ch);
            if self.peek_char().is_ascii_alphabetic() {
                self.read_char();
            } else {
                break;
            }
        }
        let ident_or_keyword = String::from_utf8(bytes).unwrap();
        match ident_or_keyword.as_str() {
            LET => Token::LET,
            FUNCTION => Token::FUNCTION,
            IF => Token::IF,
            ELSE => Token::ELSE,
            RETURN => Token::RETURN,
            TRUE => Token::TRUE,
            FALSE => Token::FALSE,
            _ => Token::IDENT(ident_or_keyword),
        }
    }

    fn next_int(&mut self) -> Token {
        let mut x: i64 = 0;
        loop {
            x = x * 10 + (self.ch - b'0') as i64;
            if self.peek_char().is_ascii_digit() {
                self.read_char();
            } else {
                break;
            }
        }
        Token::INT(x)
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespaces();

        let token = match self.ch {
            b'=' => if self.peek_char() != b'=' {
                        Token::ASSIGN
                    } else {
                        self.read_char();
                        Token::EQ
                    }
            b'+' => Token::PLUS,
            b'(' => Token::LPAREN,
            b')' => Token::RPAREN,
            b'{' => Token::LBRACE,
            b'}' => Token::RBRACE,
            b',' => Token::COMMA,
            b';' => Token::SEMICOLON,
            b'-' => Token::MINUS,
            b'/' => Token::SLASH,
            b'*' => Token::ASTERISK,
            b'<' => Token::LT,
            b'>' => Token::GT,
            b'!' => if self.peek_char() != b'=' {
                        Token::BANG
                    } else {
                        self.read_char();
                        Token::NE
                    }
            b'a'..= b'z' | b'A'..= b'Z'=> self.next_identifier_or_keyword(),
            b'0'..= b'9' => self.next_int(),
            0 => Token::EOF,
            _ => panic!("unknown char {}", String::from_utf8(vec![self.ch]).unwrap()),
        };
        self.read_char();
        token
    }

    fn skip_whitespaces(&mut self) {
        loop {
            match self.ch {
                b' ' | b'\t' | b'\n' => (),
                _ => break,
            }
            self.read_char();
        }
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
    fn test_next_token_with_ident() {
        let input = String::from("
            let five = 5;
            let ten = 10;
            let add = fn(x, y) {
                x + y;
            };
            let result = add(five, ten);
            !-/*5;
            5 < 10 > 5;
            if (5 < 10) {
                return true;
            } else {
                return false;
            }

            10 == 10;
            10 != 9;
        ");
        let mut lexer = Lexer::new(input);

        let expected = vec![
            Token::LET,
            Token::IDENT(String::from("five")),
            Token::ASSIGN,
            Token::INT(5),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("ten")),
            Token::ASSIGN,
            Token::INT(10),
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("add")),
            Token::ASSIGN,
            Token::FUNCTION,
            Token::LPAREN,
            Token::IDENT(String::from("x")),
            Token::COMMA,
            Token::IDENT(String::from("y")),
            Token::RPAREN,
            Token::LBRACE,
            Token::IDENT(String::from("x")),
            Token::PLUS,
            Token::IDENT(String::from("y")),
            Token::SEMICOLON,
            Token::RBRACE,
            Token::SEMICOLON,
            Token::LET,
            Token::IDENT(String::from("result")),
            Token::ASSIGN,
            Token::IDENT(String::from("add")),
            Token::LPAREN,
            Token::IDENT(String::from("five")),
            Token::COMMA,
            Token::IDENT(String::from("ten")),
            Token::RPAREN,
            Token::SEMICOLON,
            Token::BANG,
            Token::MINUS,
            Token::SLASH,
            Token::ASTERISK,
            Token::INT(5),
            Token::SEMICOLON,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::GT,
            Token::INT(5),
            Token::SEMICOLON,
            Token::IF,
            Token::LPAREN,
            Token::INT(5),
            Token::LT,
            Token::INT(10),
            Token::RPAREN,
            Token::LBRACE,
            Token::RETURN,
            Token::TRUE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::ELSE,
            Token::LBRACE,
            Token::RETURN,
            Token::FALSE,
            Token::SEMICOLON,
            Token::RBRACE,
            Token::INT(10),
            Token::EQ,
            Token::INT(10),
            Token::SEMICOLON,
            Token::INT(10),
            Token::NE,
            Token::INT(9),
            Token::SEMICOLON,
            Token::EOF,
        ];

        for expected_token in expected {
            let token = lexer.next_token();
            println!("{:?} {}", token, String::from_utf8(vec![lexer.ch]).unwrap());
            assert_eq!(token, expected_token);
        }
    }
}
