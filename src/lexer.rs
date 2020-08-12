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
    STRING(String),
    LBRACKET,
    RBRACKET,
}

pub struct Lexer {
    input: String,
    pos: usize,
    read_pos: usize,
    ch: u8,
}

#[derive(Debug, Clone)]
pub enum LexerError {
    UnknownChar(char),
    UncloseString(String),
}

type Result<T> = std::result::Result<T, LexerError>;

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

    fn next_string(&mut self) -> Result<Token> {
        let mut s = String::new();
        self.read_char();
        while self.ch != b'"' && self.ch != 0 {
            s.push(self.ch as char);
            self.read_char();
        }
        if self.ch == b'"' {
            Ok(Token::STRING(s))
        } else {
            Err(LexerError::UncloseString(s))
        }
    }

    pub fn next_token(&mut self) -> Result<Token> {
        self.skip_whitespaces();

        let result = match self.ch {
            b'=' => if self.peek_char() != b'=' {
                        Ok(Token::ASSIGN)
                    } else {
                        self.read_char();
                        Ok(Token::EQ)
                    }
            b'+' => Ok(Token::PLUS),
            b'(' => Ok(Token::LPAREN),
            b')' => Ok(Token::RPAREN),
            b'{' => Ok(Token::LBRACE),
            b'}' => Ok(Token::RBRACE),
            b',' => Ok(Token::COMMA),
            b';' => Ok(Token::SEMICOLON),
            b'-' => Ok(Token::MINUS),
            b'/' => Ok(Token::SLASH),
            b'*' => Ok(Token::ASTERISK),
            b'<' => Ok(Token::LT),
            b'>' => Ok(Token::GT),
            b'!' => if self.peek_char() != b'=' {
                        Ok(Token::BANG)
                    } else {
                        self.read_char();
                        Ok(Token::NE)
                    }
            b'a'..= b'z' | b'A'..= b'Z'=> Ok(self.next_identifier_or_keyword()),
            b'0'..= b'9' => Ok(self.next_int()),
            b'"' => self.next_string(),
            b'[' => Ok(Token::LBRACKET),
            b']' => Ok(Token::RBRACKET),
            0 => Ok(Token::EOF),
            _ => Err(LexerError::UnknownChar(self.ch as char)),
        };
        if result.is_ok() {
            self.read_char();
        }
        result
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
            let result = lexer.next_token();
            assert!(result.is_ok(), "unexpected lexer error: {:?}", result.unwrap_err());
            let token = result.unwrap();
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
            \"foo\"
            \"\"
            []
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
            Token::STRING(String::from("foo")),
            Token::STRING(String::from("")),
            Token::LBRACKET,
            Token::RBRACKET,
            Token::EOF,
        ];

        for expected_token in expected {
            let result = lexer.next_token();
            assert!(result.is_ok(), "unexpected lexer error: {:?}", result.unwrap_err());
            let token = result.unwrap();
            assert_eq!(token, expected_token);
        }
    }

    #[test]
    fn test_unknown_token() {
        let mut lexer = Lexer::new(String::from("?"));
        let result = lexer.next_token();
        assert!(result.is_err(), "result should be an error, got: {:?}", result.unwrap());
        assert_matches!(result.unwrap_err(), LexerError::UnknownChar(_));
    }
}
