use crate::lexer::{Token, Lexer};
use crate::ast::*;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
}

#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedToken { expected: Token, actual: Token },
    NotIdentifier(Token),
}

type Result<T> = std::result::Result<T, ParserError>;

impl Parser {
    fn new(mut lexer: Lexer) -> Parser {
        let first_token = lexer.next_token();
        Parser {
            lexer,
            cur_token: first_token,
        }
    }

    fn parse_program(&mut self) -> Result<Program> {
        let mut statements = Vec::new();
        while self.cur_token != Token::EOF {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        Ok(
            Program {
                statements,
            }
        )
    }

    fn parse_statement(&mut self) -> Result<Statement> {
        self.parse_let_statement()
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        self.expect_peak(Token::LET)?;
        let identifier = self.parse_identifier()?;
        self.expect_peak(Token::ASSIGN);
        let value = self.parse_expression()?;
        self.expect_peak(Token::SEMICOLON);
        Ok(
            Statement::Let {
                identifier,
                value,
            }
        )
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        match self.cur_token {
            Token::IDENT(_) => Ok(Expression::Identifier(self.pop_token())),
            _ => Err(ParserError::NotIdentifier(self.cur_token.clone())),
        }
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        self.next_token();
        Ok(Expression::Dummy)
    }

    fn expect_peak(&mut self, expected: Token) -> Result<()> {
        if self.cur_token != expected {
            return Err(ParserError::UnexpectedToken{ expected, actual: self.cur_token.clone() });
        }
        self.next_token();
        Ok(())
    }

    fn next_token(&mut self) {
        self.cur_token = self.lexer.next_token();
    }

    fn pop_token(&mut self) -> Token {
        let token = self.cur_token.clone();
        self.next_token();
        token
    }
}

#[cfg(test)]
mod tests {
    use crate::parser::*;

    #[test]
    fn test_let_statement() {
        let input = String::from("
            let x = 5;
            let y = 10;
            let foobar = 838383;
        ");
        let expected = vec![
            Statement::Let {
                identifier: Expression::Identifier(Token::IDENT(String::from("x"))),
                value: Expression::Dummy,
            },
            Statement::Let {
                identifier: Expression::Identifier(Token::IDENT(String::from("y"))),
                value: Expression::Dummy,
            },
            Statement::Let {
                identifier: Expression::Identifier(Token::IDENT(String::from("foobar"))),
                value: Expression::Dummy,
            },
        ];
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse_program();

        assert_eq!(parse_result.is_ok(), true);
        if let Ok(program) = parse_result {
            for (x, y) in program.statements.iter().zip(expected.iter()) {
                assert_eq!(x, y);
            }
        }
    }
}