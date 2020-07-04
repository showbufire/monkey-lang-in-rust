use crate::lexer::{Token, Lexer};
use crate::ast::*;

#[allow(dead_code)]
pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub enum ParserError {
    UnexpectedToken { expected: Token, actual: Token },
    NotIdentifier(Token),
    Unimplemented(Token),
}

#[allow(dead_code)]
type Result<T> = std::result::Result<T, ParserError>;

#[allow(dead_code)]
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
        match self.cur_token {
            Token::LET => self.parse_let_statement(),
            Token::RETURN => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_expression_statement(&mut self) -> Result<Statement> {
        let expr = self.parse_expression()?;
        self.expect_peak(Token::SEMICOLON)?;
        Ok(Statement::Expr{ expr })
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.expect_peak(Token::RETURN)?;
        let value = self.parse_expression()?;
        self.expect_peak(Token::SEMICOLON)?;
        Ok(Statement::Return{ value })
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        self.expect_peak(Token::LET)?;
        let identifier = self.parse_identifier()?;
        self.expect_peak(Token::ASSIGN)?;
        let value = self.parse_expression()?;
        self.expect_peak(Token::SEMICOLON)?;
        Ok(
            Statement::Let {
                identifier,
                value,
            }
        )
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        let res = match &self.cur_token {
            Token::IDENT(name) => Ok(Expression::Identifier{ name: name.clone() }),
            _ => Err(ParserError::NotIdentifier(self.cur_token.clone())),
        };
        if res.is_ok() {
            self.next_token();
        }
        res
    }

    fn parse_expression(&mut self) -> Result<Expression> {
        while self.cur_token != Token::SEMICOLON && self.cur_token != Token::EOF {
            self.next_token();
        }
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
                identifier: Expression::Identifier{ name: String::from("x") },
                value: Expression::Dummy,
            },
            Statement::Let {
                identifier: Expression::Identifier{ name: String::from("y") },
                value: Expression::Dummy,
            },
            Statement::Let {
                identifier: Expression::Identifier{ name: String::from("foobar") },
                value: Expression::Dummy,
            },
        ];
        test_helper(input, expected);
    }

    #[test]
    fn test_return_statement() {
        let input = String::from("
            return x;
            return foobar;
            return 5;
        ");

        let expected = vec![
            Statement::Return { value: Expression::Dummy },
            Statement::Return { value: Expression::Dummy },
        ];
        test_helper(input, expected);
    }

    fn test_helper(input: String, expected: Vec<Statement>) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse_program();

        assert_eq!(parse_result.is_ok(), true, "err: {:?}", parse_result.unwrap_err());
        if let Ok(program) = parse_result {
            for (x, y) in program.statements.iter().zip(expected.iter()) {
                assert_eq!(x, y);
            }
        }
    }

    #[test]
    fn test_expression_statement() {
        let input = String::from("
            x + 3;
            foobar;
        ");
        let expected = vec![
            Statement::Expr { expr: Expression::Dummy },
            Statement::Expr { expr: Expression::Dummy },
        ];
        test_helper(input, expected);
    }
}