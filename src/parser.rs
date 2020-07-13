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
    NotLeft(Token),
    NotIdentifier(Token),
    NotInt(Token),
    NotBool(Token),
    NotPrefixOp(Token),
}

#[allow(dead_code)]
type Result<T> = std::result::Result<T, ParserError>;

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    LOWEST,
    EQ,
    PLUS,
    MULT,
    PREFIX,
}

fn op_precedence(token: &Token) -> Precedence {
    match token {
        Token::EQ | Token::LT | Token::GT | Token::NE => Precedence::EQ,
        Token::PLUS | Token::MINUS => Precedence::PLUS,
        Token::ASTERISK | Token::SLASH => Precedence::MULT,
        _ => Precedence::LOWEST,
    }
}

fn is_infix_op(token: &Token) -> bool {
    match token {
        Token::EQ | Token::LT | Token::GT | Token::NE => true,
        Token::PLUS | Token::MINUS => true,
        Token::ASTERISK | Token::SLASH => true,
        _ => false,
    }
}

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
        let expr = self.parse_expression(Precedence::LOWEST)?;
        self.expect_token(Token::SEMICOLON)?;
        Ok(Statement::Expr{ expr })
    }

    fn parse_return_statement(&mut self) -> Result<Statement> {
        self.expect_token(Token::RETURN)?;
        let value = self.parse_expression(Precedence::LOWEST)?;
        self.expect_token(Token::SEMICOLON)?;
        Ok(Statement::Return{ value })
    }

    fn parse_let_statement(&mut self) -> Result<Statement> {
        self.expect_token(Token::LET)?;
        let identifier = self.parse_identifier()?;
        self.expect_token(Token::ASSIGN)?;
        let value = self.parse_expression(Precedence::LOWEST)?;
        self.expect_token(Token::SEMICOLON)?;
        Ok(
            Statement::Let {
                identifier,
                value,
            }
        )
    }

    fn parse_identifier(&mut self) -> Result<Expression> {
        let result = if let Token::IDENT(name) = &self.cur_token {
            Ok(Expression::Identifier{ name: name.clone() })
        } else {
            Err(ParserError::NotIdentifier(self.cur_token.clone()))
        };
        if result.is_ok() {
            self.next_token();
        }
        result
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let mut left = match self.cur_token {
            Token::IDENT(_) => self.parse_identifier()?,
            Token::INT(_) => self.parse_int()?,
            Token::TRUE | Token::FALSE => self.parse_bool()?,
            Token::LPAREN => self.parse_grouped_expression()?,
            Token::BANG | Token::MINUS => self.parse_prefix_expression()?,
            Token::IF => self.parse_if_expression()?,
            _ => return Err(ParserError::NotLeft(self.cur_token.clone())),
        };
        while is_infix_op(&self.cur_token) && precedence < op_precedence(&self.cur_token) {
            let op = self.pop_token();
            let right = self.parse_expression(op_precedence(&op))?;
            left = Expression::Infix {
                op,
                left: Box::new(left),
                right: Box::new(right),
            }
        }
        Ok(left)
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        self.expect_token(Token::IF)?;
        self.expect_token(Token::LPAREN)?;
        let condition = self.parse_expression(Precedence::LOWEST)?;
        self.expect_token(Token::RPAREN)?;
        let consequence = self.parse_block_statement()?;
        let mut alternative = None;
        if self.cur_token == Token::ELSE {
            self.next_token();
            alternative = Some(Box::new(self.parse_block_statement()?));
        }
        Ok(Expression::If {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        })
    }

    fn parse_block_statement(&mut self) -> Result<Statement> {
        self.expect_token(Token::LBRACE)?;
        let mut statements = Vec::new();
        while self.cur_token != Token::EOF && self.cur_token != Token::RBRACE {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        self.expect_token(Token::RBRACE)?;
        Ok(Statement::Block {
            statements,
        })
    }

    fn parse_bool(&mut self) -> Result<Expression> {
        let res = match self.cur_token {
            Token::TRUE => Ok(Expression::Bool { value: true }),
            Token::FALSE => Ok(Expression::Bool { value: false }),
            _ => Err(ParserError::NotBool(self.cur_token.clone())),
        };
        if res.is_ok() {
            self.next_token();
        }
        res
    }

    fn parse_grouped_expression(&mut self) -> Result<Expression> {
        self.expect_token(Token::LPAREN)?;
        let expr = self.parse_expression(Precedence::LOWEST)?;
        self.expect_token(Token::RPAREN)?;
        Ok(expr)
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression> {
        match self.cur_token {
            Token::BANG | Token::MINUS => {
                let op = self.pop_token();
                let expr = self.parse_expression(Precedence::PREFIX)?;
                Ok(Expression::Prefix { op, expr: Box::new(expr)})
            },
            _ => Err(ParserError::NotPrefixOp(self.cur_token.clone())),
        }
    }

    fn parse_int(&mut self) -> Result<Expression> {
        if let Token::INT(value) = self.cur_token {
            self.next_token();
            Ok(Expression::Int{ value })
        } else {
            Err(ParserError::NotInt(self.cur_token.clone()))
        }
    }

    fn expect_token(&mut self, expected: Token) -> Result<()> {
        if self.cur_token != expected {
            return Err(ParserError::UnexpectedToken{ expected, actual: self.cur_token.clone() });
        }
        self.next_token();
        Ok(())
    }

    fn pop_token(&mut self) -> Token {
        let token = self.cur_token.clone();
        self.next_token();
        token
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
                value: Expression::Int { value: 5 },
            },
            Statement::Let {
                identifier: Expression::Identifier{ name: String::from("y") },
                value: Expression::Int { value: 10 },
            },
            Statement::Let {
                identifier: Expression::Identifier{ name: String::from("foobar") },
                value: Expression::Int { value: 838383 },
            },
        ];
        test_helper(input, expected);
    }

    #[test]
    fn test_return_statement() {
        let input = String::from("
            return x;
            return 5;
        ");

        let expected = vec![
            Statement::Return { value: Expression::Identifier { name: String::from("x") } },
            Statement::Return { value: Expression::Int { value: 5 } },
        ];
        test_helper(input, expected);
    }

    fn test_helper(input: String, expected: Vec<Statement>) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse_program();

        assert_eq!(parse_result.is_ok(), true, "err: {:?}", parse_result.unwrap_err());
        if let Ok(program) = parse_result {
            assert_eq!(program.statements.len(), expected.len());
            for (x, y) in program.statements.iter().zip(expected.iter()) {
                assert_eq!(x, y);
            }
        }
    }

    #[test]
    fn test_expression_statement() {
        let input = String::from("
            10;
            foobar;
            !x;
            -5;
            1 + 2;
            1 + 2 * 3;
            1 + 2 == 3;
            2 * ( 3 + 4 );
            1 / ( 2 * (3 + 4) );
            true == false;
        ");
        let expected = vec![
            Statement::Expr { expr: Expression::Int { value: 10 } },
            Statement::Expr { expr: Expression::Identifier { name: String::from("foobar") } },
            Statement::Expr { expr: Expression::Prefix {
                op: Token::BANG,
                expr: Box::new(Expression::Identifier { name: String::from("x")}),
            }},
            Statement::Expr { expr: Expression::Prefix {
                op: Token::MINUS,
                expr: Box::new(Expression::Int { value: 5 }),
            }},
            Statement::Expr { expr: Expression::Infix {
                op: Token::PLUS,
                left: Box::new(Expression::Int { value: 1 }),
                right: Box::new(Expression::Int { value: 2 }),
            }},
            Statement::Expr { expr: Expression::Infix {
                op: Token::PLUS,
                left: Box::new(Expression::Int { value: 1 }),
                right: Box::new(Expression::Infix {
                    op: Token::ASTERISK,
                    left: Box::new(Expression::Int { value: 2 }),
                    right: Box::new(Expression::Int { value: 3 }),
                }),
            }},
            Statement::Expr { expr: Expression::Infix {
                op: Token::EQ,
                left: Box::new(Expression::Infix {
                    op: Token::PLUS,
                    left: Box::new(Expression::Int { value: 1 }),
                    right: Box::new(Expression::Int { value: 2 }),
                }),
                right: Box::new(Expression::Int { value: 3 }),
            }},
            Statement::Expr { expr: Expression::Infix {
                op: Token::ASTERISK,
                left: Box::new(Expression::Int { value: 2 }),
                right: Box::new(Expression::Infix {
                    op: Token::PLUS,
                    left: Box::new(Expression::Int { value: 3 }),
                    right: Box::new(Expression::Int { value: 4 }),
                }),
            }},
            Statement::Expr { expr: Expression::Infix {
                op: Token::SLASH,
                left: Box::new(Expression::Int { value: 1 }),
                right: Box::new(Expression::Infix {
                    op: Token::ASTERISK,
                    left: Box::new(Expression::Int { value: 2 }),
                    right: Box::new(Expression::Infix {
                        op: Token::PLUS,
                        left: Box::new(Expression::Int { value: 3 }),
                        right: Box::new(Expression::Int { value: 4 }),
                    }),
                }),
            }},
            Statement::Expr { expr: Expression::Infix {
                op: Token::EQ,
                left: Box::new(Expression::Bool { value: true }),
                right: Box::new(Expression::Bool { value: false }),
            }},
        ];
        test_helper(input, expected);
    }

    #[test]
    fn test_if_expression() {
        let input = String::from("
            let z = if (x < y) {
                x;
            } else {
                y;
            };
            if (x < y) {
                return x;
            };
        ");

        let expected = vec![
            Statement::Let {
                identifier: Expression::Identifier { name: String::from("z") },
                value: Expression::If {
                    condition: Box::new(Expression::Infix {
                        left: Box::new(Expression::Identifier { name: String::from("x") }),
                        op: Token::LT,
                        right: Box::new(Expression::Identifier { name: String::from("y") }),
                    }),
                    consequence: Box::new(Statement::Block {
                        statements: vec![
                            Statement::Expr { expr: Expression::Identifier{ name: String::from("x") } },
                        ],
                    }),
                    alternative: Some(Box::new(Statement::Block {
                        statements: vec![
                            Statement::Expr { expr: Expression::Identifier{ name: String::from("y") } },
                       ],
                    })),
                },

            },
            Statement::Expr {
                expr: Expression::If {
                    condition: Box::new(Expression::Infix {
                        left: Box::new(Expression::Identifier { name: String::from("x") }),
                        op: Token::LT,
                        right: Box::new(Expression::Identifier { name: String::from("y") }),
                    }),
                    consequence: Box::new(Statement::Block {
                        statements: vec![
                            Statement::Return {
                                value: Expression::Identifier { name: String::from("x") },
                            },
                        ],
                    }),
                    alternative: None,
                },
            },
        ];
        test_helper(input, expected);
    }

}