use crate::lexer::{Token, Lexer, LexerError};
use crate::ast::*;

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
}

#[derive(Debug, Clone)]
pub enum ParserError {
    UnexpectedToken { expected: Token, actual: Token },
    LexerError(LexerError),
    NotExpression(Token),
    NotIdentifier(Token),
}

type Result<T> = std::result::Result<T, ParserError>;

#[derive(PartialOrd, PartialEq)]
enum Precedence {
    LOWEST,
    EQ,
    PLUS,
    MULT,
    PREFIX,
    MEMBER,
    CALL,
}

fn op_precedence(token: &Token) -> Precedence {
    match token {
        Token::EQ | Token::LT | Token::GT | Token::NE => Precedence::EQ,
        Token::PLUS | Token::MINUS => Precedence::PLUS,
        Token::ASTERISK | Token::SLASH => Precedence::MULT,
        Token::LPAREN => Precedence::CALL,
        Token::LBRACKET => Precedence::MEMBER,
        _ => Precedence::LOWEST,
    }
}

fn is_infix_op(token: &Token) -> bool {
    match token {
        Token::EQ | Token::LT | Token::GT | Token::NE => true,
        Token::PLUS | Token::MINUS => true,
        Token::ASTERISK | Token::SLASH => true,
        Token::LPAREN => true,
        Token::LBRACKET => true,
        _ => false,
    }
}

#[allow(dead_code)]
impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        Parser {
            lexer,
            cur_token: Token::EOF,
        }
    }

    fn init(&mut self) -> Result<()> {
        self.next_token()
    }

    pub fn parse_program(&mut self) -> Result<Program> {
        self.init()?;
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
        if self.cur_token == Token::SEMICOLON {
            self.next_token()?;
        }
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
            self.next_token()?;
        }
        result
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression> {
        let mut left = match self.cur_token {
            Token::IDENT(_) => self.parse_identifier()?,
            Token::INT(int_value) => self.parse_int(int_value)?,
            Token::STRING(_) => self.parse_string()?,
            Token::TRUE | Token::FALSE => self.parse_bool()?,
            Token::LPAREN => self.parse_grouped_expression()?,
            Token::BANG | Token::MINUS => self.parse_prefix_expression()?,
            Token::IF => self.parse_if_expression()?,
            Token::FUNCTION => self.parse_function_expression()?,
            Token::LBRACKET => self.parse_array()?,
            _ => return Err(ParserError::NotExpression(self.cur_token.clone())),
        };
        while is_infix_op(&self.cur_token) && precedence < op_precedence(&self.cur_token) {
            match self.cur_token {
                Token::LPAREN => {
                    let arguments = self.parse_parameters()?;
                    left = Expression::Call {
                        function: Box::new(left),
                        arguments: arguments
                    }
                },
                Token::LBRACKET => {
                    self.expect_token(Token::LBRACKET)?;
                    let idx = self.parse_expression(Precedence::LOWEST)?;
                    self.expect_token(Token::RBRACKET)?;
                    left = Expression::ArrayMember {
                        arr: Box::new(left),
                        idx: Box::new(idx),
                    }
                },
                _ => {
                    let op = self.pop_token()?;
                    let right = self.parse_expression(op_precedence(&op))?;
                    left = Expression::Infix {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                },
            };
        }
        Ok(left)
    }

    fn parse_array(&mut self) -> Result<Expression> {
        let members = self.parse_expression_list(Token::LBRACKET, Token::RBRACKET)?;
        Ok(Expression::Array(members))
    }

    fn parse_function_expression(&mut self) -> Result<Expression> {
        self.expect_token(Token::FUNCTION)?;
        let parameters = self.parse_parameters()?;
        let body = self.parse_block_statement()?;
        Ok(Expression::Function{
            parameters,
            body: Box::new(body),
        })
    }

    fn parse_parameters(&mut self) -> Result<Vec<Expression>> {
        self.parse_expression_list(Token::LPAREN, Token::RPAREN)
    }

    fn parse_expression_list(&mut self, opening_token: Token, closing_token: Token) -> Result<Vec<Expression>> {
        self.expect_token(opening_token)?;
        let mut expr_list = Vec::new();
        while self.cur_token != Token::EOF && self.cur_token != closing_token {
            let expr = self.parse_expression(Precedence::LOWEST)?;
            expr_list.push(expr);
            if self.cur_token == Token::COMMA {
                self.next_token()?;
            }
        }
        self.expect_token(closing_token)?;
        Ok(expr_list)
    }

    fn parse_if_expression(&mut self) -> Result<Expression> {
        self.expect_token(Token::IF)?;
        self.expect_token(Token::LPAREN)?;
        let condition = self.parse_expression(Precedence::LOWEST)?;
        self.expect_token(Token::RPAREN)?;
        let consequence = self.parse_block_statement()?;
        let mut alternative = None;
        if self.cur_token == Token::ELSE {
            self.next_token()?;
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
            _ => panic!("parser expected bool, got: {:?}", self.cur_token),
        };
        if res.is_ok() {
            self.next_token()?;
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
                let op = self.pop_token()?;
                let expr = self.parse_expression(Precedence::PREFIX)?;
                Ok(Expression::Prefix { op, expr: Box::new(expr)})
            },
            _ => panic!("parser expected prefix op, got: {:?}", self.cur_token),
        }
    }

    fn parse_int(&mut self, int_value: i64) -> Result<Expression> {
        self.next_token()?;
        Ok(Expression::Int{ value: int_value })
    }

    fn parse_string(&mut self) -> Result<Expression> {
        let expr = if let Token::STRING(s) = &self.cur_token {
            Expression::StringLiteral(s.clone())
        } else {
            panic!("parser expected string(, got : {:?}", self.cur_token);
        };
        self.next_token()?;
        Ok(expr)
    }

    fn expect_token(&mut self, expected: Token) -> Result<()> {
        if self.cur_token != expected {
            return Err(ParserError::UnexpectedToken{ expected, actual: self.cur_token.clone() });
        }
        self.next_token()?;
        Ok(())
    }

    fn pop_token(&mut self) -> Result<Token> {
        let token = self.cur_token.clone();
        self.next_token()?;
        Ok(token)
    }

    fn next_token(&mut self) -> Result<()> {
        match self.lexer.next_token() {
            Err(lexer_error) => Err(ParserError::LexerError(lexer_error)),
            Ok(first_token) => {
                self.cur_token = first_token;
                Ok(())
            },
        }
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
        test_parse_statements(input, expected);
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
        test_parse_statements(input, expected);
    }

    fn test_parse_statements(input: String, expected: Vec<Statement>) {
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

    fn test_parse_expressions(input: String, expected: Vec<Expression>) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse_program();

        assert_eq!(parse_result.is_ok(), true, "err: {:?}", parse_result.unwrap_err());
        if let Ok(program) = parse_result {
            assert_eq!(program.statements.len(), expected.len());
            for (x, y) in program.statements.iter().zip(expected.iter()) {
                match x {
                    Statement::Expr { expr } => assert_eq!(expr, y),
                    _ => assert!(false, "Expected expression statement, got {:?}", x)
                }
            }
        }
    }

    #[test]
    fn test_expression_statement() {
        let input = String::from("
            10;
            foobar;
            \"foo\";
            [];
            [\"foo\", 1];
            [1, 2][0];
            !x;
            -5;
            1 + 2;
            1 + 2 * 3;
            1 + 2 == 3;
            2 * ( 3 + 4 );
            1 / ( 2 * (3 + 4) );
            true == false
        ");
        let expected = vec![
            Expression::Int { value: 10 },
            Expression::Identifier { name: String::from("foobar") },
            Expression::StringLiteral(String::from("foo")),
            Expression::Array(Vec::new()),
            Expression::Array(vec![Expression::StringLiteral(String::from("foo")), Expression::Int{value: 1}]),
            Expression::ArrayMember {
                arr: Box::new(Expression::Array(vec![Expression::Int { value: 1 }, Expression::Int { value: 2}])),
                idx: Box::new(Expression::Int { value: 0 }),
            },
            Expression::Prefix {
                op: Token::BANG,
                expr: Box::new(Expression::Identifier { name: String::from("x")}),
            },
            Expression::Prefix {
                op: Token::MINUS,
                expr: Box::new(Expression::Int { value: 5 }),
            },
            Expression::Infix {
                op: Token::PLUS,
                left: Box::new(Expression::Int { value: 1 }),
                right: Box::new(Expression::Int { value: 2 }),
            },
            Expression::Infix {
                op: Token::PLUS,
                left: Box::new(Expression::Int { value: 1 }),
                right: Box::new(Expression::Infix {
                    op: Token::ASTERISK,
                    left: Box::new(Expression::Int { value: 2 }),
                    right: Box::new(Expression::Int { value: 3 }),
                }),
            },
            Expression::Infix {
                op: Token::EQ,
                left: Box::new(Expression::Infix {
                    op: Token::PLUS,
                    left: Box::new(Expression::Int { value: 1 }),
                    right: Box::new(Expression::Int { value: 2 }),
                }),
                right: Box::new(Expression::Int { value: 3 }),
            },
            Expression::Infix {
                op: Token::ASTERISK,
                left: Box::new(Expression::Int { value: 2 }),
                right: Box::new(Expression::Infix {
                    op: Token::PLUS,
                    left: Box::new(Expression::Int { value: 3 }),
                    right: Box::new(Expression::Int { value: 4 }),
                }),
            },
            Expression::Infix {
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
            },
            Expression::Infix {
                op: Token::EQ,
                left: Box::new(Expression::Bool { value: true }),
                right: Box::new(Expression::Bool { value: false }),
            },
        ];
        test_parse_expressions(input, expected);
    }

    #[test]
    fn test_if_expression() {
        let input = String::from("
            if (x < y) {
                x
            } else {
                y
            };
            if (x < y) {
                return x;
            };
        ");

        let expected = vec![
            Expression::If {
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
            Expression::If {
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
        ];
        test_parse_expressions(input, expected);
    }

    #[test]
    fn test_function_literal() {
        let input = String::from("
            fn() {
            };
            fn(x) {
                x
            };
            fn(x, y) {
                x + y
            }
        ");
        let expected = vec![
            Expression::Function {
                parameters: vec![],
                body: Box::new(Statement::Block {
                    statements: vec![],
                }),
            },
            Expression::Function {
                parameters: vec![
                    Expression::Identifier { name: String::from("x") },
                ],
                body: Box::new(Statement::Block {
                    statements: vec![
                        Statement::Expr { expr: Expression::Identifier{ name: String::from("x") } },
                    ]
                }),
            },
            Expression::Function {
                parameters: vec![
                    Expression::Identifier { name: String::from("x") },
                    Expression::Identifier { name: String::from("y") },
                ],
                body: Box::new(Statement::Block {
                    statements: vec![
                        Statement::Expr { expr: Expression::Infix {
                            left: Box::new(Expression::Identifier{ name: String::from("x") }),
                            op: Token::PLUS,
                            right: Box::new(Expression::Identifier{ name: String::from("y") })
                        }},
                    ],
                }),
            },
        ];
        test_parse_expressions(input, expected);
    }

    #[test]
    fn test_call_expression() {
        let input = String::from("
            foo();
            bar(x+1);
            fn(){ }();
            foo(bar(x), y);
        ");
        let expected = vec![
            Expression::Call {
                function: Box::new(Expression::Identifier {
                    name: String::from("foo"),
                }),
                arguments: vec![],
            },
            Expression::Call {
                function: Box::new(Expression::Identifier {
                    name: String::from("bar"),
                }),
                arguments: vec![
                    Expression::Infix {
                        left: Box::new(Expression::Identifier { name: String::from("x") }),
                        op: Token::PLUS,
                        right: Box::new(Expression::Int { value: 1 }),
                    },
                ],
            },
            Expression::Call {
                function: Box::new(Expression::Function {
                    parameters: vec![],
                    body: Box::new(Statement::Block {
                        statements: vec![],
                    }),
                }),
                arguments: vec![],
            },
            Expression::Call {
                function: Box::new(Expression::Identifier {
                    name: String::from("foo"),
                }),
                arguments: vec![
                    Expression::Call {
                        function: Box::new(Expression::Identifier {
                            name: String::from("bar"),
                        }),
                        arguments: vec![
                            Expression::Identifier { name: String::from("x") },
                        ],
                    },
                    Expression::Identifier { name: String::from("y") },
                ],
            }
        ];
        test_parse_expressions(input, expected);
    }

    #[test]
    fn test_lexer_error() {
        let lexer = Lexer::new(String::from("? x;"));
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse_program();

        assert!(parse_result.is_err(), "expected an error, got: {:?}", parse_result.unwrap());
        assert_matches!(parse_result.unwrap_err(), ParserError::LexerError(_));
    }

    #[test]
    fn test_not_identifier_error() {
        let lexer = Lexer::new(String::from("let 3 3;"));
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse_program();

        assert!(parse_result.is_err(), "expected an error, got: {:?}", parse_result.unwrap());
        assert_matches!(parse_result.unwrap_err(), ParserError::NotIdentifier(_));
    }

    #[test]
    fn test_not_expression_error() {
        let lexer = Lexer::new(String::from("3 + return 5;"));
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse_program();

        assert!(parse_result.is_err(), "expected an error, got: {:?}", parse_result.unwrap());
        assert_matches!(parse_result.unwrap_err(), ParserError::NotExpression(_));
    }

    #[test]
    fn test_unexpected_token_error() {
        let lexer = Lexer::new(String::from("let x = 5")); // missing semicolon
        let mut parser = Parser::new(lexer);
        let parse_result = parser.parse_program();

        assert!(parse_result.is_err(), "expected an error, got: {:?}", parse_result.unwrap());
        assert_matches!(parse_result.unwrap_err(), ParserError::UnexpectedToken{ expected: _, actual: _ });
    }
}