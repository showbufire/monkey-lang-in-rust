use crate::lexer::Token;

pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Expression  {
    Identifier(Token),
    Dummy,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let { identifier: Expression, value: Expression },
}
