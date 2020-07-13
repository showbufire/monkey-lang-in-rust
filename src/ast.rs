use crate::lexer::Token;

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier { name: String },
    Int { value: i64 },
    Prefix { op: Token, expr: Box<Expression> },
    Infix { op: Token, left: Box<Expression>, right: Box<Expression> },
    Bool { value: bool },
    If { condition: Box<Expression>, consequence: Box<Statement>, alternative: Option<Box<Statement>> },
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let { identifier: Expression, value: Expression },
    Return { value: Expression },
    Expr { expr: Expression },
    Block { statements: Vec<Statement> },
}
