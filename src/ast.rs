#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

#[derive(Debug, PartialEq)]
pub enum Expression  {
    Identifier{ name: String },
    Dummy,
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let { identifier: Expression, value: Expression },
    Return { value: Expression },
}
