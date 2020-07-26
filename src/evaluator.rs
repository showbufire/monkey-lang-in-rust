use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum Object {
    Bool(bool),
    Int(i64),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum EvalError {
    Unsupported(String),
    PrefixNotApplicable(String),
    InfixNotApplicable(String),
}

type Result<T> = std::result::Result<T, EvalError>;

#[allow(dead_code)]
pub fn eval(program: &Program) -> Result<Object> {
    return eval_statements(&program.statements);
}

fn eval_statements(statements: &Vec<Statement>) -> Result<Object> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement)?;
    }
    Ok(result)
}

fn eval_statement(statement: &Statement) -> Result<Object> {
    match statement {
        Statement::Expr { expr } => eval_expression(expr),
        _ => Err(EvalError::Unsupported(format!("{:?}", statement))),
    }
}

fn eval_expression(expression: &Expression) -> Result<Object> {
    match expression {
        Expression::Int { value } => Ok(Object::Int(*value)),
        Expression::Bool { value: true } => Ok(Object::Bool(true)),
        Expression::Bool { value: false } => Ok(Object::Bool(false)),
        Expression::Prefix { op, expr } => {
            match (op, eval_expression(expr)?) {
                (Token::MINUS, Object::Int(value)) => Ok(Object::Int(-value)),
                (Token::BANG, Object::Bool(value)) => Ok(Object::Bool(!value)),
                (_, obj) => Err(EvalError::PrefixNotApplicable(format!("op: {:?} obj: {:?}", op, obj))),
            }
        },
        Expression::Infix { op, left, right } => {
            match (op, eval_expression(left)?, eval_expression(right)?) {
                (Token::EQ, Object::Bool(v1), Object::Bool(v2)) => Ok(Object::Bool(v1 == v2)),
                (Token::EQ, Object::Int(v1), Object::Int(v2)) => Ok(Object::Bool(v1 == v2)),
                (Token::LT, Object::Int(v1), Object::Int(v2)) => Ok(Object::Bool(v1 < v2)),
                (Token::GT, Object::Int(v1), Object::Int(v2)) => Ok(Object::Bool(v1 > v2)),
                (Token::NE, Object::Bool(v1), Object::Bool(v2)) => Ok(Object::Bool(v1 != v2)),
                (Token::NE, Object::Int(v1), Object::Int(v2)) => Ok(Object::Bool(v1 != v2)),
                (Token::PLUS, Object::Int(v1), Object::Int(v2)) => Ok(Object::Int(v1 + v2)),
                (Token::MINUS, Object::Int(v1), Object::Int(v2)) => Ok(Object::Int(v1 - v2)),
                (Token::ASTERISK, Object::Int(v1), Object::Int(v2)) => Ok(Object::Int(v1 * v2)),
                (Token::SLASH, Object::Int(v1), Object::Int(v2)) => Ok(Object::Int(v1 / v2)),
                (_, obj1, obj2) => Err(EvalError::InfixNotApplicable(format!("op: {:?} left: {:?} right: {:?}", op, obj1, obj2))),
            }
        },
        _ => Err(EvalError::Unsupported(format!("{:?}", expression))),
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator::*;

    #[test]
    fn test_single_expression() {
        let input = String::from("
            5;
            true;
            false;
            -5;
            !false;
            !true;
            1+2;
            1+2*3;
            2 * (3+4);
            (1+2) * (3+4);
            (3+4) / (1+2);
            1 + 2 == 3;
            1 + 2 != 3;
            1 < 2;
            1 > 2;
            true != false;
            true == false;
        ");
        let expected = vec![
            Object::Int(5),
            Object::Bool(true),
            Object::Bool(false),
            Object::Int(-5),
            Object::Bool(true),
            Object::Bool(false),
            Object::Int(3),
            Object::Int(7),
            Object::Int(14),
            Object::Int(21),
            Object::Int(2),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(false),
            Object::Bool(true),
            Object::Bool(false),
        ];
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parser_result = parser.parse_program();

        assert_eq!(parser_result.is_ok(), true, "err: {:?}", parser_result.unwrap_err());
        if let Ok(program) = parser_result {
            assert_eq!(program.statements.len(), expected.len());

            for (x, y) in program.statements.iter().zip(expected.iter()) {
                if let Statement::Expr { expr } = x {
                    let eval_result = eval_expression(expr);
                    assert_eq!(eval_result.is_ok(), true, "err: {:?}", eval_result.unwrap_err());
                    assert_eq!(eval_result.as_ref().unwrap(), y);
                } else {
                    assert!(false, "Exptected expression statement, got {:?}", x)
                }
            }
        }
    }
}