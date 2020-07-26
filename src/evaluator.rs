use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::Parser;

#[derive(Debug, PartialEq)]
pub enum Object {
    True,
    False,
    Int(i64),
    Null,
}

#[derive(Debug, PartialEq)]
pub enum EvalError {
    Unsupported(String),
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
        Expression::Bool { value: true } => Ok(Object::True),
        Expression::Bool { value: false } => Ok(Object::False),
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
        ");
        let expected = vec![
            Object::Int(5),
            Object::True,
            Object::False,
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