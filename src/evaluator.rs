use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::lexer::Token;

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

#[derive(Debug, PartialEq, Clone)]
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
    ConditionNotBoolean(String),
    UnbindedVariable(String),
}

struct Environment {
    parent: Option<Env>,
    bindings: HashMap<String, Object>,
}

struct Env(Rc<RefCell<Environment>>);

impl Environment {
    fn new() -> Self {
        Self {
            parent: None,
            bindings: HashMap::new(),
        }
    }

    fn insert(&mut self, key: String, value: Object) {
        self.bindings.insert(key, value);
    }

    fn get(&self, key: &String) -> Option<&Object> {
        self.bindings.get(key)
    }
}

impl Env {
    fn new() -> Self {
        Env(Rc::new(RefCell::new(Environment::new())))
    }

    fn of(environment: Environment) -> Self {
        Env(Rc::new(RefCell::new(environment)))
    }

    fn enter(&self) -> Self {
        let mut environment = Environment::new();
        environment.parent = Some(Env(Rc::clone(&self.0)));
        Env(Rc::new(RefCell::new(environment)))
    }

    fn find(&self, key: &String) -> Option<Object> {
        let environment = self.0.borrow();
        let value = environment.get(key);
        match (value, &self.0.borrow().parent) {
            (Some(obj), _) => Some(obj.clone()),
            (None, Some(env)) => env.find(key),
            (None, None) => None,
        }
    }
}

type Result<T> = std::result::Result<T, EvalError>;

#[allow(dead_code)]
pub fn eval(program: &Program) -> Result<Object> {
    return eval_statements(&program.statements, &Env::new());
}

fn eval_statements(statements: &Vec<Statement>, env: &Env) -> Result<Object> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement, env)?;
    }
    Ok(result)
}

fn eval_statement(statement: &Statement, env: &Env) -> Result<Object> {
    match statement {
        Statement::Expr { expr } => eval_expression(expr, env),
        Statement::Block { statements } => eval_statements(statements, &env.enter()),
        _ => Err(EvalError::Unsupported(format!("{:?}", statement))),
    }
}

fn eval_expression(expression: &Expression, env: &Env) -> Result<Object> {
    match expression {
        Expression::Int { value } => Ok(Object::Int(*value)),
        Expression::Bool { value: true } => Ok(Object::Bool(true)),
        Expression::Bool { value: false } => Ok(Object::Bool(false)),
        Expression::Prefix { op, expr } => match (op, eval_expression(expr, env)?) {
            (Token::MINUS, Object::Int(value)) => Ok(Object::Int(-value)),
            (Token::BANG, Object::Bool(value)) => Ok(Object::Bool(!value)),
            (_, obj) => Err(EvalError::PrefixNotApplicable(format!("op: {:?} obj: {:?}", op, obj))),
        },
        Expression::Infix { op, left, right } => match (op, eval_expression(left, env)?, eval_expression(right, env)?) {
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
        },
        Expression::If { condition, consequence, alternative } => match eval_expression(condition, env)? {
            Object::Bool(true) => eval_statement(consequence.as_ref(), env),
            Object::Bool(false) if alternative.is_some() => eval_statement(alternative.as_ref().unwrap(), env),
            Object::Bool(false) if alternative.is_none() => Ok(Object::Null),
            obj => Err(EvalError::ConditionNotBoolean(format!("{:?}", obj))),
        },
        Expression::Identifier { name } => match env.find(name) {
            Some(obj) => Ok(obj.clone()),
            None => Err(EvalError::UnbindedVariable(name.clone())),
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
            if (true) { 1 } else { 2 };
            if (!true) { 1 } else { 2 };
            if (false) { 1 };
            t
            x + y;
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
            Object::Int(1),
            Object::Int(2),
            Object::Null,
            Object::Bool(true),
            Object::Int(3),
        ];
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parser_result = parser.parse_program();
        let env = test_environment();

        assert_eq!(parser_result.is_ok(), true, "err: {:?}", parser_result.unwrap_err());
        if let Ok(program) = parser_result {
            assert_eq!(program.statements.len(), expected.len());

            for (x, y) in program.statements.iter().zip(expected.iter()) {
                if let Statement::Expr { expr } = x {
                    let eval_result = eval_expression(expr, &env.enter());
                    assert_eq!(eval_result.is_ok(), true, "err: {:?}", eval_result.unwrap_err());
                    assert_eq!(eval_result.as_ref().unwrap(), y);
                } else {
                    assert!(false, "Exptected expression statement, got {:?}", x)
                }
            }
        }
    }

    fn test_environment() -> Env {
        let mut environment = Environment::new();
        let keys = vec![String::from("x"), String::from("y"), String::from("z"), String::from("t"), String::from("f")];
        let values = vec![Object::Int(1), Object::Int(2), Object::Int(4), Object::Bool(true), Object::Bool(false)];
        for (x, y) in keys.iter().zip(values.iter()) {
            environment.insert(x.clone(), y.clone())
        }
        Env::of(environment)
    }
}