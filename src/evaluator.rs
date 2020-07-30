use crate::ast::*;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::lexer::Token;

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

#[derive(Clone)]
pub enum Object<'a> {
    Bool(bool),
    Int(i64),
    Null,
    Function { func: &'a Expression, env: Env<'a> },
}

impl<'a> fmt::Debug for Object<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Bool(bool_value) => f.debug_struct("Object").field("value", bool_value).finish(),
            Object::Int(int_value) => f.debug_struct("Object").field("value", int_value).finish(),
            Object::Null => f.debug_struct("Object").field("value", &"null").finish(),
            Object::Function{ func, env: _ } => f.debug_struct("Object").field("value", func).finish(),
        }
    }
}

impl<'a> PartialEq for Object<'a> {
    fn eq(&self, other: &Object) -> bool {
        match (self, other) {
            (Object::Bool(v1), Object::Bool(v2)) => v1 == v2,
            (Object::Int(v1), Object::Int(v2)) => v1 == v2,
            (Object::Null, Object::Null) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum EvalError {
    Unsupported(String),
    PrefixNotApplicable(String),
    InfixNotApplicable(String),
    ConditionNotBoolean(String),
    UnbindedVariable(String),
}

pub struct Environment<'a> {
    parent: Option<Env<'a>>,
    bindings: HashMap<String, Object<'a>>,
}

#[derive(Clone)]
pub struct Env<'a>(Rc<RefCell<Environment<'a>>>);

impl<'a> Environment<'a> {
    fn new() -> Self {
        Self {
            parent: None,
            bindings: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    fn insert(&mut self, key: String, value: Object<'a>) {
        self.bindings.insert(key, value);
    }

    fn get(&self, key: &str) -> Option<&Object<'a>> {
        self.bindings.get(key)
    }
}

impl<'a> Env<'a> {
    fn new() -> Self {
        Env(Rc::new(RefCell::new(Environment::new())))
    }

    #[allow(dead_code)]
    fn of(environment: Environment<'a>) -> Self {
        Env(Rc::new(RefCell::new(environment)))
    }

    fn enter(&self) -> Self {
        let mut environment = Environment::new();
        environment.parent = Some(Env(Rc::clone(&self.0)));
        Env(Rc::new(RefCell::new(environment)))
    }

    fn find(&self, key: &str) -> Option<Object<'a>> {
        let environment = self.0.borrow();
        let value = environment.get(key);
        match (value, &self.0.borrow().parent) {
            (Some(obj), _) => Some(obj.clone()),
            (None, Some(env)) => env.find(key).clone(),
            (None, None) => None,
        }
    }

    fn insert(&self, key: String, value: Object<'a>) {
        let mut environment = self.0.borrow_mut();
        environment.insert(key, value);
    }
}

type Result<T> = std::result::Result<T, EvalError>;

#[allow(dead_code)]
pub fn eval<'a>(program: &'a Program) -> Result<Object<'a>> {
    return eval_statements(&program.statements, &Env::new());
}

fn eval_statements<'a>(statements: &'a Vec<Statement>, env: &Env<'a>) -> Result<Object<'a>> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement, env)?;
    }
    Ok(result)
}

fn eval_statement<'a>(statement: &'a Statement, env: &Env<'a>) -> Result<Object<'a>> {
    match statement {
        Statement::Expr { expr } => eval_expression(expr, env),
        Statement::Block { statements } => eval_statements(statements, &env.enter()),
        Statement::Let { identifier: Expression::Identifier { name }, value } => {
            let value_obj = eval_expression(value, env)?;
            env.insert(name.clone(), value_obj.clone());
            Ok(value_obj)
        },
        _ => Err(EvalError::Unsupported(format!("{:?}", statement))),
    }
}

fn eval_expression<'a>(expression: &'a Expression, env: &Env<'a>) -> Result<Object<'a>> {
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
            Some(obj) => Ok(obj),
            None => Err(EvalError::UnbindedVariable(name.clone())),
        },
        Expression::Function { parameters: _, body: _ } => Ok(Object::Function { func: expression, env: env.clone() }),
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
        let env = generate_environment();

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

    fn generate_environment<'a>() -> Env<'a> {
        let mut environment = Environment::new();
        let keys = vec![String::from("x"), String::from("y"), String::from("z"), String::from("t"), String::from("f")];
        let values = vec![Object::Int(1), Object::Int(2), Object::Int(4), Object::Bool(true), Object::Bool(false)];
        for (x, y) in keys.iter().zip(values.iter()) {
            environment.insert(x.clone(), y.clone())
        }
        Env::of(environment)
    }

    #[test]
    fn test_function_literal() {
        let input = String::from("
            let x = 1;
            let y = 2;
            let foo = fn() {
                x
            };
            let x = 10;
            foo
        ");
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parser_result = parser.parse_program();
        assert_eq!(parser_result.is_ok(), true, "err: {:?}", parser_result.unwrap_err());
        let program = parser_result.unwrap();
        let eval_result = eval(&program);
        assert_eq!(eval_result.is_ok(), true, "err: {:?}", eval_result.unwrap_err());
        let object = eval_result.unwrap();
        if let Object::Function { func, env } = object {
            let expected_function = Expression::Function {
                parameters: vec![],
                body: Box::new(Statement::Block {
                        statements: vec![
                            Statement::Expr { expr: Expression::Identifier{ name: String::from("x")}},
                        ],
                })
            };
            assert_eq!(func, &expected_function);
            assert_eq!(env.find("y"), Some(Object::Int(2)));
            assert_eq!(env.find("x"), Some(Object::Int(10)));
        } else {
            assert!(false, "Not a function literal #{:?}", object);
        }
    }
}