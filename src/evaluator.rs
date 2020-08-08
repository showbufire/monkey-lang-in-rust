use crate::ast::*;
use crate::lexer::Token;

use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;

#[derive(Clone)]
pub enum Object {
    Bool(bool),
    Int(i64),
    StringLiteral(String),
    Array(Vec<Object>),
    Null,
    Function { func: Expression, env: Env },
    Return(Box<Object>),
    BuiltInFunction(BuiltIn),
}

#[derive(Clone, Debug, PartialEq)]
pub enum BuiltIn {
    LEN,
    PUSH,
}

impl fmt::Debug for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Object::Bool(bool_value) => f.debug_struct("Object").field("bool", bool_value).finish(),
            Object::Int(int_value) => f.debug_struct("Object").field("int", int_value).finish(),
            Object::Null => f.debug_struct("Object").field("value", &"null").finish(),
            Object::Function{ func, env: _ } => f.debug_struct("Object").field("func", func).finish(),
            Object::Return(ret) => f.debug_struct("Object").field("ret", ret).finish(),
            Object::StringLiteral(str_value) => f.debug_struct("Object").field("str", str_value).finish(),
            Object::BuiltInFunction(built_in) => f.debug_struct("Object").field("built_in", built_in).finish(),
            Object::Array(members) => f.debug_struct("Object").field("members", members).finish(),
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
    CallNonFunction(String),
    InvalidArguments(String),
    InvalidIndex(String),
    InvalidArray(String),
}

pub struct Environment {
    parent: Option<Env>,
    bindings: HashMap<String, Object>,
}

#[derive(Clone)]
pub struct Env(Rc<RefCell<Environment>>);

impl Environment {
    fn new() -> Self {
        Self {
            parent: None,
            bindings: HashMap::new(),
        }
    }

    #[allow(dead_code)]
    fn insert(&mut self, key: String, value: Object) {
        self.bindings.insert(key, value);
    }

    fn get(&self, key: &str) -> Option<&Object> {
        self.bindings.get(key)
    }
}

impl Env {
    pub fn new() -> Self {
        Env(Rc::new(RefCell::new(Environment::new())))
    }

    #[allow(dead_code)]
    fn of(environment: Environment) -> Self {
        Env(Rc::new(RefCell::new(environment)))
    }

    fn enter(&self) -> Self {
        let mut environment = Environment::new();
        environment.parent = Some(Env(Rc::clone(&self.0)));
        Env(Rc::new(RefCell::new(environment)))
    }

    fn find(&self, key: &str) -> Option<Object> {
        let environment = self.0.borrow();
        let value = environment.get(key);
        match (value, &self.0.borrow().parent) {
            (Some(obj), _) => Some(obj.clone()),
            (None, Some(env)) => env.find(key).clone(),
            (None, None) => None,
        }
    }

    fn insert(&self, key: String, value: Object) {
        let mut environment = self.0.borrow_mut();
        environment.insert(key, value);
    }
}

fn find_built_in(name: &str) -> Option<Object> {
    match name {
        "len" => Some(Object::BuiltInFunction(BuiltIn::LEN)),
        "push" => Some(Object::BuiltInFunction(BuiltIn::PUSH)),
        _ => None,
    }
}

type Result<T> = std::result::Result<T, EvalError>;

#[allow(dead_code)]
pub fn eval(program: & Program) -> Result<Object> {
    eval_with_env(&program, &Env::new())
}

pub fn eval_with_env(program: & Program, env: &Env) -> Result<Object> {
    eval_statements(&program.statements, env)
}

fn eval_statements(statements: & Vec<Statement>, env: &Env) -> Result<Object> {
    let mut result = Object::Null;
    for statement in statements {
        result = eval_statement(statement, env)?;
        if let Object::Return(_) = result {
            break;
        }
    }
    Ok(result)
}

fn eval_statement(statement: & Statement, env: &Env) -> Result<Object> {
    match statement {
        Statement::Expr { expr } => eval_expression(expr, env),
        Statement::Block { statements } => eval_statements(statements, env),
        Statement::Let { identifier: Expression::Identifier { name }, value } => eval_let_statement(name, value, env),
        Statement::Return { value } => {
            let value_obj = eval_expression(value, env)?;
            Ok(Object::Return(Box::new(value_obj)))
        }
        _ => Err(EvalError::Unsupported(format!("{:?}", statement))),
    }
}

fn eval_let_statement(name: &String, value: &Expression, env: &Env) -> Result<Object> {
    let value_obj = eval_expression(value, env)?;
    env.insert(name.clone(), value_obj.clone());
    Ok(value_obj)
}

fn eval_expression(expression: & Expression, env: &Env) -> Result<Object> {
    match expression {
        Expression::Int { value } => Ok(Object::Int(*value)),
        Expression::Bool { value: true } => Ok(Object::Bool(true)),
        Expression::Bool { value: false } => Ok(Object::Bool(false)),
        Expression::Prefix { op, expr } => eval_prefix_expression(op, expr, env),
        Expression::Infix { op, left, right } => eval_infix_expression(op, left, right, env),
        Expression::If { condition, consequence, alternative } => eval_if_expression(condition, consequence, alternative, env),
        Expression::Identifier { name } => eval_identifier(name, env),
        Expression::Function { parameters: _, body: _ } => Ok(Object::Function { func: expression.clone(), env: env.clone() }),
        Expression::Call { function, arguments } => eval_call_expression(function, arguments, env),
        Expression::Array(members) => eval_array_expression(members, env),
        Expression::ArrayMember{arr, idx} => eval_array_member(arr, idx, env),
        Expression::StringLiteral(s) => Ok(Object::StringLiteral(s.clone())),
    }
}

fn eval_array_member(arr: &Box<Expression>, idx: &Box<Expression>, env: &Env) -> Result<Object> {
    let arr_obj = eval_expression(arr, env)?;
    let idx_obj = eval_expression(idx, env)?;

    match (&arr_obj, &idx_obj) {
        (Object::Array(members), Object::Int(i)) => {
            if 0 <= *i && *i < members.len() as i64 {
                Ok(members[*i as usize].clone())
            } else {
                Ok(Object::Null)
            }
        },
        (Object::Array(_), _) => Err(EvalError::InvalidIndex(format!("{:?}", idx_obj))),
        (_, _) => Err(EvalError::InvalidArray(format!("{:?}", arr_obj))),
    }
}

fn eval_array_expression(members: &Vec<Expression>, env: &Env) -> Result<Object> {
    let mut member_objs = Vec::new();
    for member in members {
        let obj = eval_expression(member, env)?;
        member_objs.push(obj);
    }

    Ok(Object::Array(member_objs))
}

fn eval_call_expression(function: &Box<Expression>, arguments: &Vec<Expression>, env: &Env) -> Result<Object> {
    match &eval_expression(function, env)? {
        Object::Function { func: Expression::Function { parameters, body }, env: closure } => {
            if parameters.len() != arguments.len() {
                Err(EvalError::InvalidArguments(format!("function: {:?}, arguments: {:?}", function, arguments)))
            } else {
                let new_env = closure.enter();
                for (arg, param) in arguments.iter().zip(parameters.iter()) {
                    let arg_obj = eval_expression(arg, env)?;
                    if let Expression::Identifier { name } = param {
                        new_env.insert(name.clone(), arg_obj);
                    }
                }
                let call_result = eval_statement(body, &new_env)?;
                match call_result {
                    Object::Return(ret) => Ok(*ret),
                    _ => Ok(call_result),
                }
            }
        },
        Object::BuiltInFunction(built_in) => {
            let mut arg_objs = Vec::new();
            for arg in arguments {
                let arg_obj = eval_expression(&arg, env)?;
                arg_objs.push(arg_obj)
            }
            match built_in {
                BuiltIn::LEN => eval_built_in_len(arg_objs),
                BuiltIn::PUSH => eval_built_in_push(&mut arg_objs),
            }
        },
        _ => Err(EvalError::CallNonFunction(format!("{:?}", function))),
    }
}

fn eval_built_in_push(args: &mut Vec<Object>) -> Result<Object> {
    if args.len() != 2 {
        return Err(EvalError::InvalidArguments(format!("built-in::push {:?}", args)));
    }
    match (args.pop(), args.pop()) {
        (Some(obj), Some(Object::Array(members))) => {
            let mut new_arr = members;
            new_arr.push(obj);
            Ok(Object::Array(new_arr))
        },
        _ => Err(EvalError::InvalidArguments(format!("built-in::push {:?}", args))),
    }
}

fn eval_built_in_len(args: Vec<Object>) -> Result<Object> {
    if args.len() != 1 {
        return Err(EvalError::InvalidArguments(format!("built-in::len {:?}", args)));
    }
    match &args[0] {
        Object::StringLiteral(s) => Ok(Object::Int(s.len() as i64)),
        Object::Array(members) => Ok(Object::Int(members.len() as i64)),
        _ => Err(EvalError::InvalidArguments(format!("built-in::len {:?}", args))),
    }
}

fn eval_identifier(name: &String, env: &Env) -> Result<Object> {
    if let Some(obj) = env.find(name) {
        Ok(obj)
    } else if let Some(obj) = find_built_in(name) {
        Ok(obj)
    } else {
        Err(EvalError::UnbindedVariable(name.clone()))
    }
}

fn eval_if_expression(condition: &Expression, consequence: &Box<Statement>, alternative: &Option<Box<Statement>>, env: &Env) -> Result<Object> {
    match eval_expression(condition, env)? {
        Object::Bool(true) => eval_statement(consequence.as_ref(), env),
        Object::Bool(false) if alternative.is_some() => eval_statement(alternative.as_ref().unwrap(), env),
        Object::Bool(false) if alternative.is_none() => Ok(Object::Null),
        obj => Err(EvalError::ConditionNotBoolean(format!("{:?}", obj))),
    }
}

fn eval_infix_expression(op: &Token, left: &Expression, right: &Expression, env: &Env) -> Result<Object> {
    match (op, eval_expression(left, env)?, eval_expression(right, env)?) {
        (Token::EQ, Object::Bool(v1), Object::Bool(v2)) => Ok(Object::Bool(v1 == v2)),
        (Token::EQ, Object::Int(v1), Object::Int(v2)) => Ok(Object::Bool(v1 == v2)),
        (Token::LT, Object::Int(v1), Object::Int(v2)) => Ok(Object::Bool(v1 < v2)),
        (Token::GT, Object::Int(v1), Object::Int(v2)) => Ok(Object::Bool(v1 > v2)),
        (Token::NE, Object::Bool(v1), Object::Bool(v2)) => Ok(Object::Bool(v1 != v2)),
        (Token::NE, Object::Int(v1), Object::Int(v2)) => Ok(Object::Bool(v1 != v2)),
        (Token::PLUS, Object::Int(v1), Object::Int(v2)) => Ok(Object::Int(v1 + v2)),
        (Token::PLUS, Object::StringLiteral(v1), Object::StringLiteral(v2)) => {
            let mut v = v1.clone();
            v.push_str(&v2);
            Ok(Object::StringLiteral(v))
        },
        (Token::MINUS, Object::Int(v1), Object::Int(v2)) => Ok(Object::Int(v1 - v2)),
        (Token::ASTERISK, Object::Int(v1), Object::Int(v2)) => Ok(Object::Int(v1 * v2)),
        (Token::SLASH, Object::Int(v1), Object::Int(v2)) => Ok(Object::Int(v1 / v2)),
        (_, obj1, obj2) => Err(EvalError::InfixNotApplicable(format!("op: {:?} left: {:?} right: {:?}", op, obj1, obj2))),
    }
}

fn eval_prefix_expression(op: &Token, expr: &Expression, env: &Env) -> Result<Object> {
    match (op, eval_expression(expr, env)?) {
        (Token::MINUS, Object::Int(value)) => Ok(Object::Int(-value)),
        (Token::BANG, Object::Bool(value)) => Ok(Object::Bool(!value)),
        (_, obj) => Err(EvalError::PrefixNotApplicable(format!("op: {:?} obj: {:?}", op, obj))),
    }
}

#[cfg(test)]
mod tests {
    use crate::evaluator::*;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    impl PartialEq for Object {
        fn eq(&self, other: &Object) -> bool {
            match (self, other) {
                (Object::Bool(v1), Object::Bool(v2)) => v1 == v2,
                (Object::Int(v1), Object::Int(v2)) => v1 == v2,
                (Object::StringLiteral(v1), Object::StringLiteral(v2)) => v1 == v2,
                (Object::Null, Object::Null) => true,
                (Object::Array(members1), Object::Array(members2)) => members1 == members2,
                _ => false,
            }
        }
    }

    #[test]
    fn test_single_expression() {
        let input = String::from("
            [1, true];
            [1, true][1];
            \"foo\";
            \"foo\"+\"bar\";
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
            t;
            x + y;
            len(\"\");
            len(\"foo\");
            len([1, 2]);
            push([1], 2);
        ");
        let expected = vec![
            Object::Array(vec![Object::Int(1), Object::Bool(true)]),
            Object::Bool(true),
            Object::StringLiteral(String::from("foo")),
            Object::StringLiteral(String::from("foobar")),
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
            Object::Int(0),
            Object::Int(3),
            Object::Int(2),
            Object::Array(vec![Object::Int(1), Object::Int(2)]),
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

    fn generate_environment() -> Env {
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
            assert_eq!(func, expected_function);
            assert_eq!(env.find("y"), Some(Object::Int(2)));
            assert_eq!(env.find("x"), Some(Object::Int(10)));
        } else {
            assert!(false, "Not a function literal #{:?}", object);
        }
    }

    #[test]
    fn test_call_expression() {
        let input = String::from("
            let x = 3;
            let foo = fn (y) { x + y };
            foo(10)
        ");
        let expected = Object::Int(13);

        let input2 = String::from("
            fn(x, y) { x + y } (1, 100);
        ");
        let expected2 = Object::Int(101);

        let input3 = String::from("
            let x = 1;
            let foo = fn(x) { x };
            foo(3) + x
        ");
        let expected3 = Object::Int(4);

        check_full_program(input, expected);
        check_full_program(input2, expected2);
        check_full_program(input3, expected3);
    }

    #[test]
    fn test_return() {
        let input = String::from("
            fn() {
                return 42;
                10
            }();
        ");
        let expected = Object::Int(42);

        let input2 = String::from("
            fn(x) {
                return fn(y) { x * y };
            } (10)(20);
        ");
        let expected2 = Object::Int(200);

        let input3 = String::from("
            let fib = fn(n) {
                if (n == 0) {
                    return 0;
                } else {
                    if (n == 1) {
                        return 1;
                    }
                }
                return fib(n-2) + fib(n-1);
            };
            fib(10)
        ");
        let expected3 = Object::Int(55);

        let input4 = String::from("
            let twice = fn(f) { fn(x) { return f(f(x)); } };
            let double = fn(x) { return x * 2; };

            twice(double)(100)
        ");
        let expected4 = Object::Int(400);

        let input5 = String::from("
            let foo = fn(x) {
                if (x < 10) {
                    return 1;
                }
                return 2;
            };
            foo(3)
        ");
        let expected5 = Object::Int(1);

        check_full_program(input, expected);
        check_full_program(input2, expected2);
        check_full_program(input3, expected3);
        check_full_program(input4, expected4);
        check_full_program(input5, expected5);
    }

    #[test]
    fn test_array() {
        let input = String::from("
            let firstQuad = fn(arr) {
                arr[1-1] + arr[2*0] * 2 + arr[3+(-3)]
            };
            let nums = [13, 42];
            firstQuad(nums)
        ");
        let expected = Object::Int(52);
        check_full_program(input, expected);
    }

    fn check_full_program(input: String, expected: Object) {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let parser_result = parser.parse_program();
        assert_eq!(parser_result.is_ok(), true, "err: {:?}", parser_result.unwrap_err());
        let program = parser_result.unwrap();
        let eval_result = eval(&program);
        assert_eq!(eval_result.is_ok(), true, "err: {:?}", eval_result.unwrap_err());
        let object = eval_result.unwrap();
        assert_eq!(object, expected);
    }
}