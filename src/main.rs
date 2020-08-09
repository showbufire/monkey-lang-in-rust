extern crate termion;

mod lexer;
mod parser;
mod ast;
mod evaluator;

use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::evaluator::{eval_with_env,Env};

use std::io::{self, Write};

use termion::input::TermRead;
use termion::raw::IntoRawMode;
use termion::event::Key;
use termion::cursor::DetectCursorPos;

const PROMPT: &str = ">> ";

fn new_line(stdout: &mut io::Stdout) {
    write!(stdout, "\n").unwrap();
    let (_, y) = stdout.cursor_pos().unwrap();
    write!(stdout, "{}{}", termion::clear::CurrentLine, termion::cursor::Goto(1, y)).unwrap();
    stdout.flush().unwrap();
}

fn read_single_line(stdout: &mut io::Stdout) -> (String, bool) {
    let stdin = io::stdin();
    write!(stdout, "{}", PROMPT).unwrap();
    stdout.flush().unwrap();
    let mut input = String::new();
    for c in stdin.keys() {
        match c.unwrap() {
            Key::Ctrl('d') => return (String::from(""), true),
            Key::Ctrl('c') => {
                new_line(stdout);
                return read_single_line(stdout);
            },
            Key::Backspace => {
                write!(stdout, "{} {}", termion::cursor::Left(1), termion::cursor::Left(1)).unwrap();
                input.pop();
            },
            Key::Char('\n') => {
                new_line(stdout);
                break;
            }
            Key::Char(c) => {
                input.push(c.clone());
                write!(stdout, "{}", c).unwrap();
            },
            _ => (),
        }
        stdout.flush().unwrap();
    }
    (input, false)
}

fn main() {
    let env = Env::new();
    let mut stdout = io::stdout().into_raw_mode().unwrap();

    loop {
        let (input, quit) = read_single_line(&mut stdout);
        if quit {
            break;
        }
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        match parser.parse_program() {
            Ok(program) => match eval_with_env(&program, &env) {
                Ok(obj) => write!(stdout, "{:?}", obj),
                Err(err) => write!(stdout, "Eval error: {:?}", err),
            },
            Err(err) => write!(stdout, "Parsing error: {:?}", err),
        }.unwrap();
        new_line(&mut stdout);
    }
}
