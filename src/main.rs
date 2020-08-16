extern crate termion;
#[allow(unused_imports)]
#[macro_use]
extern crate matches;

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
const PROMPT2: &str = ".. ";

fn new_line(stdout: &mut io::Stdout) {
    write!(stdout, "\n").unwrap();
    let (_, row) = stdout.cursor_pos().unwrap();
    write!(stdout, "{}{}", termion::clear::CurrentLine, termion::cursor::Goto(1, row)).unwrap();
}

fn redraw(stdout: &mut io::Stdout, indented: bool, line: &String) {
    let (_, row) = stdout.cursor_pos().unwrap();
    write!(stdout, "{}{}", termion::clear::CurrentLine, termion::cursor::Goto(1, row)).unwrap();
    if indented {
        write!(stdout, "{}{}", PROMPT2, line).unwrap();
    } else {
        write!(stdout, "{}{}", PROMPT, line).unwrap();
    }
    stdout.flush().unwrap();
}

fn read_next(stdout: &mut io::Stdout, history: &mut Vec<String>) -> (String, bool) {
    let stdin = io::stdin();
    let mut input = String::new();
    let mut line = String::new();
    let mut indented = false;
    let mut history_idx = history.len();
    let mut tmp = String::new();
    let mut column = 0;

    redraw(stdout, indented, &line);
    for c in stdin.keys() {
        match c.unwrap() {
            Key::Backspace if column > 0 => {
                column -= 1;
                line.remove(column);
            },
            Key::Ctrl('d') => return (input, true),
            Key::Ctrl('c') => {
                new_line(stdout);
                return read_next(stdout, history);
            },
            Key::Ctrl('u') => {
                line.clear();
                column = 0;
            },
            Key::Ctrl('p') if history_idx > 0 => {
                if history_idx == history.len() {
                    tmp = line.clone();
                }
                history_idx -= 1;
                line = history[history_idx].clone();
                column = line.len();
            },
            Key::Ctrl('n') if history_idx <  history.len() => {
                history_idx += 1;
                if history_idx == history.len() {
                    line = tmp.clone();
                } else {
                    line = history[history_idx].clone();
                }
                column = line.len();
            },
            Key::Char('\n') => {
                new_line(stdout);
                if history_idx == history.len() {
                    history_idx += 1;
                }
                history.push(line.clone());
                input.push_str(" ");
                input.push_str(&line);
                match line.chars().last() {
                    Some(';') => break,
                    _ => indented = true,
                }
                line.clear();
                column = 0;
            }
            Key::Char(c) => {
                column += 1;
                line.push(c.clone());
            },
            _ => (),
        };
        redraw(stdout, indented, &line);
    }
    (input, false)
}

fn main() {
    let env = Env::new();
    let mut stdout = io::stdout().into_raw_mode().unwrap();

    let mut history = Vec::new();
    loop {
        let (input, quit) = read_next(&mut stdout, &mut history);
        if quit {
            break;
        }
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        match parser.parse_program() {
            Ok(program) => match eval_with_env(&program, &env) {
                Ok(obj) => write!(stdout, "{}", obj),
                Err(err) => write!(stdout, "Eval error: {:?}", err),
            },
            Err(err) => write!(stdout, "Parsing error: {:?}", err),
        }.unwrap();
        new_line(&mut stdout);
    }
}
