mod lexical_analyzer;
mod parser;
mod code_generator;

use std::process;
use std::collections::HashMap;

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum BracketType {
    Round,
    Curly,
    Square,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum LeftOrRight {
    Left,
    Right,
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Constant {
    Octal,
    Decimal,
    Char,
    String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash)]
pub enum Binary {
    Plus,
    Minus,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    Or,
    And,
    Shift(LeftOrRight),
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
pub enum Token {
    ExclamationMark,
    Binary(Binary),
    Assign(Option<Binary>),
    Id(Vec<u8>),
    Constant((Constant, Vec<u8>)),
    Bracket((BracketType, LeftOrRight)),
}

type TypeOfLineNo = usize;
type SymbolTable<'a> = HashMap<&'a str, Token>;

#[derive(Default)]
pub struct CompilerOptions {
    pub integers_are_unsigned: bool,
}

fn generate_error_message_with_line_no(error_str: &str, line_no: TypeOfLineNo) -> String {
    format!("Error: {}. Line: {}", error_str, line_no)
}

fn process_command_line_args_and_return_compiler_options()
    -> Result<(String, CompilerOptions), &'static str> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        return Err("not enough arguments");
    }

    Ok((args[1].clone(), CompilerOptions {
        integers_are_unsigned: false
    }))
}

fn main() {
    let (filename, compiler_options) = process_command_line_args_and_return_compiler_options()
        .unwrap_or_else(|err| {
            eprintln!("Something went wrong parsing arguments: {}", err);
            process::exit(1);
        });

    let source_code = std::fs::read_to_string(filename).unwrap_or_else(|err| {
        eprintln!("Something went wrong reading the file: {}", err);
        process::exit(1);
    }).into_bytes();

    let mut lexical_analyzer = lexical_analyzer::LexicalAnalyzer {
        compiler_options: &compiler_options,
        tokens: Vec::new(),
        reserved: &SymbolTable::new(),
    };
    let tokens = lexical_analyzer.run(&source_code).unwrap_or_else(
        |err| {
            eprintln!("Lexical analyzer returned an error: {}", err);
            process::exit(1);
        });

    let mut parser = parser::Parser {
        compiler_options: &compiler_options,
        syntax_tree: parser::AbstractSyntaxTree::new(),
    };
    let syntax_tree = parser.run(&tokens);
}