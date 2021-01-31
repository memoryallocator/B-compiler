use std::cell::Cell;
use std::fs::read;
use std::iter::FromIterator;
use std::process;

use config::CompilerOptions;
use config::TypeOfLineNo;

mod lexical_analyzer;
mod parser;
mod code_generator;
mod config;
mod token;

fn generate_error_message_with_line_no<S: AsRef<str>>(
    error_str: S,
    line_no: TypeOfLineNo,
) -> String {
    format!("Error: {}. Line: {}", error_str.as_ref(), line_no)
}

fn process_command_line_args_to_get_filename_and_compiler_options()
    -> Result<(String, CompilerOptions), &'static str> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        return Err("not enough arguments");
    }

    if args.len() > 2 {
        return Err("compiler options are not supported yet");
    }

    Ok((args[1].clone(), CompilerOptions::default()))
}

fn main() {
    let (filename, compiler_options) = process_command_line_args_to_get_filename_and_compiler_options()
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
        second_symbol_of_escape_sequence_to_character_mapping: Default::default(),
    };
    let tokens = lexical_analyzer.run(&source_code).unwrap_or_else(
        |err| {
            eprintln!("Lexical analyzer returned an error: {}", err);
            process::exit(1);
        });

    for (token, _) in tokens.clone() {
        dbg!(token);
    }

    let mut parser = parser::Parser {
        compiler_options: &compiler_options,
        syntax_tree: parser::AbstractSyntaxTree::new(),
    };
    let syntax_tree = parser.run(&tokens);
}
