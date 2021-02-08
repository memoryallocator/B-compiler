use std::process;

use crate::config::{get_default_symbols, get_second_symbol_of_escape_sequence_to_character_mapping};
use crate::config::CompilerOptions;
use crate::config::TypeOfLineNo;

mod lexical_analyzer;
mod parser;
mod code_generator;
mod config;
mod token;
mod grammar;
mod symbol;

fn generate_error_message_with_line_no<S: AsRef<str>>(
    error_str: S,
    line_no: TypeOfLineNo,
) -> String {
    format!("Error: {}. Line: {}", error_str.as_ref(), line_no.to_string())
}

fn process_command_line_arguments()
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
    let (filename, compiler_options) = process_command_line_arguments()
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
        second_symbol_of_escape_sequence_to_character_mapping: &get_second_symbol_of_escape_sequence_to_character_mapping(),
        keywords: &get_default_symbols(),
    };
    let tokens = lexical_analyzer.run(&source_code).unwrap_or_else(
        |err| {
            eprintln!("Lexical analyzer returned an error: {}", err);
            process::exit(1);
        });

    let mut parser = parser::Parser {
        compiler_options: &compiler_options,
        source_code: Some(&source_code),
        symbol_table: &get_default_symbols(),
    };
    let (syntax_tree, symbol_table) = parser.run(&tokens).unwrap_or_else(
        |err| {
            eprintln!("Parser returned an error: {}", err);
            process::exit(1);
        });

    dbg!(&syntax_tree);
}
