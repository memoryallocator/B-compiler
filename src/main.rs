use std::borrow::Borrow;
use std::process;

use crate::config::{get_default_symbols, get_second_symbol_of_escape_sequence_to_character_mapping};
use crate::config::CompilerOptions;

mod lexical_analyzer;
mod parser;
mod config;
mod token;
mod symbol;
mod ast;

fn generate_error_message_with_pos<T: Borrow<str>, U: Borrow<lexical_analyzer::TokenPos>>(
    error_str: T,
    pos: U,
) -> String {
    format!("Error at {}: {}", pos.borrow(), error_str.borrow())
}

fn process_command_line_arguments()
    -> Result<(String, CompilerOptions), &'static str> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        return Err("not enough arguments");
    }

    let mut comp_opts = CompilerOptions::default();
    let target_pattern = "--target=";

    let mut i: usize = 2;
    while i < args.len() {
        if args[i].starts_with(target_pattern) {
            match &args[i][target_pattern.len()..] {
                "native" => {
                    comp_opts = CompilerOptions::default();
                }
                _ => ()
            }
        }
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
    });

    assert!(source_code.chars().all(|c| c.is_ascii()), "Source code file must be in ASCII");

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
        compiler_options,
        source_code: Some(&source_code),
        symbol_table: get_default_symbols(),
    };
    let code = parser.run(&tokens).unwrap_or_else(
        |err| {
            eprintln!("Parser returned an error: {}", err);
            process::exit(1);
        });

    dbg!(&code);
}
