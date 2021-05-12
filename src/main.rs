use std::*;
use process;

use crate::config::*;

mod lexical_analyzer;
mod parser;
mod config;
mod code_generator;
mod ast;

fn process_command_line_arguments()
    -> Result<(String, CompilerOptions), String> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        return Err("not enough arguments".to_string());
    }

    let mut target_platform = Option::<TargetPlatform>::None;
    let target_pattern = "--target=";

    let mut i: usize = 2;
    while i < args.len() {
        let option_str = &args[i];
        if option_str.starts_with(target_pattern) {
            let target = &args[i][target_pattern.len()..];
            match target {
                "native" => {
                    target_platform = Some(TargetPlatform::native());
                }
                _ => {
                    return Err(format!("unknown target: {}", target));
                }
            }
        } else {
            return Err(format!("unknown compiler option: {}", option_str));
        }
        i += 1;
    }

    let target_platform = target_platform.unwrap_or(TargetPlatform::native());

    Ok((args[1].clone(), CompilerOptions { target_platform }))
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

    assert!(source_code.chars().all(|c| c.is_ascii()), "The source code file must be in ASCII");

    let mut lexical_analyzer = lexical_analyzer::LexicalAnalyzer {
        compiler_options: &compiler_options,
        escape_sequences: &get_escape_sequences(),
        reserved_symbols: &get_reserved_symbols(),
    };

    let tokens = lexical_analyzer.run(&source_code);
    mem::drop(lexical_analyzer);

    let tokens = tokens.unwrap_or_else(
        |err| {
            eprintln!("Lexical analyzer returned an error: {}", err);
            process::exit(1);
        });

    let standard_library_names = &get_standard_library_names();

    let mut parser = parser::Parser {
        compiler_options,
        source_code: Some(&source_code),
        standard_library_names,
    };

    let ast = parser.run(&tokens);
    mem::drop(parser);

    let ast = ast.unwrap_or_else(
        |err| {
            eprintln!("Parser returned an error: {}", err);
            process::exit(1);
        });

    let code_generator = code_generator::CodeGenerator {
        compiler_options,
        standard_library_names,
        source_code: Some(&*source_code),
    };

    let res = code_generator.run(&ast);
    mem::drop(code_generator);

    for warning in res.0 {
        eprintln!("{}", warning);
    }

    let res = res.1.unwrap_or_else(
        |err| {
            eprintln!("Code generator returned an error: {}", err);
            process::exit(1);
        });

    println!("{}", &res);
}
