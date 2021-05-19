use std::*;
use process;

use crate::config::*;

mod lexical_analyzer;
mod parser;
mod config;
mod intermediate_code_generator;
mod machine_code_generator;

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

    if !source_code.chars().all(|c| c.is_ascii()) {
        eprintln!("The source code file must be in ASCII");
        process::exit(1)
    }

    let processor = lexical_analyzer::LexicalAnalyzer {
        compiler_options: &compiler_options,
        escape_sequences: &get_escape_sequences(),
        reserved_symbols: &get_reserved_symbols(),
    };

    let tokens = processor.run(&source_code);
    let tokens = tokens.unwrap_or_else(
        |err| {
            eprintln!("Lexical analyzer returned an error: {}", err);
            process::exit(1);
        });

    let processor = parser::Parser {
        compiler_options,
        source_code: &source_code,
        standard_library_names: &get_standard_library_names(),
    };

    let (issues, ast) = processor.run(&tokens);
    let ast = ast.unwrap_or_else(
        |()| {
            eprintln!("Cannot proceed");
            process::exit(1);
        });

    let processor = intermediate_code_generator::IntermediateCodeGenerator {
        compiler_options,
    };
    let intermediate_code = processor.run(&ast);

    let processor = machine_code_generator::MachineCodeGenerator {
        compiler_options,
    };
    let res = processor.run();

    println!("{}", &res);
}
