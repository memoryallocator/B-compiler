use std::*;
use process;

use crate::config::*;

mod tokenizer;
mod parser;
mod config;
mod intermediate_code_generator;
mod machine_code_generator;

fn process_command_line_arguments() -> Result<(String, CompilerOptions), String> {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        return Err("not enough arguments".to_string());
    }

    let mut options = CompilerOptions::default();
    let target_pattern = "--target=";

    let mut i: usize = 2;
    while let Some(option_str) = args.get(i) {
        let option_str = &args[i];
        if option_str.starts_with(target_pattern) {
            let target = &args[i][target_pattern.len()..];
            match target {
                "native" => {
                    options.target_platform = TargetPlatform::native();
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
    Ok((args[1].clone(), options))
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

    let processor = tokenizer::Tokenizer {
        escape_sequences: &get_escape_sequences(),
        reserved_symbols: &get_reserved_symbols(),
        compiler_options,
    };

    let mut issues = vec![];
    let tokens = processor.run(&source_code, &mut issues);
    let tokens = tokens.unwrap_or_else(
        |err| {
            eprintln!("Lexical analyzer returned an error: {}", err);
            process::exit(1);
        });

    let processor = parser::Parser {
        compiler_options,
        source_code: &source_code,
    };

    let ast_to_scopes_mapping = processor.run(&tokens, &mut issues);
    let scope_table = ast_to_scopes_mapping.unwrap_or_else(
        |()| {
            eprintln!("Cannot proceed");
            process::exit(1);
        });

    let mut at_least_1_error = false;
    for issue in issues {
        use Issue::*;
        let error = match issue {
            BracketNotOpened(_) | BracketNotClosed(_)
            | EmptyTokenStream | ParsingError
            | NameNotDefined { .. } | NameRedefined { .. } | NameRedeclared { .. }
            | VecSizeIsNotANumber { .. } | LiteralTooLong(_)
            | UnexpectedKeyword(_) | CaseEncounteredTwice(_)
            | NameHasNoRvalue(_, _) | ExternSymbolNotFound { .. }
            | NoMainFn => true,
            VecWithNoSizeAndInits(_, _) | VecTooManyIvals { .. }
            | DeclShadowsGlobalDef { .. } | UnnecessaryImport { .. }
            | DeclShadowsFnParameter { .. } | DeclShadowsPrevious { .. } => false,
        };
        eprintln!("{}: {}", if error { "error" } else { "warning" }, issue);
        at_least_1_error |= error;
    }
    if at_least_1_error {
        println!("There were more than 0 errors. Terminating");
        process::exit(1);
    }

    let mut processor
        = intermediate_code_generator::IntermediateCodeGenerator::new(compiler_options,
                                                                      &scope_table.global);
    let (intermediate_code, pooled_strings) = processor.run(&scope_table);
    dbg!(&intermediate_code);

    let processor = machine_code_generator::MachineCodeGenerator::new(compiler_options,
                                                                      intermediate_code,
                                                                      pooled_strings);
    let res = processor.run();

    println!("{}", &res);
}
