use std::*;
use process;
use fs;
use io::Write;

use crate::config::*;

mod tokenizer;
mod parser;
mod config;
mod intermediate_code_generator;
mod machine_code_generator;

fn parse_command_line_arguments() -> Result<(String, CompilerOptions, String), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err("not enough arguments".to_string());
    }
    Ok((args[1].clone(), CompilerOptions::default(), "a.asm".to_string()))
}

fn main() {
    let (filename, compiler_options, output_file) = parse_command_line_arguments()
        .unwrap_or_else(|err| {
            eprintln!("Something went wrong parsing arguments: {}", err);
            process::exit(1);
        });

    let source_code = fs::read_to_string(filename).unwrap_or_else(|err| {
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
    let mut at_least_1_error = false;
    for issue in issues {
        use Issue::*;
        let error = match issue {
            VecWithNoSizeAndInits(..) | VecTooManyIvals { .. }
            | DeclShadowsGlobalDef { .. } | UnnecessaryImport { .. }
            | DeclShadowsFnParameter { .. } | DeclShadowsPrevious { .. }
            | InvalidParameterCount { .. } => false,

            _ => true,
        };
        eprintln!("{}: {}", if error { "error" } else { "warning" }, issue);
        at_least_1_error |= error;
    }
    if at_least_1_error {
        println!("There were more than 0 errors. Terminating");
        process::exit(1);
    }
    let scope_table = ast_to_scopes_mapping.unwrap();

    let mut processor
        = intermediate_code_generator::IntermediateCodeGenerator::new(compiler_options,
                                                                      &scope_table.global);
    let (intermediate_code, pooled_strings) = processor.run(&scope_table);

    let processor = machine_code_generator::MachineCodeGenerator::new(compiler_options,
                                                                      intermediate_code,
                                                                      pooled_strings);
    let res = processor.run();

    let ok: Result<(), io::Error> = (|| {
        let mut output_file = fs::File::create(output_file)?;
        for line in &res {
            output_file.write(line.as_bytes())?;
            output_file.write(&['\n' as u8])?;
        }
        Ok(())
    })();
    if let Err(err) = ok {
        eprintln!("Something went wrong writing the output file: {}", err);
        eprintln!("\n\n\n");
        for line in res {
            println!("{}", line)
        }
        process::exit(1);
    }
}
