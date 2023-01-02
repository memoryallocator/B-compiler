use std::fs::read_to_string;
use std::fs::File;
use std::io::Error as IoError;
use std::io::Write;
use std::process::exit;
use std::process::ExitCode;

use anyhow::{Error, Result};
use itertools::intersperse;

use b_compiler::intermediate_code_generator::IntermediateCodeGenerator;
use b_compiler::parser::Parser;
use b_compiler::tokenizer::Tokenizer;
use b_compiler::utils::Issue::{
    DeclShadowsFnParameter, DeclShadowsGlobalDef, DeclShadowsPrevious, InvalidParameterCount,
    UnnecessaryImport, VecTooManyIvals, VecWithNoSizeAndIvals,
};

fn main() -> Result<String, Error> {
    let config = b_compiler::config::Config::try_parse()?;
    let source_code = read_to_string(config.input).unwrap_or_else(|err| {
        eprintln!("Something went wrong reading the file: {}", err);
        exit(1);
    });

    if !source_code.chars().all(|c| c.is_ascii()) {
        eprintln!("The source code file must be in ASCII");
        exit(1)
    }

    let processor = Tokenizer {
        escape_sequences: &get_escape_sequences(),
        reserved_symbols: &get_reserved_symbols(),
        compiler_options,
    };

    let mut issues = vec![];
    let tokens = processor.run(&source_code, &mut issues);
    let tokens = tokens.unwrap_or_else(|err| {
        eprintln!("Lexical analyzer returned an error: {}", err);
        exit(1);
    });

    let processor = Parser {
        compiler_options,
        source_code: &source_code,
    };

    let ast_to_scopes_mapping = processor.run(&tokens, &mut issues);
    let mut at_least_1_error = false;
    for issue in issues {
        let error = !matches!(
            issue,
            VecWithNoSizeAndIvals(..)
                | VecTooManyIvals { .. }
                | DeclShadowsGlobalDef { .. }
                | UnnecessaryImport { .. }
                | DeclShadowsFnParameter { .. }
                | DeclShadowsPrevious { .. }
                | InvalidParameterCount { .. }
        );
        eprintln!("{}: {}", if error { "error" } else { "warning" }, issue);
        at_least_1_error |= error;
    }
    if at_least_1_error {
        println!("There were more than 0 errors. Terminating");
        exit(1);
    }
    let scope_table = ast_to_scopes_mapping.unwrap();

    let mut processor = IntermediateCodeGenerator::new(compiler_options, &scope_table.global);
    let (intermediate_code, pooled_strings) = processor.run(&scope_table);

    if config.ir {
        return Ok(intersperse(
            intermediate_code.into_iter().map(|ir| format!("{:?}, ir")),
            "\n",
        ));
    }
    let processor = machine_code_generator::MachineCodeGenerator::new(
        compiler_options,
        &intermediate_code,
        pooled_strings,
    );
    let res = processor.run();
    write_to_file_and_exit(res, &output_file);
}

fn print_error_message_and_exit(msg: &str) {
    eprintln!("{msg}");
    exit(ExitCode::FAILURE)
}
