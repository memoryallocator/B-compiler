use std::fs::read_to_string;

use anyhow::{Error, Result};
use itertools::Itertools;

use b_compiler::config::Config;
use b_compiler::intermediate_code_generator::IntermediateCodeGenerator;
use b_compiler::machine_code_generator::MachineCodeGenerator;
use b_compiler::parser::Parser;
use b_compiler::tokenizer::Tokenizer;
use b_compiler::utils::Issue::{
    DeclShadowsFnParameter, DeclShadowsGlobalDef, DeclShadowsPrevious, InvalidParameterCount,
    UnnecessaryImport, VecTooManyIvals, VecWithNoSizeAndIvals,
};

fn main() -> Result<()> {
    let config = Config::try_parse()?;
    let source_code = match read_to_string(config.input) {
        Ok(source_code) => source_code,
        Err(err) => {
            return Err(Error::msg(format!(
                "failed to read the file {}: {err}",
                config.input
            )))
        }
    };

    if !source_code.chars().all(|c| c.is_ascii()) {
        return Err(Error::msg("the source code file must be in ASCII"));
    }

    let processor = Tokenizer {
        escape_sequences: &get_escape_sequences(),
        reserved_symbols: &get_reserved_symbols(),
        compiler_options: config,
    };

    let mut issues = vec![];
    let tokens = processor.run(&source_code, &mut issues);
    let tokens = match tokens {
        Ok(tokens) => tokens,
        Err(err) => {
            return Err(Error::msg(format!(
                "lexical analyzer returned an error: {err}"
            )));
        }
    };

    let processor = Parser {
        compiler_options: config,
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
        return Err(Error::msg(format!(
            "there were more than 0 errors. Terminating"
        )));
    }
    let scope_table = ast_to_scopes_mapping.unwrap();

    let mut processor = IntermediateCodeGenerator::new(config, &scope_table.global);
    let (intermediate_code, pooled_strings) = processor.run(&scope_table);

    if config.ir {
        println!(
            "{}",
            intermediate_code
                .into_iter()
                .map(|ir| format!("{:?}", ir))
                .join("\n")
        );
        return Ok(());
    }
    let processor = MachineCodeGenerator::new(config, &intermediate_code, pooled_strings);
    let res = processor.run();
    println!("{}", res.join("\n"));
    Ok(())
}
