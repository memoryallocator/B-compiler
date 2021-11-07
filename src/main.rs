use io::Write;
use process::exit;
use std::*;

use crate::config::*;

mod config;
mod intermediate_code_generator;
mod machine_code_generator;
mod parser;
mod tokenizer;

fn write_to_file_and_exit(data: Vec<String>, file: &str) -> ! {
    let ok: Result<(), io::Error> = (|| {
        let mut file = fs::File::create(file)?;
        file.write_all(data.join("\n").as_bytes())?;
        exit(0);
    })();
    if let Err(err) = ok {
        eprintln!("Something went wrong writing the output file: {}", err);
        eprintln!("\n\n\n");
        for line in data {
            println!("{}", line)
        }
        exit(1);
    } else {
        unreachable!()
    }
}

fn parse_command_line_arguments() -> Result<(String, CompilerOptions, String), String> {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        return Err("not enough arguments".to_owned());
    }

    let mut out_path = "a.asm".to_owned();
    let mut opts = CompilerOptions::default();
    let stack_pat = "--stack=";
    let heap_pat = "--heap=";
    let enable_continue_pat = "--enable-continue";
    let platform_pat = "--platform=";
    let arch_pat = "--arch=";
    let ir_pat = "--ir";
    let out_pat = "--out=";

    let mut path = None;
    let mut i: usize = 1;
    while i < args.len() {
        let curr = &args[i];
        if !curr.starts_with('-') {
            if path.is_some() {
                return Err("unknown option: ".to_owned() + curr);
            }
            path = Some(curr);
            i += 1;
            continue;
        }
        if let Some(out) = curr.strip_prefix(out_pat) {
            out_path = out.to_owned();
        } else if curr == ir_pat {
            opts.mode = Mode::Ir;
        } else if curr.starts_with(enable_continue_pat) {
            opts.continue_is_enabled = true;
        } else if let Some(heap_sz) = curr.strip_prefix(heap_pat) {
            let heap_size = heap_sz.parse::<u64>();
            if let Ok(heap_size) = heap_size {
                opts.heap_size = heap_size;
            } else {
                return Err("failed to parse heap size".to_owned());
            }
        } else if let Some(stack_sz) = curr.strip_prefix(stack_pat) {
            let stack_size = stack_sz.parse::<u64>();
            if let Ok(stack_size) = stack_size {
                opts.stack_size = stack_size;
            } else {
                return Err("failed to parse stack size".to_owned());
            }
        } else if let Some(platform) = curr.strip_prefix(platform_pat) {
            let platform = platform.to_ascii_lowercase();
            opts.target_platform.platform_name = match platform.as_ref() {
                "linux" => PlatformName::Linux,
                "win" => PlatformName::Windows,
                _ => {
                    unimplemented!(
                        "The platform {} you specified \
                                    is either unknown or unsupported",
                        platform
                    )
                }
            };
        } else if let Some(arch) = curr.strip_prefix(arch_pat) {
            let arch = arch.to_ascii_lowercase();
            opts.target_platform.arch = match arch.as_ref() {
                "x86-64" | "x64" | "x86_64" | "amd64" => Arch::x86_64,
                _ => {
                    unimplemented!(
                        "The architecture {} you specified \
                                    is either unknown or unsupported",
                        arch
                    )
                }
            };
        } else {
            return Err("unknown option: ".to_owned() + curr);
        }
        i += 1;
    }
    if out_path.is_empty() {
        return Err("the output file path cannot be empty".to_owned());
    }
    if let Some(path) = path {
        Ok((path.clone(), opts, out_path))
    } else {
        Err("no input path specified".to_owned())
    }
}

fn main() {
    let (filename, compiler_options, output_file) =
        parse_command_line_arguments().unwrap_or_else(|err| {
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
    let tokens = tokens.unwrap_or_else(|err| {
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
        process::exit(1);
    }
    let scope_table = ast_to_scopes_mapping.unwrap();

    let mut processor = intermediate_code_generator::IntermediateCodeGenerator::new(
        compiler_options,
        &scope_table.global,
    );
    let (intermediate_code, pooled_strings) = processor.run(&scope_table);

    if compiler_options.mode == Mode::Ir {
        write_to_file_and_exit(
            intermediate_code
                .into_iter()
                .map(|x| format!("{:?}", x))
                .collect(),
            &output_file,
        )
    }
    let processor = machine_code_generator::MachineCodeGenerator::new(
        compiler_options,
        &intermediate_code,
        pooled_strings,
    );
    let res = processor.run();
    write_to_file_and_exit(res, &output_file);
}
