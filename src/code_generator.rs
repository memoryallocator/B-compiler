use std::*;
use collections::{HashMap, HashSet};

use crate::config;
use config::*;
use crate::ast;
use ast::*;

pub(crate) struct CodeGenerator<'a> {
    pub(crate) compiler_options: CompilerOptions,
    pub(crate) standard_library_names: &'a HashSet<StandardLibraryName>,
    pub(crate) source_code: Option<&'a str>,
}

impl CodeGenerator<'_> {
    fn flatten_ast(program_node: &ProgramNode) {

    }

    fn generate_code() {}

    pub(crate) fn run(&self, program_node: &ProgramNode) -> (Vec<Issue>, Result<String, Issue>) {
        struct StringPool {
            data: HashMap<String, usize>,
            prefix: &'static str,
        }

        let mut string_pool = StringPool {
            data: Default::default(),
            prefix: "..@STR_POOL",
        };

        fn reserve_word(compiler_options: CompilerOptions) -> &'static str {
            match compiler_options.target_platform.arch {
                Arch::x86_32 => "resd",
                Arch::x86_64 => "resq"
            }
        }

        fn decl_word(compiler_options: CompilerOptions) -> &'static str {
            match compiler_options.target_platform.arch {
                Arch::x86_32 => "dd",
                Arch::x86_64 => "dq"
            }
        }

        fn mangle_name(s: &str) -> String {
            let s = s.replace('.', "@");
            "$".to_owned() + &*s
        }

        let mut res = String::new();
        todo!()
    }
}
