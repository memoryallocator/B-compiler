use crate::config::{Issue, CompilerOptions};
use crate::parser::FlatAst;

pub(crate) struct Analyzer<'a> {
    pub(crate) compiler_options: CompilerOptions,
    pub(crate) source_code: &'a str,
}

impl Analyzer<'_> {
    pub(crate) fn run(&self, program: &FlatAst) -> (Vec<Issue>, Result<(), ()>) {
        todo!()
    }
}