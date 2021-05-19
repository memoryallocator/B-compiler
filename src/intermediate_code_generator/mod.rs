mod intermediate_code;

use std::*;
use collections::{HashMap, HashSet};

use crate::config;
use config::*;
use crate::parser::FlatAst;

use crate::intermediate_code_generator::intermediate_code::IntermediateRepresentation;

pub(crate) struct IntermediateCodeGenerator {
    pub(crate) compiler_options: CompilerOptions,
}

impl IntermediateCodeGenerator {
    pub(crate) fn run(&self, program: &FlatAst) -> Vec<IntermediateRepresentation> {
        todo!()
    }
}
