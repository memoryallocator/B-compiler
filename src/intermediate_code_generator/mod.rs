mod intermediate_code;

use std::*;
use collections::{HashMap, HashSet};

use crate::config;
use config::*;
use crate::parser::ScopeTable;

pub(crate) enum IntermediateRepresentation {}

pub(crate) struct IntermediateCodeGenerator {
    pub(crate) compiler_options: CompilerOptions,
}

impl IntermediateCodeGenerator {
    pub(crate) fn run(&self, program: &ScopeTable) -> Vec<IntermediateRepresentation> {
        todo!()
    }
}