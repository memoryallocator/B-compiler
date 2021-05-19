use crate::config::CompilerOptions;

mod x86_64;

pub(crate) struct MachineCodeGenerator {
    pub(crate) compiler_options: CompilerOptions,
}

impl MachineCodeGenerator {
    pub(crate) fn run(&self) -> String {
        todo!()
    }
}