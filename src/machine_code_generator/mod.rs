use std::*;
use collections::HashMap;

use crate::intermediate_code_generator::*;
use crate::config::*;

pub(crate) struct MachineCodeGenerator {
    pub(crate) compiler_options: CompilerOptions,
    intermediate_code: Vec<IntermRepr>,
    pooled_strings: HashMap<String, u64>,
}

impl MachineCodeGenerator {
    pub(crate) fn new(
        compiler_options: CompilerOptions,
        intermediate_code: Vec<IntermRepr>,
        pooled_strings: HashMap<String, u64>,
    ) -> Self {
        MachineCodeGenerator { compiler_options, intermediate_code, pooled_strings }
    }

    pub(crate) fn run(self) -> String {
        match self.compiler_options.target_platform.arch {
            Arch::x86_64 => {
                return self.generate_x86_64();
            }
            _ => unimplemented!()
        }
    }

    fn generate_x86_64(self) -> String {
        use IntermRepr::*;
        let mut res = String::new();
        for ir in self.intermediate_code {
            match ir {
                LoadLvalue(lv) => {
                    match lv {
                        Lvalue::NthArg(_) => {}
                        Lvalue::NthLocalVar(_) => {}
                        Lvalue::Label(_) => {}
                    }
                }
                WriteLvalue => {}
                LoadConstant(constant) => {}
                Deref => {}
                DeclLabel(lbl) => {}
                InternalLabel(lbl_id) => {}
                Jump(lbl_id) => {}
                JmpIfEq(lbl_id) => {}
                TestAndJumpIfZero(lbl_id) => {}
                CaseEq { case_val } => {}
                Save => {}
                BinOp(op) => {}
                LvUnary(un) => {}
                RvUnary(un) => {}
                SetNthArg(n) => {}
                Call { nargs: arg_count } => {}
                Goto => {}
                Ret => {}
                FnDef(_, _) => {}
                VarOrVecDef(_) => {}
                Ival(_) => {}
            }
        }
        res
    }
}