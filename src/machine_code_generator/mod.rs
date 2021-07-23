mod std_win_64;

use std::*;
use collections::{HashMap, HashSet};

use crate::intermediate_code_generator::*;
use crate::config::*;
use crate::parser::ast::IncDecType;
use crate::tokenizer;
use tokenizer::char_to_u64;
use tokenizer::token::{IncOrDec, BinaryRelation};

pub(crate) struct MachineCodeGenerator {
    pub(crate) compiler_options: CompilerOptions,
    intermediate_code: Vec<IntermRepr>,
    pooled_strings: HashMap<String, u64>,
}

fn mangle_global_def(s: &str) -> String {
    "g!".to_owned() + s
}

fn mangle_label(s: &str) -> String {
    ".".to_owned() + s
}

fn internal_label(x: u64) -> String {
    format!(".i!{}", x)
}

fn internal_def(s: &str) -> String {
    format!("c!{}", s)
}

fn pooled_str_label(idx: u64) -> String {
    format!("sp!{}", idx.to_string())
}

const START: &'static str = "start!";

fn pooled_str_end(idx: u64) -> String {
    format!("s!{}.end", idx.to_string())
}

fn readonly_section_or_segment(target: TargetPlatform) -> &'static str {
    match target {
        WIN_64 => {
            "section '.rodata' readable"
        }
        _ => todo!()
    }
}

fn readable_writable_section_or_segment(target: TargetPlatform) -> &'static str {
    match target {
        WIN_64 => {
            "section '.data' readable writeable"
        }
        _ => todo!()
    }
}

fn executable_section_or_segment(target: TargetPlatform) -> &'static str {
    match target {
        WIN_64 => {
            "section '.text' executable"
        }
        _ => todo!()
    }
}

fn declare_word_directive(word_size: u8) -> &'static str {
    match word_size {
        8 => "dq",
        _ => unimplemented!(),
    }
}

impl MachineCodeGenerator {
    pub(crate) fn new(
        compiler_options: CompilerOptions,
        intermediate_code: Vec<IntermRepr>,
        pooled_strings: HashMap<String, u64>,
    ) -> Self {
        MachineCodeGenerator {
            compiler_options,
            intermediate_code,
            pooled_strings,
        }
    }

    pub(crate) fn run(self) -> Vec<String> {
        match self.compiler_options.target_platform.arch {
            Arch::x86_64 => {
                self.generate_x86_64()
            }
        }
    }

    fn generate_std_lib_and_internals(&self, already_defined: HashSet<&str>) -> Vec<String> {
        match self.compiler_options.target_platform {
            WIN_64 => {
                use std_win_64::generate_std_lib_and_internals;
                generate_std_lib_and_internals(already_defined)
            }
            _ => todo!()
        }
    }

    fn pack_string_and_append_eot(&self, s: &str) -> Vec<u64> {
        let mut res = vec![];
        let word_size = self.compiler_options.target_platform.arch.word_size() as usize;
        let it = &mut s.chars();
        loop {
            let mut char: String = it.take(word_size as usize).take_while(|x| *x as u32 != 4)
                .collect();
            if char.len() == word_size {
                res.push(char_to_u64(&char));
            } else {
                char.push(4 as char);
                res.push(char_to_u64(&char));
                break;
            }
        }
        res
    }

    fn align_stack(n: u8) -> String {
        format!("and rsp,-{}", n)
    }

    fn call(&self, nargs: u64) -> Vec<String> {
        let call_conv = self.compiler_options.target_platform.calling_convention();
        let mut res = vec![];
        for i in 0..nargs {
            res.push(format!("pop {}", call_conv.registers_to_pass_args[i as usize]));
        }
        res.append(&mut self.align_and_call(nargs));
        res
    }

    fn align_and_call(&self, n: u64) -> Vec<String> {
        let call_conv = self.compiler_options.target_platform.calling_convention();
        let main_reg = call_conv.main_register;
        let reg_for_calls = call_conv.reg_for_calls;
        let word_size = self.compiler_options.target_platform.arch.word_size();
        let mut res: Vec<String>
            = format!(r"
                mov {reg_for_calls}, rsp
                and rsp, -{alignment}
                i = 0
                stack_var_count = {n} - {param_reg_count}
                if stack_var_count < 0
                    stack_var_count = 0
                end if
                while i <> stack_var_count
                    lea {prev}, [{reg_for_calls} + i*{word_size}]
                    lea {new}, [{new} + {word_size}]
                    mov {prev}, [{prev}]
                    mov [{new}], {prev}
                    i = i + 1
                end while
	            sub rsp, {shadow_space_size}",
                      new = call_conv.supporting_registers[0],
                      prev = call_conv.supporting_registers[1],
                      n = n,
                      word_size = word_size,
                      reg_for_calls = reg_for_calls,
                      alignment = call_conv.alignment,
                      shadow_space_size = call_conv.shadow_space_size_in_bytes,
                      param_reg_count = call_conv.registers_to_pass_args.len())
            .split('\n')
            .filter_map(|x| { if !x.is_empty() { Some(x.trim().to_owned()) } else { None } })
            .collect();
        res.push(format!("shl {},{}", main_reg, (word_size as f64).log2().round()));
        res.push(format!("call {}", main_reg));
        res.push(format!("mov rsp, {}", reg_for_calls));
        res
    }

    fn deref_to_reg(&self, dst: &str, src: &str) -> Vec<String> {
        let word_size = self.compiler_options.target_platform.arch.word_size();
        let mut res = vec![];
        if dst != src {
            res.push(format!("mov {dst},{src}", dst = dst, src = src));
        }
        res.push(format!("shl {},{}", dst, (word_size as f64).log2().round()));
        res.push(format!("mov {},[{0}]", dst));
        res
    }

    fn write_lvalue(&self, addr: &str, val: &str) -> Vec<String> {
        let word_size = self.compiler_options.target_platform.arch.word_size();
        let mut res = vec![];
        res.push(format!("shl {},{}", addr, (word_size as f64).log2().round()));
        res.push(format!("mov [{addr}],{val}", addr = addr, val = val));
        res.push(format!("mov {},[{0}]", addr));
        res
    }

    fn generate_x86_64(self) -> Vec<String> {
        use IntermRepr::*;
        let comp_opts = self.compiler_options;
        let target = comp_opts.target_platform;
        let call_conv = target.calling_convention();
        let main_reg = call_conv.main_register;
        let supp_regs = call_conv.supporting_registers;
        let reg_for_calls = call_conv.reg_for_calls;
        let reg_for_initial_rsp = call_conv.reg_for_initial_rsp;
        let word_size = target.arch.word_size();

        let mut res =
            match target {
                WIN_64 => vec!["format PE64 console".to_owned(),
                               format!("stack {}", comp_opts.stack_size),
                               format!("heap {}", comp_opts.heap_size)],
                TargetPlatform {
                    platform_name: PlatformName::Linux, arch: Arch::x86_64
                } => todo!(),
            };
        res.push(format!("entry {}", START));

        let mut user_defined = HashSet::new();
        for ir in &self.intermediate_code {
            match ir {
                LoadLvalue(lv) => {
                    match lv {
                        Lvalue::NthArg(n) => {
                            if *n >= call_conv.registers_to_pass_args.len() as u64 {
                                res.push(format!("lea {},[({}+{})*{}]",
                                                 main_reg,
                                                 reg_for_initial_rsp,
                                                 call_conv.shadow_space_size_in_bytes + 1 + n,
                                                 word_size))
                            } else {
                                res.push(format!("lea {},[({}+{})*{}]",
                                                 main_reg, reg_for_initial_rsp, n + 1, word_size))
                            }
                        }
                        Lvalue::LocalVar(st) => {
                            match st {
                                StackRange::Exact(n) => {
                                    res.push(format!("lea {},[{}-{}*{}]", main_reg,
                                                     reg_for_initial_rsp, 1 + n, word_size));
                                }
                                StackRange::Span(st) => {
                                    res.push(format!("lea {},[{}-{}*{}]", main_reg,
                                                     reg_for_initial_rsp,
                                                     1 + st.start(), word_size));
                                }
                            }
                        }
                        Lvalue::Label(lbl) => {
                            match lbl {
                                Label::Local(lbl) => {
                                    res.push(format!("mov {},{}", main_reg, mangle_label(&lbl)))
                                }
                                Label::Global(lbl) => {
                                    res.push(format!("mov {},{}", main_reg,
                                                     mangle_global_def(&lbl)))
                                }
                                Label::PooledStr(str_no) => {
                                    res.push(format!("mov {},{}", main_reg,
                                                     pooled_str_label(*str_no)))
                                }
                            }
                        }
                    }
                    res.push(format!("shr {},{}", main_reg, (word_size as f64).log2().round()))
                }
                WriteLvalue => {
                    let supp_reg = supp_regs[0];
                    res.push(format!("pop {}", supp_reg));
                    res.append(&mut self.write_lvalue(supp_reg, main_reg));
                }
                LoadConstant(constant) => {
                    res.push(format!("mov {},{}", main_reg, constant))
                }
                Deref => {
                    res.append(&mut self.deref_to_reg(main_reg, main_reg));
                }
                DeclLabel(lbl) => {
                    res.push(format!("align {}", word_size));
                    res.push(format!("{}:", mangle_label(&lbl)))
                }
                InternalLabel(lbl_id) => {
                    res.push(format!("align {}", word_size));
                    res.push(format!("{}:", internal_label(*lbl_id)))
                }
                Jump(lbl_id) => {
                    res.push(format!("jmp {}", internal_label(*lbl_id)))
                }
                JmpIfEq(lbl_id) => {
                    res.push(format!("je {}", internal_label(*lbl_id)))
                }
                TestAndJumpIfZero(lbl_id) => {
                    res.push(format!("test {},{0}", main_reg));
                    res.push(format!("jz {}", internal_label(*lbl_id)));
                }
                CaseEq { case_val } => { todo!() }
                Save => {
                    res.push(format!("push {}", main_reg))
                }
                BinOp(op) => {
                    match op {
                        BinaryOp::Cmp(rel) => {
                            let supp_reg = supp_regs[0];
                            res.push(format!("pop {}", supp_reg));
                            res.push(format!("cmp {},{}", supp_reg, main_reg));
                            res.push("pushf".to_owned());
                            let command =
                                match rel {
                                    BinaryRelation::Gt => {
                                        "setg"
                                    }
                                    BinaryRelation::Ne => {
                                        "setne"
                                    }
                                    _ => todo!()
                                };
                            res.push(format!("xor {},{0}", main_reg));
                            res.push("popf".to_owned());
                            let lower_byte = main_reg.chars().nth(1).unwrap().to_string() + "l";
                            res.push(format!("{} {}", command, lower_byte));
                        }
                        BinaryOp::Add => {
                            res.push(format!("pop {}", supp_regs[0]));
                            res.push(format!("add {},{}", supp_regs[0], main_reg));
                            res.push(format!("mov {},{}", main_reg, supp_regs[0]));
                        }
                        _ => todo!()
                    }
                }
                LvUnary(un) => {
                    match un.inc_dec_type {
                        IncDecType::Prefix => {
                            let supp_reg = supp_regs[0];
                            res.append(&mut self.deref_to_reg(supp_reg, main_reg));
                            match un.inc_or_dec {
                                IncOrDec::Increment => {
                                    res.push(format!("add {},1", supp_reg));
                                }
                                IncOrDec::Decrement => {
                                    res.push(format!("sub {},1", supp_reg));
                                }
                            }
                            res.append(&mut self.write_lvalue(main_reg, supp_reg));
                        }
                        IncDecType::Postfix => {
                            todo!()
                        }
                    }
                }
                RvUnary(un) => { todo!() }
                Call { nargs } => {
                    res.append(&mut self.call(*nargs))
                }
                Nargs => {
                    res.push(format!("mov {},{}+1*{}", main_reg, reg_for_initial_rsp, word_size))
                }
                Goto => {
                    res.push(format!("shl {},{}", main_reg, (word_size as f64).log2().round()));
                    res.push(format!("jmp {}", main_reg));
                }
                Ret => {
                    res.push(format!("mov rsp,{}", reg_for_initial_rsp));
                    res.push(format!("pop {}", reg_for_initial_rsp));
                    res.push(format!("pop {}", reg_for_calls));
                    res.push("ret".to_owned());
                }
                FnDef(name, info) => {
                    res.push(executable_section_or_segment(target).to_owned());
                    res.push(format!("align {}", word_size));
                    res.push(format!("{}:", mangle_global_def(&name)));

                    res.push(format!("push {}", reg_for_calls));
                    res.push(format!("push {}", reg_for_initial_rsp));
                    res.push(format!("mov {},rsp", reg_for_initial_rsp));
                    res.push(format!("sub rsp,{}*{}", info.stack_size, word_size));
                    user_defined.insert(name.as_str());
                }
                VarOrVecDef(_) => { todo!() }
                Ival(_) => { todo!() }
            }
        }
        if !self.pooled_strings.is_empty() {
            res.push(readonly_section_or_segment(target).to_owned());
            for (s, n) in &self.pooled_strings {
                res.push(format!("align {}", word_size));
                res.push(format!("{}:", pooled_str_label(*n)));
                let s = (|| {
                    let mut res = String::new();
                    let mut first = true;
                    for v in self.pack_string_and_append_eot(s) {
                        if !first {
                            res += ",";
                        }
                        res += &v.to_string();
                        first = false;
                    }
                    res
                })();
                res.push(format!("dq {}", s));
                res.push(format!("{}:", pooled_str_end(*n)))
            }
        }
        res.append(&mut self.generate_std_lib_and_internals(user_defined));
        match self.compiler_options.target_platform {
            WIN_64 => {
                res.extend(r"
                section '.idata' import data readable writeable

                dd 0,0,0,RVA kernel_name,RVA kernel_table
                dd 0,0,0,0,0

                kernel_table:
                ExitProcess dq RVA _ExitProcess
                GetStdHandle dq RVA _GetStdHandle
                ReadFile dq RVA _ReadFile
                WriteFile dq RVA _WriteFile
                GetCommandLineA dq RVA _GetCommandLineA
                dq 0

                kernel_name db 'KERNEL32.DLL',0

                _ExitProcess dw 0
                    db 'ExitProcess',0
                _GetStdHandle dw 0
                    db 'GetStdHandle',0
                _ReadFile dw 0
                    db 'ReadFile',0
                _WriteFile dw 0
                    db 'WriteFile',0
                _GetCommandLineA dw 0
                    db 'GetCommandLineA',0
                ".split('\n')
                    .filter_map(|x| {
                        if !x.is_empty() { Some(x.trim().to_owned()) } else { None }
                    }))
            }
            _ => todo!()
        }
        res
    }
}