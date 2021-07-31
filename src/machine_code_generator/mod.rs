mod std_win_64;

use std::*;
use collections::{HashMap, HashSet};
use ops::Div;

use crate::intermediate_code_generator;
use intermediate_code_generator::*;
use crate::config::*;
use crate::parser::ast::IncDecType;
use crate::tokenizer;
use tokenizer::char_to_u64;
use tokenizer::token::{IncOrDec, BinaryRelation, LeftOrRight};

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

    fn get_lvalue_offset_in_bits(&self) -> u64 {
        (self.compiler_options.target_platform.arch.word_size() as f64).log2().round() as u64
    }

    fn align_and_call(&self, nargs: u64) -> Vec<String> {
        let call_conv = self.compiler_options.target_platform.calling_convention();
        let mut res = vec![];
        let regs_for_args = &call_conv.registers_to_pass_args;
        for i in 0..cmp::min(nargs as usize, regs_for_args.len()) {
            res.push(format!("pop {}", regs_for_args[i as usize]));
        }

        let main_reg = call_conv.main_register;
        let reg_for_calls = call_conv.reg_for_calls;
        let word_size = self.compiler_options.target_platform.arch.word_size();
        res.append(&mut format!(r"
                mov {reg_for_calls}, rsp
                and rsp, -{alignment}
                i = 0
                stack_var_count = {nargs} - {param_reg_count}
                while i < stack_var_count
                    lea {prev}, [{reg_for_calls} + i*{word_size}]
                    lea {new}, [{new} + {word_size}]
                    mov {prev}, [{prev}]
                    mov [{new}], {prev}
                    i = i + 1
                end while
	            sub rsp, {shadow_space_size}",
                                new = call_conv.supporting_registers[0],
                                prev = call_conv.supporting_registers[1],
                                nargs = nargs,
                                word_size = word_size,
                                reg_for_calls = reg_for_calls,
                                alignment = call_conv.alignment,
                                shadow_space_size = call_conv.shadow_space_size_in_bytes,
                                param_reg_count = call_conv.registers_to_pass_args.len())
            .split('\n')
            .filter_map(|x| { if !x.is_empty() { Some(x.trim().to_owned()) } else { None } })
            .collect());
        res.push(format!("rol {},{}", main_reg, self.get_lvalue_offset_in_bits()));
        res.push(format!("call {}", main_reg));
        res.push(format!("mov rsp, {}", reg_for_calls));
        res
    }

    fn deref_to_reg(&self, dst: &str, src: &str) -> Vec<String> {
        let mut res = vec![];
        if dst != src {
            res.push(format!("mov {dst},{src}", dst = dst, src = src));
        }
        res.push(format!("rol {},{}", dst, self.get_lvalue_offset_in_bits()));
        res.push(format!("mov {},[{0}]", dst));
        res
    }

    fn write_lvalue(&self, addr: &str, val: &str) -> Vec<String> {
        let mut res = vec![];
        res.push(format!("rol {},{}", addr, self.get_lvalue_offset_in_bits()));
        res.push(format!("mov [{addr}],{val}", addr = addr, val = val));
        res.push(format!("mov {},[{0}]", addr));
        res
    }

    fn get_lower_byte_of_reg(&self, reg: &str) -> String {
        match self.compiler_options.target_platform.arch {
            Arch::x86_64 => reg.chars().nth(1).unwrap().to_string() + "l",
        }
    }

    fn bin_op_to_machine_code(&self, bin_op: BinaryOp) -> Vec<String> {
        let call_conv = self.compiler_options.target_platform.calling_convention();
        let main_reg = call_conv.main_register;
        let supp_reg = call_conv.supporting_registers[0];

        let mut res = vec![];
        res.push(format!("pop {}", supp_reg));
        match bin_op {
            BinaryOp::Cmp(rel) => {
                res.push(format!("cmp {},{}", supp_reg, main_reg));
                res.push("pushf".to_owned());
                res.push(format!("xor {},{0}", main_reg));
                res.push("popf".to_owned());
                let lower_byte = self.get_lower_byte_of_reg(main_reg);
                let command =
                    "set".to_owned() + match rel {
                        BinaryRelation::Gt => "g",
                        BinaryRelation::Ne => "ne",
                        BinaryRelation::Eq => "e",
                        BinaryRelation::Lt => "l",
                        BinaryRelation::Ge => "ge",
                        BinaryRelation::Le => "le",
                    };
                res.push(format!("{} {}", command, lower_byte));
            }

            BinaryOp::Div | BinaryOp::Mod => {
                res.push("push rdx".to_owned());
                res.push("xor rdx,rdx".to_owned());
                res.push(format!("xchg {},{}", supp_reg, main_reg));
                res.push(format!("idiv {}", supp_reg));
                match bin_op {
                    BinaryOp::Div => (),
                    BinaryOp::Mod => res.push(format!("mov {},rdx", main_reg)),
                    _ => unreachable!()
                }
                res.push("pop rdx".to_owned());
            }

            BinaryOp::Shift(l_or_r) => {
                res.push("push rcx".to_owned());
                res.push(format!("mov rcx,{}", main_reg));
                match l_or_r {
                    LeftOrRight::Left => res.push(format!("shl {},cl", supp_reg)),
                    LeftOrRight::Right => res.push(format!("shr {},cl", supp_reg)),
                }
                res.push(format!("mov {},{}", main_reg, supp_reg));
                res.push("pop rcx".to_owned());
            }

            BinaryOp::LogicalAnd => {
                let snd_supp = call_conv.supporting_registers[1];
                res.push(format!("test {},{0}", main_reg));
                res.push("pushf".to_owned());
                res.push(format!("xor {},{0}", main_reg));
                res.push("popf".to_owned());

                let main_lower_byte = self.get_lower_byte_of_reg(main_reg);
                res.push(format!("setnz {}", main_lower_byte));
                res.push(format!("mov {},{}", snd_supp, main_reg));

                res.push(format!("test {},{0}", supp_reg));
                res.push("pushf".to_owned());
                res.push(format!("xor {},{0}", supp_reg));
                res.push("popf".to_owned());
                res.push(format!("setnz {}", main_lower_byte));

                res.push(format!("push {}", snd_supp));
                res.append(&mut self.bin_op_to_machine_code(BinaryOp::BitwiseAnd));
            }

            b => {
                res.push(match b {
                    BinaryOp::Or => "or",
                    BinaryOp::Xor => "xor",
                    BinaryOp::Add => "add",
                    BinaryOp::Sub => "sub",
                    BinaryOp::Mul => "imul",
                    BinaryOp::BitwiseAnd => "and",
                    _ => unreachable!()
                }.to_owned() + &format!(" {},{}", supp_reg, main_reg));
                res.push(format!("mov {},{}", main_reg, supp_reg));
            }
        }
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
        let reg_for_initial_rsp_minus_2_words = call_conv.reg_for_initial_rsp;
        let regs_for_args = &call_conv.registers_to_pass_args;
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
                            res.push(format!(
                                "lea {main_reg},[{reg_for_initial_rsp_minus_2_words}+{word_size}*(2+1+1+{n})]",
                                main_reg = main_reg,
                                reg_for_initial_rsp_minus_2_words = reg_for_initial_rsp_minus_2_words,
                                n = n, word_size = word_size))
                        }
                        Lvalue::LocalVar(st) => {
                            res.push(format!("lea {},[{}-(1+{offset_in_words})*{}]", main_reg,
                                             reg_for_initial_rsp_minus_2_words, word_size,
                                             offset_in_words = match st {
                                                 StackRange::Exact(n) => n,
                                                 StackRange::Span(st) => st.end()
                                             }));
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
                    res.push(format!("ror {},{}", main_reg, self.get_lvalue_offset_in_bits()))
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
                CaseEq { case_val } => {
                    res.push(format!("cmp {},{}", main_reg, case_val));
                }
                Save => {
                    res.push(format!("push {}", main_reg))
                }

                LvUnary(un) => {
                    match un.inc_dec_type {
                        IncDecType::Prefix => {
                            let supp_reg = supp_regs[0];
                            res.append(&mut self.deref_to_reg(supp_reg, main_reg));
                            res.push(
                                match un.inc_or_dec {
                                    IncOrDec::Increment => "inc",
                                    IncOrDec::Decrement => "dec",
                                }.to_owned() + " " + supp_reg);
                            res.append(&mut self.write_lvalue(main_reg, supp_reg));
                        }
                        IncDecType::Postfix => {
                            let supp_reg = supp_regs[0];
                            res.append(&mut self.deref_to_reg(supp_reg, main_reg));
                            res.push(
                                match un.inc_or_dec {
                                    IncOrDec::Increment => "inc",
                                    IncOrDec::Decrement => "dec",
                                }.to_owned() + " " + supp_reg);
                            res.append(&mut self.write_lvalue(supp_reg, supp_reg));
                        }
                    }
                }
                RvUnary(un) => {
                    match un {
                        RvalueUnary::Minus => {
                            res.push(format!("neg {}", main_reg))
                        }
                        RvalueUnary::Complement => {
                            res.push(format!("not {}", main_reg))
                        }
                        RvalueUnary::LogicalNot => {
                            res.push(format!("test {},{0}", main_reg));
                            res.push("pushf".to_owned());
                            res.push(format!("xor {},{0}", main_reg));
                            res.push("popf".to_owned());
                            res.push(format!("setz {}", self.get_lower_byte_of_reg(main_reg)));
                        }
                    }
                }
                Call { nargs } => {
                    res.append(&mut self.align_and_call(*nargs + 1))
                }
                Nargs => {
                    res.push(format!("mov {},{}+{}*(2+1)", main_reg,
                                     reg_for_initial_rsp_minus_2_words, word_size))
                }
                Goto => {
                    res.push(format!("rol {},{}", main_reg, self.get_lvalue_offset_in_bits()));
                    res.push(format!("jmp {}", main_reg));
                }
                Ret => {
                    res.push(format!("mov rsp,{}", reg_for_initial_rsp_minus_2_words));
                    res.push(format!("pop {}", reg_for_initial_rsp_minus_2_words));
                    res.push(format!("pop {}", reg_for_calls));
                    res.push("ret".to_owned());
                }

                FnDef(name, info) => {
                    res.push(executable_section_or_segment(target).to_owned());
                    res.push(format!("align {}", word_size));
                    res.push(format!("{}:", mangle_global_def(&name)));

                    res.push(format!("push {}", reg_for_calls));
                    res.push(format!("push {}", reg_for_initial_rsp_minus_2_words));
                    res.push(format!("mov {},rsp", reg_for_initial_rsp_minus_2_words));
                    res.push(format!("sub rsp,{}*{}", info.stack_size, word_size));
                    user_defined.insert(name.as_str());

                    let num_of_args_to_shadow_space =
                        *vec![regs_for_args.len() as u64,
                              call_conv.shadow_space_size_in_bytes.div(word_size as u64),
                              info.num_of_params + 1].iter().min().unwrap() as usize;
                    for (i, reg) in regs_for_args[
                        ..num_of_args_to_shadow_space].iter().enumerate() {
                        let supp_reg = supp_regs[0];
                        res.push(format!(
                            "lea {supp_reg},[{reg_for_initial_rsp_minus_2_words}+{word_size}*({i}+1+2)]",
                            supp_reg = supp_reg,
                            reg_for_initial_rsp_minus_2_words = reg_for_initial_rsp_minus_2_words,
                            i = i, word_size = word_size));
                        res.push(format!("mov [{}],{}", supp_reg, reg));
                    }
                }
                VarOrVecDef(name) => {
                    res.push(readable_writable_section_or_segment(target).to_owned());
                    res.push(format!("{}:", mangle_global_def(name)));
                    user_defined.insert(name.as_str());
                }
                Ival(iv) => {
                    match iv {
                        intermediate_code_generator::Ival::Name(_) => unreachable!(),
                        intermediate_code_generator::Ival::Number(x) => res.push(
                            format!("{} {}", declare_word_directive(word_size), x)),
                        intermediate_code_generator::Ival::ReserveWords(n) => {
                            for _ in 0..*n {
                                res.push(format!("{} ?", declare_word_directive(word_size)))
                            }
                        }
                    }
                }
                BinOp(bin_op) => {
                    res.append(&mut self.bin_op_to_machine_code(*bin_op))
                }
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
                GetProcessHeap dq RVA _GetProcessHeap
                HeapFree dq RVA _HeapFree
                HeapAlloc dq RVA _HeapAlloc
                ExitProcess dq RVA _ExitProcess
                GetStdHandle dq RVA _GetStdHandle
                ReadFile dq RVA _ReadFile
                WriteFile dq RVA _WriteFile
                GetCommandLineA dq RVA _GetCommandLineA
                dq 0

                kernel_name db 'KERNEL32.DLL',0

                _GetProcessHeap dw 0
                    db 'GetProcessHeap',0
                _HeapFree dw 0
                    db 'HeapFree',0
                _HeapAlloc dw 0
                    db 'HeapAlloc',0
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
                        let x = x.trim().to_owned();
                        if !x.is_empty() { Some(x) } else { None }
                    }))
            }
            _ => todo!()
        }
        res
    }
}