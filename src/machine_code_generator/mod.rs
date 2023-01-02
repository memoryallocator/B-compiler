use std::cmp::min;
use std::collections::{HashMap, HashSet};

use crate::intermediate_code_generator::{
    BinaryOp, IntermRepr, Ival, Label, Lvalue, RvalueUnary, StackRange,
};
use crate::parser::ast::IncDecType;
use crate::tokenizer;
use crate::utils;

use tokenizer::char_to_u64;
use tokenizer::token::{BinaryRelation, IncOrDec, LeftOrRight};
use utils::get_standard_library_names;
use utils::{Arch, CompilerOptions, StdNameInfo, TargetPlatform};
use utils::{LINUX_64, WIN_64};

mod std_linux_64;
mod std_win_64;

pub struct MachineCodeGenerator<'a> {
    pub(crate) compiler_options: CompilerOptions,
    intermediate_code: &'a [IntermRepr],
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

const START: &str = "start!";

fn pooled_str_end(idx: u64) -> String {
    format!("s!{}.end", idx.to_string())
}

fn readonly_section_or_segment(target: TargetPlatform) -> &'static str {
    match target {
        WIN_64 => "section '.rodata' readable",
        LINUX_64 => "segment readable",
    }
}

fn readable_writable_section_or_segment(target: TargetPlatform) -> &'static str {
    match target {
        WIN_64 => "section '.data' readable writeable",
        LINUX_64 => "segment readable writeable",
    }
}

fn executable_section_or_segment(target: TargetPlatform) -> &'static str {
    match target {
        WIN_64 => "section '.text' executable",
        LINUX_64 => "segment executable",
    }
}

fn declare_word_directive(word_size: u8) -> &'static str {
    match word_size {
        8 => "dq",
        _ => unimplemented!(),
    }
}

impl<'a> MachineCodeGenerator<'a> {
    pub fn new(
        compiler_options: CompilerOptions,
        intermediate_code: &'a [IntermRepr],
        pooled_strings: HashMap<String, u64>,
    ) -> Self {
        MachineCodeGenerator {
            compiler_options,
            intermediate_code,
            pooled_strings,
        }
    }

    pub fn run(self) -> Vec<String> {
        match self.compiler_options.target_platform.arch {
            Arch::x86_64 => self.generate_x86_64(),
        }
    }

    fn generate_std_lib_and_internals(&self, already_defined: HashSet<&str>) -> Vec<String> {
        let target = self.compiler_options.target_platform;
        let mut res = vec![];
        for (name, info) in get_standard_library_names() {
            if already_defined.contains(name) {
                continue;
            }
            match info {
                StdNameInfo::Variable { ival } => {
                    res.push(readable_writable_section_or_segment(target).to_owned());
                    res.push(format!("{}:", mangle_global_def(name)));
                    res.push(format!("{} {}", declare_word_directive(8), ival))
                }
                StdNameInfo::Function(_) => {
                    res.push(executable_section_or_segment(target).to_owned());
                    res.push(format!("{}:", mangle_global_def(name)));
                    res.push(format!("jmp {}", internal_def(name)));
                }
            }
        }

        res.extend(match target {
            WIN_64 => std_win_64::generate_std_lib_and_internals(),
            LINUX_64 => std_linux_64::generate_std_lib_and_internals(),
        });
        res
    }

    fn pack_string_and_append_eot(&self, s: &str) -> Vec<u64> {
        let mut res = vec![];
        let word_size = self.compiler_options.target_platform.arch.word_size() as usize;
        let it = &mut s.chars();
        loop {
            let mut char: String = it
                .take(word_size as usize)
                .take_while(|x| *x as u32 != 4)
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
        (self.compiler_options.target_platform.arch.word_size() as f64)
            .log2()
            .round() as u64
    }

    fn align_and_call(&self, nargs: u64) -> Vec<String> {
        let call_conv = self.compiler_options.target_platform.calling_convention();
        let mut res = vec![];
        let regs_for_args = &call_conv.registers_to_pass_args;
        for i in 0..min(nargs as usize, regs_for_args.len()) {
            res.push(format!("pop {}", regs_for_args[i as usize]));
        }

        let main_reg = call_conv.main_register;
        let reg_for_calls = call_conv.reg_for_calls;
        let word_size = self.compiler_options.target_platform.arch.word_size();
        res.push(format!(
            r"
                mov {reg_for_calls}, rsp
                and rsp, -{alignment}
                i = 0
                stack_var_count = {nargs} - {param_reg_count}
                while i < stack_var_count
                    mov {supp}, [{reg_for_calls} + i*{word_size}]
                    mov [rsp + i*{word_size}], {supp}
                    i = i + 1
                end while",
            supp = call_conv.supporting_registers[0],
            nargs = nargs,
            word_size = word_size,
            reg_for_calls = reg_for_calls,
            alignment = call_conv.alignment,
            param_reg_count = regs_for_args.len()
        ));
        if call_conv.use_shadow_space {
            res.push(format!(
                "sub rsp, {}",
                regs_for_args.len() * word_size as usize
            ))
        }
        res.push(format!(
            "rol {},{}",
            main_reg,
            self.get_lvalue_offset_in_bits()
        ));
        res.push(format!("call {}", main_reg));
        res.push(format!("mov rsp, {}", reg_for_calls));
        res.push(format!(
            r"
                if stack_var_count > 0
                    add rsp, {word_size}*stack_var_count
                end if",
            word_size = word_size
        ));
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
                let lower_byte = self.get_lower_byte_of_reg(main_reg);
                let command = "set".to_owned()
                    + match rel {
                        BinaryRelation::Gt => "g",
                        BinaryRelation::Ne => "ne",
                        BinaryRelation::Eq => "e",
                        BinaryRelation::Lt => "l",
                        BinaryRelation::Ge => "ge",
                        BinaryRelation::Le => "le",
                    };
                res.push(format!("{} {}", command, lower_byte));
                res.push(format!("movzx {},{}", main_reg, lower_byte))
            }

            BinaryOp::Div | BinaryOp::Mod => {
                res.push(format!("xchg rax,{}", supp_reg));
                res.push("cqo".to_owned());
                res.push(format!("idiv {}", supp_reg));
                match bin_op {
                    BinaryOp::Div => (),
                    BinaryOp::Mod => res.push(format!("mov {},rdx", main_reg)),
                    _ => unreachable!(),
                }
            }

            BinaryOp::Shift(direction) => {
                res.push(format!("mov rcx,{}", main_reg));
                match direction {
                    LeftOrRight::Left => res.push(format!("shl {},cl", supp_reg)),
                    LeftOrRight::Right => res.push(format!("shr {},cl", supp_reg)),
                }
                res.push(format!("mov {},{}", main_reg, supp_reg));
            }

            BinaryOp::LogicalAnd => {
                let snd_supp = call_conv.supporting_registers[1];
                res.push(format!("test {},{0}", main_reg));

                let main_lower_byte = self.get_lower_byte_of_reg(main_reg);
                res.push(format!("setnz {}", main_lower_byte));
                res.push(format!("movzx {},{}", snd_supp, main_lower_byte));

                res.push(format!("test {},{0}", supp_reg));
                res.push(format!("setnz {}", main_lower_byte));
                res.push(format!("movzx {},{}", main_reg, main_lower_byte));

                res.push(format!("push {}", snd_supp));
                res.extend(self.bin_op_to_machine_code(BinaryOp::BitwiseAnd));
            }

            b => {
                res.push(
                    match b {
                        BinaryOp::Or => "or",
                        BinaryOp::Xor => "xor",
                        BinaryOp::Add => "add",
                        BinaryOp::Sub => "sub",
                        BinaryOp::Mul => "imul",
                        BinaryOp::BitwiseAnd => "and",
                        _ => unreachable!(),
                    }
                    .to_owned()
                        + &format!(" {},{}", supp_reg, main_reg),
                );
                res.push(format!("mov {},{}", main_reg, supp_reg));
            }
        }
        res
    }

    fn generate_x86_64(self) -> Vec<String> {
        let comp_opts = self.compiler_options;
        let target = comp_opts.target_platform;
        let call_conv = target.calling_convention();
        let main_reg = call_conv.main_register;
        let supp_regs = call_conv.supporting_registers;
        let reg_for_calls = call_conv.reg_for_calls;
        let reg_for_initial_rsp = call_conv.reg_for_initial_rsp;
        let regs_for_args = &call_conv.registers_to_pass_args;
        let word_size = target.arch.word_size();
        let use_shadow_space = call_conv.use_shadow_space;

        let mut args_passed_to_curr_fn = None;

        let mut res = match target {
            LINUX_64 => vec!["format ELF64 executable 3".to_owned()],
            WIN_64 => vec![
                "format PE64 console".to_owned(),
                format!("stack {},{0}", comp_opts.stack_size),
                format!("heap {},{0}", comp_opts.heap_size),
            ],
        };
        res.push(format!("entry {}", START));

        let mut user_defined = HashSet::new();

        for ir in self.intermediate_code {
            let regs_for_args_len = regs_for_args.len() as u64;
            match ir {
                IntermRepr::LoadLvalue(lv) => {
                    match lv {
                        Lvalue::NthArg(n) => {
                            if use_shadow_space || *n >= regs_for_args_len {
                                res.push(format!(
                                    "lea {main_reg},[{initial_rsp}+{word_size}*(2+1+{n})]",
                                    main_reg = main_reg,
                                    initial_rsp = reg_for_initial_rsp,
                                    n = if use_shadow_space {
                                        *n
                                    } else {
                                        n - regs_for_args_len
                                    },
                                    word_size = word_size
                                ))
                            } else {
                                res.push(format!(
                                    "lea {main_reg},[{initial_rsp}-(1+{n})*{word_size}]",
                                    main_reg = main_reg,
                                    initial_rsp = reg_for_initial_rsp,
                                    n = n,
                                    word_size = word_size
                                ))
                            }
                        }
                        Lvalue::LocalVar(st) => {
                            res.push(format!(
                                "lea {main},[{base}-({offset_in_words})*{word_sz}]",
                                main = main_reg,
                                word_sz = word_size,
                                base = reg_for_initial_rsp,
                                offset_in_words =
                                    1 + match st {
                                        StackRange::Exact(n) => n,
                                        StackRange::Span(st) => st.end(),
                                    } + if use_shadow_space {
                                        0
                                    } else {
                                        min(args_passed_to_curr_fn.unwrap(), regs_for_args_len)
                                            as u64
                                    }
                            ));
                        }
                        Lvalue::Label(lbl) => res.push(format!(
                            "mov {},{}",
                            main_reg,
                            match lbl {
                                Label::Local(lbl) => mangle_label(lbl),
                                Label::Global(lbl) => mangle_global_def(lbl),
                                Label::PooledStr(str_no) => pooled_str_label(*str_no),
                            }
                        )),
                    }
                    res.push(format!(
                        "ror {},{}",
                        main_reg,
                        self.get_lvalue_offset_in_bits()
                    ))
                }
                IntermRepr::WriteLvalue => {
                    let supp_reg = supp_regs[0];
                    res.push(format!("pop {}", supp_reg));
                    res.push(format!(
                        "rol {},{}",
                        supp_reg,
                        self.get_lvalue_offset_in_bits()
                    ));
                    res.push(format!("mov [{}],{}", supp_reg, main_reg));
                }
                IntermRepr::LoadConstant(constant) => {
                    res.push(format!("mov {},{}", main_reg, constant))
                }
                IntermRepr::Deref => {
                    res.push(format!(
                        "rol {},{}",
                        main_reg,
                        self.get_lvalue_offset_in_bits()
                    ));
                    res.push(format!("mov {},[{0}]", main_reg));
                }
                IntermRepr::DeclLabel(lbl) => res.push(format!("{}:", mangle_label(lbl))),
                IntermRepr::InternalLabel(lbl_id) => {
                    res.push(format!("{}:", internal_label(*lbl_id)))
                }
                IntermRepr::Jump(lbl_id) => res.push(format!("jmp {}", internal_label(*lbl_id))),
                IntermRepr::CaseJmpIfEq { case_val, label_no } => {
                    res.push(format!("cmp {},{}", main_reg, case_val));
                    res.push(format!("je {}", internal_label(*label_no)))
                }
                IntermRepr::TestAndJumpIfZero(lbl_id) => {
                    res.push(format!("test {},{0}", main_reg));
                    res.push(format!("jz {}", internal_label(*lbl_id)));
                }
                IntermRepr::Save => res.push(format!("push {}", main_reg)),

                IntermRepr::LvUnary(un) => {
                    res.push(format!(
                        "rol {},{}",
                        main_reg,
                        self.get_lvalue_offset_in_bits()
                    ));
                    let inc_or_dec = match un.inc_or_dec {
                        IncOrDec::Increment => "inc",
                        IncOrDec::Decrement => "dec",
                    };
                    let (supp_reg, prev_val_reg) = (supp_regs[0], supp_regs[1]);

                    res.push(format!("mov {},[{}]", supp_reg, main_reg));
                    if un.inc_dec_type == IncDecType::Postfix {
                        res.push(format!("mov {},{}", prev_val_reg, supp_reg));
                    }
                    res.push(format!("{} {}", inc_or_dec, supp_reg));
                    res.push(format!("mov [{}],{}", main_reg, supp_reg));
                    res.push(format!(
                        "mov {},{}",
                        main_reg,
                        match un.inc_dec_type {
                            IncDecType::Postfix => prev_val_reg,
                            IncDecType::Prefix => supp_reg,
                        }
                    ));
                }
                IntermRepr::RvUnary(un) => match un {
                    RvalueUnary::Minus => res.push(format!("neg {}", main_reg)),
                    RvalueUnary::Complement => res.push(format!("not {}", main_reg)),
                    RvalueUnary::LogicalNot => {
                        res.push(format!("test {},{0}", main_reg));
                        let lower_byte = self.get_lower_byte_of_reg(main_reg);
                        res.push(format!("setz {}", lower_byte));
                        res.push(format!("movzx {},{}", main_reg, lower_byte));
                    }
                },
                IntermRepr::Call { nargs } => res.extend(self.align_and_call(*nargs + 1)),
                IntermRepr::Goto => {
                    res.push(format!(
                        "rol {},{}",
                        main_reg,
                        self.get_lvalue_offset_in_bits()
                    ));
                    res.push(format!("jmp {}", main_reg));
                }
                IntermRepr::Ret => {
                    res.push(format!("mov rsp,{}", reg_for_initial_rsp));
                    res.push(format!("pop {}", reg_for_initial_rsp));
                    res.push(format!("pop {}", reg_for_calls));
                    res.push("ret".to_owned());
                }

                IntermRepr::FnDef(name, info) => {
                    res.push(executable_section_or_segment(target).to_owned());
                    res.push(format!("{}:", mangle_global_def(name)));

                    res.push(format!("push {}", reg_for_calls));
                    res.push(format!("push {}", reg_for_initial_rsp));
                    res.push(format!("mov {},rsp", reg_for_initial_rsp));

                    user_defined.insert(name.as_str());

                    let args_to_curr_fn = info.num_of_params + 1;
                    let num_of_args_to_save = min(regs_for_args.len() as u64, args_to_curr_fn);
                    args_passed_to_curr_fn = Some(args_to_curr_fn);

                    let words_needed = info.stack_size
                        + if use_shadow_space {
                            0
                        } else {
                            num_of_args_to_save
                        };
                    if words_needed != 0 {
                        res.push(format!("sub rsp,{}*{}", words_needed, word_size));
                    }

                    for (i, reg) in regs_for_args[..num_of_args_to_save as usize]
                        .iter()
                        .enumerate()
                    {
                        res.push(format!("i = {}", i));
                        res.push(format!(
                            "mov [{base}+{word_size}*({offset})],{reg}",
                            offset = if use_shadow_space { "2+1+i" } else { "-(1+i)" },
                            reg = reg,
                            word_size = word_size,
                            base = reg_for_initial_rsp
                        ));
                    }
                }
                IntermRepr::VarOrVecDef(name) => {
                    res.push(readable_writable_section_or_segment(target).to_owned());
                    res.push(format!("{}:", mangle_global_def(name)));
                    user_defined.insert(name.as_str());
                }
                IntermRepr::Ival(iv) => match iv {
                    Ival::Number(x) => {
                        res.push(format!("{} {}", declare_word_directive(word_size), x))
                    }
                    Ival::ReserveWords(n) => {
                        res.push(format!("{} {} dup ?", declare_word_directive(word_size), n))
                    }
                    Ival::Name(lbl) => match lbl {
                        Label::Local(_) => unreachable!(),
                        Label::Global(name) => res.push(format!("dq {}", mangle_global_def(name))),
                        Label::PooledStr(s) => res.push(format!("dq {}", pooled_str_label(*s))),
                    },
                },
                IntermRepr::BinOp(bin_op) => res.extend(self.bin_op_to_machine_code(*bin_op)),
            }
        }
        if !self.pooled_strings.is_empty() {
            res.push(readonly_section_or_segment(target).to_owned());
            for (s, n) in &self.pooled_strings {
                res.push(format!("{}:", pooled_str_label(*n)));
                let s_as_words = self
                    .pack_string_and_append_eot(s)
                    .into_iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(",");
                res.push(format!("dq {}", s_as_words));
                res.push(format!("{}:", pooled_str_end(*n)))
            }
        }
        res.extend(self.generate_std_lib_and_internals(user_defined));
        if self.compiler_options.target_platform == WIN_64 {
            res.push(
                r"
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
                    db 'GetCommandLineA',0"
                    .to_owned(),
            )
        }
        res.into_iter().fold(vec![], |mut acc, x| {
            acc.extend(x.split('\n').filter_map(|x| {
                let x = x.trim().to_owned();
                if !x.is_empty() {
                    Some(x)
                } else {
                    None
                }
            }));
            acc
        })
    }
}
