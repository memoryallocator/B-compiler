use std::*;
use collections::HashSet;

use super::*;

use crate::config::*;

const TARGET: TargetPlatform = WIN_64;

pub(crate) fn generate_std_lib_and_internals(already_defined: HashSet<&str>) -> Vec<String> {
    let call_conv = TARGET.calling_convention();
    let stdin = internal_def("stdin");
    let stdout = internal_def("stdout");
    let mut res = vec![];
    for (name, info) in get_standard_library_names() {
        if already_defined.contains(name) {
            continue;
        }
        match info {
            StdNameInfo::Variable { ival } => {
                res.push(readable_writable_section_or_segment(TARGET).to_owned());
                res.push(format!("{}:", mangle_global_def(name)));
                res.push(format!("{} {}", declare_word_directive(8), ival))
            }
            StdNameInfo::Function(_) => {
                res.push(executable_section_or_segment(TARGET).to_owned());
                res.push(format!("{}:", mangle_global_def(name)));
                res.extend(match name {
                    "putchar" => {
                        format!(r"
                        push rcx
                        xor rcx, rcx
                    .loop:
                        mov rdx, 8-1
                        sub rdx, rcx
                        lea rdx, [rsp + rdx]
                        mov al, [rdx]
                        test al, al
                        je .iter
                        cmp al, 4
                        je .iter

                        push rcx
                        mov rcx, [{}]
                        mov r8, 1
                        mov r9, 0
                        push 0
                        sub rsp, 4 * 8
                        call [WriteFile]
                        add rsp, 5 * 8
                        pop rcx
                    .iter:
                        cmp rcx, 8
                        je .out
                        inc rcx
                        jmp .loop
                    .out:
                        add rsp, 8
                        ret", stdout)
                    }
                    "getchar" => {
                        format!(r"
                        push 0
                        lea rdx, [rsp]
                        mov rcx, [{}]
                        mov r8, 1
                        mov r9, 0
                        push 0
                        sub rsp, 4 * 8
                        call [ReadFile]
                        add rsp, 5 * 8
                        pop rax
                        ret", stdin)
                    }
                    "exit" => {
                        r"xor rcx, rcx
                        call [ExitProcess]".to_owned()
                    }
                    "char" => {
                        r"shl rcx, 3
                        add rcx, rdx
                        xor rax, rax
                        mov al, [rcx]
                        ret".to_owned()
                    }
                    _ => "dq 0".to_owned(),
                }.split('\n')
                    .filter_map(|x| {
                        if !x.is_empty() { Some(x.trim().to_owned()) } else { None }
                    }))
            }
        }
    }
    res.push(readable_writable_section_or_segment(TARGET).to_owned());
    res.append(&mut vec![format!("{} dq ?", stdout), format!("{} dq ?", stdin)]);

    res.push(executable_section_or_segment(TARGET).to_owned());
    res.push(format!("{}:", START));
    res.extend(&mut vec![
        "mov rcx, -11",
        "call [GetStdHandle]",
        &format!("mov [{}], rax", internal_def("stdout")),
        "mov rcx, -10",
        "call [GetStdHandle]",
        &format!("mov [{}], rax", internal_def("stdin"))
    ].into_iter().map(String::from));
    res.push(MachineCodeGenerator::align_stack(call_conv.alignment));
    res.push(format!("call {}", mangle_global_def("main")));
    res.push(format!("call {}", mangle_global_def("exit")));
    res
}