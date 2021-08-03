use std::*;
use collections::HashSet;

use super::*;

use crate::config::*;

const TARGET: TargetPlatform = WIN_64;

pub(crate) fn generate_std_lib_and_internals(already_defined: HashSet<&str>) -> Vec<String> {
    let call_conv = TARGET.calling_convention();
    let stdin = internal_def("stdin");
    let stdout = internal_def("stdout");
    let proc_heap = internal_def("proc_heap");
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
                        mov [rsp + 16], rdx
                        xor rcx, rcx
                    .loop:
                        lea rdx, [rsp + 16 + rcx]
                        mov al, [rdx]
                        test al, al
                        je .iter
                        cmp al, 4
                        je .iter

                        mov [rsp + 8], rcx
                        mov rcx, [{}]
                        mov r8, 1
                        lea r9, [rsp - (8+4)]
                        push 0  ; filler
                        push 0  ; 5th parameter
                        sub rsp, 4 * 8
                        call [WriteFile]
                        add rsp, 6 * 8
                        mov rcx, [rsp + 8]
                    .iter:
                        cmp rcx, 7
                        je .out
                        inc rcx
                        jmp .loop
                    .out:
                        ret", stdout)
                    }
                    "getchar" => {
                        format!(r"
                        lea rdx, [rsp + 8]
                        mov rcx, [{}]
                        mov r8, 1
                        mov r9, 0
                        push 0  ; filler
                        push 0  ; 5th parameter
                        sub rsp, 4 * 8
                        call [ReadFile]
                        add rsp, 6 * 8
                        xor rax, rax
                        mov al, [rsp + 8]
                        ret", stdin)
                    }
                    "exit" => {
                        r"xor rcx, rcx
                        jmp [ExitProcess]".to_owned()
                    }
                    "char" => {
                        r"
                        mov rcx, rdx
                        rol rcx, 3
                        add rcx, r8
                        xor rax, rax
                        mov al, [rcx]
                        ret".to_owned()
                    }
                    "getvec" => {
                        format!(r"
                        inc rdx
                        mov r8, rdx
                        shl r8, 3
                        mov rcx, [{}]
                        mov rdx, 0x00000008  ; HEAP_ZERO_MEMORY
                        sub rsp, 5 * 8
                        call [HeapAlloc]
                        add rsp, 5 * 8
                        ror rax, 3
                        ret", proc_heap)
                    }
                    "rlsevec" => {
                        format!(r"
                        rol rdx, 3
                        mov r8, rdx
                        mov rcx, [{}]
                        mov rdx, 0
                        sub rsp, 4 * 8
                        call [HeapFree]
                        add rsp, 4 * 8
                        ret", proc_heap)
                    }
                    _ => "dq 0".to_owned(),
                }.split('\n').map(String::from))
            }
        }
    }
    res.push(readable_writable_section_or_segment(TARGET).to_owned());
    res.append(&mut vec![format!("{} dq ?", stdout), format!("{} dq ?", stdin)]);
    res.push(format!("{} dq ?", proc_heap));

    res.push(executable_section_or_segment(TARGET).to_owned());
    res.push(format!("{}:", START));
    res.push(MachineCodeGenerator::align_stack(call_conv.alignment));
    res.push("sub rsp, 4 * 8".to_owned());
    res.extend(&mut vec![
        "mov rcx, -11",
        "call [GetStdHandle]",
        &format!("mov [{}], rax", internal_def("stdout")),
        "mov rcx, -10",
        "call [GetStdHandle]",
        &format!("mov [{}], rax", internal_def("stdin")),
        "call [GetProcessHeap]",
        &format!("mov [{}], rax", internal_def("proc_heap")),
    ].into_iter().map(String::from));
    res.push("xor rcx, rcx".to_owned());
    res.push(format!("call {}", mangle_global_def("main")));
    res.push(format!("jmp {}", mangle_global_def("exit")));
    res
}