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

    (|| {
        res.push(executable_section_or_segment(TARGET).to_owned());
        for (name,
            info) in get_standard_library_names() {
            if let StdNameInfo::Variable { .. } = info {
                continue;
            }
            res.push(format!("{}:", internal_def(name)));
            res.push(match name {
                "putchar" => format!(r"
                    push r12
                    push r13
                    lea r12, [rsp + 2*8 + 2*8]
                    mov [r12], rdx
                    xor r13, r13
                    sub rsp, 6 * 8
                .loop:
                    xor rax, rax
                    mov al, [r12 + r13]
                    test al, al
                    je .iter
                    cmp al, 4
                    je .iter

                    mov rcx, [{stdout}]
                    lea rdx, [r12 + r13]
                    mov r8, 1
                    lea r9, [rsp + 6*8 + 2*8 + 1*8]
                    mov QWORD [rsp + 4*8], 0  ; 5th parameter
                    call [WriteFile]
                .iter:
                    cmp r13, 7
                    je .out
                    inc r13
                    jmp .loop
                .out:
                    add rsp, 6 * 8
                    pop r13
                    pop r12
                    ret", stdout = stdout),

                "getchar" => format!(r"
                    lea rdx, [rsp + 8]
                    mov rcx, [{stdin}]
                    mov r8, 1
                    mov r9, 0
                    push 0  ; filler
                    push 0  ; 5th parameter
                    sub rsp, 4 * 8
                    call [ReadFile]
                    add rsp, 6 * 8
                    xor rax, rax
                    mov al, [rsp + 8]
                    ret", stdin = stdin),

                "exit" => r"xor rcx, rcx
                            jmp [ExitProcess]".to_owned(),

                "char" => r"
                    rol rdx, 3
                    lea rdx, [rdx + r8]
                    xor rax, rax
                    mov al, [rdx]
                    ret".to_owned(),

                "getvec" => format!(r"
                    inc rdx
                    mov r8, rdx
                    shl r8, 3
                    mov rcx, [{}]
                    mov rdx, 0x00000008  ; HEAP_ZERO_MEMORY
                    sub rsp, 5 * 8
                    call [HeapAlloc]
                    add rsp, 5 * 8
                    ror rax, 3
                    ret", proc_heap),

                "rlsevec" => format!(r"
                    rol rdx, 3
                    mov r8, rdx
                    mov rcx, [{}]
                    mov rdx, 0
                    sub rsp, 4 * 8
                    call [HeapFree]
                    add rsp, 4 * 8
                    ret", proc_heap),

                "printf" => format!(r"
                    push r12
                    push r13
                    push r14
                    push r15
                    lea r14, [rsp + 4*8 + 3*8]
                    mov [r14], r8
                    mov [r14 + 1*8], r9
                    sub rsp, 4 * 8
                    rol rdx, 3
                    mov r12, rdx
                    xor r13, r13
                .loop:
                    xor rdx, rdx
                    mov dl, [r12]
                    cmp dl, 4
                    je .out
                    inc r12
                    cmp dl, '%'
                    je .format
                    call {internal_putchar}
                    jmp .loop

                .format:
                    mov al, [r12]
                    inc r12
                    cmp al, 'd'
                    je .dec
                    cmp al, 's'
                    je .str
                    cmp al, 'c'
                    je .char
                    cmp al, '%'
                    je .percent
                    cmp al, 'o'
                    je .oct
                    jmp .loop

                .char:
                    mov rdx, [r14]
                    call {internal_putchar}
                    jmp .advance_arg_and_repeat

                .oct:
                    mov rdx, [r14]
                    mov r8, 1*8
                    call {print_unsigned}
                    jmp .advance_arg_and_repeat

                .dec:
                    mov rax, -9'223'372'036'854'775'808
                    mov r15, [r14]
                    cmp r15, rax
                    je .min_word
                    test r15, r15
                    js .negative

                    .dec_unsigned:
                    mov rdx, r15
                    mov r8, 10
                    call {print_unsigned}
                    jmp .advance_arg_and_repeat

                    .negative:
                        mov rdx, '-'
                        call {internal_putchar}
                        neg r15
                        jmp .dec_unsigned

                    .min_word:
                        mov rdx, '-'
                        call {internal_putchar}
                        mov rdx, '9'
                        call {internal_putchar}
                        mov r15, 223'372'036'854'775'808
                        jmp .dec_unsigned

                .str:
                    mov r15, [r14]
                    rol r15, 3
                    .iter_str:
                        xor rdx, rdx
                        mov dl, [r15]
                        cmp dl, 4
                        je .advance_arg_and_repeat
                        call {internal_putchar}
                    .next_iter:
                        inc r15
                        jmp .iter_str

                .percent:
                    mov rdx, '%'
                    call {internal_putchar}

                .advance_arg_and_repeat:
                    add r14, 8
                    jmp .loop

                .out:
                    add rsp, 4 * 8
                    pop r15
                    pop r14
                    pop r13
                    pop r12
                    ret

                {print_unsigned}:
                    push r12
                    xor r12, r12
                    sub rsp, 4*8
                    mov rax, rdx
                    mov BYTE [rsp + 4*8 + 32 + 7], 4
                    .fill_digit_array:
                        inc r12
                        xor rdx, rdx
                        div r8
                        add rdx, '0'
                        lea rcx, [rsp + 4*8 + 32 + 7]
                        sub rcx, r12
                        mov [rcx], dl
                        test rax, rax
                        jnz .fill_digit_array

                    .print:
                        mov rdx, rcx
                        ror rdx, 3
                        call {internal_printf}

                    add rsp, 4*8
                    pop r12
                    ret", internal_putchar = internal_def("putchar"),
                                    print_unsigned = internal_def("print_unsigned"),
                                    internal_printf = internal_def("printf")),

                _ => "dq 0".to_owned(),
            });
        }
    })();

    (|| {
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
                    res.push(format!("jmp {}", internal_def(name)));
                }
            }
        }
    })();

    res.push(readable_writable_section_or_segment(TARGET).to_owned());
    res.extend(vec![format!("{} dq ?", stdout), format!("{} dq ?", stdin)]);
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
    res.into_iter().fold(vec![], |mut acc, x| {
        acc.extend(x.split('\n').map(|x| x.to_owned()));
        acc
    })
}