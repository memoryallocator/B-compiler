use std::collections::HashMap;
use std::iter::FromIterator;

use crate::machine_code_generator;
use crate::utils::{get_standard_library_names, StdNameInfo, TargetPlatform, LINUX_64};

use machine_code_generator::MachineCodeGenerator;
use machine_code_generator::START;
use machine_code_generator::{executable_section_or_segment, internal_def, mangle_global_def};

const TARGET: TargetPlatform = LINUX_64;

pub(crate) fn generate_std_lib_and_internals() -> Vec<String> {
    let call_conv = TARGET.calling_convention();
    let stdin = 0;
    let stdout = 1;
    let syscalls = HashMap::<&str, i32>::from_iter(
        vec![
            ("read", 0),
            ("write", 1),
            ("mmap", 9),
            ("munmap", 11),
            ("exit", 60),
        ]
        .into_iter(),
    );
    let mut res = vec![executable_section_or_segment(TARGET).to_string()];

    for (name, info) in get_standard_library_names() {
        if let StdNameInfo::Variable { .. } = info {
            continue;
        }
        res.push(format!("{}:", internal_def(name)));
        res.push(match name {
            "putchar" => format!(
                r"
                    push r13
                    xor r13, r13
                    lea rsp, [rsp - 2 * 8]
                    mov [rsp], rsi
                .loop:
                    mov al, [rsp + r13]
                    test al, al
                    je .iter
                    cmp al, 4
                    je .iter

                    mov rdi, {stdout}
                    lea rsi, [rsp + r13]
                    mov rdx, 1
                    mov rax, {write}
                    syscall
                .iter:
                    cmp r13, 7
                    je .out
                    inc r13
                    jmp .loop
                .out:
                    lea rsp, [rsp + 2 * 8]
                    pop r13
                    ret",
                stdout = stdout,
                write = syscalls["write"]
            ),

            "getchar" => format!(
                r"
                    mov rdi, {stdin}
                    lea rsp, [rsp - 8]
                    mov rsi, rsp
                    mov rdx, 1
                    mov rax, {read}
                    syscall
                    movzx rax, BYTE [rsp]
                    lea rsp, [rsp + 8]
                    ret",
                stdin = stdin,
                read = syscalls["read"]
            ),

            "exit" => format!(
                r"
                            xor rdi, rdi
                            mov rax, {exit}
                            syscall",
                exit = syscalls["exit"]
            ),

            "char" => r"
                    rol rsi, 3
                    movzx rax, BYTE [rsi + rdx]
                    ret"
            .to_string(),

            "getvec" => {
                const PROT_READ: i32 = 0x1;
                const PROT_WRITE: i32 = 0x2;
                const MAP_PRIVATE: i32 = 0x2;
                const MAP_ANONYMOUS: i32 = 0x20;

                format!(
                    r"
                    xor rdi, rdi
                    inc rsi
                    shl rsi, 3
                    mov rdx, {prot}
                    mov r10, {flags}
                    mov r8, -1
                    xor r9, r9
                    mov rax, {mmap}
                    lea rsp, [rsp - 8]
                    syscall
                    lea rsp, [rsp + 8]
                    cmp rax, -4096
                    ja .assign_null
                    .ok:
                        ror rax, 3
                        ret
                    .assign_null:
                        xor rax, rax
                        ret",
                    prot = PROT_READ | PROT_WRITE,
                    flags = MAP_PRIVATE | MAP_ANONYMOUS,
                    mmap = syscalls["mmap"]
                )
            }

            "rlsevec" => format!(
                r"
                    rol rsi, 3
                    inc rdx
                    mov rax, {munmap}
                    lea rsp, [rsp - 8]
                    syscall
                    lea rsp, [rsp + 8]
                    ret",
                munmap = syscalls["munmap"]
            ),

            "printf" => format!(
                r"
                    pop rax
                    push r9
                    push r8
                    push rcx
                    push rdx
                    push rax
                    push r12
                    push r13
                    push r14
                    rol rsi, 3
                    mov r12, rsi
                    lea r14, [rsp + (3+1) * 8]
                .loop:
                    movzx rsi, BYTE [r12]
                    inc r12
                    cmp rsi, '%'
                    je .format
                    cmp rsi, 4
                    je .out
                    call {internal_putchar}
                    jmp .loop

                .format:
                    movzx rsi, BYTE [r12]
                    inc r12
                    cmp rsi, 'd'
                    je .dec
                    cmp rsi, 's'
                    je .str
                    cmp rsi, 'c'
                    je .char
                    cmp rsi, 'o'
                    je .oct
                    call {internal_putchar}
                    jmp .loop

                .char:
                    mov rsi, [r14]
                    call {internal_putchar}
                    jmp .advance_arg_and_repeat

                .str:
                    mov rsi, [r14]
                    call {internal_putstr}
                    jmp .advance_arg_and_repeat

                .oct:
                    mov rsi, [r14]
                    mov rdx, 8
                    call {print_unsigned}
                    jmp .advance_arg_and_repeat

                .min_word:
                    mov rsi, '9'
                    call {internal_putchar}
                    mov rsi, 223'372'036'854'775'808
                    jmp .dec_unsigned_reg_is_set

                .dec:
                    mov r13, [r14]
                    test r13, r13
                    jns .dec_unsigned

                    .negative:
                        mov rsi, '-'
                        call {internal_putchar}
                        neg r13
                        test r13, r13
                        jz .min_word

                    .dec_unsigned:
                        mov rsi, r13
                    .dec_unsigned_reg_is_set:
                        mov rdx, 10
                        call {print_unsigned}

                .advance_arg_and_repeat:
                    lea r14, [r14 + 8]
                    jmp .loop

                .out:
                    pop r14
                    pop r13
                    pop r12
                    pop rax
                    lea rsp, [rsp + (4-1) * 8]
                    jmp rax

                {print_unsigned}:
                    xor rcx, rcx
                    lea rsp, [rsp - 3 * 8]
                    mov rax, rsi
                    mov r8, rdx
                    mov BYTE [rsp + {MAX_LEN}], 4

                    .fill_digit_array:
                        inc rcx
                        xor rdx, rdx
                        div r8
                        add dl, '0'
                        lea rsi, [rsp + {MAX_LEN}]
                        sub rsi, rcx
                        mov [rsi], dl
                        test rax, rax
                        jnz .fill_digit_array

                    .print:
                        ror rsi, 3
                        call {internal_putstr}

                    lea rsp, [rsp + 3 * 8]
                    ret",
                MAX_LEN = 22,
                internal_putchar = internal_def("putchar"),
                print_unsigned = internal_def("print_unsigned"),
                internal_putstr = internal_def("putstr")
            ),

            "putstr" => format!(
                r"
                    push r15
                    mov r15, rsi
                    rol r15, 3

                    .iter_str:
                        movzx rsi, BYTE [r15]
                        cmp rsi, 4
                        je .out
                        call {internal_putchar}
                    .next_iter:
                        inc r15
                        jmp .iter_str
                    .out:
                        pop r15
                        ret",
                internal_putchar = internal_def("putchar")
            ),

            "nargs" => {
                format!(
                    "mov rax, [{base} - 8]
                        ret",
                    base = call_conv.reg_for_initial_rsp
                )
            }

            _ => "dq 0".to_string(),
        });
    }

    res.push(executable_section_or_segment(TARGET).to_string());
    res.push(format!("{}:", START));
    res.push(MachineCodeGenerator::align_stack(call_conv.alignment));
    res.push("xor rdi, rdi".to_string());
    res.push(format!("call {}", mangle_global_def("main")));
    res.push(format!("jmp {}", mangle_global_def("exit")));
    res
}
