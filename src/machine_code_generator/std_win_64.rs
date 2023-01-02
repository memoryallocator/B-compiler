use crate::machine_code_generator;
use crate::utils;

use machine_code_generator::MachineCodeGenerator;
use machine_code_generator::START;
use machine_code_generator::{
    executable_section_or_segment, internal_def, mangle_global_def,
    readable_writable_section_or_segment,
};
use utils::get_standard_library_names;
use utils::{StdNameInfo, TargetPlatform, WIN_64};

const TARGET: TargetPlatform = WIN_64;

pub(crate) fn generate_std_lib_and_internals() -> Vec<String> {
    let call_conv = TARGET.calling_convention();
    let stdin = internal_def("stdin");
    let stdout = internal_def("stdout");
    let proc_heap = internal_def("proc_heap");
    let mut res = vec![executable_section_or_segment(TARGET).to_string()];

    for (name, info) in get_standard_library_names() {
        if let StdNameInfo::Variable { .. } = info {
            continue;
        }
        res.push(format!("{}:", internal_def(name)));
        res.push(match name {
            "putchar" => format!(
                r"
                    push r12
                    push r13
                    lea r12, [rsp + (2 + 2) * 8]
                    mov [r12], rdx
                    xor r13, r13
                    lea rsp, [rsp - 5 * 8]
                .loop:
                    mov al, [r12 + r13]
                    test al, al
                    je .iter
                    cmp al, 4
                    je .iter

                    mov rcx, [{stdout}]
                    lea rdx, [r12 + r13]
                    mov r8, 1
                    lea r9, [rsp + (5 + 2 + 1) * 8]
                    mov QWORD [rsp + 4*8], 0  ; 5th parameter
                    call [WriteFile]
                .iter:
                    cmp r13, 7
                    je .out
                    inc r13
                    jmp .loop
                .out:
                    lea rsp, [rsp + 5 * 8]
                    pop r13
                    pop r12
                    ret",
                stdout = stdout
            ),

            "getchar" => format!(
                r"
                    lea rdx, [rsp + 8]
                    mov rcx, [{stdin}]
                    mov r8, 1
                    xor r9, r9
                    push 0  ; 5th parameter
                    lea rsp, [rsp - 4 * 8]
                    call [ReadFile]
                    lea rsp, [rsp + 5 * 8]
                    movzx rax, BYTE [rsp + 8]
                    ret",
                stdin = stdin
            ),

            "exit" => r"xor rcx, rcx
                            jmp [ExitProcess]"
                .to_string(),

            "char" => r"
                    rol rdx, 3
                    movzx rax, BYTE [rdx + r8]
                    ret"
            .to_string(),

            "getvec" => format!(
                r"
                    inc rdx
                    mov r8, rdx
                    shl r8, 3
                    mov rcx, [{}]
                    mov rdx, 0x00000008  ; HEAP_ZERO_MEMORY
                    lea rsp, [rsp - 5 * 8]
                    call [HeapAlloc]
                    lea rsp, [rsp + 5 * 8]
                    ror rax, 3
                    ret",
                proc_heap
            ),

            "rlsevec" => format!(
                r"
                    rol rdx, 3
                    mov r8, rdx
                    mov rcx, [{}]
                    xor rdx, rdx
                    lea rsp, [rsp - 5 * 8]
                    call [HeapFree]
                    lea rsp, [rsp + 5 * 8]
                    ret",
                proc_heap
            ),

            "printf" => format!(
                r"
                    push r12
                    push r13
                    push r14
                    lea r14, [rsp + (3 + 3) * 8]
                    mov [r14], r8
                    mov [r14 + 1*8], r9
                    lea rsp, [rsp - 4 * 8]
                    rol rdx, 3
                    mov r12, rdx
                .loop:
                    movzx rdx, BYTE [r12]
                    inc r12
                    cmp dl, '%'
                    je .format
                    cmp dl, 4
                    je .out
                    call {internal_putchar}
                    jmp .loop

                .format:
                    movzx rdx, BYTE [r12]
                    inc r12
                    cmp dl, 'd'
                    je .dec
                    cmp dl, 's'
                    je .str
                    cmp dl, 'c'
                    je .char
                    cmp dl, 'o'
                    je .oct
                    call {internal_putchar}
                    jmp .loop

                .oct:
                    mov rdx, [r14]
                    mov r8, 8
                    call {print_unsigned}
                    jmp .advance_arg_and_repeat

                .char:
                    mov rdx, [r14]
                    call {internal_putchar}
                    jmp .advance_arg_and_repeat

                .str:
                    mov rdx, [r14]
                    call {internal_putstr}
                    jmp .advance_arg_and_repeat

                .min_word:
                    mov rdx, '9'
                    call {internal_putchar}
                    mov rdx, 223'372'036'854'775'808
                    jmp .dec_unsigned_reg_is_set

                .dec:
                    mov r13, [r14]
                    test r13, r13
                    jns .dec_unsigned

                    .negative:
                        mov rdx, '-'
                        call {internal_putchar}
                        neg r13
                        test r13, r13
                        jz .min_word

                    .dec_unsigned:
                        mov rdx, r13
                    .dec_unsigned_reg_is_set:
                        mov r8, 10
                        call {print_unsigned}

                .advance_arg_and_repeat:
                    lea r14, [r14 + 8]
                    jmp .loop

                .out:
                    lea rsp, [rsp + 4 * 8]
                    pop r14
                    pop r13
                    pop r12
                    ret

                {print_unsigned}:
                    xor r9, r9
                    lea rsp, [rsp - (1+4) * 8]
                    mov rax, rdx
                    mov BYTE [rsp + (4+2) * 8 + {MAX_LEN}], 4

                    .fill_digit_array:
                        inc r9
                        xor rdx, rdx
                        div r8
                        add dl, '0'
                        lea rcx, [rsp + (4+2) * 8 + {MAX_LEN}]
                        sub rcx, r9
                        mov [rcx], dl
                        test rax, rax
                        jnz .fill_digit_array

                    .print:
                        mov rdx, rcx
                        ror rdx, 3
                        call {internal_putstr}

                    lea rsp, [rsp + (1+4) * 8]
                    ret",
                MAX_LEN = 22,
                internal_putchar = internal_def("putchar"),
                print_unsigned = internal_def("print_unsigned"),
                internal_putstr = internal_def("putstr")
            ),

            "putstr" => format!(
                r"
                    push r15
                    lea rsp, [rsp - 4 * 8]
                    mov r15, rdx
                    rol r15, 3

                    .iter_str:
                        movzx rdx, BYTE [r15]
                        cmp dl, 4
                        je .out
                        call {internal_putchar}
                    .next_iter:
                        inc r15
                        jmp .iter_str
                    .out:
                        lea rsp, [rsp + 4 * 8]
                        pop r15
                        ret",
                internal_putchar = internal_def("putchar")
            ),

            "nargs" => {
                format!(
                    "mov rax, [{base} + (2+1)*8]
                        ret",
                    base = call_conv.reg_for_initial_rsp
                )
            }

            _ => "dq 0".to_string(),
        });
    }

    res.push(readable_writable_section_or_segment(TARGET).to_string());
    res.extend(vec![format!("{} dq ?", stdout), format!("{} dq ?", stdin)]);
    res.push(format!("{} dq ?", proc_heap));

    res.push(executable_section_or_segment(TARGET).to_string());
    res.push(format!("{}:", START));
    res.push(MachineCodeGenerator::align_stack(call_conv.alignment));
    res.push("sub rsp, 4 * 8".to_string());
    res.extend(
        vec![
            "mov rcx, -11",
            "call [GetStdHandle]",
            &format!("mov [{}], rax", internal_def("stdout")),
            "mov rcx, -10",
            "call [GetStdHandle]",
            &format!("mov [{}], rax", internal_def("stdin")),
            "call [GetProcessHeap]",
            &format!("mov [{}], rax", internal_def("proc_heap")),
        ]
        .into_iter()
        .map(String::from),
    );
    res.push("xor rcx, rcx".to_string());
    res.push(format!("call {}", mangle_global_def("main")));
    res.push(format!("jmp {}", mangle_global_def("exit")));
    res
}
