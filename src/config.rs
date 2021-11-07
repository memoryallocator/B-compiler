use collections::{HashMap, HashSet};
use fmt::{Display, Formatter};
use std::*;

use crate::parser::{DeclInfoAndPos, DefInfoAndPos};
use crate::tokenizer::token;
use token::*;

pub(crate) enum Issue {
    BracketNotOpened(TokenPos),
    BracketNotClosed(TokenPos),
    EmptyTokenStream,
    ParsingError(TokenPos),
    FailedToParseExact(TokenPos),
    UnexpectedToken(TokenPos),
    ExpectedTokenNotFound(TokenPos),
    StmtTooShort(TokenPos),
    NoNextStmtAfterDecl(TokenPos),
    EmptyBracketedExpr(TokenPos),
    WrongConstant(TokenPos),
    ExpectedPrimaryExpr(TokenPos),
    OpCannotBeApplied {
        op_pos: TokenPos,
        expr_pos: Option<TokenPos>,
    },
    UnexpectedOperand(TokenPos),
    NoOperandForOperator(Operator, TokenPos, Option<LeftOrRight>),
    NoCondition(TokenPos),
    NoColonInCond(TokenPos),
    NameNotDefined {
        name: String,
        pos: TokenPos,
    },
    NameRedefined {
        curr_def: (String, TokenPos),
        prev_def: DefInfoAndPos,
    },
    NameRedeclared {
        decl: (String, DeclInfoAndPos),
        prev_decl: DeclInfoAndPos,
    },
    VecWithNoSizeAndIvals(String, TokenPos),
    VecSizeIsNotANumber {
        name: String,
        pos: TokenPos,
    },
    VecTooManyIvals {
        vec_def: (String, TokenPos),
        ivals_len: usize,
        specified_size_plus_1: usize,
    },
    ExternSymbolNotFound {
        name: String,
        extern_pos: TokenPos,
    },
    DeclShadowsGlobalDef {
        decl: (String, DeclInfoAndPos),
        global_def: DefInfoAndPos,
    },
    DeclShadowsFnParameter {
        decl: (String, DeclInfoAndPos),
        param_pos: TokenPos,
        fn_def: (String, TokenPos),
    },
    UnnecessaryImport {
        curr_decl: (String, TokenPos),
        prev_import_pos: TokenPos,
    },
    DeclShadowsPrevious {
        decl: (String, DeclInfoAndPos),
        prev_decl: DeclInfoAndPos,
    },
    UnexpectedKeyword(TokenPos),
    CaseEncounteredTwice {
        curr: TokenPos,
        prev: TokenPos,
    },
    LiteralTooLong(TokenPos),
    NoMainFn,
    InvalidParameterCount {
        expected: NumberOfParameters,
        actual: usize,
        pos: TokenPos,
    },
}

fn def_pos_to_string(def_pos: &Option<TokenPos>) -> String {
    if let Some(def_pos) = def_pos {
        format!("at {}", def_pos)
    } else {
        "in the standard library".to_string()
    }
}

impl Display for Issue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use Issue::*;
        let msg = match self {
            ParsingError(pos) => {
                format!("{}: failed to parse", pos)
            }

            NameRedefined {
                curr_def: (name, pos),
                prev_def:
                    DefInfoAndPos {
                        pos_if_user_defined,
                        ..
                    },
            } => {
                format!(
                    "name {}, defined at {}, was already defined {}",
                    name,
                    pos,
                    def_pos_to_string(pos_if_user_defined)
                )
            }

            VecSizeIsNotANumber { name, pos } => {
                format!(
                    "size of vector {}, defined at {}, is not a number",
                    name, pos
                )
            }

            DeclShadowsGlobalDef {
                decl: (name, info),
                global_def,
            } => {
                format!(
                    "declaration of name {} at {} shadows global definition {}",
                    name,
                    info.pos,
                    def_pos_to_string(&global_def.pos_if_user_defined)
                )
            }

            BracketNotOpened(pos) => {
                format!(
                    "there is a closing bracket at {}, but there is no opening bracket",
                    pos
                )
            }

            BracketNotClosed(pos) => {
                format!(
                    "there is an opening bracket at {}, but there is no closing bracket",
                    pos
                )
            }

            EmptyTokenStream => "failed to recognize a single token".to_owned(),

            FailedToParseExact(pos) => {
                format!("failed to parse construct at {}", pos)
            }

            UnexpectedToken(pos) => {
                format!("unexpected token at {}", pos)
            }

            ExpectedTokenNotFound(pos) => {
                format!("expected token not found at {}", pos)
            }

            StmtTooShort(pos) => {
                format!("expected more tokens at {}", pos)
            }

            NoNextStmtAfterDecl(pos) => {
                format!("there must be a statement after a declaration at {}", pos)
            }

            EmptyBracketedExpr(pos) => {
                format!("empty bracketed expression at {}", pos)
            }

            WrongConstant(pos) => {
                format!("bad type of a constant at {}", pos)
            }

            ExpectedPrimaryExpr(pos) => {
                format!("expression at {} is not a primary expression", pos)
            }

            OpCannotBeApplied { op_pos, expr_pos } => {
                let reason = if let Some(expr_pos) = expr_pos {
                    format!("expression at {} is not a valid operand", expr_pos)
                } else {
                    "no operand".to_string()
                };
                format!("operator at {} cannot be applied: ", op_pos) + &reason
            }

            UnexpectedOperand(pos) => {
                format!("unexpected operand at {}", pos)
            }

            NoOperandForOperator(op, op_pos, left_or_right) => {
                format!(
                    "{}: no {}operand for operator {}",
                    op_pos,
                    if let Some(left_or_right) = left_or_right {
                        format!("{} ", left_or_right)
                    } else {
                        "".to_string()
                    },
                    op
                )
            }

            NoCondition(pos) => {
                format!("{}: no condition in a conditional expression", pos)
            }

            NoColonInCond(pos) => {
                format!("{}: missing colon in a conditional expression", pos)
            }

            NameNotDefined { name, pos } => {
                format!("{}: name {} is not defined", pos, name)
            }

            NameRedeclared {
                decl: (name, decl),
                prev_decl,
            } => {
                format!(
                    "name {}, declared at {}, is redeclared at {}",
                    name, decl.pos, prev_decl.pos
                )
            }

            VecWithNoSizeAndIvals(v, pos) => {
                format!("{}: vector {} has neither size nor initial values", pos, v)
            }

            VecTooManyIvals {
                vec_def: (vec, pos),
                ivals_len,
                specified_size_plus_1,
            } => {
                format!(
                    "{}: vector {} has size {} and {} initial values",
                    pos, vec, specified_size_plus_1, ivals_len
                )
            }

            ExternSymbolNotFound { name, extern_pos } => {
                format!("name {}, imported at {}, not found", name, extern_pos)
            }

            DeclShadowsFnParameter {
                decl: (name, decl),
                param_pos,
                fn_def: (fn_name, fn_pos),
            } => {
                format!("in function {} defined at {}: name {}, declared at {}, shadows a parameter name specified at {}",
                        fn_name, fn_pos, name, decl.pos, param_pos)
            }

            UnnecessaryImport {
                curr_decl: (name, pos),
                prev_import_pos,
            } => {
                format!(
                    "{}: name {} is already imported ({})",
                    pos, name, prev_import_pos
                )
            }

            DeclShadowsPrevious {
                decl: (name, decl),
                prev_decl,
            } => {
                format!(
                    "name {}, declared at {}, shadows declaration at {}",
                    name, decl.pos, prev_decl.pos
                )
            }

            UnexpectedKeyword(pos) => {
                format!("unexpected keyword at {}", pos)
            }

            CaseEncounteredTwice { curr, prev } => {
                format!(
                    "case encountered twice: first at {}, then at {}",
                    prev, curr
                )
            }

            LiteralTooLong(pos) => {
                format!("the literal at {} is too long", pos)
            }

            NoMainFn => "no main function found".to_owned(),

            InvalidParameterCount {
                expected,
                actual,
                pos,
            } => {
                format!(
                    "{}: expected {} parameter(s), got {}",
                    pos, expected, actual
                )
            }
        };
        write!(f, "{}", msg)
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum Arch {
    x86_64,
}

impl Display for Arch {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Arch::x86_64 => write!(f, "x86-64"),
        }
    }
}

impl Arch {
    pub(crate) fn word_size(&self) -> u8 {
        match self {
            Arch::x86_64 => 8,
        }
    }
}

#[warn(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) enum PlatformName {
    Linux,
    Windows,
}

impl Display for PlatformName {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            PlatformName::Linux => write!(f, "Linux"),
            PlatformName::Windows => write!(f, "Windows"),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub(crate) struct TargetPlatform {
    pub(crate) platform_name: PlatformName,
    pub(crate) arch: Arch,
}

pub(crate) const WIN_64: TargetPlatform = TargetPlatform {
    platform_name: PlatformName::Windows,
    arch: Arch::x86_64,
};

pub(crate) const LINUX_64: TargetPlatform = TargetPlatform {
    platform_name: PlatformName::Linux,
    arch: Arch::x86_64,
};

impl Default for TargetPlatform {
    fn default() -> Self {
        LINUX_64
    }
}

pub(crate) struct CallingConvention {
    pub(crate) registers_to_pass_args: Vec<&'static str>,
    pub(crate) use_shadow_space: bool,
    pub(crate) main_register: &'static str,
    pub(crate) supporting_registers: [&'static str; 2],
    pub(crate) alignment: u8,
    pub(crate) reg_for_calls: &'static str,
    pub(crate) reg_for_initial_rsp: &'static str,
}

impl TargetPlatform {
    pub(crate) fn calling_convention(&self) -> CallingConvention {
        match self {
            TargetPlatform {
                platform_name: PlatformName::Windows,
                arch: Arch::x86_64,
            } => CallingConvention {
                registers_to_pass_args: vec!["rcx", "rdx", "r8", "r9"],
                use_shadow_space: true,
                main_register: "rax",
                supporting_registers: ["r10", "r11"],
                alignment: 16,
                reg_for_calls: "r14",
                reg_for_initial_rsp: "rbp",
            },
            TargetPlatform {
                platform_name: PlatformName::Linux,
                arch: Arch::x86_64,
            } => CallingConvention {
                registers_to_pass_args: vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"],
                use_shadow_space: false,
                main_register: "rax",
                supporting_registers: ["r10", "r11"],
                alignment: 16,
                reg_for_calls: "r14",
                reg_for_initial_rsp: "rbp",
            },
        }
    }

    pub(crate) fn native() -> Self {
        TargetPlatform {
            platform_name: if cfg!(target_os = "linux") {
                PlatformName::Linux
            } else if cfg!(target_os = "windows") {
                PlatformName::Windows
            } else {
                let default_platform = TargetPlatform::default().platform_name;
                println!(
                    "Failed to determine the native OS. Assuming it's {}",
                    default_platform
                );
                default_platform
            },
            arch: {
                if cfg!(target_pointer_width = "32") {
                    unimplemented!("32-bit platforms are not supported yet")
                } else if cfg!(target_pointer_width = "64") {
                    Arch::x86_64
                } else {
                    let default_arch = TargetPlatform::default().arch;
                    println!(
                        "Failed to determine the native architecture. Assuming it's {}",
                        default_arch
                    );
                    default_arch
                }
            },
        }
    }
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub(crate) enum Mode {
    Ir,
    Default,
}

#[derive(Copy, Clone)]
pub(crate) struct CompilerOptions {
    pub(crate) target_platform: TargetPlatform,
    pub(crate) stack_size: u64,
    pub(crate) heap_size: u64,
    pub(crate) continue_is_enabled: bool,
    pub(crate) mode: Mode,
}

impl Default for CompilerOptions {
    fn default() -> Self {
        CompilerOptions {
            target_platform: TargetPlatform::native(),
            stack_size: 4096,
            heap_size: 65536,
            continue_is_enabled: false,
            mode: Mode::Default,
        }
    }
}

pub(crate) fn get_escape_sequences() -> HashMap<String, String> {
    vec![
        ('0', '\0'),
        ('e', 4 as char), // ASCII EOT, B end of string),
        ('(', '{'),
        (')', '}'),
        ('t', '\t'),
        ('*', '*'),
        ('\'', '\\'),
        ('"', '"'),
        ('n', '\n'),
    ]
    .into_iter()
    .map(|x| (format!("*{}", x.0), x.1.to_string()))
    .collect()
}

pub(crate) type ReservedSymbolsTable = HashMap<String, ReservedName>;

pub(crate) fn get_reserved_symbols() -> ReservedSymbolsTable {
    use CtrlStmtIdent::*;
    use DeclarationSpecifier::*;

    vec![
        ("auto", ReservedName::DeclarationSpecifier(Auto)),
        ("extrn", ReservedName::DeclarationSpecifier(Extrn)),
        ("goto", ReservedName::CtrlStmt(Goto)),
        ("switch", ReservedName::CtrlStmt(Switch)),
        ("case", ReservedName::CtrlStmt(Case)),
        ("return", ReservedName::CtrlStmt(Return)),
        ("if", ReservedName::CtrlStmt(If)),
        ("else", ReservedName::CtrlStmt(Else)),
        ("while", ReservedName::CtrlStmt(While)),
        ("break", ReservedName::CtrlStmt(Break)),
        ("continue", ReservedName::CtrlStmt(Continue)),
        ("default", ReservedName::CtrlStmt(Default)),
    ]
    .into_iter()
    .map(|x| (x.0.to_string(), x.1))
    .collect()
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum NumberOfParameters {
    Exact(usize),
    AtLeast(usize),
}

impl Display for NumberOfParameters {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                NumberOfParameters::Exact(n) => n.to_string(),
                NumberOfParameters::AtLeast(n) => {
                    format!("at least {}", n)
                }
            }
        )
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) struct FnDef {
    pub(crate) num_of_params: NumberOfParameters,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum StdNameInfo {
    Variable { ival: i64 },
    Function(FnDef),
}

pub(crate) fn get_standard_library_names() -> HashMap<&'static str, StdNameInfo> {
    let mut std_lib_fns_with_exact_num_of_params = Vec::<(&'static str, usize)>::new();

    std_lib_fns_with_exact_num_of_params.extend(vec![
        ("getchar", 0),
        ("putchar", 1),
        ("openr", 2),
        ("openw", 2),
        ("getstr", 1),
        ("putstr", 1),
        ("system", 1),
        ("close", 1),
        ("flush", 0),
        ("reread", 0),
    ]);

    std_lib_fns_with_exact_num_of_params.push(("ioerrors", 1));

    std_lib_fns_with_exact_num_of_params.extend(vec![("char", 2), ("lchar", 3), ("getarg", 3)]);

    std_lib_fns_with_exact_num_of_params.extend(vec![
        ("getvec", 1),
        ("rlsevec", 2),
        ("nargs", 0),
        ("exit", 0),
    ]);

    let std_lib_fns_with_exact_num_of_params = std_lib_fns_with_exact_num_of_params
        .into_iter()
        .map(|(name, num_of_params)| {
            (
                name,
                StdNameInfo::Function(FnDef {
                    num_of_params: NumberOfParameters::Exact(num_of_params),
                }),
            )
        })
        .collect::<HashMap<&'static str, StdNameInfo>>();

    let printf = (
        "printf",
        StdNameInfo::Function(FnDef {
            num_of_params: NumberOfParameters::AtLeast(1),
        }),
    );
    let concat = (
        "concat",
        StdNameInfo::Function(FnDef {
            num_of_params: NumberOfParameters::AtLeast(1),
        }),
    );
    let mut std_lib_fns_with_variable_num_of_params = HashSet::new();
    std_lib_fns_with_variable_num_of_params.extend(vec![printf, concat].into_iter());

    let mut std_lib_fns = std_lib_fns_with_exact_num_of_params;
    std_lib_fns.extend(std_lib_fns_with_variable_num_of_params.into_iter());

    let std_lib_vars: HashMap<&'static str, StdNameInfo> = [("wr.unit", 1), ("rd.unit", 0)]
        .into_iter()
        .map(|(var_name, ival)| (var_name, StdNameInfo::Variable { ival }))
        .collect();
    let mut res = std_lib_vars;
    res.extend(std_lib_fns);
    res
}
