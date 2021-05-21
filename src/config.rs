use std::*;
use collections::{HashMap, HashSet};
use fmt;

use crate::parser::ast::*;
use crate::lexical_analyzer::token;
use token::*;

pub(crate) enum Issue {
    BracketNotOpened(TokenPos),
    BracketNotClosed(TokenPos),
    EmptyTokenStream,
    ParsingError,
    NameNotDefined { name: String, pos: TokenPos },
    NameRedefined { curr_def: (String, TokenPos), prev_def_pos: Option<TokenPos> },
    VecWithNoSizeAndInits(String, TokenPos),
    VecSizeIsNotANumber { vec_def: (String, TokenPos), size: String },
    VecTooManyIvals {
        vec_def: (String, TokenPos),
        ivals_len: usize,
        specified_size_plus_1: usize,
    },
    FnBodyIsNullStatement(FunctionDefinitionNode),
    IntegerConstantIsTooLong(String, TokenPos),
}

impl fmt::Display for Issue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Issue::*;
        write!(f, "{}", match self {
            ParsingError => "Failed to parse".to_string(),

            NameRedefined { curr_def: (name, pos), prev_def_pos } => {
                format!("Name {}, defined {}, is redefined at {}",
                        name,
                        if let Some(prev_def_pos) = prev_def_pos {
                            format!("at {}", prev_def_pos)
                        } else {
                            "in the standard library".to_string()
                        },
                        pos)
            }

            VecSizeIsNotANumber { vec_def: (name, def_pos), size } => {
                format!("Size {} of vector {}, defined at {}, is not a number",
                        size, name, def_pos)
            }

            _ => todo!()
        })
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone)]
pub(crate) enum Arch {
    x86_32,
    x86_64,
}

impl fmt::Display for Arch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arch::x86_32 => write!(f, "x86-32"),
            Arch::x86_64 => write!(f, "x86-64"),
        }
    }
}

#[warn(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq)]
pub(crate) enum PlatformName {
    Linux,
    Bsd,
    Windows,
    MacOs,
}

impl fmt::Display for PlatformName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlatformName::Linux => write!(f, "Linux"),
            PlatformName::Bsd => write!(f, "BSD"),
            PlatformName::Windows => write!(f, "Windows"),
            PlatformName::MacOs => write!(f, "macOS"),
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) struct TargetPlatform {
    pub(crate) platform_name: PlatformName,
    pub(crate) arch: Arch,
}

impl TargetPlatform {
    pub(crate) fn native() -> Self {
        TargetPlatform {
            platform_name: if cfg!(any
            (target_os = "freebsd",
            target_os = "dragonfly",
            target_os = "openbsd",
            target_os = "netbsd")
            ) {
                PlatformName::Bsd
            } else if cfg!(target_os = "linux") {
                PlatformName::Linux
            } else if cfg!(target_os = "windows") {
                PlatformName::Windows
            } else if cfg!(target_os = "macos") {
                PlatformName::MacOs
            } else {
                let default_platform = TargetPlatform::default().platform_name;
                println!("Failed to determine the native OS. Assuming it's {}", default_platform);
                default_platform
            },
            arch: {
                if cfg!(target_pointer_width = "32") {
                    Arch::x86_32
                } else if cfg!(target_pointer_width = "64") {
                    Arch::x86_64
                } else {
                    let default_arch = TargetPlatform::default().arch;
                    println!("Failed to determine the native architecture. Assuming it's {}", default_arch);
                    default_arch
                }
            },
        }
    }
}

impl Default for TargetPlatform {
    fn default() -> Self {
        TargetPlatform {
            platform_name: PlatformName::Linux,
            arch: Arch::x86_64,
        }
    }
}

#[derive(Default, Copy, Clone)]
pub(crate) struct CompilerOptions {
    pub(crate) target_platform: TargetPlatform,
}

pub(crate) fn get_escape_sequences() -> HashMap<String, String> {
    vec![('0', '\0'),
         ('e', 4 as char),  // ASCII EOT, B end of string),
         ('(', '{'),
         (')', '}'),
         ('t', '\t'),
         ('*', '*'),
         ('\'', '\''),
         ('"', '"'),
         ('n', '\n')]
        .into_iter()
        .map(|x| (format!("*{}", x.0), x.1.to_string()))
        .collect()
}

pub(crate) type ReservedSymbolsTable = HashMap<String, ReservedName>;

pub(crate) fn get_reserved_symbols() -> ReservedSymbolsTable {
    use DeclarationSpecifier::*;
    use ControlStatementIdentifier::*;

    vec![
        ("auto", ReservedName::DeclarationSpecifier(Auto)),
        ("extrn", ReservedName::DeclarationSpecifier(Extrn)),
        ("goto", ReservedName::ControlStatement(Goto)),
        ("switch", ReservedName::ControlStatement(Switch)),
        ("case", ReservedName::ControlStatement(Case)),
        ("return", ReservedName::ControlStatement(Return)),
        ("if", ReservedName::ControlStatement(If)),
        ("else", ReservedName::ControlStatement(Else)),
        ("while", ReservedName::ControlStatement(While)),
        ("break", ReservedName::ControlStatement(Break)),
        ("default", ReservedName::ControlStatement(Default)),
        // ("for", SymbolType::Reserved::ControlStatement(For))),
    ].into_iter().map(|x| (x.0.to_string(), x.1)).collect()
}

#[derive(Eq, PartialEq, Hash)]
pub(crate) enum NumberOfParameters {
    Exact(usize),
    AtLeast(usize),
}

#[derive(Eq, PartialEq, Hash)]
pub(crate) enum VarOrVecOrFnInfo {
    Variable,
    Vector { size: usize },
    Function { number_of_parameters: NumberOfParameters },
}

#[derive(Eq, PartialEq, Hash)]
pub(crate) struct StandardLibraryName {
    pub(crate) name: String,
    pub(crate) info: VarOrVecOrFnInfo,
}

pub(crate) fn get_standard_library_names() -> HashSet<StandardLibraryName> {
    let mut std_lib_fns_with_exact_num_of_params = Vec::<(&str, usize)>::new();

    std_lib_fns_with_exact_num_of_params.append(&mut (|| {
        vec![
            ("getchar", 0),
            ("putchar", 1),
            ("openr", 2),
            ("openw", 2),
            ("getstr", 1),
            ("putstr", 1),
            ("system", 1),
            ("close", 1),
            ("flush", 0),
            ("reread", 0)
        ].into_iter()
            .map(|x| (x.0, x.1))
            .collect()
    })());

    std_lib_fns_with_exact_num_of_params.push(("ioerrors", 1));

    std_lib_fns_with_exact_num_of_params.append(&mut (|| {
        vec![
            ("char", 2),
            ("lchar", 3),
            ("getarg", 3),
        ].into_iter()
            .map(|x| (x.0, x.1))
            .collect()
    })());

    std_lib_fns_with_exact_num_of_params.append(&mut (|| {
        vec![
            ("getvec", 1),
            ("rlsevec", 2),
            ("nargs", 0),
            ("exit", 0)
        ].into_iter()
            .map(|x| (x.0, x.1))
            .collect()
    })());

    let std_lib_fns_with_exact_num_of_params = std_lib_fns_with_exact_num_of_params
        .into_iter()
        .map(|x| StandardLibraryName {
            name: x.0.to_string(),
            info: VarOrVecOrFnInfo::Function {
                number_of_parameters: NumberOfParameters::Exact(x.1)
            },
        })
        .collect::<HashSet<StandardLibraryName>>();

    let printf = StandardLibraryName {
        name: "printf".to_string(),
        info: VarOrVecOrFnInfo::Function { number_of_parameters: NumberOfParameters::AtLeast(1) },
    };
    let concat = StandardLibraryName {
        name: "concat".to_string(),
        info: VarOrVecOrFnInfo::Function {
            number_of_parameters: NumberOfParameters::AtLeast(1)
        },
    };
    let mut std_lib_fns_with_variable_num_of_params = HashSet::new();
    std_lib_fns_with_variable_num_of_params.extend(vec![printf, concat].into_iter());

    let mut std_lib_fns = std_lib_fns_with_exact_num_of_params;
    std_lib_fns.extend(std_lib_fns_with_variable_num_of_params.into_iter());

    let std_lib_vars: HashSet<StandardLibraryName> = vec!["wr.unit", "rd.unit"]
        .into_iter()
        .map(|var_name|
            StandardLibraryName {
                name: var_name.to_string(),
                info: VarOrVecOrFnInfo::Variable,
            })
        .collect();
    let mut res = std_lib_vars;
    res.extend(std_lib_fns);
    res
}