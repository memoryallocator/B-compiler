use collections::{HashMap, HashSet};
use fmt;
use rc::Rc;
use std::*;

use token::ControlStatementIdentifier::*;
use token::DeclarationSpecifier::*;
use token::ReservedName;

use crate::lexical_analyzer::TokenPos;
// use crate::symbol::SymbolType;
use crate::parser::ast::{DefinitionNode, VectorDefinitionNode};
use crate::token;

pub(crate) enum Error {
    BracketNotOpened(TokenPos),
    BracketNotClosed(TokenPos),
    EmptyTokenStream,
    ParsingError,
    NameNotDefined(Rc<String>, TokenPos),
    NameRedefined { curr_def: Rc<DefinitionNode>, prev_def_pos: Option<TokenPos> },
    VecSizeIsString(Rc<VectorDefinitionNode>),
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg =
            match self {
                Error::BracketNotOpened(pos) =>
                    format!("closing bracket in {}, doesn't have a matching opening bracket", pos),
                Error::BracketNotClosed(pos) =>
                    format!("bracket opened in {}, not closed", pos),

                Error::EmptyTokenStream => format!("failed to recognise a single token"),
                Error::ParsingError => format!("failed to parse"),

                Error::NameNotDefined(name, pos) =>
                    format!("name {} used in {} is not defined", name, pos),

                _ => todo!()
            };

        write!(f, "{}", msg)
    }
}

pub(crate) enum Warning {
    InitVarWithItself(Rc<DefinitionNode>, TokenPos),
    StandardNameRedefined(Rc<DefinitionNode>),
    VecWithNoSizeAndInits(Rc<VectorDefinitionNode>),
    VecSizeIsNotANumber(Rc<VectorDefinitionNode>),
}

impl fmt::Display for Warning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg =
            match self {
                Warning::InitVarWithItself(def_node, same_name_pos) => {
                    match def_node.as_ref() {
                        DefinitionNode::Variable(var_def) => {
                            let var_name = &var_def.name;
                            format!("Variable {}, defined at {}, is initialized with itself at {}",
                                    var_name, var_def.get_position(), same_name_pos)
                        }

                        DefinitionNode::Vector(vec_def) => {
                            let vec_name = &vec_def.name;
                            format!("Vector {}, defined at {}, has its name as one of its initial values ({})",
                                    vec_name, vec_def.get_position(), same_name_pos)
                        }

                        DefinitionNode::Function(_) => unreachable!(),
                    }
                }

                _ => todo!(),
            };

        write!(f, "{}", msg)
    }
}

#[allow(non_camel_case_types)]
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

pub(crate) struct TargetPlatform {
    pub(crate) platform_name: PlatformName,
    pub(crate) arch: Arch,
}

impl TargetPlatform {
    fn native() -> Self {
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

#[derive(Default)]
pub(crate) struct CompilerOptions {
    pub(crate) target_platform: TargetPlatform,
    // pub(crate) redefinition_of_std_names_is_allowed: bool,
}

pub(crate) fn get_second_symbol_of_escape_sequence_to_character_mapping() -> HashMap<char, char> {
    vec![
        ('0', '\0'),
        ('e', 4 as char),  // ASCII EOT, B end of string),
        ('(', '{'),
        (')', '}'),
        ('t', '\t'),
        ('*', '*'),
        ('"', '"'),
        ('n', '\n')
    ].into_iter().collect()
}

pub(crate) type ReservedSymbolsTable = HashMap<String, ReservedName>;

pub(crate) fn get_reserved_symbols() -> ReservedSymbolsTable {
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
        // ("for", SymbolType::Reserved::ControlStatement(For))),
    ].into_iter().map(|x| (x.0.to_string(), x.1)).collect()
}

#[derive(Eq, PartialEq, Hash)]
pub(crate) enum StandardLibraryName {
    Function { fn_name: Rc<String>, parameter_list_length: Option<usize> },
    Variable { var_name: Rc<String> },
}

pub(crate) fn get_standard_library_names() -> HashSet<StandardLibraryName> {
    let mut std_lib_fns = vec![];

    std_lib_fns.append(&mut (|| {
        let io_routines_without_printf = vec![
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
            .map(|x| (x.0, Some(x.1)))
            .collect();

        let printf = ("printf", None);

        let mut io_routines: Vec<(&str, Option<usize>)> = io_routines_without_printf;
        io_routines.push(printf);

        io_routines
    })());

    std_lib_fns.push(("ioerrors", Some(1)));

    std_lib_fns.append(&mut (|| {
        let mut str_manip: Vec<(&str, Option<usize>)> = vec![
            ("char", 2),
            ("lchar", 3),
            ("getarg", 3),
        ].into_iter()
            .map(|x| (x.0, Some(x.1)))
            .collect();

        str_manip.push(("concat", None));
        str_manip
    })());

    std_lib_fns.append(&mut (|| {
        let other_functions: Vec<(&str, Option<usize>)> = vec![
            ("getvec", 1),
            ("rlsevec", 2),
            ("nargs", 0),
            ("exit", 0)
        ].into_iter()
            .map(|x| (x.0, Some(x.1)))
            .collect();

        other_functions
    })());

    let std_lib_vars: HashSet<StandardLibraryName> = vec!["wr.unit", "rd.unit"].into_iter()
        .map(|var_name|
            StandardLibraryName::Variable { var_name: Rc::new(var_name.to_string()) })
        .collect();

    let mut res = std_lib_vars;
    res
        .extend(
            std_lib_fns.into_iter()
                .map(|x| StandardLibraryName::Function {
                    fn_name: Rc::new(x.0.to_string()),
                    parameter_list_length: x.1,
                }));
    res
}