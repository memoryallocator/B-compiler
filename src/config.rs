use std::collections::HashMap;
use std::fmt;

use crate::symbol::SymbolType;
use crate::token::ControlStatementIdentifier::*;
use crate::token::DeclarationSpecifier::*;
use crate::token::TokenType;

pub(crate) struct Warning {
    pub(crate) text: String,
    pub(crate) pos: crate::lexical_analyzer::TokenPos,
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

pub(crate) type SymbolTable = HashMap<String,
    SymbolType>;

pub(crate) fn get_default_symbols() -> SymbolTable {
    use crate::symbol::SymbolType::Reserved;

    let mut res = Vec::<(&str, SymbolType)>::new();
    res.push(("rd.unit", Reserved(TokenType::Name)));
    res.push(("wr.unit", Reserved(TokenType::Name)));
    {
        let mut keywords: Vec<(&str, SymbolType)> = vec![
            ("auto", Reserved(TokenType::DeclarationSpecifier(Auto))),
            ("extrn", Reserved(TokenType::DeclarationSpecifier(Extrn))),
            ("goto", Reserved(TokenType::ControlStatement(Goto))),
            ("switch", Reserved(TokenType::ControlStatement(Switch))),
            ("case", Reserved(TokenType::ControlStatement(Case))),
            ("return", Reserved(TokenType::ControlStatement(Return))),
            ("if", Reserved(TokenType::ControlStatement(If))),
            ("else", Reserved(TokenType::ControlStatement(Else))),
            ("while", Reserved(TokenType::ControlStatement(While))),
            ("break", Reserved(TokenType::ControlStatement(Break))),
            // ("for", SymbolType::Reserved(TokenType::ControlStatement(For))),
        ];
        res.append(&mut keywords);
    }

    {
        let mut io_routines_without_printf: Vec<(&str, SymbolType)> = vec![
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
        ].into_iter().map(|x| (x.0, SymbolType::Function {
            par_list_len: Some(x.1..=x.1),
            first_decl: None,
        })).collect();

        let printf = ("printf", SymbolType::Function {
            par_list_len: Some(1..=11),
            first_decl: None,
        });
        res.append(&mut io_routines_without_printf);
        res.push(printf);
        res.push(("ioerrors", SymbolType::Function {
            par_list_len: Some(1..=1),
            first_decl: None,
        }));
    }

    {
        let mut str_manip: Vec<(&str, SymbolType)> = vec![
            ("char", 2),
            ("lchar", 3),
            ("getarg", 3),
        ].into_iter().map(|x| (x.0, SymbolType::Function {
            par_list_len: Some(x.1..=x.1),
            first_decl: None,
        })).collect();

        str_manip.push(("concat", SymbolType::Function {
            par_list_len: Some(1..=11),
            first_decl: None,
        }));
        res.append(&mut str_manip);
    }

    {
        let mut other_functions: Vec<(&str, SymbolType)> = vec![
            ("getvec", 1),
            ("rlsevec", 2),
            ("nargs", 0),
            ("exit", 0)
        ].into_iter().map(|x| (x.0, SymbolType::Function {
            par_list_len: Some(x.1..=x.1),
            first_decl: None,
        })).collect();
        res.append(&mut other_functions);
    }

    res.into_iter().map(|x| (x.0.to_string(), x.1)).collect()
}