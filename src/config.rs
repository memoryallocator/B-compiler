use std::collections::HashMap;

use crate::symbol::SymbolType;
use crate::token::ControlStatementIdentifier::*;
use crate::token::DeclarationSpecifier::*;
use crate::token::TokenType;

pub(crate) enum Arch {
    x86_32,
    x86_64,
}

pub(crate) enum PlatformName {
    Linux,
    Bsd,
    Windows,
    MacOs,
}

pub(crate) struct TargetPlatform {
    platform_name: PlatformName,
    pub(crate) arch: Arch,
}

impl Default for TargetPlatform {
    fn default() -> Self {
        Self {
            platform_name: {
                #[cfg(any(
                target_os = "freebsd",
                target_os = "dragonfly",
                target_os = "openbsd",
                target_os = "netbsd"
                ))] {
                    PlatformName::Bsd
                }
                #[cfg(target_os = "linux")] {
                    PlatformName::Linux
                }
                #[cfg(target_os = "windows")] {
                    PlatformName::Windows
                }
                #[cfg(target_os = "macos")] {
                    PlatformName::MacOs
                }
            },
            arch: {
                #[cfg(target_pointer_width = "32")] {
                    Arch::x86_32
                }
                #[cfg(target_pointer_width = "64")] {
                    Arch::x86_64
                }
            },
        }
        // eprintln!("Unknown platform. Assuming it's 64-bit Linux")
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

pub(crate) type TypeOfLineNo = usize;
pub(crate) type TypeOfColumnNo = usize;
pub(crate) type SymbolTable = HashMap<String, SymbolType>;

pub(crate) fn get_default_symbols() -> SymbolTable {
    use SymbolType::*;
    // use std::ops::RangeInclusive;

    let mut res = Vec::<(&str, SymbolType)>::new();
    {
        let mut keywords: Vec<(&str, SymbolType)> = vec![
            ("auto", SymbolType::Reserved(TokenType::DeclarationSpecifier(Auto))),
            ("extrn", SymbolType::Reserved(TokenType::DeclarationSpecifier(Extrn))),
            ("goto", SymbolType::Reserved(TokenType::ControlStatement(Goto))),
            ("switch", SymbolType::Reserved(TokenType::ControlStatement(Switch))),
            ("case", SymbolType::Reserved(TokenType::ControlStatement(Case))),
            ("return", SymbolType::Reserved(TokenType::ControlStatement(Return))),
            ("if", SymbolType::Reserved(TokenType::ControlStatement(If))),
            ("else", SymbolType::Reserved(TokenType::ControlStatement(Else))),
            ("while", SymbolType::Reserved(TokenType::ControlStatement(While))),
            ("break", SymbolType::Reserved(TokenType::ControlStatement(Break))),
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