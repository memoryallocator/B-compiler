use std::collections::HashMap;

use crate::symbol::SymbolType;
use crate::token::ControlStatementIdentifier::*;
use crate::token::DeclarationSpecifier::*;
use crate::token::TokenType;

#[derive(Default)]
pub(crate) struct CompilerOptions {}

pub(crate) fn get_second_symbol_of_escape_sequence_to_character_mapping() -> HashMap<u8, u8> {
    vec![
        (b'0', b'\0'),
        (b'e', 4),  // ASCII EOT, B end of string),
        (b'(', b'{'),
        (b')', b'}'),
        (b't', b'\t'),
        (b'*', b'*'),
        (b'"', b'"'),
        (b'n', b'\n')
    ].into_iter().collect()
}

pub(crate) type TypeOfLineNo = usize;
pub(crate) type TypeOfColumnNo = usize;
pub(crate) type SymbolTable = HashMap<Vec<u8>, SymbolType>;

pub(crate) fn get_default_symbols() -> SymbolTable {
    use SymbolType::*;
    use crate::symbol::ParameterListLength;

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
            // ("for", SymbolType::Reserved(TokenType::ControlStatement(For))),
            ("break", SymbolType::Reserved(TokenType::ControlStatement(Break))),
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
        ].into_iter().map(|x| (x.0, Function(ParameterListLength::Precise(x.1)))).collect();

        let printf = ("printf", Function(ParameterListLength::Variable(Some(1..=11))));
        res.append(&mut io_routines_without_printf);
        res.push(printf);
        res.push(("ioerrors", Function(ParameterListLength::Precise(1))));
    }

    {
        let mut str_manip: Vec<(&str, SymbolType)> = vec![
            ("char", 2),
            ("lchar", 3),
            ("getarg", 3),
        ].into_iter().map(|x| (x.0, Function(ParameterListLength::Precise(x.1)))).collect();

        str_manip.push(("concat", Function(ParameterListLength::Variable(Some(1..=11)))));
        res.append(&mut str_manip);
    }

    {
        let mut other_functions: Vec<(&str, SymbolType)> = vec![
            ("getvec", 1),
            ("rlsevec", 2),
            ("nargs", 0),
            ("exit", 0)
        ].into_iter().map(|x| (x.0, Function(ParameterListLength::Precise(x.1)))).collect();
        res.append(&mut other_functions);
    }

    res.into_iter().map(|x| (Vec::from(x.0), x.1)).collect()
}