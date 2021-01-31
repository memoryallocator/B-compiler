use std::collections::HashMap;

use crate::token::ControlStatement::*;
use crate::token::DeclarationSpecifier::*;
use crate::token::Token;

#[derive(Default)]
pub struct CompilerOptions {}

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

fn get_keywords() -> SymbolTable<'static> {
    vec![
        ("auto", Token::DeclarationSpecifier(Auto)),
        ("extrn", Token::DeclarationSpecifier(Extrn)),
        ("goto", Token::ControlStatement(Goto)),
        ("switch", Token::ControlStatement(Switch)),
        ("return", Token::ControlStatement(Return)),
        ("if", Token::ControlStatement(If)),
        ("else", Token::ControlStatement(Else)),
        ("while", Token::ControlStatement(While)),
        ("for", Token::ControlStatement(For)),
        ("case", Token::ControlStatement(Case)),
        ("break", Token::ControlStatement(Break))
    ].into_iter().collect()
}

pub(crate) type TypeOfLineNo = usize;
pub(crate) type SymbolTable<'a> = HashMap<&'a str, Token>;