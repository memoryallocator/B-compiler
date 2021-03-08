use std::borrow::Borrow;
use std::collections::HashMap;
use std::rc::Weak;

use crate::config::{Arch, CompilerOptions, SymbolTable, TargetPlatform};
use crate::lexical_analyzer::TokenPos;
use crate::token::{BracketType, Token, TokenType};

// use crate::ast::DefinitionsTree;

#[derive(Clone, Debug, Ord, PartialOrd, Eq, PartialEq)]
enum AndInterpretation {
    Logical,
    Bitwise,
}

pub(crate) struct Parser<'a> {
    pub(crate) compiler_options: CompilerOptions,
    pub(crate) symbol_table: SymbolTable,
    pub(crate) source_code: Option<&'a str>,
}

#[derive(Eq, PartialEq)]
enum BracketsStatus {
    Ok,
    NotClosed(TokenPos),
    NotOpened(TokenPos),
}

#[derive(Debug, Ord, PartialOrd, Eq, PartialEq, Clone)]
enum TokenOrCompoundStatement {
    Token(crate::token::Token),
    CompoundStatement(Vec<(crate::token::Token, TokenPos)>),
}

fn generate_unexpected_token_error_message<
    T: Borrow<Token>,
    U: Borrow<TokenPos>,
    Z: Borrow<(T, U)>
>(tok_and_pos: Z) -> String {
    let (t, pos) = tok_and_pos.borrow();
    return format!("{}: unexpected token {}", pos.borrow(), t.borrow().token_type);
}

fn generate_redefinition_error_message<
    T: Borrow<Token>,
    U: Borrow<TokenPos>,
    Z: Borrow<(T, U)>,
    S: Borrow<crate::symbol::SymbolType>
>(tok_and_pos: Z, prev_def_tok: S) -> String {
    let (t, pos) = tok_and_pos.borrow();
    let t = t.borrow();
    return format!("{}: token {} has name {}, but that name is already declared (token {:?})",
                   pos.borrow(),
                   t.token_type,
                   t.value.as_ref().unwrap().clone(),
                   prev_def_tok.borrow());
}

impl Parser<'_> {
    fn are_brackets_balanced<'b, I>(mut tok_it: I) -> BracketsStatus
        where I: Iterator<Item=&'b (crate::token::Token, TokenPos)> {
        use crate::token::LeftOrRight::*;
        use crate::token::Bracket;

        let mut stack = Vec::<(&Bracket, &TokenPos)>::new();
        while let Some(token) = tok_it.next() {
            if let (Token {
                token_type: TokenType::Bracket(br),
                ..
            }, pos) = token {
                if br.left_or_right == Left {
                    stack.push((br, pos));
                } else {
                    match stack.pop() {
                        None => return BracketsStatus::NotOpened(pos.clone()),
                        Some((last, _)) => {
                            if *last != Bracket::paired_bracket(&br) {
                                return BracketsStatus::NotOpened(pos.clone());
                            }
                        }
                    }
                }
            }
        }
        if stack.is_empty() {
            return BracketsStatus::Ok;
        }
        BracketsStatus::NotClosed(stack.last().unwrap().1.clone())
    }

    pub(crate) fn run(
        &mut self,
        tokens: &Vec<(Token, TokenPos)>,
    ) -> Result<String, String> {
        // use crate::grammar::DataType::*;
        // use crate::grammar::DeclarationType::*;
        // use crate::grammar::WeakNonTerminal;
        use crate::token::LeftOrRight::{Left, Right};
        use crate::token::BracketType::*;
        use crate::symbol::SymbolType;

        match Parser::are_brackets_balanced(tokens.into_iter()) {
            BracketsStatus::Ok => (),
            BracketsStatus::NotClosed(pos) => {
                return Err(format!(
                    "bracket opened in line {}, column {}, not closed",
                    pos.line.to_string(), pos.column.to_string()));
            }
            BracketsStatus::NotOpened(pos) => {
                return Err(format!(
                    "closing bracket in line {}, column {}, doesn't have a matching opening bracket",
                    pos.line.to_string(), pos.column.to_string()));
            }
        }

        if tokens.is_empty() {
            return Ok(String::new());
        }

        let mut data_seg = Vec::<String>::new();
        let mut bss_seg = Vec::<String>::new();
        let mut text_seg = Vec::<String>::new();
        let mut i: usize = 0;
        while i < tokens.len() {
            let tok_and_pos = &tokens[i];
            let (t, pos) = tok_and_pos;
            match t.token_type {
                TokenType::Name => {
                    let next_tok_and_pos = &tokens[i + 1];
                    let (next_tok, next_tok_pos) = next_tok_and_pos;
                    match next_tok.token_type {
                        TokenType::Bracket(
                            crate::token::Bracket {
                                left_or_right: Left,
                                bracket_type: br_type,
                            }) => {
                            match br_type {
                                Round => {
                                    todo!("fn def")
                                }
                                Curly => {
                                    return Err(generate_unexpected_token_error_message(next_tok_and_pos));
                                }
                                Square => {
                                    todo!("vec def")
                                }
                            }
                        }
                        TokenType::Constant(const_tok) => {
                            todo!()
                        }
                        TokenType::Semicolon => {
                            if let Some(prev_value) = self.symbol_table.insert(
                                t.value.as_ref().unwrap().clone(),
                                SymbolType::Variable {
                                    declaration_specifier: None,
                                    first_occurrence: Some(pos.clone()),
                                }) {
                                return Err(generate_redefinition_error_message(
                                    tok_and_pos,
                                    prev_value,
                                ));
                            }
                            bss_seg.push(
                                format!("{} rq {}", t.value.as_ref().unwrap(),
                                        match self.compiler_options.target_platform.arch {
                                            Arch::x86_32 => { 1 }
                                            Arch::x86_64 => { 2 }
                                        }
                                )
                            );
                            i += 2;
                            continue;
                        }
                        _ => return Err(generate_unexpected_token_error_message(next_tok_and_pos))
                    }
                }
                _ => return Err(generate_unexpected_token_error_message(tok_and_pos))
            }
            i += 1;
        }
        panic!();
    }
}