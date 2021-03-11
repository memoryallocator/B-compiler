use std::borrow::Borrow;

use crate::config::*;
use crate::lexical_analyzer::TokenPos;
use crate::token::{Token, TokenType};

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
    return format!("{}: unexpected token {}", pos.borrow(), t.borrow().r#type);
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
                   t.r#type,
                   t.val.as_ref().unwrap().clone(),
                   prev_def_tok.borrow());
}

impl Parser<'_> {
    fn are_brackets_balanced<'b, I>(mut tok_it: I) -> BracketsStatus
        where I: Iterator<Item=&'b Token> {
        use crate::token::LeftOrRight::*;
        use crate::token::Bracket;

        let mut stack = Vec::<(&Bracket, &TokenPos)>::new();
        while let Some(token) = tok_it.next() {
            if let Token {
                r#type: TokenType::Bracket(br),
                pos,
                ..
            } = token {
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
        BracketsStatus::NotClosed(*stack.pop().unwrap().1)
    }

    pub(crate) fn run(
        &mut self,
        tokens: &[Token],
    ) -> Result<crate::ast::ProgramNode, String> {
        use crate::ast::*;

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
            return Ok(ProgramNode::new());
        }

        if let Some((pr_node, adv)) = ProgramNode::parse(tokens) {
            if adv != tokens.len() {  // some tokens remain unused!
                return Err("Internal error".to_string());
            }
            return Ok(pr_node);
        }
        return Err("Failed to parse".to_string());
        // TODO: failure analysis & detailed error report
    }
}