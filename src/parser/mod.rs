use std::borrow::Borrow;

use crate::config::*;
use crate::lexical_analyzer::TokenPos;
use crate::parser::ast::*;
use crate::token::{Token, TokenType};

mod ast;
mod expression_parser;

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
    fn find_brackets_pairs<'b, I>(tok_it: I) -> (BracketsStatus, Option<Vec<Token>>)
        where I: Iterator<Item=&'b Token> {
        use crate::token::LeftOrRight::*;
        use crate::token::Bracket;

        let mut stack = Vec::<(&Bracket, &TokenPos, usize)>::new();
        let mut processed_tokens = Vec::<Token>::new();
        let mut tok_it = tok_it.enumerate();
        while let Some((i, token)) = tok_it.next() {
            if let Token {
                r#type: TokenType::Bracket(br),
                pos,
                ..
            } = token {
                if br.left_or_right == Left {
                    stack.push((br, pos, i));
                    processed_tokens.push(token.clone());
                } else {
                    let pos = *pos;
                    match stack.pop() {
                        None => return (BracketsStatus::NotOpened(pos.clone()), None),
                        Some((&left_br, left_br_pos, left_br_idx)) => {
                            if left_br != Bracket::paired_bracket(&br) {
                                return (BracketsStatus::NotOpened(pos.clone()), None);
                            }

                            let left_br = &mut processed_tokens[left_br_idx];
                            left_br.val = Some(pos.repr());

                            processed_tokens.push(Token {
                                r#type: TokenType::Bracket(*br),
                                val: Some(left_br_pos.repr()),
                                pos,
                            });
                        }
                    }
                }
            } else {
                processed_tokens.push(token.clone());
            }
        }
        if stack.is_empty() {
            return (BracketsStatus::Ok, Some(processed_tokens));
        }
        (BracketsStatus::NotClosed(*stack.pop().unwrap().1), None)
    }

    // fn extract_function_definitions(pr: &ProgramNode) -> Vec<Weak<ast::FunctionDefinitionNode>> {
    //     pr.get_definitions()
    //         .filter_map(|def|
    //             if let DefinitionNode::Function(fn_def) = def {
    //                 Some(Rc::downgrade(fn_def))
    //             } else {
    //                 None
    //             })
    //         .collect()
    // }

    fn postprocess(&mut self, root: &ProgramNode) -> Result<Vec<Warning>, String> {
        dbg!(&root);
        todo!()
    }

    pub(crate) fn run(
        &mut self, tokens: &[Token],
    ) -> Result<(ast::ProgramNode, &SymbolTable, Vec<Warning>), String> {
        let tokens =
            match Parser::find_brackets_pairs(tokens.into_iter()) {
                (BracketsStatus::Ok, processed_tokens) => processed_tokens.unwrap(),
                (BracketsStatus::NotClosed(pos), _) => {
                    return Err(format!(
                        "bracket opened in line {}, column {}, not closed",
                        pos.line.to_string(), pos.column.to_string()));
                }
                (BracketsStatus::NotOpened(pos), _) => {
                    return Err(format!(
                        "closing bracket in line {}, column {}, doesn't have a matching opening bracket",
                        pos.line.to_string(), pos.column.to_string()));
                }
            };

        if tokens.is_empty() {
            return Ok((ProgramNode::new(), &self.symbol_table, vec![]));
        }

        let prog_node = ProgramNode::parse_exact(&tokens);
        if prog_node.is_none() {
            return Err("Failed to parse".to_string());
        }
        // TODO: failure analysis & detailed error report

        let prog_node = prog_node.unwrap();

        let warnings = self.postprocess(&prog_node);
        Ok((prog_node, &self.symbol_table, warnings?))
    }
}