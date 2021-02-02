use crate::config::{CompilerOptions, SymbolTable, TypeOfColumnNo, TypeOfLineNo};
use crate::generate_error_message_with_line_no;
use crate::lexical_analyzer::TokenPos;
use crate::token::{Bracket, BracketType, LeftOrRight, Token};

pub struct AbstractSyntaxNode<'a> {
    parent: Option<&'a AbstractSyntaxNode<'a>>,
    children: Option<Vec<&'a AbstractSyntaxNode<'a>>>,
}

impl<'a> AbstractSyntaxNode<'a> {
    fn new() -> AbstractSyntaxNode<'a> {
        AbstractSyntaxNode {
            parent: None,
            children: None,
        }
    }
}

pub struct AbstractSyntaxTree<'a> {
    root: Option<AbstractSyntaxNode<'a>>
}

impl<'a> AbstractSyntaxTree<'a> {
    pub fn new() -> AbstractSyntaxTree<'a> {
        AbstractSyntaxTree {
            root: Some(AbstractSyntaxNode::new())
        }
    }
}

pub struct Parser<'a> {
    pub compiler_options: &'a CompilerOptions,
    pub symbol_table: &'a SymbolTable,
    pub source_code: Option<&'a Vec<u8>>,
}

#[derive(Eq, PartialEq)]
enum BracketsStatus {
    Ok,
    NotClosed(TokenPos),
    NotOpened(TokenPos),
}

impl<'a> Parser<'a> {
    fn generate_ast(&self, tokens: &Vec<(Token, TokenPos)>) -> AbstractSyntaxTree {
        AbstractSyntaxTree::new()
    }

    fn are_brackets_balanced<'b, I>(mut tok_it: I) -> BracketsStatus
        where I: Iterator<Item=&'b (Token, TokenPos)> {
        use LeftOrRight::*;

        let mut stack = Vec::<(&Bracket, &TokenPos)>::new();
        while let Some(token) = tok_it.next() {
            if let (Token::Bracket(bracket), pos) = token {
                if bracket.left_or_right == Left {
                    stack.push((bracket, pos));
                } else {
                    match stack.pop() {
                        None => return BracketsStatus::NotOpened(pos.clone()),
                        Some((last, _)) => {
                            if *last != Bracket::paired_bracket(&bracket) {
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

    pub fn run(
        &mut self,
        tokens: &Vec<(Token, TokenPos)>,
    ) -> Result<(AbstractSyntaxTree, &SymbolTable), String> {
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
        Ok((AbstractSyntaxTree::new(), self.symbol_table))
    }
}