use std::rc::Weak;

use crate::lexical_analyzer::TokenPos;
use crate::token::{Token, TokenType};

pub(crate) trait Parse {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized;
}

#[derive(Debug)]
enum Ival {
    Constant(Constant),
    Name(String),
}

impl Parse for Ival {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        return Some((
            if let Some(t) = tokens.first() {
                match t.r#type {
                    TokenType::Name => Ival::Name(t.val.as_ref().unwrap().clone()),
                    TokenType::Constant(constant_type) => Ival::Constant(
                        Constant {
                            constant_type,
                            value: t.val.as_ref().unwrap().clone(),
                        }),
                    _ => return None,
                }
            } else {
                return None;
            }, 1));
    }
}

#[derive(Debug, Clone)]
struct Constant {
    constant_type: crate::token::Constant,
    value: String,
}

impl Parse for Constant {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if let Some(Token {
                        r#type: TokenType::Constant(constant_type),
                        val,
                        ..
                    }) = tokens.first() {
            return Some((Constant {
                constant_type: constant_type.clone(),
                value: val.as_ref().unwrap().clone(),
            }, 1));
        }
        None
    }
}

#[derive(Debug)]
pub(crate) struct VariableDefinitionNode {
    name: String,
    initial_value: Option<Ival>,
}

impl Parse for VariableDefinitionNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)> {
        let tokens_cut: Vec<&Token> = tokens.into_iter()
            .take_while(|t| t.r#type != TokenType::Semicolon)
            .collect();
        if tokens_cut.len() == tokens.len() {  // no semicolon found
            return None;
        }
        return
            Some((
                if let Some(
                    Token {
                        r#type: TokenType::Name,
                        val: name,
                        ..
                    }) = tokens_cut.get(0) {
                    let name = name.as_ref().unwrap().clone();

                    VariableDefinitionNode {
                        name,
                        initial_value: match tokens_cut.get(1) {
                            None => None,
                            Some(&t) =>
                                if let Some((ival, _)) = Ival::parse(&[t.clone()]) {
                                    Some(ival)
                                } else {
                                    return None;
                                }
                        },
                    }
                } else {
                    return None;
                }, tokens_cut.len() + 1));
    }
}

#[derive(Debug)]
pub(crate) struct VectorDefinitionNode {
    name: String,
    specified_element_count: Option<Constant>,
    initial_values: Option<Vec<Ival>>,
}

impl Parse for VectorDefinitionNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::{Bracket, BracketType};
        use crate::token::LeftOrRight::*;

        let tokens_cut: Vec<&Token> = tokens.into_iter()
            .take_while(|t| t.r#type != TokenType::Semicolon)
            .collect();
        if tokens_cut.len() == tokens.len() {  // no semicolon found
            return None;
        }

        const LEFT_SQUARE_BRACKET: Bracket = Bracket {
            left_or_right: Left,
            bracket_type: BracketType::Square,
        };
        const RIGHT_SQUARE_BRACKET: Bracket = Bracket {
            left_or_right: Right,
            bracket_type: BracketType::Square,
        };

        if tokens_cut.len() < 2 {
            return None;
        }
        let node = match tokens_cut[..2] {
            [Token {
                r#type: TokenType::Name,
                val: name,
                ..
            }, Token {
                r#type: TokenType::Bracket(LEFT_SQUARE_BRACKET),
                ..
            }] => {
                let name = name.as_ref().unwrap().clone();
                let specified_element_count;
                let right_bracket_pos;

                if let Some((r#const, _)) = Constant::parse(&tokens[2..]) {
                    right_bracket_pos = 3;
                    specified_element_count = Some(r#const);
                } else {
                    right_bracket_pos = 2;
                    specified_element_count = None;
                }

                if let Some(Token {
                                r#type: TokenType::Bracket(RIGHT_SQUARE_BRACKET), ..
                            }) = tokens_cut.get(right_bracket_pos) {
                    if tokens_cut.get(right_bracket_pos + 1).is_none() {  // name [ const? ] ;
                        VectorDefinitionNode {
                            name,
                            specified_element_count,
                            initial_values: None,
                        }
                    } else {
                        let mut initial_values = Vec::<Ival>::new();
                        let first_ival_pos: usize = right_bracket_pos + 1;
                        while let Some((ival, _)) = Ival::parse(
                            &tokens[first_ival_pos + 2 * initial_values.len()..]) {
                            let comma_pos = first_ival_pos + 2 * initial_values.len() + 1;
                            let next = tokens_cut.get(comma_pos);
                            match next {
                                Some(
                                    Token {
                                        r#type: TokenType::Comma,
                                        ..
                                    }) => {  // ival , ...
                                    initial_values.push(ival);
                                }
                                None => {  // ival $
                                    initial_values.push(ival);
                                    break;
                                }
                                _ => return None,
                            }
                        }
                        VectorDefinitionNode {
                            name,
                            specified_element_count,
                            initial_values: Some(initial_values),
                        }
                    }
                } else {
                    return None;
                }
            }
            _ => return None
        };
        Some((node, tokens_cut.len() + 1))
    }
}

#[derive(Debug)]
pub(crate) struct FunctionDefinitionNode {
    name: String,
    parameter_names: Option<Vec<String>>,
    body: StatementNode,
}

impl Parse for FunctionDefinitionNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        todo!()
    }
}

#[derive(Debug)]
struct AutoDeclarationNode {
    name: String,
    vector_size: Option<Constant>,
    next_statement: Box<StatementNode>,
}

#[derive(Debug)]
struct ExternDeclarationNode {
    name: String,
    next_statement: Box<StatementNode>,
}

#[derive(Debug)]
struct LabelDeclarationNode {
    name: String,
    next_statement: Box<StatementNode>,
}

#[derive(Debug)]
struct RvalueNode {}

#[derive(Debug)]
struct CaseStatementNode {
    value: Constant,
    statement: StatementNode,
}

#[derive(Debug)]
struct SwitchStatementNode {
    rvalue: RvalueNode,
    cases: Vec<CaseStatementNode>,
}

#[derive(Debug)]
struct IfStatementNode {
    condition: RvalueNode,
    r#else: Option<Box<StatementNode>>,
}

#[derive(Debug)]
struct WhileNode {
    condition: RvalueNode,
    body: Box<StatementNode>,
}

#[derive(Debug)]
struct CompoundStatementNode {}

#[derive(Debug)]
struct GotoNode {}

#[derive(Debug)]
struct ReturnNode {
    rvalue: Option<RvalueNode>
}

#[derive(Debug)]
struct RvalueAndSemicolonNode {
    rvalue: RvalueNode
}

#[derive(Debug)]
struct NullStatementNode {}

#[derive(Debug)]
enum Statement {
    AutoDeclaration(AutoDeclarationNode),
    ExternDeclaration(ExternDeclarationNode),
    LabelDeclaration(LabelDeclarationNode),
    SwitchCase(SwitchStatementNode),
    Compound(CompoundStatementNode),
    If(IfStatementNode),
    Goto(GotoNode),
    Return(ReturnNode),
    RvalueAndSemicolon(RvalueAndSemicolonNode),
    NullStatement(NullStatementNode),
}

#[derive(Debug)]
struct StatementNode {
    position: TokenPos,
    statement: Statement,
    parent: Option<Weak<StatementNode>>,
}

#[derive(Debug)]
pub(crate) enum DefinitionNode {
    Variable(VariableDefinitionNode),
    Vector(VectorDefinitionNode),
    Function(FunctionDefinitionNode),
}

impl Parse for DefinitionNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if let Some((var_def_node, adv)) = VariableDefinitionNode::parse(tokens) {
            return Some((DefinitionNode::Variable(var_def_node), adv));
        }
        if let Some((vec_def_node, adv)) = VectorDefinitionNode::parse(tokens) {
            return Some((DefinitionNode::Vector(vec_def_node), adv));
        }
        if let Some((fn_def_node, adv)) = FunctionDefinitionNode::parse(tokens) {
            return Some((DefinitionNode::Function(fn_def_node), adv));
        }
        None
    }
}

// #[derive(Debug)]
// enum AbstractSyntaxNode {
//     Definition(DefinitionNode)
// }

// #[derive(Debug)]
// struct AbstractSyntaxTree {
//     root: ProgramNode
// }

#[derive(Debug)]
pub(crate) struct ProgramNode {
    definitions: Vec<DefinitionNode>,
}

impl ProgramNode {
    pub(crate) fn new() -> Self {
        ProgramNode::default()
    }

    // pub(crate) fn append_definition(&mut self, def_node: DefinitionNode) {
    //     self.definitions.push(def_node);
    // }
}

impl From<Vec<DefinitionNode>> for ProgramNode {
    fn from(defs: Vec<DefinitionNode>) -> Self {
        ProgramNode {
            definitions: defs
        }
    }
}

impl Default for ProgramNode {
    fn default() -> Self {
        ProgramNode {
            definitions: Vec::default()
        }
    }
}

impl Parse for ProgramNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        let mut defs = Vec::<DefinitionNode>::new();
        let mut offset: usize = 0;
        while let Some((def_node, adv)) = DefinitionNode::parse(&tokens[offset..]) {
            offset += adv;
            defs.push(def_node);
        }
        Some((ProgramNode::from(defs), offset))
    }
}