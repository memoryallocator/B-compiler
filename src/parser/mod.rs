use collections::{HashMap, HashSet};
use rc::{Rc, Weak};
use std::*;

use token::{Token, TokenType};

use crate::config::*;
use crate::lexical_analyzer::TokenPos;
use crate::parser::ast::*;
use crate::token;

pub(crate) mod ast;
mod expression_parser;

pub(crate) struct Parser<'a> {
    pub(crate) compiler_options: CompilerOptions,
    pub(crate) standard_library_names: &'a HashSet<StandardLibraryName>,
    pub(crate) source_code: Option<&'a str>,
}

#[derive(Eq, PartialEq)]
enum BracketsStatus {
    Ok,
    NotClosed(TokenPos),
    NotOpened(TokenPos),
}

#[derive(Debug)]
enum GlobalDefinition {
    Variable,
    Function { parameter_list_length: Option<usize> },
}

type GlobalDefinitions<'a> = HashMap<Rc<String>, (GlobalDefinition, Option<TokenPos>)>;

pub(crate) struct Scope<'a> {
    global_scope: &'a GlobalDefinitions<'a>,
    curr_fn: Weak<FunctionDefinitionNode>,
    parent_scope_variables: HashMap<&'a String, usize>,
    curr_scope_variables: HashSet<&'a String, usize>,
    imported_variables: HashSet<&'a String>,
    labels: HashSet<&'a String>,
}

type ScopeTable<'a> = HashMap<Weak<StatementNode>, Scope<'a>>;

impl Parser<'_> {
    fn find_brackets_pairs<'b, I>(tok_it: I) -> (BracketsStatus, Option<Vec<Token>>)
        where I: Iterator<Item=&'b Token> {
        use token::LeftOrRight::*;
        use token::Bracket;

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
                    let pos = pos.clone();
                    match stack.pop() {
                        None => return (BracketsStatus::NotOpened(pos.clone()), None),
                        Some((
                                 &left_br,
                                 left_br_pos,
                                 left_br_idx)
                        ) => {
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
        (BracketsStatus::NotClosed(stack.pop().unwrap().1.clone()), None)
    }

    fn postprocess(&mut self, root: &ProgramNode) -> (Vec<Warning>, Result<ScopeTable, Error>) {
        let mut global_defs: GlobalDefinitions
            = self.standard_library_names
            .into_iter()
            .map(|x|
                match x {
                    StandardLibraryName::Function { fn_name, parameter_list_length } =>
                        (fn_name.clone(), (GlobalDefinition::Function {
                            parameter_list_length: *parameter_list_length
                        }, None)),

                    StandardLibraryName::Variable { var_name } =>
                        (var_name.clone(), (GlobalDefinition::Variable, None))
                })
            .collect();

        let mut warnings = Vec::<Warning>::new();

        fn redefinition_check(
            curr_def: Rc<DefinitionNode>,
            global_defs: &GlobalDefinitions,
        ) -> (Vec<Warning>, Result<(), Error>) {
            let name = curr_def.get_name();
            let mut warnings = Vec::<Warning>::new();

            if let Some(
                (_, prev_def_pos)
            ) = global_defs.get(name.as_ref()) {
                let curr_def = curr_def.clone();
                let prev_def_pos = prev_def_pos.clone();

                if prev_def_pos.is_some() {
                    return (warnings, Err(Error::NameRedefined { curr_def, prev_def_pos }));
                }

                warnings.push(Warning::StandardNameRedefined(curr_def.clone()));
            }

            (warnings, Ok(()))
        }

        for curr_def in root.get_definitions() {
            let redef_check_res = redefinition_check(curr_def.clone(),
                                                     &global_defs);
            warnings.extend(redef_check_res.0);

            if let Err(error) = redef_check_res.1 {
                return (warnings, Err(error));
            }

            match curr_def.as_ref() {
                DefinitionNode::Variable(var_def) => {
                    let var_name = &var_def.name;

                    if let Some(
                        (_, prev_def_pos)
                    ) = global_defs.get(var_name) {
                        let curr_def = curr_def.clone();
                        let prev_def_pos = prev_def_pos.clone();

                        if prev_def_pos.is_some() {
                            return (warnings, Err(Error::NameRedefined { curr_def, prev_def_pos }));
                        }

                        warnings.push(Warning::StandardNameRedefined(curr_def.clone()));
                    }

                    if let Some(
                        Ival::Name(init_name, init_pos)
                    ) = &var_def.initial_value {
                        if init_name == var_name {
                            warnings.push(Warning::InitVarWithItself(curr_def.clone(),
                                                                     init_pos.clone()));
                        } else {
                            if !global_defs.contains_key(init_name) {
                                return (warnings, Err(Error::NameNotDefined(init_name.clone(),
                                                                            init_pos.clone())));
                            }
                        }
                    }

                    global_defs.insert(var_name.clone(),
                                       (GlobalDefinition::Variable, Some(var_def.get_position())));
                }

                DefinitionNode::Vector(vec_def) => {
                    let vec_name = &vec_def.name;
                    if global_defs.contains_key(vec_name) {}

                    let sp_size = vec_def.get_specified_size();
                    let inits = vec_def.get_initial_values();

                    if sp_size.is_none() && inits.is_none() {
                        warnings.push(Warning::VecWithNoSizeAndInits(vec_def.clone()))
                    }

                    if let Some(sp_size) = sp_size {
                        match sp_size.get_type() {
                            token::Constant::Char =>
                                warnings.push(Warning::VecSizeIsNotANumber(vec_def.clone())),
                            token::Constant::String =>
                                return (warnings, Err(Error::VecSizeIsString(vec_def.clone()))),
                            _ => ()
                        }
                    }

                    global_defs.insert(vec_name.clone(),
                                       (GlobalDefinition::Variable, Some(vec_def.get_position())));
                }

                DefinitionNode::Function(fn_def) => {
                    dbg!(&global_defs);
                    todo!()
                }
            }
        }

        todo!()
    }

    pub(crate) fn run(
        &mut self,
        tokens: &[Token],
    ) -> (Vec<Warning>, Result<(ast::ProgramNode, ScopeTable), Error>) {
        let tokens =
            match Parser::find_brackets_pairs(tokens.into_iter()) {
                (BracketsStatus::Ok, processed_tokens) => processed_tokens.unwrap(),
                (BracketsStatus::NotClosed(pos), _) => {
                    return (vec![], Err(Error::BracketNotClosed(pos)));
                }
                (BracketsStatus::NotOpened(pos), _) => {
                    return (vec![], Err(Error::BracketNotOpened(pos)));
                }
            };

        if tokens.is_empty() {
            return (vec![], Err(Error::EmptyTokenStream));
            // return Ok((ProgramNode::new(), &self.symbol_table, vec![]));
        }

        let prog_node = ProgramNode::parse_exact(&tokens);
        if prog_node.is_none() {
            return (vec![], Err(Error::ParsingError));
            // TODO: failure analysis & detailed error report
        }

        let prog_node = prog_node.unwrap();

        let (warnings, scope_table) = self.postprocess(&prog_node);

        (warnings, match scope_table {
            Ok(scope_table) => Ok((prog_node, scope_table)),
            Err(err) => Err(err),
        })
    }
}