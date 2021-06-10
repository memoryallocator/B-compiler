use std::*;
use borrow::Borrow;
use collections::HashMap;
use convert::TryFrom;
use hash::Hash;

use crate::parser::analyzer::Analyzer;
pub(crate) use crate::parser::analyzer::ScopeTable;
pub(crate) use crate::parser::analyzer::{DeclInfoAndPos, DefInfoAndPos};
use crate::parser::ast::*;
use flat_ast::{FlatNodeAndPos, FlattenNode};

use crate::config::*;
use crate::lexical_analyzer::token;
use token::*;

pub mod ast;
mod expression_parser;
mod analyzer;

pub(crate) type FlatAst = Vec<FlatNodeAndPos>;

pub(crate) trait Parse {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized;
}

pub(crate) trait ParseExact: Parse {
    fn parse_exact(input: &[Token]) -> Result<Self, ()>
        where Self: Sized {
        if let Ok((obj, adv)) = Self::parse(&input) {
            if adv == input.len() {
                return Ok(obj);
            }
        }
        Err(())
    }
}

impl Parse for Ival {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        return Ok((
            if let Some(t) = input.first() {
                match &t.token {
                    WrappedToken::Name(name) => Ival::Name(name.clone(), t.pos),

                    WrappedToken::Constant(_) =>
                        Ival::Constant(ConstantNode::try_from(t).unwrap()),
                    _ => return Err(()),
                }
            } else {
                return Err(());
            }, 1));
    }
}

impl Parse for ConstantNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if let Some(t) = input.get(0) {
            if let Ok(constant) = ConstantNode::try_from(t) {
                return Ok((constant, 1));
            }
        }
        Err(())
    }
}

impl ParseExact for ConstantNode {}

impl Parse for VariableDefinitionNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()> {
        let toks_trim: Vec<&Token> = input.into_iter()
            .take_while(|t| t.token != WrappedToken::Semicolon)
            .collect();

        if toks_trim.len() == input.len() {  // no semicolon found
            return Err(());
        }
        return
            Ok((
                if let Some(
                    Token {
                        token: WrappedToken::Name(name),
                        pos: name_pos,
                    }) = toks_trim.get(0) {
                    let name = name.clone();

                    VariableDefinitionNode {
                        position: name_pos.clone(),
                        name,
                        initial_value: match toks_trim.get(1) {
                            None => None,
                            Some(&t) =>
                                if let Ok((ival, _)) = Ival::parse(&[t.clone()]) {
                                    Some(ival)
                                } else {
                                    return Err(());
                                }
                        },
                    }
                } else {
                    return Err(());
                }, toks_trim.len() + 1));
    }
}

impl Parse for VectorDefinitionNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        let toks_trim: Vec<&Token> =
            input
                .into_iter()
                .take_while(|t| t.token != WrappedToken::Semicolon)
                .collect();

        if toks_trim.len() == input.len() {  // no semicolon found
            return Err(());
        }

        {
            let minimal_len = 3;  // name [ ]
            if toks_trim.len() < minimal_len {
                return Err(());
            }
        }

        let name_and_left_br_len = 2;
        let node =
            match toks_trim[..name_and_left_br_len] {
                [Token {
                    token: WrappedToken::Name(name),
                    pos: name_pos
                }, Token {
                    token: WrappedToken::Bracket(
                        Bracket {
                            left_or_right: LeftOrRight::Left,
                            bracket_type: BracketType::Square, ..
                        }
                    ), ..
                }] => {
                    let name = name.clone();
                    let left_br_idx = name_and_left_br_len - 1;

                    let right_bracket_idx
                        = get_right_bracket_index(input, left_br_idx).ok_or(())?;

                    if right_bracket_idx - left_br_idx > 2 {  // name [ ... ... ]
                        return Err(());
                    }

                    let specified_size =
                        if right_bracket_idx == left_br_idx + 1 {  // name [ ]
                            None
                        } else {
                            if let Ok(size) = ConstantNode::parse_exact(
                                &input[left_br_idx + 1..right_bracket_idx]) {
                                Some(size)
                            } else {
                                return Err(());
                            }
                        };

                    if toks_trim.get(right_bracket_idx + 1).is_none() {  // name [ const? ] ;
                        VectorDefinitionNode {
                            position: name_pos.clone(),
                            name,
                            specified_size,
                            initial_values: vec![],
                        }
                    } else {
                        let mut initial_values = vec![];
                        let first_ival_idx: usize = right_bracket_idx + 1;
                        let mut next_ival_idx = first_ival_idx;

                        while let Ok((ival, _)) = Ival::parse(&input[next_ival_idx..]) {
                            let comma_pos = next_ival_idx + 1;
                            initial_values.push(ival);

                            let next = toks_trim.get(comma_pos);
                            match next {
                                Some(Token {
                                         token: WrappedToken::Comma, ..
                                     }) => (),  // ival , ...

                                None => {  // ival $
                                    break;
                                }
                                _ => return Err(()),
                            }
                            next_ival_idx = first_ival_idx + 2 * initial_values.len();
                        }

                        VectorDefinitionNode {
                            position: name_pos.clone(),
                            name,
                            specified_size,
                            initial_values,
                        }
                    }
                }
                _ => return Err(())
            };
        Ok((node, toks_trim.len() + 1))
    }
}

fn parse_name_list(
    tokens: &[Token],
    allow_duplicates: bool,
) -> Result<Vec<(String, TokenPos)>, ()> {
    if tokens.is_empty() {
        return Ok(Default::default());
    }

    let mut res = MultiMap::new();
    loop {
        let name_idx = res.len() * 2;
        let name_pos;
        let name;
        let comma_idx;

        if let Some(
            Token { token: WrappedToken::Name(tok_name), pos }
        ) = tokens.get(name_idx) {
            name_pos = *pos;
            name = tok_name.clone();
            comma_idx = name_idx + 1;
        } else {
            return Err(());  // the input is not empty, but no name found
        }

        if res.contains_key(&name) && !allow_duplicates {
            return Err(());  // duplicate found
        }
        res.insert(name, (res.len(), name_pos));

        match tokens.get(comma_idx) {
            Some(Token { token: WrappedToken::Comma, .. }) => (),  // OK, matched "name ,"
            None => {  // end of input, matched "[name_list ,] name"
                let mut res = res.extract_inner()
                    .into_iter()
                    .map(|(k, v)|
                        v.into_iter()
                            .map(|x| (k.clone(), x))
                            .collect::<Vec<(String, (usize, TokenPos))>>())
                    .flatten()
                    .collect::<Vec<(String, (usize, TokenPos))>>();

                res.sort_unstable_by(|(_, (lhs_order, _)), (_, (rhs_order, _))|
                    lhs_order.partial_cmp(rhs_order).unwrap());

                return Ok(res.into_iter()
                    .map(|(name, (_, pos))| (name, pos))
                    .collect());
            }
            Some(_) => return Err(()),  // the next symbol is not a comma
        }
    }
}

impl Parse for FunctionDefinitionNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 4 {  // name ( ) ;
            return Err(());
        }

        match &input[..2] {
            [Token {
                token: WrappedToken::Name(name),
                pos: name_pos
            }, Token {
                token: WrappedToken::Bracket(
                    Bracket {
                        left_or_right: LeftOrRight::Left,
                        bracket_type: BracketType::Round, ..
                    }
                ), ..
            }] => {
                let mut toks_consumed: usize = 2;

                let left_br_idx = 1;
                let right_bracket_index
                    = get_right_bracket_index(input, left_br_idx).ok_or(())?;

                let parameter_names =
                    if let Ok(names) = parse_name_list(&input[toks_consumed..right_bracket_index],
                                                       false) {
                        names
                    } else {
                        return Err(());
                    };

                toks_consumed = right_bracket_index + 1;
                return if let Ok(
                    (stmt, adv)
                ) = StatementNode::parse(&input[toks_consumed..]) {
                    let name = name.clone();
                    toks_consumed += adv;

                    Ok((
                        FunctionDefinitionNode {
                            position: name_pos.clone(),
                            name,
                            parameters: parameter_names,
                            body: stmt,
                        }, toks_consumed
                    ))
                } else {
                    Err(())
                };
            }
            _ => Err(())
        }
    }
}

impl Parse for AutoDeclaration {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.is_empty() {
            return Err(());
        }

        if input.len() >= 4 {
            if let [
            Token { token: WrappedToken::Name(name), .. },
            Token {
                token: WrappedToken::Bracket(
                    Bracket {
                        left_or_right: LeftOrRight::Left,
                        bracket_type: BracketType::Square, ..
                    }
                ), ..
            },
            Token {
                token: WrappedToken::Constant(vec_size),
                pos: const_pos,
            },
            Token {
                token: WrappedToken::Bracket(
                    Bracket {
                        left_or_right: LeftOrRight::Right,
                        bracket_type: BracketType::Square, ..
                    }
                ), ..
            }] = &input[..4] {
                let name = name.clone();

                let vector_size = Some(ConstantNode {
                    position: const_pos.clone(),
                    constant: vec_size.clone(),
                });

                return Ok((AutoDeclaration {
                    position: input[0].pos,
                    name,
                    size_if_vector: vector_size,
                }, 4));
            }
        }

        if let Token { token: WrappedToken::Name(name), pos } = &input[0] {
            let name = name.clone();

            return Ok((AutoDeclaration {
                position: *pos,
                name,
                size_if_vector: None,
            }, 1));
        }
        Err(())
    }
}

fn get_semicolon_pos(tokens: &[Token]) -> Option<usize> {
    tokens.into_iter()
        .position(|c| c.token == WrappedToken::Semicolon)
}

impl Parse for AutoDeclarationNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 4 {  // auto name ; null_stmt
            return Err(());
        }

        return if let Some(
            Token {
                token: WrappedToken::ReservedName(ReservedName::DeclarationSpecifier(
                                                      DeclarationSpecifier::Auto)),
                pos, ..
            }
        ) = input.first() {
            let semicolon_pos =
                if let Some(sp_minus_one) = get_semicolon_pos(&input[1..]) {
                    sp_minus_one + 1
                } else {
                    return Err(());
                };

            let next_statement_starts_at = semicolon_pos + 1;
            if input.len() <= next_statement_starts_at {
                return Err(());
            }

            let declarations;
            if let Ok(decls) = get_auto_decl_list(
                &input[1..semicolon_pos]) {
                declarations = decls;
            } else {
                return Err(());
            }

            if let Ok(
                (stmt_node, adv)
            ) = StatementNode::parse(&input[next_statement_starts_at..]) {
                Ok((AutoDeclarationNode {
                    position: pos.clone(),
                    declarations,
                    next_statement: stmt_node,
                }, next_statement_starts_at + adv))
            } else {
                Err(())
            }
        } else {
            Err(())
        };
    }
}

fn get_auto_decl_list(tokens: &[Token]) -> Result<Vec<AutoDeclaration>, ()> {
    if tokens.is_empty() {
        return Err(());
    }

    let mut res = HashMap::<String, (AutoDeclaration, usize)>::new();

    let mut i: usize = 0;
    while i < tokens.len() {
        while let Ok(
            (auto_decl, adv)
        ) = AutoDeclaration::parse(&tokens[i..]) {
            if let Some(_prev_decl) = res.insert(auto_decl.name.clone(),
                                                 (auto_decl, res.len())) {
                return Err(());
            }
            i += adv;

            match tokens.get(i) {
                None => {
                    let mut res = res.into_iter()
                        .map(|(_, v)| v)
                        .collect::<Vec<(AutoDeclaration, usize)>>();

                    res.sort_unstable_by(|(_, lhs_order), (_, rhs_order)|
                        lhs_order.partial_cmp(rhs_order).unwrap());

                    return Ok(res.into_iter()
                        .map(|(auto_decl, _)| auto_decl)
                        .collect());
                }
                Some(Token { token: WrappedToken::Comma, .. }) => {
                    i += 1;
                    continue;
                }
                _ => return Err(())
            }
        }
        return Err(());
    }
    unreachable!()
}

impl Parse for ExternDeclarationNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 4 {  // extrn name ; null_stmt
            return Err(());
        }

        use token::DeclarationSpecifier::Extrn;

        if let Some(
            Token {
                token: WrappedToken::ReservedName(ReservedName::DeclarationSpecifier(Extrn)), ..
            }
        ) = input.first() {
            let semicolon_pos =
                if let Some(sp_minus_one) = get_semicolon_pos(&input[1..]) {
                    sp_minus_one + 1
                } else {
                    return Err(());
                };

            let names =
                if let Ok(names) = parse_name_list(&input[1..semicolon_pos], true) {
                    names
                } else {
                    return Err(());
                };

            let next_statement_starts_at = semicolon_pos + 1;
            if input.len() <= next_statement_starts_at {
                return Err(());
            }

            if let Ok(
                (next_stmt, adv)
            ) = StatementNode::parse(&input[next_statement_starts_at..]) {
                return Ok((ExternDeclarationNode {
                    position: input[0].pos.clone(),
                    names,
                    next_statement: next_stmt,
                }, next_statement_starts_at + adv));
            }
        };

        Err(())
    }
}

impl Parse for LabelDeclarationNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 3 {
            return Err(());
        }

        return if let [
        Token { token: WrappedToken::Name(name), .. },
        Token { token: WrappedToken::Colon, .. }
        ] = &input[..2] {
            let name = name.clone();

            if let Ok((next_stmt, adv)) = StatementNode::parse(&input[2..]) {
                Ok((LabelDeclarationNode {
                    position: input[0].pos.clone(),
                    label_name: name,
                    next_statement: next_stmt,
                }, 2 + adv))
            } else {
                Err(())
            }
        } else {
            Err(())
        };
    }
}

fn extract_bracketed_rvalue(
    tokens: &[Token],
    bracket_type: BracketType,
) -> Result<(RvalueNode, usize), ()> {
    return if let Some(
        Token {
            token: WrappedToken::Bracket(br), ..
        }) = tokens.first() {
        if br.bracket_type != bracket_type {
            return Err(());
        }

        let br_expr = extract_bracketed_expression(tokens)?;
        if br_expr.is_empty() {
            return Err(());
        }

        Ok((RvalueNode::parse_exact(br_expr)?, 1 + br_expr.len() + 1))
    } else {
        Err(())
    };
}

impl ParseExact for RvalueNode {}

impl Parse for SwitchNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 5 {  // switch ( rvalue ) null_stmt
            return Err(());
        }

        use ControlStatementIdentifier::Switch;

        let left_br_idx = 1;
        return match &input[..left_br_idx + 1] {
            [
            Token { token: WrappedToken::ReservedName(ReservedName::ControlStatement(Switch)), .. },
            Token {
                token: WrappedToken::Bracket(
                    Bracket {
                        left_or_right: LeftOrRight::Left,
                        bracket_type: BracketType::Round, ..
                    }
                ), ..
            }
            ] => {
                let right_br_idx
                    = get_right_bracket_index(input, left_br_idx).ok_or(())?;

                if right_br_idx == left_br_idx + 1 {  // switch ( )
                    return Err(());
                }

                let rvalue = RvalueNode::parse_exact(
                    &input[left_br_idx + 1..right_br_idx])?;

                let (body, adv) = StatementNode::parse(&input[right_br_idx + 1..])?;
                let toks_consumed = right_br_idx + 1 + adv;

                Ok((SwitchNode {
                    rvalue,
                    body,
                }, toks_consumed))
            }
            _ => Err(())
        };
    }
}

impl Parse for CaseNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 3 {  // default : null_stmt
            return Err(());
        }

        use token::ControlStatementIdentifier::{Case, Default};

        let colon_pos: usize;
        let constant =
            match input[0].token {
                WrappedToken::ReservedName(ReservedName::ControlStatement(Case)) => {
                    colon_pos = 2;
                    Some(ConstantNode::parse_exact(&input[1..=1])?)
                }

                WrappedToken::ReservedName(ReservedName::ControlStatement(Default)) => {
                    colon_pos = 1;
                    None
                }

                _ => return Err(()),
            };

        if input[colon_pos].token != WrappedToken::Colon {
            return Err(());
        }

        let next_stmt_idx = colon_pos + 1;
        let (next_stmt, adv) = StatementNode::parse(&input[next_stmt_idx..])?;
        let toks_consumed = next_stmt_idx + adv;

        Ok((CaseNode {
            constant_if_not_default: constant,
            next_statement: next_stmt,
        }, toks_consumed))
    }
}

impl Parse for IfNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 5 {  // if ( rvalue ) ;
            return Err(());
        }

        use token::ControlStatementIdentifier::{If, Else};
        use token::BracketType::Round;

        if input[0].token != WrappedToken::ReservedName(ReservedName::ControlStatement(If)) {
            return Err(());
        }

        let (condition, adv) = extract_bracketed_rvalue(&input[1..],
                                                        Round)?;
        let mut toks_consumed = 1 + adv;

        let condition =
            if let Some(truth_value) = condition.try_to_truth_value() {
                truth_value
            } else {
                condition
            };

        let (body, adv) = StatementNode::parse(&input[toks_consumed..])?;
        toks_consumed += adv;

        let r#else =
            if let Some(tok) = input.get(toks_consumed) {
                if tok.token == WrappedToken::ReservedName(ReservedName::ControlStatement(Else)) {
                    toks_consumed += 1;
                    if toks_consumed == input.len() {
                        return Err(());
                    }

                    let (else_body, adv) = StatementNode::parse(
                        &input[toks_consumed..])?;
                    toks_consumed += adv;

                    Some(ElseNode { position: tok.pos, else_body })
                } else {
                    None
                }
            } else {
                None
            };

        Ok((IfNode {
            condition,
            body,
            r#else,
        }, toks_consumed))
    }
}

impl Parse for WhileNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 5 {  // while ( x ) ;
            return Err(());
        }

        use ControlStatementIdentifier::While;
        use BracketType::Round;

        if input[0].token != WrappedToken::ReservedName(ReservedName::ControlStatement(While)) {
            return Err(());
        }

        let (condition, adv) = extract_bracketed_rvalue(&input[1..],
                                                        Round)?;
        let mut toks_consumed = 1 + adv;

        let condition =
            if let Some(truth_value) = condition.try_to_truth_value() {
                truth_value
            } else {
                condition
            };

        if input.len() <= toks_consumed {
            return Err(());
        }

        let (body, adv) = StatementNode::parse(&input[toks_consumed..])?;
        toks_consumed += adv;

        return Ok((WhileNode {
            condition,
            body,
        }, toks_consumed));
    }
}

fn get_right_bracket_index(tokens: &[Token], left_br_idx: usize) -> Option<usize> {
    if let Some(
        Token {
            token: WrappedToken::Bracket(
                Bracket { left_or_right: LeftOrRight::Left, pair_pos, .. }
            ), ..
        }) = tokens.get(left_br_idx) {
        let right_bracket_pos = pair_pos.as_ref().unwrap();

        tokens
            .into_iter()
            .position(|t| t.pos == *right_bracket_pos)
    } else {
        unreachable!()
    }
}

pub(crate) fn extract_bracketed_expression(tokens: &[Token]) -> Result<&[Token], ()> {
    return if let Token {
        token: WrappedToken::Bracket(Bracket { left_or_right: LeftOrRight::Left, .. }), ..
    } = tokens.get(0).ok_or(())? {
        let right_br_idx
            = get_right_bracket_index(tokens, 0).ok_or(())?;

        Ok(&tokens[1..right_br_idx])
    } else {
        Err(())
    };
}

impl Parse for CompoundStatementNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 2 {
            return Err(());
        }

        use BracketType::Curly;

        if let WrappedToken::Bracket(
            Bracket {
                left_or_right: LeftOrRight::Left,
                bracket_type: Curly, ..
            }) = input[0].token {
            // OK
        } else {
            return Err(());
        }

        let br_expr = extract_bracketed_expression(input)?;
        let body_len = br_expr.len();
        let mut statement_list = vec![];

        let mut read: usize = 0;
        while read < body_len {
            let (stmt, adv) = StatementNode::parse(&br_expr[read..])?;
            read += adv;
            statement_list.push(stmt);
        }

        Ok((CompoundStatementNode {
            position: input[0].pos,
            statement_list,
        }, 1 + body_len + 1))
    }
}

impl Parse for GotoNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 3 {  // goto label ;
            return Err(());
        }

        use token::ControlStatementIdentifier::Goto;

        if input[0].token != WrappedToken::ReservedName(ReservedName::ControlStatement(Goto)) {
            return Err(());
        }

        let semicolon_pos = get_semicolon_pos(input).ok_or(())?;
        let toks_consumed = semicolon_pos + 1;

        let rvalue = &input[1..semicolon_pos];

        return Ok((GotoNode {
            label: RvalueNode::parse_exact(&rvalue)?,
        }, toks_consumed));
    }
}

impl Parse for ReturnNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.is_empty() {
            return Err(());
        }

        use BracketType::Round;
        use ControlStatementIdentifier::Return;

        if input[0].token != WrappedToken::ReservedName(ReservedName::ControlStatement(Return)) {
            return Err(());
        }

        let semicolon_pos = get_semicolon_pos(input).ok_or(())?;
        let toks_consumed = semicolon_pos + 1;

        if semicolon_pos == 1 {  // return ;
            return Ok((ReturnNode { rvalue: None }, 2));
        }

        let toks_trim = &input[..semicolon_pos];

        return Ok((ReturnNode {
            rvalue: Some(extract_bracketed_rvalue(&toks_trim[1..], Round)?.0),
        }, toks_consumed));
    }
}

impl Parse for RvalueAndSemicolonNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 2 {  // x ;
            return Err(());
        }

        let semicolon_pos = get_semicolon_pos(input).ok_or(())?;
        let toks_consumed = semicolon_pos + 1;

        let rvalue = &input[..semicolon_pos];

        return Ok((RvalueAndSemicolonNode {
            rvalue: RvalueNode::parse_exact(&rvalue)?
        }, toks_consumed));
    }
}

impl Parse for BreakNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.len() < 2 {
            return Err(());
        }

        use token::ControlStatementIdentifier::Break;

        match &input[..2] {
            [Token { token: WrappedToken::ReservedName(ReservedName::ControlStatement(Break)), .. },
            Token { token: WrappedToken::Semicolon, .. }] =>
                Ok((BreakNode {}, 2)),
            _ => Err(())
        }
    }
}

impl ParseExact for BreakNode {}

impl Parse for DeclarationNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.is_empty() {
            return Err(());
        }

        let first = &input[0];
        match first.token {
            WrappedToken::ReservedName(ReservedName::DeclarationSpecifier(ds)) => {
                match ds {
                    DeclarationSpecifier::Auto => {
                        if let Ok((auto_decl, adv)) = AutoDeclarationNode::parse(input) {
                            Ok((DeclarationNode::AutoDeclaration(auto_decl), adv))
                        } else {
                            Err(())
                        }
                    }
                    DeclarationSpecifier::Extrn => {
                        if let Ok((extern_decl, adv)) = ExternDeclarationNode::parse(input) {
                            Ok((DeclarationNode::ExternDeclaration(extern_decl), adv))
                        } else {
                            Err(())
                        }
                    }
                }
            }
            WrappedToken::Name(_) => {
                if let Ok((label_decl, adv)) = LabelDeclarationNode::parse(input) {
                    Ok((DeclarationNode::LabelDeclaration(label_decl), adv))
                } else {
                    Err(())
                }
            }
            _ => Err(()),
        }
    }
}

impl Parse for Statement {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.is_empty() {
            return Err(());
        }

        match &input[0].token {
            WrappedToken::ReservedName(res_name) => {
                match res_name {
                    ReservedName::ControlStatement(cs) => {
                        match cs {
                            ControlStatementIdentifier::If => {
                                if let Ok((r#if, adv)) = IfNode::parse(input) {
                                    Ok((Statement::If(r#if), adv))
                                } else {
                                    Err(())
                                }
                            }
                            ControlStatementIdentifier::Else => {
                                Err(())
                            }
                            ControlStatementIdentifier::Goto => {
                                if let Ok((goto, adv)) = GotoNode::parse(input) {
                                    Ok((Statement::Goto(goto), adv))
                                } else {
                                    Err(())
                                }
                            }
                            ControlStatementIdentifier::Switch => {
                                if let Ok((switch, adv)) = SwitchNode::parse(input) {
                                    Ok((Statement::Switch(switch), adv))
                                } else {
                                    Err(())
                                }
                            }
                            ControlStatementIdentifier::While => {
                                if let Ok((r#while, adv)) = WhileNode::parse(input) {
                                    Ok((Statement::While(r#while), adv))
                                } else {
                                    Err(())
                                }
                            }
                            ControlStatementIdentifier::Break => {
                                if let Ok((r#break, adv)) = BreakNode::parse(input) {
                                    Ok((Statement::Break(r#break), adv))
                                } else {
                                    Err(())
                                }
                            }
                            ControlStatementIdentifier::Return => {
                                if let Ok((r#return, adv)) = ReturnNode::parse(input) {
                                    Ok((Statement::Return(r#return), adv))
                                } else {
                                    Err(())
                                }
                            }
                            ControlStatementIdentifier::Case
                            | ControlStatementIdentifier::Default => {
                                if let Ok((case, adv)) = CaseNode::parse(input) {
                                    Ok((Statement::Case(case), adv))
                                } else {
                                    Err(())
                                }
                            }
                        }
                    }
                    ReservedName::DeclarationSpecifier(_) => {
                        if let Ok((decl_node, adv)) = DeclarationNode::parse(input) {
                            Ok((Statement::Declaration(decl_node), adv))
                        } else {
                            Err(())
                        }
                    }
                }
            }
            WrappedToken::Semicolon => return Ok((Statement::NullStatement, 1)),
            WrappedToken::Bracket(
                Bracket { left_or_right: LeftOrRight::Left, bracket_type: BracketType::Curly, .. }
            ) => {
                if let Ok(
                    (comp_stmt, adv)
                ) = CompoundStatementNode::parse(input) {
                    Ok((Statement::Compound(comp_stmt), adv))
                } else {
                    Err(())
                }
            }
            t => {
                if let WrappedToken::Name(_) = t {
                    if let Ok((label_decl, adv)) = DeclarationNode::parse(input) {
                        return Ok((Statement::Declaration(label_decl), adv));
                    }
                }
                if let Ok(
                    (rv_and_semicolon, adv)
                ) = RvalueAndSemicolonNode::parse(input) {
                    Ok((Statement::RvalueAndSemicolon(rv_and_semicolon), adv))
                } else {
                    Err(())
                }
            }
        }
    }
}

impl Parse for StatementNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if input.is_empty() {
            return Err(());
        }

        return if let Ok((statement, adv)) = Statement::parse(input) {
            Ok((StatementNode {
                position: input[0].pos.clone(),
                statement: Box::new(statement),
            }, adv))
        } else {
            Err(())
        };
    }
}

impl Parse for DefinitionNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        if let Ok(
            (var_def_node, adv)
        ) = VariableDefinitionNode::parse(input) {
            return Ok((DefinitionNode::Variable(var_def_node), adv));
        }

        if let Ok(
            (vec_def_node, adv)
        ) = VectorDefinitionNode::parse(input) {
            return Ok((DefinitionNode::Vector(vec_def_node), adv));
        }

        if let Ok(
            (fn_def_node, adv)
        ) = FunctionDefinitionNode::parse(input) {
            return Ok((DefinitionNode::Function(fn_def_node), adv));
        }

        Err(())
    }
}

impl Parse for ProgramNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), ()>
        where Self: Sized {
        let mut defs = vec![];
        let mut offset: usize = 0;

        while let Ok(
            (def_node, adv)
        ) = DefinitionNode::parse(&input[offset..]) {
            offset += adv;
            defs.push(def_node);
        }

        if offset != input.len() {
            Err(())
        } else {
            Ok((ProgramNode::from(defs), offset))
        }
    }
}

impl ParseExact for ProgramNode {}

pub(crate) struct Parser<'a> {
    pub(crate) compiler_options: CompilerOptions,
    pub(crate) source_code: &'a str,
}

#[derive(PartialEq)]
enum BracketsError {
    NotClosed(TokenPos),
    NotOpened(TokenPos),
}

#[derive(Debug, Clone)]
pub(crate) struct MultiMap<K: cmp::Eq + Hash, V> {
    data: HashMap<K, Vec<V>>,
    size: usize,
}

impl<K: Eq + Hash + Clone, V: Clone> TryFrom<MultiMap<K, V>> for HashMap<K, V> {
    type Error = ();

    fn try_from(value: MultiMap<K, V>) -> Result<Self, Self::Error> {
        let mut res = HashMap::<K, V>::new();
        for (k, v) in value.get_inner() {
            if v.len() != 1 {
                return Err(());
            }
            res.insert(k.clone(), v[0].clone());
        }
        Ok(res)
    }
}

impl<K: Eq + Hash, V> MultiMap<K, V> {
    pub fn new() -> Self {
        MultiMap { data: Default::default(), size: 0 }
    }

    pub fn get_inner(&self) -> &HashMap<K, Vec<V>> {
        &self.data
    }

    pub fn extract_inner(self) -> HashMap<K, Vec<V>> {
        self.data
    }
}

impl<K: Eq + Hash, V> MultiMap<K, V> {
    fn len(&self) -> usize {
        self.size
    }

    fn insert(&mut self, key: K, value: V) {
        self.size += 1;

        if let Some(vals) = self.data.get_mut(&key) {
            vals.push(value)
        } else {
            self.data.insert(key, vec![value]);
        }
    }

    fn get_last<Q: ?Sized>(&self, k: &Q) -> Option<&V>
        where K: Borrow<Q>, Q: Hash + Eq {
        self.data.get(k)?.last()
    }

    fn contains_key<Q: ?Sized>(&self, k: &Q) -> bool
        where K: Borrow<Q>, Q: Hash + Eq {
        self.data.contains_key(k)
    }
}

impl<K: Eq + Hash, V> Default for MultiMap<K, V> {
    fn default() -> Self {
        MultiMap {
            data: Default::default(),
            size: 0,
        }
    }
}

impl<K, V> Extend<(K, V)> for MultiMap<K, V>
    where K: Eq + Hash {
    fn extend<T: IntoIterator<Item=(K, V)>>(&mut self, iter: T) {
        for (k, v) in iter {
            self.insert(k, v)
        }
    }
}

impl Parser<'_> {
    fn find_bracket_pairs<'b, I>(tok_it: I) -> Result<Vec<Token>, BracketsError>
        where I: Iterator<Item=&'b Token> {
        use LeftOrRight::*;
        let mut stack = vec![];
        let mut processed_tokens = vec![];
        let mut tok_it = tok_it.enumerate();
        //  Vec::<(&Bracket, &TokenPos, usize)>::new()

        while let Some((i, token)) = tok_it.next() {
            if let Token {
                token: WrappedToken::Bracket(br),
                pos,
            } = token {
                if br.left_or_right == Left {
                    stack.push((br, pos, i));
                    processed_tokens.push(token.clone());
                } else {
                    let pos = pos.clone();

                    match stack.pop() {
                        None => return Err(BracketsError::NotOpened(pos.clone())),
                        Some((
                                 &left_br,
                                 &left_br_pos,
                                 left_br_idx)
                        ) => {
                            if !left_br.is_pair(br) {
                                return Err(BracketsError::NotOpened(pos));
                            }

                            let left_br = &mut processed_tokens[left_br_idx];
                            if let WrappedToken::Bracket(br) = &mut left_br.token {
                                br.pair_pos = Some(pos);
                            } else {
                                unreachable!()
                            }
                            let mut br = *br;
                            br.pair_pos = Some(left_br_pos);

                            processed_tokens.push(Token {
                                token: WrappedToken::Bracket(br),
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
            return Ok(processed_tokens);
        }
        Err(BracketsError::NotClosed(stack.pop().unwrap().1.clone()))
    }

    pub(crate) fn run(
        &self,
        tokens: &[Token],
        issues: &mut Vec<Issue>,
    ) -> Result<ScopeTable, ()> {
        let tokens =
            match Parser::find_bracket_pairs(tokens.into_iter()) {
                Ok(processed_tokens) => processed_tokens,
                Err(BracketsError::NotClosed(pos)) => {
                    issues.push(Issue::BracketNotClosed(pos));
                    return Err(());
                }
                Err(BracketsError::NotOpened(pos)) => {
                    issues.push(Issue::BracketNotOpened(pos));
                    return Err(());
                }
            };
        if tokens.is_empty() {
            issues.push(Issue::EmptyTokenStream)
        }

        let prog_node = ProgramNode::parse_exact(&tokens);
        if let Err(err) = prog_node {
            issues.push(Issue::ParsingError);
            return Err(err);
        }

        let analyzer = Analyzer { source_code: self.source_code };
        let res = prog_node.unwrap().flatten_node();
        Ok(analyzer.run(res, issues))
    }
}