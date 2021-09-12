use std::*;
use borrow::Borrow;
use collections::HashMap;
use convert::TryFrom;
use hash::Hash;

use crate::parser::analyzer::Analyzer;
pub(crate) use crate::parser::analyzer::ScopeTable;
pub(crate) use crate::parser::analyzer::{DeclInfoAndPos, DefInfoAndPos};
use crate::parser::ast::*;
use flat_ast::FlattenNode;

use crate::config::*;
use Issue::*;
use crate::tokenizer::token;
use token::*;

pub mod ast;
mod expression_parser;
pub(crate) mod analyzer;

pub(crate) trait Parse {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized;
}

pub(crate) trait ParseExact: Parse {
    fn parse_exact(input: &[Token]) -> Result<Self, Vec<Issue>>
        where Self: Sized {
        let (obj, adv) = Self::parse(&input)?;
        if adv == input.len() {
            Ok(obj)
        } else {
            Err(vec![Issue::FailedToParseExact(input[0].pos)])
        }
    }
}

impl Parse for Ival {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        let t = &input[0];
        Ok((match &t.token {
            WrappedToken::Name(name) => Ival::Name(name.clone(), t.pos),

            WrappedToken::Constant(_) => {
                match ConstantNode::try_from(t) {
                    Ok(c) => {
                        Ival::Constant(c)
                    }
                    Err(err) => {
                        return Err(vec![err]);
                    }
                }
            }
            _ => return Err(vec![UnexpectedToken(t.pos)]),
        }, 1))
    }
}

impl Parse for ConstantNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        let t = &input[0];
        if let Ok(constant) = ConstantNode::try_from(t) {
            return Ok((constant, 1));
        }
        Err(vec![ParsingError(t.pos)])
    }
}

impl ParseExact for ConstantNode {}

impl Parse for VariableDefinitionNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>> {
        let toks_trim: Vec<&Token> = input.into_iter()
            .take_while(|t| t.token != WrappedToken::Semicolon)
            .collect();

        if toks_trim.len() == input.len() {  // no semicolon found
            return Err(vec![ExpectedTokenNotFound(toks_trim.last().unwrap().pos)]);
        }
        return if let Token {
            token: WrappedToken::Name(name),
            pos: name_pos,
        } = toks_trim[0] {
            let name = name.clone();

            Ok((VariableDefinitionNode {
                position: name_pos.clone(),
                name,
                initial_value: match toks_trim.get(1) {
                    None => None,
                    Some(&t) =>
                        if let Ok((ival, _)) = Ival::parse(&[t.clone()]) {
                            Some(ival)
                        } else {
                            return Err(vec![ExpectedTokenNotFound(t.pos)]);  // expected ival
                        }
                },
            }, toks_trim.len() + 1))
        } else {
            Err(vec![UnexpectedToken(toks_trim[0].pos)])
        };
    }
}

impl Parse for VectorDefinitionNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        let toks_trim: Vec<&Token> =
            input
                .into_iter()
                .take_while(|t| t.token != WrappedToken::Semicolon)
                .collect();

        if toks_trim.len() == input.len() {  // no semicolon found
            return Err(vec![ExpectedTokenNotFound(toks_trim.last().unwrap().pos)]);
        }

        {
            let minimal_len = 3;  // name [ ]
            if toks_trim.len() < minimal_len {
                return Err(vec![StmtTooShort(toks_trim[0].pos)]);
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

                    let right_bracket_idx = get_right_bracket_index(input, left_br_idx);

                    if right_bracket_idx - left_br_idx > 2 {  // name [ ... ... ]
                        return Err(vec![UnexpectedToken(input[left_br_idx + 2].pos)]);
                    }

                    let specified_size =
                        if right_bracket_idx == left_br_idx + 1 {  // name [ ]
                            None
                        } else {
                            if let Ok(size) = ConstantNode::parse_exact(
                                &input[left_br_idx + 1..right_bracket_idx]) {
                                Some(size)
                            } else {
                                let size = &input[left_br_idx + 1];
                                return Err(vec![VecSizeIsNotANumber { name, pos: size.pos }]);
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
                                Some(t) => return Err(vec![UnexpectedToken(t.pos)]),
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
                _ => return Err(vec![ExpectedTokenNotFound(input[1].pos)])
            };
        Ok((node, toks_trim.len() + 1))
    }
}

fn parse_comma_separated_list(tokens: &[Token]) -> Result<Vec<(String, TokenPos)>, Vec<Issue>> {
    if tokens.is_empty() {
        return Ok(Default::default());
    }
    let mut res = Vec::new();
    let err =
        loop {
            let name_idx = res.len() * 2;
            let name_pos;
            let name;
            let comma_idx;

            match tokens.get(name_idx) {
                Some(
                    Token { token: WrappedToken::Name(tok_name), pos }
                ) => {
                    name_pos = *pos;
                    name = tok_name.clone();
                    comma_idx = name_idx + 1;
                }
                Some(t) => {
                    break UnexpectedToken(t.pos);
                }
                None => {
                    // the input is not empty, but no name found
                    break ExpectedTokenNotFound(tokens[name_idx - 1].pos);
                }
            }
            res.push((name, name_pos));

            match tokens.get(comma_idx) {
                Some(Token { token: WrappedToken::Comma, .. }) => (),  // OK, matched "name ,"
                None => {  // end of input, matched "[name_list ,] name"
                    return Ok(res);
                }
                Some(t) => break UnexpectedToken(t.pos),  // the next symbol is not a comma
            }
        };
    Err(vec![err])
}

impl Parse for FunctionDefinitionNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if input.len() < 4 {  // name ( ) ;
            return Err(vec![StmtTooShort(input[0].pos)]);
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
                let right_bracket_index = get_right_bracket_index(input, left_br_idx);

                let parameter_names
                    = parse_comma_separated_list(&input[toks_consumed..right_bracket_index])?;

                toks_consumed = right_bracket_index + 1;
                let (stmt, adv) = StatementNode::parse(&input[toks_consumed..])?;
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
            }
            _ => Err(vec![ExpectedTokenNotFound(input[1].pos)])
        }
    }
}

impl Parse for AutoDeclaration {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
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
            Ok((AutoDeclaration {
                position: *pos,
                name: name.clone(),
                size_if_vector: None,
            }, 1))
        } else {
            Err(vec![ExpectedTokenNotFound(input[0].pos)])
        }
    }
}

fn get_semicolon_pos(tokens: &[Token]) -> Result<usize, Vec<Issue>> {
    tokens.into_iter()
        .position(|c| c.token == WrappedToken::Semicolon)
        .ok_or_else(|| vec![Issue::ExpectedTokenNotFound(tokens.last().unwrap().pos)])
}

impl Parse for AutoDeclarationNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if input.len() < 4 {  // auto name ; null_stmt
            return Err(vec![StmtTooShort(input[0].pos)]);
        }
        return if let Some(
            Token {
                token: WrappedToken::ReservedName(ReservedName::DeclarationSpecifier(
                                                      DeclarationSpecifier::Auto)),
                pos, ..
            }
        ) = input.first() {
            let sp_minus_one = get_semicolon_pos(&input[1..])?;
            let semicolon_pos = sp_minus_one + 1;

            let next_statement_starts_at = semicolon_pos + 1;
            if input.len() <= next_statement_starts_at {
                return Err(vec![NoNextStmtAfterDecl(input.last().unwrap().pos)]);
            }

            let declarations = get_auto_decl_list(&input[1..semicolon_pos])?;
            let (stmt_node, adv) = StatementNode::parse(&input[next_statement_starts_at..])?;
            Ok((AutoDeclarationNode {
                position: pos.clone(),
                declarations,
                next_statement: stmt_node,
            }, next_statement_starts_at + adv))
        } else {
            Err(vec![ExpectedTokenNotFound(input[0].pos)])
        };
    }
}

fn get_auto_decl_list(tokens: &[Token]) -> Result<Vec<AutoDeclaration>, Vec<Issue>> {
    let mut res = Vec::new();

    let mut i: usize = 0;
    while i < tokens.len() {
        match AutoDeclaration::parse(&tokens[i..]) {
            Ok((auto_decl, adv)) => {
                res.push(auto_decl);
                i += adv;

                match tokens.get(i) {
                    None => {
                        return Ok(res);
                    }
                    Some(Token { token: WrappedToken::Comma, .. }) => {
                        i += 1;
                        continue;
                    }
                    Some(t) => {
                        return Err(vec![UnexpectedToken(t.pos)]);
                    }
                }
            }
            Err(err) => {
                return Err(err);
            }
        }
    }
    unreachable!()
}

impl Parse for ExternDeclarationNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if input.len() < 4 {  // extrn name ; null_stmt
            return Err(vec![StmtTooShort(input[0].pos)]);
        }

        use token::DeclarationSpecifier::Extrn;

        if let Some(
            Token {
                token: WrappedToken::ReservedName(ReservedName::DeclarationSpecifier(Extrn)), ..
            }
        ) = input.first() {
            let sp_minus_one = get_semicolon_pos(&input[1..])?;
            let semicolon_pos = sp_minus_one + 1;

            let names = parse_comma_separated_list(&input[1..semicolon_pos])?;

            let next_statement_starts_at = semicolon_pos + 1;
            if input.len() <= next_statement_starts_at {
                return Err(vec![NoNextStmtAfterDecl(input.last().unwrap().pos)]);
            }

            let (next_stmt, adv) = StatementNode::parse(&input[next_statement_starts_at..])?;
            Ok((ExternDeclarationNode {
                position: input[0].pos.clone(),
                names,
                next_statement: next_stmt,
            }, next_statement_starts_at + adv))
        } else {
            Err(vec![ExpectedTokenNotFound(input.first().unwrap().pos)])
        }
    }
}

impl Parse for LabelDeclarationNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        let pos = input[0].pos;
        if input.len() < 3 {
            return Err(vec![StmtTooShort(pos)]);
        }

        return if let [
        Token { token: WrappedToken::Name(name), .. },
        Token { token: WrappedToken::Colon, .. }
        ] = &input[..2] {
            let (next_stmt, adv) = StatementNode::parse(&input[2..])?;
            Ok((LabelDeclarationNode {
                position: pos,
                label_name: name.clone(),
                next_statement: next_stmt,
            }, 2 + adv))
        } else {
            Err(vec![ExpectedTokenNotFound(input[1].pos)])
        };
    }
}

fn extract_bracketed_rvalue(
    tokens: &[Token],
    br_type: BracketType,
) -> Result<(RvalueNode, usize), Vec<Issue>> {
    let first = &tokens[0];
    if let Token {
        token: WrappedToken::Bracket(
            Bracket {
                left_or_right: LeftOrRight::Left, bracket_type, ..
            }
        ), pos
    } = first {
        if *bracket_type != br_type {
            return Err(vec![ExpectedTokenNotFound(first.pos)]);
        }
        let br_expr = extract_bracketed_expression(tokens)?;
        if br_expr.is_empty() {
            return Err(vec![EmptyBracketedExpr(*pos)]);
        }
        Ok((RvalueNode::parse_exact(br_expr)?, 1 + br_expr.len() + 1))
    } else {
        Err(vec![UnexpectedToken(tokens.first().unwrap().pos)])
    }
}

impl ParseExact for RvalueNode {}

impl Parse for SwitchNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if input.len() < 5 {  // switch ( rvalue ) null_stmt
            return Err(vec![StmtTooShort(input[0].pos)]);
        }
        use CtrlStmtIdent::Switch;

        let left_br_idx = 1;
        return match &input[..left_br_idx + 1] {
            [
            Token {
                token: WrappedToken::ReservedName(ReservedName::CtrlStmt(Switch)), ..
            },
            Token {
                token: WrappedToken::Bracket(
                    Bracket {
                        left_or_right: LeftOrRight::Left,
                        bracket_type: BracketType::Round, ..
                    }
                ), ..
            }
            ] => {
                let right_br_idx = get_right_bracket_index(input, left_br_idx);

                if right_br_idx == left_br_idx + 1 {  // switch ( )
                    return Err(vec![UnexpectedToken(input[left_br_idx].pos)]);
                }

                let rvalue = RvalueNode::parse_exact(&input[left_br_idx + 1..right_br_idx])?;

                let (body, adv) = StatementNode::parse(&input[right_br_idx + 1..])?;
                let toks_consumed = right_br_idx + 1 + adv;

                Ok((SwitchNode {
                    rvalue,
                    body,
                }, toks_consumed))
            }
            _ => Err(vec![UnexpectedToken(input[1].pos)])
        };
    }
}

impl Parse for CaseNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if input.len() < 3 {  // default : null_stmt
            return Err(vec![StmtTooShort(input[0].pos)]);
        }

        use token::CtrlStmtIdent::{Case, Default};

        let pos = input[0].pos;
        let colon_pos: usize;
        let constant =
            match input[0].token {
                WrappedToken::ReservedName(ReservedName::CtrlStmt(Case)) => {
                    colon_pos = 2;
                    let constant = ConstantNode::parse_exact(&input[1..=1])?;
                    if let ConstantNode { constant: Constant::Number(_), .. } = constant {
                        Some(constant)
                    } else {
                        return Err(vec![WrongConstant(pos)]);
                    }
                }
                WrappedToken::ReservedName(ReservedName::CtrlStmt(Default)) => {
                    colon_pos = 1;
                    None
                }
                _ => return Err(vec![UnexpectedToken(pos)]),
            };
        if input[colon_pos].token != WrappedToken::Colon {
            return Err(vec![ExpectedTokenNotFound(input[colon_pos].pos)]);
        }
        let next_stmt_idx = colon_pos + 1;
        let slice = &input[next_stmt_idx..];
        if slice.is_empty() {
            return Err(vec![StmtTooShort(input[next_stmt_idx - 1].pos)]);
        }
        let (next_stmt, adv) = StatementNode::parse(slice)?;
        let toks_consumed = next_stmt_idx + adv;

        Ok((CaseNode {
            constant_if_not_default: constant,
            next_statement: next_stmt,
        }, toks_consumed))
    }
}

impl Parse for IfNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if input.len() < 5 {  // if ( rvalue ) ;
            return Err(vec![StmtTooShort(input[0].pos)]);
        }

        use token::CtrlStmtIdent::{If, Else};
        use token::BracketType::Round;

        if input[0].token != WrappedToken::ReservedName(ReservedName::CtrlStmt(If)) {
            return Err(vec![ExpectedTokenNotFound(input[0].pos)]);
        }

        let (condition, adv) = extract_bracketed_rvalue(&input[1..], Round)?;
        let mut toks_consumed = 1 + adv;
        let condition = condition.into_truth_value();
        let (body, adv) = StatementNode::parse(&input[toks_consumed..])?;
        toks_consumed += adv;

        let r#else =
            if let Some(tok) = input.get(toks_consumed) {
                if tok.token == WrappedToken::ReservedName(ReservedName::CtrlStmt(Else)) {
                    toks_consumed += 1;
                    if toks_consumed == input.len() {
                        return Err(vec![StmtTooShort(input.last().unwrap().pos)]);
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
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if input.len() < 5 {  // while ( x ) ;
            return Err(vec![StmtTooShort(input[0].pos)]);
        }
        use CtrlStmtIdent::While;
        use BracketType::Round;
        if input[0].token != WrappedToken::ReservedName(ReservedName::CtrlStmt(While)) {
            return Err(vec![ExpectedTokenNotFound(input[0].pos)]);
        }

        let (condition, adv) = extract_bracketed_rvalue(&input[1..],
                                                        Round)?;
        let mut toks_consumed = 1 + adv;
        let condition = condition.into_truth_value();

        if input.len() <= toks_consumed {
            return Err(vec![StmtTooShort(input.last().unwrap().pos)]);
        }
        let (body, adv) = StatementNode::parse(&input[toks_consumed..])?;
        toks_consumed += adv;

        return Ok((WhileNode { condition, body }, toks_consumed));
    }
}

fn get_right_bracket_index(tokens: &[Token], left_br_idx: usize) -> usize {
    if let Some(
        Token {
            token: WrappedToken::Bracket(
                Bracket { left_or_right: LeftOrRight::Left, pair_pos, .. }
            ), ..
        }) = tokens.get(left_br_idx) {
        let right_bracket_pos = pair_pos.unwrap();

        tokens
            .into_iter()
            .position(|t| t.pos == right_bracket_pos)
            .unwrap()
    } else {
        unreachable!()
    }
}

pub(crate) fn extract_bracketed_expression(tokens: &[Token]) -> Result<&[Token], Vec<Issue>> {
    if let Token {
        token: WrappedToken::Bracket(Bracket { left_or_right: LeftOrRight::Left, .. }), ..
    } = tokens[0] {
        let right_br_idx = get_right_bracket_index(tokens, 0);
        return Ok(&tokens[1..right_br_idx]);
    }
    unreachable!()
}

impl Parse for CompoundStatementNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        use BracketType::Curly;

        if let WrappedToken::Bracket(
            Bracket { left_or_right, bracket_type, .. }
        ) = input[0].token {
            debug_assert_eq!(left_or_right, LeftOrRight::Left);
            debug_assert_eq!(bracket_type, Curly)
        } else {
            unreachable!()
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
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        let pos = input[0].pos;
        if input.len() < 3 {  // goto label ;
            return Err(vec![StmtTooShort(pos)]);
        }

        use token::CtrlStmtIdent::Goto;

        if input[0].token != WrappedToken::ReservedName(ReservedName::CtrlStmt(Goto)) {
            return Err(vec![ExpectedTokenNotFound(pos)]);
        }

        let semicolon_pos = get_semicolon_pos(input)?;
        let toks_consumed = semicolon_pos + 1;

        let rvalue = &input[1..semicolon_pos];

        return Ok((GotoNode {
            label: RvalueNode::parse_exact(&rvalue)?,
        }, toks_consumed));
    }
}

impl Parse for ReturnNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        debug_assert!(!input.is_empty());

        use BracketType::Round;
        use CtrlStmtIdent::Return;

        if input[0].token != WrappedToken::ReservedName(ReservedName::CtrlStmt(Return)) {
            return Err(vec![ExpectedTokenNotFound(input[0].pos)]);
        }

        let semicolon_pos = get_semicolon_pos(input)?;
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
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if input.len() < 2 {  // x ;
            return Err(vec![StmtTooShort(input[0].pos)]);
        }

        let semicolon_pos = get_semicolon_pos(input)?;
        let toks_consumed = semicolon_pos + 1;

        let rvalue = &input[..semicolon_pos];

        return Ok((RvalueAndSemicolonNode {
            rvalue: RvalueNode::parse_exact(&rvalue)?
        }, toks_consumed));
    }
}

impl Parse for BreakNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if input.len() < 2 {  // break ;
            return Err(vec![StmtTooShort(input[0].pos)]);
        }

        use token::CtrlStmtIdent::Break;

        match &input[..2] {
            [Token {
                token: WrappedToken::ReservedName(
                    ReservedName::CtrlStmt(Break)), ..
            },
            Token { token: WrappedToken::Semicolon, .. }] =>
                Ok((BreakNode {}, 2)),
            _ => Err(vec![ExpectedTokenNotFound(input[1].pos)])
        }
    }
}

impl Parse for ContinueNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if input.len() < 2 {  // break ;
            return Err(vec![StmtTooShort(input[0].pos)]);
        }

        use token::CtrlStmtIdent::Continue;

        match &input[..2] {
            [Token {
                token: WrappedToken::ReservedName(
                    ReservedName::CtrlStmt(Continue)), ..
            },
            Token { token: WrappedToken::Semicolon, .. }] =>
                Ok((ContinueNode {}, 2)),
            _ => Err(vec![ExpectedTokenNotFound(input[1].pos)])
        }
    }
}

impl ParseExact for BreakNode {}

impl Parse for DeclarationNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        debug_assert!(!input.is_empty());

        let first = &input[0];
        match first.token {
            WrappedToken::ReservedName(ReservedName::DeclarationSpecifier(ds)) => {
                match ds {
                    DeclarationSpecifier::Auto => {
                        let (auto_decl, adv) = AutoDeclarationNode::parse(input)?;
                        Ok((DeclarationNode::AutoDeclaration(auto_decl), adv))
                    }
                    DeclarationSpecifier::Extrn => {
                        let (extern_decl, adv) = ExternDeclarationNode::parse(input)?;
                        Ok((DeclarationNode::ExternDeclaration(extern_decl), adv))
                    }
                }
            }
            WrappedToken::Name(_) => {
                let (label_decl, adv) = LabelDeclarationNode::parse(input)?;
                Ok((DeclarationNode::LabelDeclaration(label_decl), adv))
            }
            _ => Err(vec![UnexpectedToken(first.pos)]),
        }
    }
}

impl Parse for Statement {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        debug_assert!(!input.is_empty());
        use WrappedToken::*;
        let t = &input[0];
        let pos = t.pos;
        match &t.token {
            ReservedName(res_name) => {
                use CtrlStmtIdent::*;
                match res_name {
                    token::ReservedName::CtrlStmt(cs) => {
                        match cs {
                            If => {
                                let (r#if, adv) = IfNode::parse(input)?;
                                Ok((Statement::If(r#if), adv))
                            }
                            Else => {
                                Err(vec![UnexpectedToken(pos)])
                            }
                            Goto => {
                                let (goto, adv) = GotoNode::parse(input)?;
                                Ok((Statement::Goto(goto), adv))
                            }
                            Switch => {
                                let (switch, adv) = SwitchNode::parse(input)?;
                                Ok((Statement::Switch(switch), adv))
                            }
                            While => {
                                let (r#while, adv) = WhileNode::parse(input)?;
                                Ok((Statement::While(r#while), adv))
                            }
                            Break => {
                                let (r#break, adv) = BreakNode::parse(input)?;
                                Ok((Statement::Break(r#break), adv))
                            }
                            Continue => {
                                let (r#break, adv) = ContinueNode::parse(input)?;
                                Ok((Statement::Continue(r#break), adv))
                            }
                            Return => {
                                let (r#return, adv) = ReturnNode::parse(input)?;
                                Ok((Statement::Return(r#return), adv))
                            }
                            Case | Default => {
                                let (case, adv) = CaseNode::parse(input)?;
                                Ok((Statement::Case(case), adv))
                            }
                        }
                    }
                    token::ReservedName::DeclarationSpecifier(_) => {
                        let (decl_node, adv) = DeclarationNode::parse(input)?;
                        Ok((Statement::Declaration(decl_node), adv))
                    }
                }
            }
            Semicolon => return Ok((Statement::NullStatement, 1)),
            Bracket(
                token::Bracket {
                    left_or_right: LeftOrRight::Left,
                    bracket_type: BracketType::Curly, ..
                }
            ) => {
                let (comp_stmt, adv) = CompoundStatementNode::parse(input)?;
                Ok((Statement::Compound(comp_stmt), adv))
            }
            t => {
                if let Name(_) = t {
                    if input[1].token == WrappedToken::Colon {
                        let (label_decl, adv) = DeclarationNode::parse(input)?;
                        return Ok((Statement::Declaration(label_decl), adv));
                    }
                }
                let (rv_and_semicolon, adv) = RvalueAndSemicolonNode::parse(input)?;
                Ok((Statement::RvalueAndSemicolon(rv_and_semicolon), adv))
            }
        }
    }
}

impl Parse for StatementNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        debug_assert!(!input.is_empty());

        let (statement, adv) = Statement::parse(input)?;
        Ok((StatementNode {
            position: input[0].pos.clone(),
            statement: Box::new(statement),
        }, adv))
    }
}

impl Parse for DefinitionNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        if let WrappedToken::Bracket(
            Bracket {
                left_or_right: LeftOrRight::Left, bracket_type: BracketType::Round, ..
            }) = input[1].token {
            let (fn_def_node, adv) = FunctionDefinitionNode::parse(input)?;
            Ok((DefinitionNode::Function(fn_def_node), adv))
        } else if let WrappedToken::Bracket(
            Bracket {
                left_or_right: LeftOrRight::Left, bracket_type: BracketType::Square, ..
            }) = input[1].token {
            let (vec_def_node, adv) = VectorDefinitionNode::parse(input)?;
            Ok((DefinitionNode::Vector(vec_def_node), adv))
        } else {
            let (var_def_node, adv) = VariableDefinitionNode::parse(input)?;
            Ok((DefinitionNode::Variable(var_def_node), adv))
        }
    }
}

impl Parse for ProgramNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
        where Self: Sized {
        let mut defs = vec![];
        let mut offset: usize = 0;
        while offset < input.len() {
            let (def_node, adv) = DefinitionNode::parse(&input[offset..])?;
            offset += adv;
            defs.push(def_node);
        }
        Ok((ProgramNode::from(defs), offset))
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
pub(crate) struct MultiMap<K: Eq + Hash, V> {
    data: HashMap<K, Vec<V>>,
    size: usize,
}

impl<K: Eq + Hash, V> From<HashMap<K, V>> for MultiMap<K, V> {
    fn from(value: HashMap<K, V>) -> Self {
        MultiMap {
            size: value.len(),
            data: value.into_iter().map(|(k, v)| (k, vec![v])).collect(),
        }
    }
}

impl<K: Eq + Hash + Clone, V: Clone> TryFrom<MultiMap<K, V>> for HashMap<K, V> {
    type Error = ();

    fn try_from(value: MultiMap<K, V>) -> Result<Self, Self::Error> {
        let mut res = HashMap::new();
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
    fn get_inner(&self) -> &HashMap<K, Vec<V>> {
        &self.data
    }

    pub fn total_items(&self) -> usize {
        self.size
    }

    pub fn insert(&mut self, key: K, value: V) {
        self.size += 1;
        if let Some(vals) = self.data.get_mut(&key) {
            vals.push(value)
        } else {
            self.data.insert(key, vec![value]);
        }
    }

    pub fn get_last<Q: ?Sized>(&self, k: &Q) -> Option<&V>
        where K: Borrow<Q>, Q: Hash + Eq {
        self.data.get(k)?.last()
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
                    issues.push(BracketNotClosed(pos));
                    return Err(());
                }
                Err(BracketsError::NotOpened(pos)) => {
                    issues.push(BracketNotOpened(pos));
                    return Err(());
                }
            };
        if tokens.is_empty() {
            issues.push(EmptyTokenStream)
        }

        let prog_node = ProgramNode::parse_exact(&tokens);
        match prog_node {
            Err(err) => {
                issues.extend(err);
                return Err(());
            }
            Ok(prog_node) => {
                let res = prog_node.flatten_node();
                let mut analyzer = Analyzer::new(self.source_code);
                Ok(analyzer.run(res, issues))
            }
        }
    }
}