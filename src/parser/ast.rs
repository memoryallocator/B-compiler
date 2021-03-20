use core::slice::Iter;
use std::cell::RefCell;
use std::rc::Rc;

use crate::lexical_analyzer::TokenPos;
use crate::token::{Token, TokenType};

pub(crate) trait Parse {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized;
}

pub(crate) trait ParseExact: Parse {
    fn parse_exact(tokens: &[Token]) -> Option<Self>
        where Self: Sized {
        if let Some((obj, adv)) = Self::parse(&tokens) {
            return if adv == tokens.len() {
                Some(obj)
            } else {
                None
            };
        }
        None
    }
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

impl ParseExact for Constant {}

#[derive(Debug)]
pub(crate) struct VariableDefinitionNode {
    name: String,
    initial_value: Option<Ival>,
}

impl Parse for VariableDefinitionNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)> {
        let toks_trim: Vec<&Token> = tokens.into_iter()
            .take_while(|t| t.r#type != TokenType::Semicolon)
            .collect();
        if toks_trim.len() == tokens.len() {  // no semicolon found
            return None;
        }
        return
            Some((
                if let Some(
                    Token {
                        r#type: TokenType::Name,
                        val: name,
                        ..
                    }) = toks_trim.get(0) {
                    let name = name.as_ref().unwrap().clone();

                    VariableDefinitionNode {
                        name,
                        initial_value: match toks_trim.get(1) {
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
                }, toks_trim.len() + 1));
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
        use crate::token::LEFT_SQUARE_BRACKET;

        let toks_trim: Vec<&Token> = tokens.into_iter()
            .take_while(|t| t.r#type != TokenType::Semicolon)
            .collect();
        if toks_trim.len() == tokens.len() {  // no semicolon found
            return None;
        }

        {
            let name_and_left_right_br_len = 3;  // name [ ]
            if toks_trim.len() < name_and_left_right_br_len {
                return None;
            }
        }
        let name_and_left_br_len = 2;
        let node = match toks_trim[..name_and_left_br_len] {
            [Token {
                r#type: TokenType::Name,
                val: name, ..
            }, Token {
                r#type: LEFT_SQUARE_BRACKET,
                val: pair_pos, ..
            }] => {
                let name = name.as_ref().unwrap().clone();
                let left_br_idx = name_and_left_br_len - 1;

                let right_bracket_pos = TokenPos::parse(pair_pos.as_ref()?)?;
                let right_bracket_idx = tokens.into_iter()
                    .position(|t| t.pos == right_bracket_pos)?;

                if right_bracket_idx - left_br_idx > 2 {  // name [ ... ... ]
                    return None;
                }

                let specified_element_count =
                    if right_bracket_idx == left_br_idx + 1 {  // name [ ]
                        None
                    } else {
                        if let Some(r#const) = Constant::parse_exact(
                            &tokens[left_br_idx + 1..left_br_idx + 2]) {
                            Some(r#const)
                        } else {
                            None
                        }
                    };

                if toks_trim.get(right_bracket_idx + 1).is_none() {  // name [ const? ] ;
                    VectorDefinitionNode {
                        name,
                        specified_element_count,
                        initial_values: None,
                    }
                } else {
                    let mut initial_values = Vec::<Ival>::new();
                    let first_ival_pos: usize = right_bracket_idx + 1;
                    while let Some((ival, _)) = Ival::parse(
                        &tokens[first_ival_pos + 2 * initial_values.len()..]) {
                        let comma_pos = first_ival_pos + 2 * initial_values.len() + 1;
                        let next = toks_trim.get(comma_pos);
                        match next {
                            Some(Token {
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
            }
            _ => return None
        };
        Some((node, toks_trim.len() + 1))
    }
}

fn parse_name_list(tokens: &[Token]) -> Option<Vec<String>> {
    if tokens.is_empty() {
        return Some(vec![]);
    }

    let mut res = Vec::<String>::new();
    loop {
        let name_idx = res.len() * 2;
        if let Some(Token {
                        r#type: TokenType::Name,
                        val: name, ..
                    }) = tokens.get(name_idx) {
            let comma_idx = name_idx + 1;
            match tokens.get(comma_idx) {
                None => return Some(res),  // end of input, matched "[name_list ,] name"
                Some(Token { r#type: TokenType::Comma, .. }) => {  // OK, matched "name ,"
                    res.push(name.as_ref().unwrap().clone());
                }
                Some(_) => return None,  // the next symbol is not a comma
            }
        } else {
            return None;
        }
    }
}

#[derive(Debug)]
pub(crate) struct FunctionDefinitionNode {
    pub(crate) name: String,
    pub(crate) parameter_names: Vec<String>,
    pub(crate) local_variables: RefCell<Vec<String>>,
    body: Box<StatementNode>,
}

impl Parse for FunctionDefinitionNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::{LEFT_ROUND_BRACKET, RIGHT_ROUND_BRACKET};

        if tokens.len() < 4 {
            return None;
        }
        match &tokens[..2] {
            [Token {
                r#type: TokenType::Name,
                val: name, ..
            }, Token {
                r#type: LEFT_ROUND_BRACKET, ..
            }] => {
                let mut toks_consumed: usize = 2;
                let right_bracket_pos = tokens[toks_consumed..].iter()
                    .position(|c| c.r#type == RIGHT_ROUND_BRACKET);

                let right_bracket_pos =
                    if let Some(rbp) = right_bracket_pos {
                        rbp
                    } else {
                        return None;
                    } + toks_consumed;

                let parameter_names;
                if let Some(v) = parse_name_list(&tokens[toks_consumed..right_bracket_pos]) {
                    parameter_names = v;
                } else {
                    return None;  // failed to parse parameter names list!
                }

                toks_consumed = right_bracket_pos + 1;
                return if let Some((stmt, adv)) = StatementNode::parse(&tokens[toks_consumed..]) {
                    let name = name.as_ref().unwrap().clone();
                    toks_consumed += adv;
                    Some((
                        FunctionDefinitionNode {
                            name,
                            parameter_names,
                            local_variables: RefCell::default(),
                            body: Box::new(stmt),
                        }, toks_consumed
                    ))
                } else {
                    None
                };
            }
            _ => None
        }
    }
}

#[derive(Debug)]
struct AutoDeclaration {
    name: String,
    vector_size: Option<Constant>,
}

impl Parse for AutoDeclaration {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::Bracket;

        if tokens.is_empty() {
            return None;
        }

        const LEFT_SQUARE_BRACKET: Bracket = Bracket::left_square_bracket();
        const RIGHT_SQUARE_BRACKET: Bracket = Bracket::right_square_bracket();

        if tokens.len() >= 4 {
            if let [
            Token {
                r#type: TokenType::Name,
                val: name, ..
            },
            Token { r#type: TokenType::Bracket(LEFT_SQUARE_BRACKET), .. },
            Token {
                r#type: TokenType::Constant(vec_size_type),
                val: vec_size, ..
            },
            Token { r#type: TokenType::Bracket(RIGHT_SQUARE_BRACKET), .. }
            ] = &tokens[..4] {
                let name = name.as_ref().unwrap().clone();
                let vector_size = Some(Constant {
                    constant_type: *vec_size_type,
                    value: vec_size.as_ref().unwrap().clone(),
                });
                return Some((AutoDeclaration {
                    name,
                    vector_size,
                }, 4));
            }
        }

        if let Token {
            r#type: TokenType::Name,
            val: name, ..
        } = &tokens[0] {
            let name = name.as_ref().unwrap().clone();
            return Some((AutoDeclaration {
                name,
                vector_size: None,
            }, 1));
        }
        None
    }
}

fn get_semicolon_pos(tokens: &[Token]) -> Option<usize> {
    if let Some(sp) = tokens.into_iter()
        .position(|c| c.r#type == TokenType::Semicolon) {
        Some(sp)
    } else {
        None
    }
}

#[derive(Debug)]
struct AutoDeclarationNode {
    declarations: Vec<AutoDeclaration>,
    next_statement: Box<StatementNode>,
}

impl Parse for AutoDeclarationNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::DeclarationSpecifier::Auto;

        if tokens.len() < 4 {  // auto name ; null_stmt
            return None;
        }

        return if let Some(
            Token { r#type: TokenType::DeclarationSpecifier(Auto), .. }
        ) = tokens.first() {
            let semicolon_pos =
                if let Some(sp_minus_one) = get_semicolon_pos(&tokens[1..]) {
                    sp_minus_one + 1
                } else {
                    return None;
                };

            let declarations;
            if let Some(decls) = AutoDeclarationNode::get_auto_decl_list(&tokens[1..semicolon_pos]) {
                declarations = decls;
            } else {
                return None;
            }

            if semicolon_pos >= tokens.len() {  // actually, == would work
                return None;
            }
            if let Some((stmt_node, adv)) = StatementNode::parse(&tokens[semicolon_pos..]) {
                Some((AutoDeclarationNode {
                    declarations,
                    next_statement: Box::new(stmt_node),
                }, semicolon_pos + adv))
            } else {
                None
            }
        } else {
            None
        };
    }
}

impl AutoDeclarationNode {
    fn get_auto_decl_list(tokens: &[Token]) -> Option<Vec<AutoDeclaration>> {
        if tokens.is_empty() {
            return Some(vec![]);
        }

        let mut res = Vec::<AutoDeclaration>::new();
        let mut i: usize = 0;
        while i < tokens.len() {
            while let Some((auto_decl, adv)) = AutoDeclaration::parse(&tokens[i..]) {
                res.push(auto_decl);
                i += adv;

                match tokens.get(i) {
                    None => return Some(res),
                    Some(Token {
                             r#type: TokenType::Comma, ..
                         }) => continue,
                    _ => return None
                }
            }
            return None;
        }
        unreachable!()
    }
}

#[derive(Debug)]
struct ExternDeclarationNode {
    names: Vec<String>,
    next_statement: Box<StatementNode>,
}

impl Parse for ExternDeclarationNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::DeclarationSpecifier::Extrn;

        if tokens.len() < 4 {  // extrn name ; null_stmt
            return None;
        }

        return if let Some(
            Token { r#type: TokenType::DeclarationSpecifier(Extrn), .. }
        ) = tokens.first() {
            let semicolon_pos =
                if let Some(sp_minus_one) = get_semicolon_pos(&tokens[1..]) {
                    sp_minus_one + 1
                } else {
                    return None;
                };

            let names;
            if let Some(v) = parse_name_list(&tokens[1..semicolon_pos]) {
                names = v;
            } else {
                return None;
            }

            if semicolon_pos >= tokens.len() {  // actually, == would work
                return None;
            }
            if let Some((next_stmt, adv)) = StatementNode::parse(&tokens[semicolon_pos..]) {
                Some((ExternDeclarationNode {
                    names,
                    next_statement: Box::new(next_stmt),
                }, semicolon_pos + adv))
            } else {
                None
            }
        } else {
            None
        };
    }
}

#[derive(Debug)]
struct LabelDeclarationNode {
    name: String,
    next_statement: Box<StatementNode>,
}

impl Parse for LabelDeclarationNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.len() < 3 {
            return None;
        }

        return if let [
        Token { r#type: TokenType::Name, val: name, .. },
        Token { r#type: TokenType::Colon, .. }
        ] = &tokens[..2] {
            let name = name.as_ref().unwrap().clone();
            if let Some((next_stmt, adv)) = StatementNode::parse(&tokens[2..]) {
                Some((LabelDeclarationNode {
                    name,
                    next_statement: Box::new(next_stmt),
                }, 2 + adv))
            } else {
                None
            }
        } else {
            None
        };
    }
}

fn extract_bracketed_rvalue(
    tokens: &[Token],
    bracket_type: crate::token::BracketType,
) -> Option<(RvalueNode, usize)> {
    return if let Some(
        Token {
            r#type: TokenType::Bracket(br), ..
        }) = tokens.first() {
        if br.bracket_type != bracket_type {
            return None;
        }

        let br_expr = extract_bracketed_expression(tokens)?;
        if br_expr.is_empty() {
            return None;
        }

        Some((RvalueNode::parse_exact(br_expr)?, 1 + br_expr.len() + 1))
    } else {
        None
    };
}

#[derive(Debug)]
struct RvalueNode {}

impl Parse for RvalueNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.is_empty() {
            return None;
        }

        if tokens.len() == 1 {
            return Some((RvalueNode {}, 1));
        }

        todo!()
    }
}

impl ParseExact for RvalueNode {}

#[derive(Debug)]
struct SwitchStatementNode {
    rvalue: RvalueNode,
    body: Box<StatementNode>,
    cases: RefCell<Vec<Constant>>,
    default_case: bool,
}

// impl SetParent for SwitchStatementNode {
//     fn set_parent(&mut self, parent: Weak<AbstractSyntaxNode>) -> Result<(), String> {
//         self.rvalue.set_parent(parent.clone())?;
//         self.body.deref_mut().set_parent(parent)
//     }
// }

impl Parse for SwitchStatementNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::ControlStatementIdentifier::Switch;
        use crate::token::LEFT_ROUND_BRACKET;

        if tokens.len() < 5 {  // switch ( rvalue ) null_stmt
            return None;
        }

        let left_br_idx = 1;
        return match &tokens[..left_br_idx + 1] {
            [
            Token { r#type: TokenType::ControlStatement(Switch), .. },
            Token { r#type: LEFT_ROUND_BRACKET, val, .. }
            ] => {
                let right_br_pos = TokenPos::parse(val.as_ref()?)?;
                let right_br_idx = tokens.into_iter()
                    .position(|t| t.pos == right_br_pos)?;

                if right_br_idx == left_br_idx + 1 {  // switch ( )
                    return None;
                }

                let rvalue = RvalueNode::parse_exact(
                    &tokens[left_br_idx + 1..right_br_idx])?;

                let (body, adv) = StatementNode::parse(&tokens[right_br_idx + 1..])?;
                let toks_consumed = right_br_idx + 1 + adv;
                let body = Box::new(body);

                Some((SwitchStatementNode {
                    rvalue,
                    body,
                    cases: RefCell::default(),
                    default_case: false,
                }, toks_consumed))
            }
            _ => None
        };
    }
}

#[derive(Debug)]
struct CaseStatementNode {
    constant: Constant,
    next_statement: Box<StatementNode>,
}

// impl SetParent for CaseStatementNode {
//     fn set_parent(&mut self, parent: Weak<AbstractSyntaxNode>) -> Result<(), String> {
//         self.next_statement.set_parent(parent)
//     }
// }

impl Parse for CaseStatementNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::ControlStatementIdentifier::Case;

        if tokens.len() < 4 {  // case const : null_stmt
            return None;
        }

        if tokens.first()?.r#type != TokenType::ControlStatement(Case) {
            return None;
        }

        let constant = Constant::parse_exact(&tokens[1..=1])?;

        if tokens[2].r#type != TokenType::Colon {
            return None;
        }

        let (next_stmt, adv) = StatementNode::parse(&tokens[3..])?;
        let toks_consumed = 3 + adv;  // case const : adv

        Some((CaseStatementNode {
            constant,
            next_statement: Box::new(next_stmt),
        }, toks_consumed))
    }
}

#[derive(Debug)]
struct IfStatementNode {
    condition: RvalueNode,
    body: Box<StatementNode>,
    else_body: Option<Box<StatementNode>>,
}

impl Parse for IfStatementNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::ControlStatementIdentifier::{If, Else};
        use crate::token::BracketType::Round;

        if tokens.len() < 5 {  // if ( rvalue ) ;
            return None;
        }

        if tokens[0].r#type != TokenType::ControlStatement(If) {
            return None;
        }

        let (condition, adv) = extract_bracketed_rvalue(&tokens[1..], Round)?;
        let mut toks_consumed = 1 + adv;

        let (body, adv) = StatementNode::parse(&tokens[toks_consumed..])?;
        let body = Box::new(body);
        toks_consumed += adv;

        let else_body =
            if let Some(tok) = tokens.get(toks_consumed) {
                if tok.r#type == TokenType::ControlStatement(Else) {
                    toks_consumed += 1;
                    if toks_consumed == tokens.len() {
                        return None;
                    }

                    let (else_body, adv) = StatementNode::parse(
                        &tokens[toks_consumed..])?;
                    toks_consumed += adv;

                    Some(Box::new(else_body))
                } else {
                    None
                }
            } else {
                None
            };

        Some((IfStatementNode {
            condition,
            body,
            else_body,
        }, toks_consumed))
    }
}

#[derive(Debug)]
struct WhileNode {
    condition: RvalueNode,
    body: Box<StatementNode>,
}

impl Parse for WhileNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        todo!()
    }
}

fn extract_bracketed_expression(tokens: &[Token]) -> Option<&[Token]> {
    use crate::token::LeftOrRight::Left;
    use crate::token::Bracket;

    if tokens.len() < 2 {
        return None;
    }

    return if let Token {
        r#type: TokenType::Bracket(Bracket { left_or_right: Left, .. }),
        val, ..
    } = &tokens[0] {
        let right_br_pos = TokenPos::parse(val.as_ref()?)?;
        let right_br_idx = tokens.into_iter()
            .position(|t| t.pos == right_br_pos)?;
        Some(&tokens[1..right_br_idx])
    } else {
        None
    };
}

#[derive(Debug)]
struct CompoundStatementNode {
    statement_list: Vec<Box<StatementNode>>
}

impl Parse for CompoundStatementNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::LEFT_CURLY_BRACKET;

        if tokens.len() < 2 {
            return None;
        }

        if tokens[0].r#type != LEFT_CURLY_BRACKET {
            return None;
        }

        let br_expr = extract_bracketed_expression(tokens)?;
        let body_len = br_expr.len();
        let mut statement_list = Vec::<StatementNode>::new();

        let mut read: usize = 0;
        while read < body_len {
            let (stmt, adv) = StatementNode::parse(&br_expr[read..])?;
            read += adv;
            statement_list.push(stmt);
        }

        let statement_list = statement_list.into_iter().map(Box::new).collect();
        Some((CompoundStatementNode {
            statement_list
        }, 1 + body_len + 1))
    }
}

#[derive(Debug)]
struct GotoNode {
    label_name: String,
}

impl Parse for GotoNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        todo!()
    }
}

#[derive(Debug)]
struct ReturnNode {
    rvalue: Option<RvalueNode>
}

impl Parse for ReturnNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::BracketType::Round;
        use crate::token::ControlStatementIdentifier::Return;

        if tokens.is_empty() {
            return None;
        }

        if tokens[0].r#type != TokenType::ControlStatement(Return) {
            return None;
        }

        let semicolon_pos = get_semicolon_pos(tokens)?;
        let toks_consumed = semicolon_pos + 1;

        if semicolon_pos == 1 {  // return ;
            return Some((ReturnNode { rvalue: None }, toks_consumed));
        }

        let toks_trim = &tokens[..semicolon_pos];

        return Some((ReturnNode {
            rvalue: Some(extract_bracketed_rvalue(&toks_trim[1..], Round)?.0)
        }, toks_consumed));
    }
}

#[derive(Debug)]
struct RvalueAndSemicolonNode {
    rvalue: RvalueNode
}

// impl SetParent for RvalueAndSemicolonNode {
//     fn set_parent(&mut self, parent: Weak<AbstractSyntaxNode>) -> Result<(), String> {
//         self.rvalue.set_parent(parent)
//     }
// }

#[derive(Debug)]
enum Statement {
    NullStatement,
    Compound(CompoundStatementNode),
    AutoDeclaration(AutoDeclarationNode),
    ExternDeclaration(ExternDeclarationNode),
    LabelDeclaration(LabelDeclarationNode),
    Return(ReturnNode),
    Switch(SwitchStatementNode),
    Case(CaseStatementNode),
    If(IfStatementNode),
    While(WhileNode),
    Goto(GotoNode),
    RvalueAndSemicolon(RvalueAndSemicolonNode),
}

impl Parse for Statement {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.is_empty() {
            return None;
        }

        if tokens[0].r#type == TokenType::Semicolon {
            return Some((Statement::NullStatement, 1));
        }

        if let Some((comp_stmt, adv)) = CompoundStatementNode::parse(tokens) {
            return Some((Statement::Compound(comp_stmt), adv));
        }

        if let Some((auto_decl, adv)) = AutoDeclarationNode::parse(tokens) {
            return Some((Statement::AutoDeclaration(auto_decl), adv));
        }

        if let Some((extern_decl, adv)) = ExternDeclarationNode::parse(tokens) {
            return Some((Statement::ExternDeclaration(extern_decl), adv));
        }

        if let Some((label_decl, adv)) = LabelDeclarationNode::parse(tokens) {
            return Some((Statement::LabelDeclaration(label_decl), adv));
        }

        if let Some((switch, adv)) = SwitchStatementNode::parse(tokens) {
            return Some((Statement::Switch(switch), adv));
        }

        if let Some((case, adv)) = CaseStatementNode::parse(tokens) {
            return Some((Statement::Case(case), adv));
        }

        if let Some((r#return, adv)) = ReturnNode::parse(tokens) {
            return Some((Statement::Return(r#return), adv));
        }

        if let Some((r#while, adv)) = WhileNode::parse(tokens) {
            return Some((Statement::While(r#while), adv));
        }

        if let Some((goto, adv)) = GotoNode::parse(tokens) {
            return Some((Statement::Goto(goto), adv));
        }

        todo!()
    }
}

#[derive(Debug)]
pub(crate) struct StatementNode {
    position: TokenPos,
    statement: Box<Statement>,
}

impl Parse for StatementNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.is_empty() {
            return None;
        }

        return if let Some((statement, adv)) = Statement::parse(tokens) {
            Some((StatementNode {
                position: tokens[0].pos,
                statement: Box::new(statement),
            }, adv))
        } else {
            None
        };
    }
}

#[derive(Debug)]
pub(crate) enum DefinitionNode {
    Variable(VariableDefinitionNode),
    Vector(VectorDefinitionNode),
    Function(Rc<FunctionDefinitionNode>),
}

// impl SetParent for DefinitionNode {
//     fn set_parent(&mut self, parent: Weak<AbstractSyntaxNode>) -> Result<(), String> {
//         match self.definition {
//             Definition::Function(fn_def) => fn_def.set_parent(parent),
//             _ => Ok(())
//         }
//     }
// }

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
            return Some((DefinitionNode::Function(Rc::new(fn_def_node)), adv));
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
    pub(crate) fn get_definitions(&self) -> Iter<DefinitionNode> {
        self.definitions.iter()
    }
}

impl ProgramNode {
    pub fn new() -> Self {
        ProgramNode::default()
    }
}

impl From<Vec<DefinitionNode>> for ProgramNode {
    fn from(definitions: Vec<DefinitionNode>) -> Self {
        ProgramNode { definitions }
    }
}

impl Default for ProgramNode {
    fn default() -> Self {
        ProgramNode {
            definitions: Vec::default()
        }
    }
}

// impl SetParent for ProgramNode {
//     fn set_parent(&mut self, parent: Weak<AbstractSyntaxNode>) -> Result<(), String> {
//         for def in self.definitions {
//             def.set_parent(parent);
//         }
//     }
// }

impl Parse for ProgramNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        let mut defs = Vec::<DefinitionNode>::new();
        let mut offset: usize = 0;
        while let Some((def_node, adv)) = DefinitionNode::parse(&tokens[offset..]) {
            offset += adv;
            defs.push(def_node);
        }
        if offset != tokens.len() {
            None
        } else {
            Some((ProgramNode::from(defs), offset))
        }
    }
}

impl ParseExact for ProgramNode {}

// #[derive(Debug)]
// pub(crate) enum AbstractSyntaxNode {
//     Program(ProgramNode),
//     Definition(DefinitionNode),
//     Statement(StatementNode),
//     Operator,
// }
//
// impl AbstractSyntaxNode {
//     pub(crate) fn new() -> Self {
//         AbstractSyntaxNode::Program(ProgramNode::new())
//     }
// }
//
// impl SetParent for AbstractSyntaxNode {
//     fn set_parent(&mut self, parent: Weak<AbstractSyntaxNode>) -> Result<(), String> {
//         match self {
//             AbstractSyntaxNode::Program(prog) => prog.set_parent(parent),
//             AbstractSyntaxNode::Definition(def) => {
//                 def.parent = parent;
//                 let ptr_to_self: *const AbstractSyntaxNode = self;
//                 let weak_ptr_to_self = unsafe {
//                     Weak::from_raw(ptr_to_self)
//                 };
//             }
//             AbstractSyntaxNode::Statement(stmt) => { stmt.set_parent(parent) }
//             AbstractSyntaxNode::Operator => {}
//         }
//     }
// }
//
// impl Parse for AbstractSyntaxNode {
//     fn parse(tokens: &[Token]) -> Option<(Self, usize)>
//         where Self: Sized {
//         todo!()
//     }
// }