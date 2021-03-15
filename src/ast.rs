use std::cell::RefCell;
use std::rc::{Rc, Weak};

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
        use crate::token::Bracket;

        let tokens_cut: Vec<&Token> = tokens.into_iter()
            .take_while(|t| t.r#type != TokenType::Semicolon)
            .collect();
        if tokens_cut.len() == tokens.len() {  // no semicolon found
            return None;
        }

        const LEFT_SQUARE_BRACKET: Bracket = Bracket::left_square_bracket();
        const RIGHT_SQUARE_BRACKET: Bracket = Bracket::right_square_bracket();

        if tokens_cut.len() < 3 {
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
                } else {
                    return None;
                }
            }
            _ => return None
        };
        Some((node, tokens_cut.len() + 1))
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
    name: String,
    parameter_names: Vec<String>,
    body: StatementNode,
}

impl Parse for FunctionDefinitionNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use crate::token::Bracket;

        const LEFT_ROUND_BRACKET: Bracket = Bracket::left_round_bracket();
        const RIGHT_ROUND_BRACKET: Bracket = Bracket::right_round_bracket();

        if tokens.len() < 4 {
            return None;
        }
        match &tokens[..2] {
            [Token {
                r#type: TokenType::Name,
                val: name, ..
            }, Token {
                r#type: TokenType::Bracket(LEFT_ROUND_BRACKET), ..
            }] => {
                let name = name.as_ref().unwrap().clone();
                let mut toks_consumed: usize = 2;
                let right_bracket_pos = tokens[toks_consumed..].iter()
                    .position(|c| c.r#type == TokenType::Bracket(RIGHT_ROUND_BRACKET));

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

                toks_consumed = right_bracket_pos;
                return if let Some((stmt, adv)) = StatementNode::parse(&tokens[toks_consumed..]) {
                    toks_consumed += adv;
                    Some((
                        FunctionDefinitionNode {
                            name,
                            parameter_names,
                            body: stmt,
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

#[derive(Debug)]
struct RvalueNode {}

// #[derive(Debug)]
// struct CaseStatementNode {
//     value: Constant,
//     statement: StatementNode,
// }

#[derive(Debug)]
struct SwitchStatementNode {
    rvalue: RvalueNode,
    body: Box<StatementNode>,
    cases: RefCell<Vec<Constant>>,
    default_case: bool,
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
    Return(ReturnNode),
    SwitchCase(SwitchStatementNode),
    Compound(CompoundStatementNode),
    If(IfStatementNode),
    Goto(GotoNode),
    RvalueAndSemicolon(RvalueAndSemicolonNode),
    NullStatement(NullStatementNode),
}

impl Parse for Statement {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.is_empty() {
            return None;
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

        todo!()
    }
}

#[derive(Debug)]
struct StatementNode {
    position: TokenPos,
    statement: Statement,
    parent: Option<Weak<StatementNode>>,
}

impl Parse for StatementNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.is_empty() {
            return None;
        }

        let res = StatementNode {
            position: tokens[0].pos,
            statement: Statement::NullStatement(NullStatementNode {}),
            parent: None,
        };
        let res = Rc::new(res);
        let res_weak_ptr = Some(Rc::downgrade(&res));

        if let Some((statement, adv)) = Statement::parse(tokens) {
            match statement {
                Statement::AutoDeclaration(mut auto_decl) => {
                    auto_decl.next_statement.parent = res_weak_ptr;
                    let mut res = Rc::<StatementNode>::try_unwrap(res).unwrap();
                    res.statement = Statement::AutoDeclaration(auto_decl);
                    return Some((res, adv));
                }
                Statement::ExternDeclaration(mut extern_decl) => {
                    extern_decl.next_statement.parent = res_weak_ptr;
                    let mut res = Rc::<StatementNode>::try_unwrap(res).unwrap();
                    res.statement = Statement::ExternDeclaration(extern_decl);
                    return Some((res, adv));
                }
                Statement::LabelDeclaration(mut label_decl) => {
                    label_decl.next_statement.parent = res_weak_ptr;
                    let mut res = Rc::<StatementNode>::try_unwrap(res).unwrap();
                    res.statement = Statement::LabelDeclaration(label_decl);
                    return Some((res, adv));
                }
                _ => todo!()
            }
        }
        todo!()
    }
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
        if offset != tokens.len() {
            None
        } else {
            Some((ProgramNode::from(defs), offset))
        }
    }
}