use std::cell::RefCell;
use std::convert::TryFrom;
use std::rc::Rc;

use token::{Token, TokenType};

use crate::lexical_analyzer::TokenPos;
use crate::token;

pub(crate) trait Parse {
    fn parse(input: &[Token]) -> Option<(Self, usize)>
        where Self: Sized;
}

pub(crate) trait ParseExact: Parse {
    fn parse_exact(input: &[Token]) -> Option<Self>
        where Self: Sized {
        if let Some((obj, adv)) = Self::parse(&input) {
            return if adv == input.len() {
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
pub(crate) struct Constant {
    constant_type: token::Constant,
    value: String,
}

impl TryFrom<&Token> for Constant {
    type Error = ();

    fn try_from(t: &Token) -> Result<Self, Self::Error> {
        return if let TokenType::Constant(const_type) = t.r#type {
            Ok(Constant {
                constant_type: const_type,
                value: t.val.as_ref().unwrap().clone(),
            })
        } else {
            Err(())
        };
    }
}

impl Parse for Constant {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        Some((Constant::try_from(tokens.get(0)?).ok()?, 1))
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
        use token::LEFT_SQUARE_BRACKET;

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
            let name = name.as_ref().unwrap().clone();
            let comma_idx = name_idx + 1;

            match tokens.get(comma_idx) {
                Some(Token { r#type: TokenType::Comma, .. }) => {  // OK, matched "name ,"
                    res.push(name);
                }
                None => {  // end of input, matched "[name_list ,] name"
                    res.push(name);
                    return Some(res);
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
        if tokens.len() < 4 {
            return None;
        }

        use token::{LEFT_ROUND_BRACKET, RIGHT_ROUND_BRACKET};

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
                return if let Some(
                    (stmt, adv)
                ) = StatementNode::parse(&tokens[toks_consumed..]) {
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
        use token::Bracket;

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
    position: TokenPos,
    declarations: Vec<AutoDeclaration>,
    next_statement: Box<StatementNode>,
}

impl Parse for AutoDeclarationNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.len() < 4 {  // auto name ; null_stmt
            return None;
        }

        use token::DeclarationSpecifier::Auto;

        return if let Some(
            Token { r#type: TokenType::DeclarationSpecifier(Auto), pos, .. }
        ) = tokens.first() {
            let semicolon_pos =
                if let Some(sp_minus_one) = get_semicolon_pos(&tokens[1..]) {
                    sp_minus_one + 1
                } else {
                    return None;
                };

            let next_statement_starts_at = semicolon_pos + 1;
            if tokens.len() <= next_statement_starts_at {
                return None;
            }

            let declarations;
            if let Some(decls) = AutoDeclarationNode::get_auto_decl_list(
                &tokens[1..semicolon_pos]) {
                declarations = decls;
            } else {
                return None;
            }

            if let Some(
                (stmt_node, adv)
            ) = StatementNode::parse(&tokens[next_statement_starts_at..]) {
                Some((AutoDeclarationNode {
                    position: *pos,
                    declarations,
                    next_statement: Box::new(stmt_node),
                }, next_statement_starts_at + adv))
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
                         }) => {
                        i += 1;
                        continue;
                    }
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
        if tokens.len() < 4 {  // extrn name ; null_stmt
            return None;
        }

        use token::DeclarationSpecifier::Extrn;

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

            let next_statement_starts_at = semicolon_pos + 1;
            if tokens.len() <= next_statement_starts_at {
                return None;
            }

            if let Some(
                (next_stmt, adv)
            ) = StatementNode::parse(&tokens[next_statement_starts_at..]) {
                Some((ExternDeclarationNode {
                    names,
                    next_statement: Box::new(next_stmt),
                }, next_statement_starts_at + adv))
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
    bracket_type: token::BracketType,
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

#[derive(Debug, Clone)]
pub(crate) enum Lvalue {
    Name(String),
    DerefRvalue(Box<RvalueNode>),
    Indexing { vector: Box<RvalueNode>, index: Box<RvalueNode> },
}

impl TryFrom<RvalueNode> for Lvalue {
    type Error = ();

    fn try_from(rvalue_node: RvalueNode) -> Result<Self, Self::Error> {
        if let RvalueNode { rvalue: Rvalue::Lvalue(lvalue), .. } = rvalue_node {
            Ok(lvalue)
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub(crate) enum IncDecType {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone)]
pub(crate) enum IncDec {
    Increment,
    Decrement,
}

#[derive(Debug, Clone)]
pub(crate) struct IncDecNode {
    inc_or_dec: IncDec,
    inc_dec_type: IncDecType,
    lvalue: Lvalue,
}

impl From<(IncDec, IncDecType, Lvalue)> for IncDecNode {
    fn from(data: (IncDec, IncDecType, Lvalue)) -> Self {
        let (inc_or_dec, inc_dec_type, lvalue) = data;
        IncDecNode { inc_or_dec, inc_dec_type, lvalue }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum Unary {
    Plus,
    Minus,
    LogicalNot,
    Complement,
}

// enum Expression {
//     Primary(PrimaryExpression),
//     Operator,
// }
//
// impl Parse for Expression {
//     fn parse(tokens: &[Token]) -> Option<(Self, usize)>
//         where Self: Sized {
//         let primary_expr = PrimaryExpression::parse(tokens);
//         if let Some((expr, adv)) = primary_expr {}
//
//         todo!()
//     }
// }

#[derive(Debug, Clone)]
pub(crate) enum Rvalue {
    Constant(Constant),
    Lvalue(Lvalue),
    Assign { lhs: Lvalue, assign: token::Assign, rhs: Box<RvalueNode> },
    IncDec(IncDecNode),
    Unary(Unary, Box<RvalueNode>),
    TakeAddress(Lvalue),
    Binary { lhs: Box<RvalueNode>, bin_op: token::RichBinaryOperation, rhs: Box<RvalueNode> },
    ConditionalExpression {
        condition: Box<RvalueNode>,
        on_true: Box<RvalueNode>,
        on_false: Box<RvalueNode>,
        colon_pos: TokenPos,
    },
    BracketedExpression(Box<RvalueNode>),
    FunctionCall { fn_name: Box<RvalueNode>, arguments: Vec<Box<RvalueNode>> },
}

#[derive(Debug, Clone)]
pub(crate) struct RvalueNode {
    position: TokenPos,
    pub(crate) rvalue: Rvalue,
}

impl From<(TokenPos, Rvalue)> for RvalueNode {
    fn from(pos_and_rvalue: (TokenPos, Rvalue)) -> Self {
        let (position, rvalue) = pos_and_rvalue;
        RvalueNode { position, rvalue }
    }
}

impl RvalueNode {
    pub(crate) fn get_position(&self) -> TokenPos {
        self.position
    }

    pub(crate) fn to_truth_value(&self) -> Option<RvalueNode> {
        use token::{RichBinaryOperation, BinaryOperation};

        match &self.rvalue {
            Rvalue::Constant(_) => Some(self.clone()),
            Rvalue::Lvalue(lvalue) =>
                if let Lvalue::Name(_) = lvalue {
                    Some(self.clone())
                } else {
                    None
                },
            Rvalue::Binary {
                lhs,
                bin_op,
                rhs
            } => {
                match bin_op {
                    RichBinaryOperation::LogicalAnd => Some(self.clone()),

                    RichBinaryOperation::RegularBinary(BinaryOperation::Or) => {
                        let lhs = Box::new(lhs.to_truth_value()?);
                        let rhs = Box::new(rhs.to_truth_value()?);
                        let bin_op = *bin_op;

                        Some(RvalueNode::from(
                            (self.position,
                             Rvalue::Binary {
                                 lhs,
                                 bin_op,
                                 rhs,
                             })))
                    }
                    RichBinaryOperation::BitwiseAnd => {
                        let lhs = Box::new(lhs.to_truth_value()?);
                        let rhs = Box::new(rhs.to_truth_value()?);

                        Some(RvalueNode::from(
                            (self.position,
                             Rvalue::Binary {
                                 lhs,
                                 bin_op: RichBinaryOperation::LogicalAnd,
                                 rhs,
                             })))
                    }
                    _ => return None,
                }
            }

            Rvalue::ConditionalExpression {
                condition,
                on_true,
                on_false,
                colon_pos
            } => {
                let condition = Box::new(condition.to_truth_value()?);
                let on_true = on_true.clone();
                let on_false = on_false.clone();
                let colon_pos = *colon_pos;

                Some(
                    RvalueNode::from(
                        (self.position,
                         Rvalue::ConditionalExpression {
                             condition,
                             on_true,
                             on_false,
                             colon_pos,
                         })
                    ))
            }

            Rvalue::BracketedExpression(br_expr) =>
                Some(
                    RvalueNode::from(
                        (self.position,
                         Rvalue::BracketedExpression(
                             Box::new(br_expr.to_truth_value()?)
                         ))
                    )
                ),

            Rvalue::Unary(Unary::LogicalNot, rvalue_node) => {
                Some(
                    RvalueNode::from(
                        (self.position,
                         Rvalue::Unary(Unary::LogicalNot,
                                       Box::new(rvalue_node.to_truth_value()?)))
                    )
                )
            }
            _ => None
        }
    }

    // fn is_lvalue(&self) -> bool {
    //     if let RvalueNode { rvalue: Rvalue::Lvalue(_), .. } = self {
    //         true
    //     } else {
    //         false
    //     }
    // }
}

impl ParseExact for RvalueNode {}

#[derive(Debug)]
struct SwitchNode {
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

impl Parse for SwitchNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use token::ControlStatementIdentifier::Switch;
        use token::LEFT_ROUND_BRACKET;

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

                Some((SwitchNode {
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
struct CaseNode {
    constant: Constant,
    next_statement: Box<StatementNode>,
}

// impl SetParent for CaseStatementNode {
//     fn set_parent(&mut self, parent: Weak<AbstractSyntaxNode>) -> Result<(), String> {
//         self.next_statement.set_parent(parent)
//     }
// }

impl Parse for CaseNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use token::ControlStatementIdentifier::Case;

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

        Some((CaseNode {
            constant,
            next_statement: Box::new(next_stmt),
        }, toks_consumed))
    }
}

#[derive(Debug)]
struct IfNode {
    condition: RvalueNode,
    body: Box<StatementNode>,
    else_body: Option<Box<StatementNode>>,
}

impl Parse for IfNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use token::ControlStatementIdentifier::{If, Else};
        use token::BracketType::Round;

        if tokens.len() < 5 {  // if ( rvalue ) ;
            return None;
        }

        if tokens[0].r#type != TokenType::ControlStatement(If) {
            return None;
        }

        let (condition, adv) = extract_bracketed_rvalue(&tokens[1..],
                                                        Round)?;
        let mut toks_consumed = 1 + adv;

        let condition =
            if let Some(truth_value) = condition.to_truth_value() {
                truth_value
            } else {
                condition
            };

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

        Some((IfNode {
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
        if tokens.len() < 5 {  // while ( x ) ;
            return None;
        }

        use token::ControlStatementIdentifier::While;
        use token::BracketType::Round;

        if tokens[0].r#type != TokenType::ControlStatement(While) {
            return None;
        }

        let (condition, adv) = extract_bracketed_rvalue(&tokens[1..],
                                                        Round)?;
        let mut toks_consumed = 1 + adv;

        let condition =
            if let Some(truth_value) = condition.to_truth_value() {
                truth_value
            } else {
                condition
            };

        if tokens.len() <= toks_consumed {
            return None;
        }

        let (body, adv) = StatementNode::parse(&tokens[toks_consumed..])?;
        toks_consumed += adv;

        return Some((WhileNode {
            condition,
            body: Box::new(body),
        }, toks_consumed));
    }
}

pub(crate) fn extract_bracketed_expression(tokens: &[Token]) -> Option<&[Token]> {
    const LEFT: token::LeftOrRight = token::LeftOrRight::Left;

    use token::Bracket;

    return if let Token {
        r#type: TokenType::Bracket(Bracket { left_or_right: LEFT, .. }),
        val, ..
    } = tokens.get(0)? {
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
        use token::LEFT_CURLY_BRACKET;

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

        let statement_list = statement_list
            .into_iter()
            .map(Box::new)
            .collect();
        Some((CompoundStatementNode {
            statement_list
        }, 1 + body_len + 1))
    }
}

#[derive(Debug)]
struct GotoNode {
    label: RvalueNode,
}

impl Parse for GotoNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.len() < 3 {  // goto label ;
            return None;
        }

        use token::ControlStatementIdentifier::Goto;

        if tokens[0].r#type != TokenType::ControlStatement(Goto) {
            return None;
        }

        let semicolon_pos = get_semicolon_pos(tokens)?;
        let toks_consumed = semicolon_pos + 1;

        let rvalue = &tokens[1..semicolon_pos];

        return Some((GotoNode {
            label: RvalueNode::parse_exact(&rvalue)?
        }, toks_consumed));
    }
}

#[derive(Debug)]
struct ReturnNode {
    rvalue: Option<RvalueNode>
}

impl Parse for ReturnNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.is_empty() {
            return None;
        }

        use token::BracketType::Round;
        use token::ControlStatementIdentifier::Return;

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

impl Parse for RvalueAndSemicolonNode {
    fn parse(input: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if input.len() < 2 {  // x ;
            return None;
        }

        let semicolon_pos = get_semicolon_pos(input)?;
        let toks_consumed = semicolon_pos + 1;

        let rvalue = &input[..semicolon_pos];

        return Some((RvalueAndSemicolonNode {
            rvalue: RvalueNode::parse_exact(&rvalue)?
        }, toks_consumed));
    }
}

#[derive(Debug)]
enum Statement {
    NullStatement,
    Compound(CompoundStatementNode),
    AutoDeclaration(AutoDeclarationNode),
    ExternDeclaration(ExternDeclarationNode),
    LabelDeclaration(LabelDeclarationNode),
    Return(ReturnNode),
    Switch(SwitchNode),
    Case(CaseNode),
    If(IfNode),
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

        if let Some(
            (rv_and_semicolon, adv)
        ) = RvalueAndSemicolonNode::parse(tokens) {
            return Some((Statement::RvalueAndSemicolon(rv_and_semicolon), adv));
        }

        if let Some((r#return, adv)) = ReturnNode::parse(tokens) {
            return Some((Statement::Return(r#return), adv));
        }

        if let Some((r#if, adv)) = IfNode::parse(tokens) {
            return Some((Statement::If(r#if), adv));
        }

        if let Some((r#while, adv)) = WhileNode::parse(tokens) {
            return Some((Statement::While(r#while), adv));
        }

        if let Some(
            (comp_stmt, adv)
        ) = CompoundStatementNode::parse(tokens) {
            return Some((Statement::Compound(comp_stmt), adv));
        }

        if let Some((switch, adv)) = SwitchNode::parse(tokens) {
            return Some((Statement::Switch(switch), adv));
        }

        if let Some((case, adv)) = CaseNode::parse(tokens) {
            return Some((Statement::Case(case), adv));
        }

        if let Some(
            (auto_decl, adv)
        ) = AutoDeclarationNode::parse(tokens) {
            return Some((Statement::AutoDeclaration(auto_decl), adv));
        }

        if let Some(
            (extern_decl, adv)
        ) = ExternDeclarationNode::parse(tokens) {
            return Some((Statement::ExternDeclaration(extern_decl), adv));
        }

        if let Some((goto, adv)) = GotoNode::parse(tokens) {
            return Some((Statement::Goto(goto), adv));
        }

        if let Some(
            (label_decl, adv)
        ) = LabelDeclarationNode::parse(tokens) {
            return Some((Statement::LabelDeclaration(label_decl), adv));
        }

        None
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
        ProgramNode { definitions: Vec::default() }
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

impl ParseExact for ProgramNode {}