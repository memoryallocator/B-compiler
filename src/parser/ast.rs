use std::*;
use cell::RefCell;
use collections::HashMap;
use convert::TryFrom;
use rc::{Rc, Weak};

use crate::parser::MultiMap;
use crate::lexical_analyzer::token;
use token::*;

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
pub(crate) enum Ival {
    Constant(Rc<ConstantNode>),
    Name(Rc<String>, TokenPos),
}

impl Parse for Ival {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        return Some((
            if let Some(t) = tokens.first() {
                match &t.token {
                    WrappedToken::Name(name) => Ival::Name(name.clone(), t.pos),

                    WrappedToken::Constant(_) =>
                        Ival::Constant(Rc::new(ConstantNode::try_from(t).unwrap())),
                    _ => return None,
                }
            } else {
                return None;
            }, 1));
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub(crate) struct ConstantNode {
    pub(crate) position: TokenPos,
    pub(crate) constant: Rc<token::Constant>,
}

impl TryFrom<&Token> for ConstantNode {
    type Error = ();

    fn try_from(t: &Token) -> Result<Self, Self::Error> {
        return if let WrappedToken::Constant(constant) = &t.token {
            Ok(ConstantNode {
                position: t.pos,
                constant: Rc::new(constant.clone()),
            })
        } else {
            Err(())
        };
    }
}

impl Parse for ConstantNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        Some((ConstantNode::try_from(tokens.get(0)?).ok()?, 1))
    }
}

impl ParseExact for ConstantNode {}

#[derive(Debug)]
pub(crate) struct VariableDefinitionNode {
    position: TokenPos,
    pub(crate) name: Rc<String>,
    pub(crate) initial_value: Option<Rc<Ival>>,
}

impl VariableDefinitionNode {
    pub(crate) fn get_position(&self) -> TokenPos {
        self.position.clone()
    }
}

impl Parse for VariableDefinitionNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)> {
        let toks_trim: Vec<&Token> = tokens.into_iter()
            .take_while(|t| t.token != WrappedToken::Semicolon)
            .collect();
        if toks_trim.len() == tokens.len() {  // no semicolon found
            return None;
        }
        return
            Some((
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
                                if let Some((ival, _)) = Ival::parse(&[t.clone()]) {
                                    Some(Rc::new(ival))
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
    pub(crate) position: TokenPos,
    pub(crate) name: Rc<String>,
    pub(crate) specified_size: Option<Rc<ConstantNode>>,
    pub(crate) initial_values: Option<Vec<Rc<Ival>>>,
}

impl Parse for VectorDefinitionNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        let toks_trim: Vec<&Token> =
            tokens
                .into_iter()
                .take_while(|t| t.token != WrappedToken::Semicolon)
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

                    let right_bracket_idx = get_right_bracket_index(tokens, left_br_idx)?;

                    if right_bracket_idx - left_br_idx > 2 {  // name [ ... ... ]
                        return None;
                    }

                    let specified_size =
                        if right_bracket_idx == left_br_idx + 1 {  // name [ ]
                            None
                        } else {
                            if let Some(r#const) = ConstantNode::parse_exact(
                                &tokens[left_br_idx + 1..left_br_idx + 2]) {
                                Some(Rc::new(r#const))
                            } else {
                                None
                            }
                        };

                    if toks_trim.get(right_bracket_idx + 1).is_none() {  // name [ const? ] ;
                        VectorDefinitionNode {
                            position: name_pos.clone(),
                            name,
                            specified_size,
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
                                         token: WrappedToken::Comma,
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
                            position: name_pos.clone(),
                            name,
                            specified_size,
                            initial_values: Some(initial_values
                                .into_iter()
                                .map(Rc::new)
                                .collect()),
                        }
                    }
                }
                _ => return None
            };
        Some((node, toks_trim.len() + 1))
    }
}

fn parse_name_list(
    tokens: &[Token],
    allow_duplicates: bool,
) -> Option<MultiMap<Rc<String>, (TokenPos, usize)>> {
    if tokens.is_empty() {
        return Some(Default::default());
    }

    let mut res = MultiMap::<Rc<String>, (TokenPos, usize)>::new();
    loop {
        let name_idx = res.len() * 2;
        if let Some(
            Token { token: WrappedToken::Name(name), pos }
        ) = tokens.get(name_idx) {
            let pos = *pos;
            let name = name.clone();
            let comma_idx = name_idx + 1;

            match tokens.get(comma_idx) {
                Some(Token { token: WrappedToken::Comma, .. }) => {  // OK, matched "name ,"
                    if !allow_duplicates {
                        if res.contains_key(&*name) {
                            return None;  // duplicate found
                        }
                    }

                    res.insert(name, (pos, res.len()))
                }
                None => {  // end of input, matched "[name_list ,] name"
                    if !allow_duplicates {
                        if res.contains_key(&*name) {
                            return None;  // duplicate found
                        }
                    }

                    res.insert(name, (pos, res.len()));
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
    pub(crate) position: TokenPos,
    pub(crate) name: Rc<String>,
    pub(crate) parameters: HashMap<Rc<String>, (TokenPos, usize)>,
    // pub(crate) max_local_variables: RefCell<usize>,
    pub(crate) body: Rc<StatementNode>,
    pub(crate) labels: RefCell<HashMap<Rc<String>, Weak<LabelDeclarationNode>>>,
}

impl Parse for FunctionDefinitionNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.len() < 4 {
            return None;
        }

        match &tokens[..2] {
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
                let right_bracket_index = get_right_bracket_index(tokens, left_br_idx)?;

                let parameter_names = HashMap::<Rc<String>,
                    (TokenPos, usize)>::try_from(
                    parse_name_list(&tokens[toks_consumed..right_bracket_index],
                                    false)?).ok()?;

                toks_consumed = right_bracket_index + 1;
                return if let Some(
                    (stmt, adv)
                ) = StatementNode::parse(&tokens[toks_consumed..]) {
                    let name = name.clone();
                    toks_consumed += adv;

                    Some((
                        FunctionDefinitionNode {
                            position: name_pos.clone(),
                            name,
                            parameters: parameter_names,
                            // max_local_variables: RefCell::default(),
                            body: Rc::new(stmt),
                            labels: RefCell::default(),
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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct AutoDeclaration {
    pub(crate) position: TokenPos,
    pub(crate) name: Rc<String>,
    pub(crate) vector_size: Option<ConstantNode>,
}

impl Parse for AutoDeclaration {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.is_empty() {
            return None;
        }

        if tokens.len() >= 4 {
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
            }
            ] = &tokens[..4] {
                let name = name.clone();

                let vector_size = Some(ConstantNode {
                    position: const_pos.clone(),
                    constant: Rc::new(vec_size.clone()),
                });

                return Some((AutoDeclaration {
                    position: tokens[0].pos,
                    name,
                    vector_size,
                }, 4));
            }
        }

        if let Token { token: WrappedToken::Name(name), pos } = &tokens[0] {
            let name = name.clone();

            return Some((AutoDeclaration {
                position: *pos,
                name,
                vector_size: None,
            }, 1));
        }
        None
    }
}

fn get_semicolon_pos(tokens: &[Token]) -> Option<usize> {
    if let Some(sp) = tokens.into_iter()
        .position(|c| c.token == WrappedToken::Semicolon) {
        Some(sp)
    } else {
        None
    }
}

#[derive(Debug)]
pub(crate) struct AutoDeclarationNode {
    pub(crate) position: TokenPos,
    pub(crate) declarations: HashMap<Rc<AutoDeclaration>, (TokenPos, usize)>,
    pub(crate) next_statement: Rc<StatementNode>,
}

impl Parse for AutoDeclarationNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.len() < 4 {  // auto name ; null_stmt
            return None;
        }

        use token::DeclarationSpecifier::Auto;

        return if let Some(
            Token {
                token: WrappedToken::ReservedName(ReservedName::DeclarationSpecifier(Auto)),
                pos, ..
            }
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
                    position: pos.clone(),
                    declarations,
                    next_statement: Rc::new(stmt_node),
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
    fn get_auto_decl_list(
        tokens: &[Token]
    ) -> Option<HashMap<Rc<AutoDeclaration>, (TokenPos, usize)>> {
        if tokens.is_empty() {
            return None;
        }

        let mut res = HashMap::<Rc<AutoDeclaration>, (TokenPos, usize)>::new();

        let mut i: usize = 0;
        while i < tokens.len() {
            while let Some(
                (auto_decl, adv)
            ) = AutoDeclaration::parse(&tokens[i..]) {
                if let Some(_) = res.insert(Rc::new(auto_decl),
                                            (tokens[i].pos, res.len())) {
                    return None;
                }
                i += adv;

                match tokens.get(i) {
                    None => return Some(res),
                    Some(Token {
                             token: WrappedToken::Comma, ..
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
pub(crate) struct ExternDeclarationNode {
    pub(crate) position: TokenPos,
    pub(crate) names: HashMap<Rc<String>, (TokenPos, usize)>,
    pub(crate) next_statement: Rc<StatementNode>,
}

impl Parse for ExternDeclarationNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.len() < 4 {  // extrn name ; null_stmt
            return None;
        }

        use token::DeclarationSpecifier::Extrn;

        if let Some(
            Token { token: WrappedToken::ReservedName(ReservedName::DeclarationSpecifier(Extrn)), .. }
        ) = tokens.first() {
            let semicolon_pos =
                if let Some(sp_minus_one) = get_semicolon_pos(&tokens[1..]) {
                    sp_minus_one + 1
                } else {
                    return None;
                };

            let names = HashMap::<Rc<String>,
                (TokenPos, usize)>::try_from(parse_name_list(&tokens[1..semicolon_pos],
                                                             false)?).ok()?;

            let next_statement_starts_at = semicolon_pos + 1;
            if tokens.len() <= next_statement_starts_at {
                return None;
            }

            if let Some(
                (next_stmt, adv)
            ) = StatementNode::parse(&tokens[next_statement_starts_at..]) {
                return Some((ExternDeclarationNode {
                    position: tokens[0].pos.clone(),
                    names,
                    next_statement: Rc::new(next_stmt),
                }, next_statement_starts_at + adv));
            }
        };

        None
    }
}

#[derive(Debug)]
pub(crate) struct LabelDeclarationNode {
    pub(crate) position: TokenPos,
    pub(crate) label_name: Rc<String>,
    pub(crate) next_statement: Rc<StatementNode>,
}

impl Parse for LabelDeclarationNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.len() < 3 {
            return None;
        }

        return if let [
        Token { token: WrappedToken::Name(name), .. },
        Token { token: WrappedToken::Colon, .. }
        ] = &tokens[..2] {
            let name = name.clone();

            if let Some((next_stmt, adv)) = StatementNode::parse(&tokens[2..]) {
                Some((LabelDeclarationNode {
                    position: tokens[0].pos.clone(),
                    label_name: name,
                    next_statement: Rc::new(next_stmt),
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
    bracket_type: BracketType,
) -> Option<(RvalueNode, usize)> {
    return if let Some(
        Token {
            token: WrappedToken::Bracket(br), ..
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
    Name(Rc<String>),
    DerefRvalue(Rc<RvalueNode>),
    Indexing { vector: Rc<RvalueNode>, index: Rc<RvalueNode> },
}

#[derive(Debug, Clone)]
pub(crate) struct LvalueNode {
    pub(crate) position: TokenPos,
    pub(crate) lvalue: Rc<Lvalue>,
}

impl TryFrom<&RvalueNode> for Rc<LvalueNode> {
    type Error = ();

    fn try_from(rvalue_node: &RvalueNode) -> Result<Self, Self::Error> {
        if let Rvalue::Lvalue(lvalue) = rvalue_node.rvalue.as_ref() {
            Ok(lvalue.clone())
        } else {
            Err(())
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) enum IncDecType {
    Prefix,
    Postfix,
}

#[derive(Debug, Clone)]
pub(crate) struct IncDecNode {
    pub(crate) inc_or_dec: IncDec,
    pub(crate) inc_dec_type: IncDecType,
    pub(crate) lvalue: Rc<LvalueNode>,
}

#[derive(Debug, Clone)]
pub(crate) enum Unary {
    Plus,
    Minus,
    LogicalNot,
    Complement,
}

#[derive(Debug, Clone)]
pub(crate) struct FunctionCallNode {
    pub(crate) fn_name: Rc<RvalueNode>,
    pub(crate) arguments: Vec<Rc<RvalueNode>>,
}

#[derive(Debug, Clone)]
pub(crate) enum Rvalue {
    Constant(Rc<ConstantNode>),
    Lvalue(Rc<LvalueNode>),
    Assign { lhs: Rc<LvalueNode>, assign: token::Assign, rhs: Rc<RvalueNode> },
    IncDec(Rc<IncDecNode>),
    Unary(Unary, Rc<RvalueNode>),
    TakeAddress(Rc<LvalueNode>),
    Binary { lhs: Rc<RvalueNode>, bin_op: token::RichBinaryOperation, rhs: Rc<RvalueNode> },
    ConditionalExpression {
        condition: Rc<RvalueNode>,
        on_true: Rc<RvalueNode>,
        on_false: Rc<RvalueNode>,
        colon_pos: TokenPos,
    },
    BracketedExpression(Rc<RvalueNode>),
    FunctionCall(FunctionCallNode),
}

#[derive(Debug, Clone)]
pub(crate) struct RvalueNode {
    pub(crate) position: TokenPos,
    pub(crate) rvalue: Rc<Rvalue>,
}

impl RvalueNode {
    pub(crate) fn try_to_truth_value(&self) -> Option<Self> {
        match self.rvalue.as_ref() {
            Rvalue::Constant(_) => Some(self.clone()),
            Rvalue::Lvalue(lvalue_node) =>
                if let Lvalue::Name(_) = *lvalue_node.lvalue {
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
                        let lhs = Rc::new(lhs.try_to_truth_value()?);
                        let rhs = Rc::new(rhs.try_to_truth_value()?);

                        Some(RvalueNode {
                            position: self.position,
                            rvalue: Rc::new(Rvalue::Binary {
                                lhs,
                                bin_op: *bin_op,
                                rhs,
                            }),
                        })
                    }

                    RichBinaryOperation::BitwiseAnd => {
                        let lhs = Rc::new(lhs.try_to_truth_value()?);
                        let rhs = Rc::new(rhs.try_to_truth_value()?);

                        Some(RvalueNode {
                            position: self.position,
                            rvalue: Rc::new(Rvalue::Binary {
                                lhs,
                                bin_op: RichBinaryOperation::LogicalAnd,
                                rhs,
                            }),
                        })
                    }

                    _ => return None,
                }
            }

            Rvalue::ConditionalExpression {
                condition,
                on_true,
                on_false,
                colon_pos,
            } => {
                let condition = Rc::new(condition.try_to_truth_value()?);
                let on_true = on_true.clone();
                let on_false = on_false.clone();

                Some(RvalueNode {
                    position: self.position,
                    rvalue: Rc::new(Rvalue::ConditionalExpression {
                        condition,
                        on_true,
                        on_false,
                        colon_pos: *colon_pos,
                    }),
                })
            }

            Rvalue::BracketedExpression(br_expr) =>
                Some(RvalueNode {
                    position: self.position,
                    rvalue: Rc::new(Rvalue::BracketedExpression(
                        Rc::new(br_expr.try_to_truth_value()?)
                    )),
                }),

            Rvalue::Unary(Unary::LogicalNot, rvalue_node) => {
                Some(RvalueNode {
                    position: self.position,
                    rvalue: Rc::new(
                        Rvalue::Unary(Unary::LogicalNot,
                                      Rc::new(rvalue_node.try_to_truth_value()?))),
                })
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
pub(crate) struct SwitchNode {
    pub(crate) rvalue: Rc<RvalueNode>,
    pub(crate) body: Rc<StatementNode>,
    pub(crate) cases: RefCell<HashMap<Rc<CaseNode>, usize>>,
}

impl Parse for SwitchNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use ControlStatementIdentifier::Switch;

        if tokens.len() < 5 {  // switch ( rvalue ) null_stmt
            return None;
        }

        let left_br_idx = 1;
        return match &tokens[..left_br_idx + 1] {
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
                let right_br_idx = get_right_bracket_index(tokens, left_br_idx)?;

                if right_br_idx == left_br_idx + 1 {  // switch ( )
                    return None;
                }

                let rvalue = RvalueNode::parse_exact(
                    &tokens[left_br_idx + 1..right_br_idx])?;

                let (body, adv) = StatementNode::parse(&tokens[right_br_idx + 1..])?;
                let toks_consumed = right_br_idx + 1 + adv;
                let body = Rc::new(body);

                Some((SwitchNode {
                    rvalue: Rc::new(rvalue),
                    body,
                    cases: RefCell::default(),
                }, toks_consumed))
            }
            _ => None
        };
    }
}

#[derive(Debug)]
pub(crate) struct CaseNode {
    pub(crate) constant: Option<ConstantNode>,
    pub(crate) next_statement: Rc<StatementNode>,
    // pub(crate) switch: RefCell<Weak<SwitchNode>>,
}

impl Parse for CaseNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use token::ControlStatementIdentifier::{Case, Default};

        if tokens.len() < 3 {  // default : null_stmt
            return None;
        }

        let colon_pos: usize;
        let constant =
            match tokens[0].token {
                WrappedToken::ReservedName(ReservedName::ControlStatement(Case)) => {
                    colon_pos = 2;
                    Some(ConstantNode::parse_exact(&tokens[1..=1])?)
                }

                WrappedToken::ReservedName(ReservedName::ControlStatement(Default)) => {
                    colon_pos = 1;
                    None
                }

                _ => return None,
            };

        if tokens[colon_pos].token != WrappedToken::Colon {
            return None;
        }

        let next_stmt_idx = colon_pos + 1;
        let (next_stmt, adv) = StatementNode::parse(&tokens[next_stmt_idx..])?;
        let toks_consumed = next_stmt_idx + adv;

        Some((CaseNode {
            constant,
            next_statement: Rc::new(next_stmt),
        }, toks_consumed))
    }
}

#[derive(Debug)]
pub(crate) struct IfNode {
    pub(crate) condition: Rc<RvalueNode>,
    pub(crate) body: Rc<StatementNode>,
    pub(crate) else_body: Option<Rc<StatementNode>>,
}

impl Parse for IfNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use token::ControlStatementIdentifier::{If, Else};
        use token::BracketType::Round;

        if tokens.len() < 5 {  // if ( rvalue ) ;
            return None;
        }

        if tokens[0].token != WrappedToken::ReservedName(ReservedName::ControlStatement(If)) {
            return None;
        }

        let (condition, adv) = extract_bracketed_rvalue(&tokens[1..],
                                                        Round)?;
        let mut toks_consumed = 1 + adv;

        let condition =
            if let Some(truth_value) = condition.try_to_truth_value() {
                truth_value
            } else {
                condition
            };

        let (body, adv) = StatementNode::parse(&tokens[toks_consumed..])?;
        let body = Rc::new(body);
        toks_consumed += adv;

        let else_body =
            if let Some(tok) = tokens.get(toks_consumed) {
                if tok.token == WrappedToken::ReservedName(ReservedName::ControlStatement(Else)) {
                    toks_consumed += 1;
                    if toks_consumed == tokens.len() {
                        return None;
                    }

                    let (else_body, adv) = StatementNode::parse(
                        &tokens[toks_consumed..])?;
                    toks_consumed += adv;

                    Some(Rc::new(else_body))
                } else {
                    None
                }
            } else {
                None
            };

        let condition = Rc::new(condition);

        Some((IfNode {
            condition,
            body,
            else_body,
        }, toks_consumed))
    }
}

#[derive(Debug)]
pub(crate) struct WhileNode {
    pub(crate) condition: Rc<RvalueNode>,
    pub(crate) body: Rc<StatementNode>,
}

impl Parse for WhileNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.len() < 5 {  // while ( x ) ;
            return None;
        }

        use ControlStatementIdentifier::While;
        use BracketType::Round;

        if tokens[0].token != WrappedToken::ReservedName(ReservedName::ControlStatement(While)) {
            return None;
        }

        let (condition, adv) = extract_bracketed_rvalue(&tokens[1..],
                                                        Round)?;
        let mut toks_consumed = 1 + adv;

        let condition =
            if let Some(truth_value) = condition.try_to_truth_value() {
                truth_value
            } else {
                condition
            };

        if tokens.len() <= toks_consumed {
            return None;
        }

        let (body, adv) = StatementNode::parse(&tokens[toks_consumed..])?;
        toks_consumed += adv;

        let condition = Rc::new(condition);

        return Some((WhileNode {
            condition,
            body: Rc::new(body),
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

pub(crate) fn extract_bracketed_expression(tokens: &[Token]) -> Option<&[Token]> {
    return if let Token {
        token: WrappedToken::Bracket(Bracket { left_or_right: LeftOrRight::Left, .. }), ..
    } = tokens.get(0)? {
        let right_br_idx = get_right_bracket_index(tokens, 0)?;

        Some(&tokens[1..right_br_idx])
    } else {
        None
    };
}

#[derive(Debug)]
pub(crate) struct CompoundStatementNode {
    pub(crate) position: TokenPos,
    pub(crate) statement_list: Vec<Rc<StatementNode>>,
}

impl Parse for CompoundStatementNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.len() < 2 {
            return None;
        }

        use BracketType::Curly;

        if let WrappedToken::Bracket(
            Bracket {
                left_or_right: LeftOrRight::Left,
                bracket_type: Curly, ..
            }) = tokens[0].token {
            // OK
        } else {
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
            .map(Rc::new)
            .collect();
        Some((CompoundStatementNode {
            position: tokens[0].pos,
            statement_list,
        }, 1 + body_len + 1))
    }
}

#[derive(Debug)]
pub(crate) struct GotoNode {
    label: RvalueNode,
}

impl Parse for GotoNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.len() < 3 {  // goto label ;
            return None;
        }

        use token::ControlStatementIdentifier::Goto;

        if tokens[0].token != WrappedToken::ReservedName(ReservedName::ControlStatement(Goto)) {
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
pub(crate) struct ReturnNode {
    pub(crate) rvalue: Option<Rc<RvalueNode>>
}

impl Parse for ReturnNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.is_empty() {
            return None;
        }

        use BracketType::Round;
        use ControlStatementIdentifier::Return;

        if tokens[0].token != WrappedToken::ReservedName(ReservedName::ControlStatement(Return)) {
            return None;
        }

        let semicolon_pos = get_semicolon_pos(tokens)?;
        let toks_consumed = semicolon_pos + 1;

        if semicolon_pos == 1 {  // return ;
            return Some((ReturnNode { rvalue: None }, toks_consumed));
        }

        let toks_trim = &tokens[..semicolon_pos];

        return Some((ReturnNode {
            rvalue: Some(Rc::new(
                extract_bracketed_rvalue(&toks_trim[1..], Round)?.0))
        }, toks_consumed));
    }
}

#[derive(Debug)]
pub(crate) struct RvalueAndSemicolonNode {
    pub(crate) rvalue: Rc<RvalueNode>
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
            rvalue: Rc::new(RvalueNode::parse_exact(&rvalue)?)
        }, toks_consumed));
    }
}

#[derive(Debug, Clone)]
pub(crate) enum BreakableStatement {
    While(Weak<WhileNode>),
    Switch(Weak<SwitchNode>),
}

#[derive(Debug)]
pub(crate) struct BreakNode {
    position: TokenPos,
    break_from: RefCell<Option<BreakableStatement>>,
}

impl Parse for BreakNode {
    fn parse(input: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if input.len() < 2 {
            return None;
        }

        use token::ControlStatementIdentifier::Break;

        match &input[..2] {
            [Token {
                token: WrappedToken::ReservedName(ReservedName::ControlStatement(Break)),
                pos, ..
            },
            Token { token: WrappedToken::Semicolon, .. }] =>
                Some((BreakNode {
                    position: pos.clone(),
                    break_from: RefCell::new(None),
                }, 2)),
            _ => None
        }
    }
}

impl ParseExact for BreakNode {}

#[derive(Debug)]
pub(crate) enum DeclarationNode {
    AutoDeclaration(AutoDeclarationNode),
    ExternDeclaration(Rc<ExternDeclarationNode>),
    LabelDeclaration(Rc<LabelDeclarationNode>),
}

impl DeclarationNode {
    pub(crate) fn get_next_statement(&self) -> Rc<StatementNode> {
        match &self {
            DeclarationNode::AutoDeclaration(decl) =>
                decl.next_statement.clone(),

            DeclarationNode::ExternDeclaration(decl) =>
                decl.next_statement.clone(),

            DeclarationNode::LabelDeclaration(decl) =>
                decl.next_statement.clone(),
        }
    }
}

impl Parse for DeclarationNode {
    fn parse(input: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if let Some(
            (auto_decl, adv)
        ) = AutoDeclarationNode::parse(input) {
            return Some((DeclarationNode::AutoDeclaration(auto_decl), adv));
        }

        if let Some(
            (extern_decl, adv)
        ) = ExternDeclarationNode::parse(input) {
            return Some((DeclarationNode::ExternDeclaration(Rc::new(extern_decl)), adv));
        }

        if let Some(
            (label_decl, adv)
        ) = LabelDeclarationNode::parse(input) {
            return Some((DeclarationNode::LabelDeclaration(Rc::new(label_decl)), adv));
        }

        None
    }
}

#[derive(Debug)]
pub(crate) enum Statement {
    NullStatement,
    Compound(Rc<CompoundStatementNode>),
    Declaration(Rc<DeclarationNode>),
    Return(Rc<ReturnNode>),
    Switch(Rc<SwitchNode>),
    Case(Rc<CaseNode>),
    If(Rc<IfNode>),
    While(Rc<WhileNode>),
    Goto(Rc<GotoNode>),
    RvalueAndSemicolon(Rc<RvalueAndSemicolonNode>),
    Break(Rc<BreakNode>),
}

impl Parse for Statement {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.is_empty() {
            return None;
        }

        if tokens[0].token == WrappedToken::Semicolon {
            return Some((Statement::NullStatement, 1));
        }

        if let Some(
            (rv_and_semicolon, adv)
        ) = RvalueAndSemicolonNode::parse(tokens) {
            return Some((Statement::RvalueAndSemicolon(Rc::new(rv_and_semicolon)), adv));
        }

        if let Some(
            (decl_node, adv)
        ) = DeclarationNode::parse(tokens) {
            return Some((Statement::Declaration(Rc::new(decl_node)), adv));
        }

        if let Some((r#return, adv)) = ReturnNode::parse(tokens) {
            return Some((Statement::Return(Rc::new(r#return)), adv));
        }

        if let Some((r#if, adv)) = IfNode::parse(tokens) {
            return Some((Statement::If(Rc::new(r#if)), adv));
        }

        if let Some((r#break, adv)) = BreakNode::parse(tokens) {
            return Some((Statement::Break(Rc::new(r#break)), adv));
        }

        if let Some((r#while, adv)) = WhileNode::parse(tokens) {
            return Some((Statement::While(Rc::new(r#while)), adv));
        }

        if let Some(
            (comp_stmt, adv)
        ) = CompoundStatementNode::parse(tokens) {
            return Some((Statement::Compound(Rc::new(comp_stmt)), adv));
        }

        if let Some((switch, adv)) = SwitchNode::parse(tokens) {
            return Some((Statement::Switch(Rc::new(switch)), adv));
        }

        if let Some((case, adv)) = CaseNode::parse(tokens) {
            return Some((Statement::Case(Rc::new(case)), adv));
        }

        if let Some((goto, adv)) = GotoNode::parse(tokens) {
            return Some((Statement::Goto(Rc::new(goto)), adv));
        }

        None
    }
}

#[derive(Debug)]
pub(crate) struct StatementNode {
    pub(crate) position: TokenPos,
    pub(crate) statement: Rc<Statement>,
}

impl Parse for StatementNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if tokens.is_empty() {
            return None;
        }

        return if let Some((statement, adv)) = Statement::parse(tokens) {
            Some((StatementNode {
                position: tokens[0].pos.clone(),
                statement: Rc::new(statement),
            }, adv))
        } else {
            None
        };
    }
}

#[derive(Debug)]
pub(crate) enum DefinitionNode {
    Variable(Rc<VariableDefinitionNode>),
    Vector(Rc<VectorDefinitionNode>),
    Function(Rc<FunctionDefinitionNode>),
}

impl DefinitionNode {
    pub(crate) fn get_position(&self) -> TokenPos {
        match self {
            DefinitionNode::Variable(var_def) => var_def.position.clone(),
            DefinitionNode::Vector(vec_def) => vec_def.position.clone(),
            DefinitionNode::Function(fn_def) => fn_def.position.clone(),
        }
    }

    pub(crate) fn get_name(&self) -> Rc<String> {
        match self {
            DefinitionNode::Variable(var_def) =>
                var_def.name.clone(),
            DefinitionNode::Vector(vec_def) =>
                vec_def.name.clone(),
            DefinitionNode::Function(fn_def) =>
                fn_def.name.clone(),
        }
    }
}

impl Parse for DefinitionNode {
    fn parse(input: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if let Some(
            (var_def_node, adv)
        ) = VariableDefinitionNode::parse(input) {
            return Some((DefinitionNode::Variable(Rc::new(var_def_node)), adv));
        }

        if let Some(
            (vec_def_node, adv)
        ) = VectorDefinitionNode::parse(input) {
            return Some((DefinitionNode::Vector(Rc::new(vec_def_node)), adv));
        }

        if let Some(
            (fn_def_node, adv)
        ) = FunctionDefinitionNode::parse(input) {
            return Some((DefinitionNode::Function(Rc::new(fn_def_node)), adv));
        }

        None
    }
}

#[derive(Debug)]
pub(crate) struct ProgramNode {
    definitions: Vec<Rc<DefinitionNode>>,
}

impl ProgramNode {
    pub(crate) fn get_definitions(&self) -> &Vec<Rc<DefinitionNode>> {
        &self.definitions
    }
}

impl From<Vec<Rc<DefinitionNode>>> for ProgramNode {
    fn from(definitions: Vec<Rc<DefinitionNode>>) -> Self {
        ProgramNode { definitions }
    }
}

impl Parse for ProgramNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        let mut defs = Vec::<Rc<DefinitionNode>>::new();
        let mut offset: usize = 0;

        while let Some(
            (def_node, adv)
        ) = DefinitionNode::parse(&tokens[offset..]) {
            offset += adv;
            defs.push(Rc::new(def_node));
        }

        if offset != tokens.len() {
            None
        } else {
            Some((ProgramNode::from(defs), offset))
        }
    }
}

impl ParseExact for ProgramNode {}