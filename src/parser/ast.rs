use core::slice::Iter;
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

#[derive(Debug)]
struct Constant {
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
        use token::{LEFT_ROUND_BRACKET, RIGHT_ROUND_BRACKET};

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
    declarations: Vec<AutoDeclaration>,
    next_statement: Box<StatementNode>,
}

impl Parse for AutoDeclarationNode {
    fn parse(tokens: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        use token::DeclarationSpecifier::Auto;

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
        use token::DeclarationSpecifier::Extrn;

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

#[derive(Debug)]
enum Lvalue {
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

#[derive(Debug)]
struct OperatorNode {}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum IncDecType {
    Prefix,
    Postfix,
}

#[derive(Debug)]
enum IncDec {
    Increment,
    Decrement,
}

#[derive(Debug)]
struct IncDecNode {
    inc_or_dec: IncDec,
    inc_dec_type: IncDecType,
    lvalue: Lvalue,
}

#[derive(Debug)]
enum Unary {
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

#[derive(Debug)]
enum PrimaryExpression {
    Name(String),
    Constant(Constant),
    BracketedRvalue(Box<RvalueNode>),
    Indexing { vector: Box<PrimaryExpressionAndPos>, index: Box<RvalueNode> },
    FnCall { fn_name: Box<PrimaryExpressionAndPos>, arguments: Vec<Box<RvalueNode>> },
}

#[derive(Debug)]
struct PrimaryExpressionAndPos {
    prim_expr: PrimaryExpression,
    position: TokenPos,
}

#[derive(Debug)]
enum Rvalue {
    Constant(Constant),
    Lvalue(Lvalue),
    Assign { lhs: Lvalue, op: token::Assign, rhs: Box<RvalueNode> },
    IncDec(IncDecNode),
    Unary(Unary, Box<RvalueNode>),
    TakeAddress(Lvalue),
    Binary { lhs: Box<RvalueNode>, op: token::RichBinaryOperation, rhs: Box<RvalueNode> },
    ConditionalExpression {
        condition: Box<RvalueNode>,
        on_true: Box<RvalueNode>,
        on_false: Box<RvalueNode>,
    },
    BracketedExpression(Box<RvalueNode>),
    FunctionCall { fn_name: Box<RvalueNode>, arguments: Vec<Box<RvalueNode>> },
}

#[derive(Debug)]
struct RvalueNode {
    position: TokenPos,
    rvalue: Rvalue,
}

impl RvalueNode {
    fn to_truth_value(&self) -> Option<RvalueNode> {
        todo!()
    }

    fn is_lvalue(&self) -> bool {
        if let RvalueNode { rvalue: Rvalue::Lvalue(_), .. } = self {
            true
        } else {
            false
        }
    }
}

impl From<PrimaryExpressionAndPos> for RvalueNode {
    fn from(x: PrimaryExpressionAndPos) -> Self {
        // fn extract_rvalue_node(x: PrimaryExpressionAndPos) -> Option<RvalueNode {
        //     let rvalue_node= RvalueNode::from(x);
        //     if let Some(rvalue_node) = rvalue_node {
        //         Some(rvalue_node)
        //     } else {
        //         None
        //     }
        // }

        let position = x.position;
        match x.prim_expr {
            PrimaryExpression::BracketedRvalue(br_rvalue) => {
                RvalueNode {
                    position,
                    rvalue: Rvalue::BracketedExpression(br_rvalue),
                }
            }
            PrimaryExpression::Constant(constant) =>
                RvalueNode {
                    position,
                    rvalue: Rvalue::Constant(constant),
                },
            // PrimaryExpression::Indexing { vector, index } =>
            // Ok(RvalueNode {
            //     position,
            //     rvalue: Rvalue::Indexing {
            //         vector: Box::new({
            //             if let Some(rvalue_node) = extract_rvalue_node(*vector) {
            //                 rvalue_node
            //             } else {
            //                 return Err(());
            //             }
            //         }),
            //         index,
            //     },
            // })
            // ,
            PrimaryExpression::FnCall { fn_name, arguments } =>
                RvalueNode {
                    position,
                    rvalue: Rvalue::FunctionCall {
                        fn_name: Box::new(RvalueNode::from(*fn_name)),
                        arguments,
                    },
                },
            _ => unreachable!(),
        }
    }
}

impl Parse for RvalueNode {
    fn parse(input: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if input.is_empty() {
            return None;
        }

        fn parse_primary_expressions(input: &[Token]) -> Option<Vec<TokenOrRvalueNode>> {
            enum TokenOrPrimaryOrBracketedExpression {
                Token(Token),
                PrimaryExpression(PrimaryExpressionAndPos),
                FunctionArgumentList(Vec<RvalueNode>),
                SquareBracketedExpression(RvalueNode),
            }

            impl TokenOrPrimaryOrBracketedExpression {
                fn is_lvalue(&self) -> bool {
                    return if let TokenOrPrimaryOrBracketedExpression::PrimaryExpression(
                        PrimaryExpressionAndPos { prim_expr, position }
                    ) = self {
                        match prim_expr {
                            PrimaryExpression::Name(_) => true,
                            PrimaryExpression::Indexing { .. } => true,
                            _ => false
                        }
                    } else {
                        false
                    };
                }
            }

            struct LvalueAndPos {
                lvalue: Lvalue,
                pos: TokenPos,
            }

            impl TryFrom<TokenOrPrimaryOrBracketedExpression> for LvalueAndPos {
                type Error = ();

                fn try_from(x: TokenOrPrimaryOrBracketedExpression) -> Result<Self, Self::Error> {
                    return if let TokenOrPrimaryOrBracketedExpression::PrimaryExpression(
                        PrimaryExpressionAndPos { prim_expr, position }
                    ) = x {
                        Ok(match prim_expr {
                            PrimaryExpression::Name(name) =>
                                LvalueAndPos {
                                    lvalue: Lvalue::Name(name),
                                    pos: position,
                                },
                            PrimaryExpression::Indexing { vector, index } =>
                                LvalueAndPos {
                                    lvalue: Lvalue::Indexing {
                                        vector: Box::new(RvalueNode::try_from(*vector).unwrap()),
                                        index,
                                    },
                                    pos: position,
                                },
                            _ => return Err(())
                        })
                    } else {
                        Err(())
                    };
                }
            }

            impl TryFrom<TokenOrPrimaryOrBracketedExpression> for TokenOrRvalueNode {
                type Error = ();

                fn try_from(
                    x: TokenOrPrimaryOrBracketedExpression
                ) -> Result<Self, Self::Error> {
                    if x.is_lvalue() {
                        let lvalue_and_pos = LvalueAndPos::try_from(x).unwrap();
                        return Ok(TokenOrRvalueNode::RvalueNode(
                            RvalueNode {
                                rvalue: Rvalue::Lvalue(lvalue_and_pos.lvalue),
                                position: lvalue_and_pos.pos,
                            }
                        ));
                    }

                    match x {
                        TokenOrPrimaryOrBracketedExpression::Token(t) =>
                            Ok(TokenOrRvalueNode::Token(t)),
                        TokenOrPrimaryOrBracketedExpression::PrimaryExpression(prim_expr_and_pos) => {
                            if let Some(rvalue_node) = RvalueNode::try_from(prim_expr_and_pos).ok() {
                                Ok(TokenOrRvalueNode::RvalueNode(rvalue_node))
                            } else {
                                return Err(());
                            }
                        }
                        _ => return Err(())
                    }
                }
            }

            let mut toks_or_prim_or_br_exprs
                = Vec::<TokenOrPrimaryOrBracketedExpression>::new();

            let mut i: usize = 0;
            while let Some(t) = input.get(i) {
                let pos = t.pos;
                match t.r#type {
                    TokenType::Name =>
                        toks_or_prim_or_br_exprs.push(
                            TokenOrPrimaryOrBracketedExpression::PrimaryExpression(
                                PrimaryExpressionAndPos {
                                    prim_expr: PrimaryExpression::Name(t.val.as_ref().unwrap().clone()),
                                    position: pos,
                                })),
                    TokenType::Constant(_) =>
                        toks_or_prim_or_br_exprs.push(
                            TokenOrPrimaryOrBracketedExpression::PrimaryExpression(
                                PrimaryExpressionAndPos {
                                    prim_expr: PrimaryExpression::Constant(
                                        Constant::try_from(&input[i]).ok()?),
                                    position: pos,
                                })),
                    TokenType::Bracket(br) => {
                        if br.bracket_type == token::BracketType::Curly {
                            return None;
                        }

                        fn extract_primary_expression_and_pos(x: TokenOrPrimaryOrBracketedExpression)
                                                              -> Option<PrimaryExpressionAndPos> {
                            if let TokenOrPrimaryOrBracketedExpression::PrimaryExpression(
                                prim_expr_and_pos
                            ) = x {
                                Some(prim_expr_and_pos)
                            } else {
                                None
                            }
                        }

                        let last_prim_expr_and_pos =
                            if toks_or_prim_or_br_exprs.is_empty() {
                                None
                            } else {
                                extract_primary_expression_and_pos(toks_or_prim_or_br_exprs.pop()?)
                            };

                        let (br_expr, adv) = BracketedExpression::parse(&input[i..])?;
                        i += adv;

                        if let Some(last_prim_expr_and_pos) = last_prim_expr_and_pos {
                            toks_or_prim_or_br_exprs.push(TokenOrPrimaryOrBracketedExpression::PrimaryExpression(
                                PrimaryExpressionAndPos {
                                    position: last_prim_expr_and_pos.position,
                                    prim_expr: match br_expr {
                                        BracketedExpression::RoundBracketedExpression(single_arg) =>
                                            PrimaryExpression::FnCall {
                                                fn_name: Box::new(last_prim_expr_and_pos),
                                                arguments: vec![Box::new(single_arg)],
                                            },
                                        BracketedExpression::FunctionArgumentList(fn_args) =>
                                            PrimaryExpression::FnCall {
                                                fn_name: Box::new(last_prim_expr_and_pos),
                                                arguments: fn_args.into_iter()
                                                    .map(|x| Box::new(x))
                                                    .collect(),
                                            },
                                        BracketedExpression::SquareBracketedExpression(index) =>
                                            PrimaryExpression::Indexing {
                                                vector: Box::new(last_prim_expr_and_pos),
                                                index: Box::new(index),
                                            }
                                    },
                                }
                            ));
                            continue;
                        }

                        if let BracketedExpression::RoundBracketedExpression(br_rvalue) = br_expr {
                            toks_or_prim_or_br_exprs.push(
                                TokenOrPrimaryOrBracketedExpression::PrimaryExpression(
                                    PrimaryExpressionAndPos {
                                        position: br_rvalue.position,
                                        prim_expr: PrimaryExpression::BracketedRvalue(
                                            Box::new(br_rvalue)),
                                    }
                                )
                            );
                        } else {  // not_a_prim_expr [ ... ] | not_a_prim_expr ( ... , ... )
                            return None;
                        }

                        continue;
                    }
                    TokenType::Colon | TokenType::QuestionMark | TokenType::Operator(_) =>
                        toks_or_prim_or_br_exprs.push(
                            TokenOrPrimaryOrBracketedExpression::Token(t.clone())),
                    _ => return None,
                }
                i += 1;
            }

            let mut res = Vec::<TokenOrRvalueNode>::new();
            for x in toks_or_prim_or_br_exprs {
                res.push(if let Ok(x) = TokenOrRvalueNode::try_from(x) {
                    x
                } else {
                    return None;
                });
            }
            Some(res)
        }

        let res = parse_primary_expressions(input)?;

        fn parse_unary_operators(mut input: Vec<TokenOrRvalueNode>) -> Option<Vec<TokenOrRvalueNode>> {
            use token::Operator;

            fn op_may_be_both_binary_and_unary(op: Operator) -> bool {
                match op {
                    Operator::Plus | Operator::Minus
                    | Operator::Asterisk | Operator::Ampersand => true,
                    _ => false
                }
            }

            fn op_may_be_postfix(op: Operator) -> bool {
                match op {
                    Operator::Inc | Operator::Dec => true,
                    _ => false
                }
            }

            fn op_may_be_prefix(op: Operator) -> bool {
                match op {
                    Operator::Inc | Operator::Dec
                    | Operator::Plus | Operator::Minus
                    | Operator::Asterisk | Operator::Ampersand | Operator::Unary(_) => true,

                    _ => false
                }
            }

            type PrefixOrPostfix = IncDecType;

            fn unary_op_can_be_applied(
                op: Operator,
                rvalue_node: &RvalueNode,
                prefix_or_postfix: PrefixOrPostfix,
            ) -> bool {
                if !(
                    if prefix_or_postfix == PrefixOrPostfix::Postfix {
                        op_may_be_postfix
                    } else {
                        op_may_be_prefix
                    }(op)) {
                    return false;
                }

                match op {
                    Operator::Plus | Operator::Minus
                    | Operator::Asterisk | Operator::Unary(_) => true,

                    Operator::Ampersand | Operator::Inc | Operator::Dec => {
                        if let RvalueNode { rvalue: Rvalue::Lvalue(_), .. } = rvalue_node {
                            true
                        } else {
                            false
                        }
                    }

                    Operator::Binary(_) | Operator::Assign(_) => false,
                }
            }

            fn apply_unary_op(
                op_and_pos: (Operator, TokenPos),
                rvalue_node: RvalueNode,
                prefix_or_postfix: PrefixOrPostfix,
            ) -> RvalueNode {
                let (op, position) = op_and_pos;
                assert!(unary_op_can_be_applied(op, &rvalue_node, prefix_or_postfix));

                use token::UnaryOperation;

                match op {
                    Operator::Plus =>
                        RvalueNode {
                            rvalue: Rvalue::Unary(Unary::Plus, Box::new(rvalue_node)),
                            position,
                        },
                    Operator::Minus =>
                        RvalueNode {
                            rvalue: Rvalue::Unary(Unary::Minus, Box::new(rvalue_node)),
                            position,
                        },
                    Operator::Asterisk =>
                        RvalueNode {
                            rvalue: Rvalue::Lvalue(
                                Lvalue::DerefRvalue(Box::new(rvalue_node))
                            ),
                            position,
                        },
                    Operator::Ampersand =>
                        RvalueNode {
                            rvalue: Rvalue::TakeAddress(Lvalue::try_from(rvalue_node).unwrap()),
                            position,
                        },
                    Operator::Unary(unary_op) =>
                        match unary_op {
                            UnaryOperation::LogicalNot =>
                                RvalueNode {
                                    rvalue: Rvalue::Unary(Unary::LogicalNot, Box::new(rvalue_node)),
                                    position,
                                },
                            UnaryOperation::Complement =>
                                RvalueNode {
                                    rvalue: Rvalue::Unary(Unary::Complement, Box::new(rvalue_node)),
                                    position,
                                }
                        },
                    Operator::Inc =>
                        RvalueNode {
                            rvalue: Rvalue::IncDec(IncDecNode {
                                inc_or_dec: IncDec::Increment,
                                inc_dec_type: prefix_or_postfix,
                                lvalue: Lvalue::try_from(rvalue_node).unwrap(),
                            }),
                            position,
                        },
                    Operator::Dec => RvalueNode {
                        rvalue: Rvalue::IncDec(IncDecNode {
                            inc_or_dec: IncDec::Decrement,
                            inc_dec_type: prefix_or_postfix,
                            lvalue: Lvalue::try_from(rvalue_node).unwrap(),
                        }),
                        position,
                    },
                    _ => unreachable!()
                }
            }

            fn try_applying_unary_op_to_last_elem_of_vec(
                op_and_pos: (Operator, TokenPos),
                vec: &mut Vec<TokenOrRvalueNode>,
                prefix_or_postfix: PrefixOrPostfix,
            ) -> Option<RvalueNode> {
                let (op, pos) = op_and_pos;
                if let Some(
                    TokenOrRvalueNode::RvalueNode(rvalue_node)
                ) = vec.last() {
                    if unary_op_can_be_applied(op,
                                               rvalue_node,
                                               prefix_or_postfix) {
                        let op_node = apply_unary_op(
                            op_and_pos,
                            if let Some(
                                TokenOrRvalueNode::RvalueNode(rvalue_node)
                            ) = vec.pop() {
                                rvalue_node
                            } else {
                                unreachable!()
                            }, prefix_or_postfix);
                        return Some(op_node);
                    }
                }
                None
            }

            let mut res = Vec::<TokenOrRvalueNode>::new();
            while let Some(tok_or_rvalue) = input.pop() {
                match &tok_or_rvalue {
                    TokenOrRvalueNode::Token(t) => {
                        match t.r#type {
                            TokenType::Operator(op) => {
                                if op_may_be_both_binary_and_unary(op) {
                                    let prev_tok = input.last();
                                    match prev_tok {
                                        None => (),
                                        Some(TokenOrRvalueNode::RvalueNode(_)) => {
                                            res.push(tok_or_rvalue);  // rvalue + ...
                                            continue;
                                        }
                                        Some(TokenOrRvalueNode::Token(prev_tok)) => {
                                            if let Ok(op) = Operator::try_from(prev_tok) {
                                                if op_may_be_postfix(op) {  // a++ - ...
                                                    res.push(tok_or_rvalue);  // - is binary
                                                    continue;
                                                }
                                            }
                                        }
                                    }
                                }

                                let op_node = (|| {
                                    if op_may_be_prefix(op) {
                                        let prefix_or_postfix = PrefixOrPostfix::Prefix;
                                        if let Some(
                                            op_node
                                        ) = try_applying_unary_op_to_last_elem_of_vec((op, t.pos),
                                                                                      &mut res,
                                                                                      prefix_or_postfix) {
                                            return Some(op_node);
                                        }
                                    }

                                    if op_may_be_postfix(op) {
                                        let prefix_or_postfix = PrefixOrPostfix::Postfix;
                                        if let Some(
                                            op_node
                                        ) = try_applying_unary_op_to_last_elem_of_vec((op, t.pos),
                                                                                      &mut input,
                                                                                      prefix_or_postfix) {
                                            return Some(op_node);
                                        }
                                    }

                                    None
                                })();

                                res.push(TokenOrRvalueNode::RvalueNode(op_node?));
                            }
                            TokenType::Colon | TokenType::QuestionMark => res.push(tok_or_rvalue),
                            _ => unreachable!()
                        }
                    }
                    TokenOrRvalueNode::RvalueNode(_) => res.push(tok_or_rvalue),
                }
            }

            res.reverse();
            Some(res)
        }

        let res = parse_unary_operators(res);
        dbg!(&res);
        todo!()
    }
}

impl ParseExact for RvalueNode {}

#[derive(Debug)]
enum BracketedExpression {
    RoundBracketedExpression(RvalueNode),
    // examples: (a), (a + b)
    FunctionArgumentList(Vec<RvalueNode>),
    // examples: (a, b, c), ()
    SquareBracketedExpression(RvalueNode),
}

impl Parse for BracketedExpression {
    fn parse(input: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if input.is_empty() {
            return None;
        }

        fn parse_round_brackets_content(input: &[Token]) -> Option<BracketedExpression> {
            if input.is_empty() {  // ( )
                return Some(BracketedExpression::FunctionArgumentList(vec![]));
            }

            let mut arguments = Vec::<RvalueNode>::new();
            let mut next_arg_idx: usize = 0;

            for (i, t) in input.into_iter().enumerate() {
                if t.r#type == TokenType::Comma {
                    let arg = RvalueNode::parse_exact(&input[next_arg_idx..i])?;
                    arguments.push(arg);
                    next_arg_idx = i + 1;
                }
            }

            if arguments.is_empty() {
                let rvalue_node = RvalueNode::parse_exact(&input)?;
                return Some(BracketedExpression::RoundBracketedExpression(rvalue_node));
            }

            return Some(BracketedExpression::FunctionArgumentList(arguments));
        }

        return if let TokenType::Bracket(br) = input[0].r#type {
            let expr = extract_bracketed_expression(input)?;
            let adv = 1 + expr.len() + 1;

            use token::BracketType::*;
            match br.bracket_type {
                Round => {
                    let node = parse_round_brackets_content(expr)?;
                    Some((node, adv))
                }
                Square => {
                    let node = RvalueNode::parse_exact(expr)?;
                    Some((BracketedExpression::SquareBracketedExpression(node), adv))
                }
                Curly => None,
            }
        } else {
            None
        };
    }
}

#[derive(Debug)]
enum TokenOrRvalueNode {
    Token(Token),
    RvalueNode(RvalueNode),
}

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
        use token::ControlStatementIdentifier::{If, Else};
        use token::BracketType::Round;

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
    use token::Bracket;

    const LEFT: token::LeftOrRight = token::LeftOrRight::Left;

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
        use token::BracketType::Round;
        use token::ControlStatementIdentifier::Return;

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