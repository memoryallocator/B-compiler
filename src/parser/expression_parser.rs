use std::cell::Cell;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::iter::FromIterator;

use crate::parser;
use crate::tokenizer::token::*;
use crate::utils::Issue;

use parser::ast::*;
use parser::extract_bracketed_expression;
use parser::{Parse, ParseExact};

type PrefixOrPostfix = IncDecType;

#[derive(Debug)]
enum BracketedExpr {
    /// examples: (a), (a + b)
    Round(RvalueNode),
    /// examples: (a, b, c), ()
    FunctionArgumentList(Vec<RvalueNode>),
    Square(RvalueNode),
}

fn op_is_not_unary(op: Operator) -> bool {
    matches!(op, Operator::Binary(_) | Operator::Assign(_))
}

fn op_may_be_both_binary_and_unary(op: Operator) -> bool {
    matches!(
        op,
        Operator::Plus | Operator::Minus | Operator::Asterisk | Operator::Ampersand
    )
}

fn op_may_be_postfix(op: Operator) -> bool {
    matches!(op, Operator::IncDec(_))
}

fn op_may_be_prefix(op: Operator) -> bool {
    matches!(
        op,
        Operator::IncDec(_)
            | Operator::Plus
            | Operator::Minus
            | Operator::Asterisk
            | Operator::Ampersand
            | Operator::Unary(_)
    )
}

fn parse_round_brackets_content(input: &[Token]) -> Result<BracketedExpr, Vec<Issue>> {
    if input.is_empty() {
        // ( )
        return Ok(BracketedExpr::FunctionArgumentList(vec![]));
    }
    let mut arguments = vec![];
    let mut arg_starts_from: usize = 0;
    let mut i: usize = 0;
    while let Some(t) = input.get(i) {
        match t.token {
            WrappedToken::Comma => {
                let arg = RvalueNode::parse_exact(&input[arg_starts_from..i])?;
                arguments.push(arg);
                arg_starts_from = i + 1;
            }
            WrappedToken::Bracket(br) => {
                if br.bracket_type == BracketType::Round {
                    let inner = extract_bracketed_expression(&input[i..])?;
                    let adv = 1 + inner.len() + 1;
                    i += adv;
                    continue;
                }
            }
            _ => (),
        }
        i += 1;
    }
    let last_rvalue = RvalueNode::parse_exact(&input[arg_starts_from..])?;
    if arguments.is_empty() {
        Ok(BracketedExpr::Round(last_rvalue))
    } else {
        arguments.push(last_rvalue);
        Ok(BracketedExpr::FunctionArgumentList(arguments))
    }
}

fn token_to_binary_operator(x: &Token) -> Option<Operator> {
    if let Token {
        token: WrappedToken::Operator(op),
        ..
    } = x
    {
        match op {
            Operator::Plus
            | Operator::Minus
            | Operator::Asterisk
            | Operator::Ampersand
            | Operator::Binary(_) => Some(*op),
            Operator::Assign(_) => None,
            _ => unreachable!(),
        }
    } else {
        None
    }
}

fn apply_last_bin_op(
    ops_and_positions: &mut Vec<(Operator, TokenPos)>,
    operands: &mut Vec<RvalueNode>,
) -> Result<RvalueNode, Vec<Issue>> {
    let (op, pos) = ops_and_positions.pop().unwrap();

    let rhs = operands.pop();
    if rhs.is_none() {
        return Err(vec![Issue::NoOperandForOperator(
            op,
            pos,
            Some(LeftOrRight::Right),
        )]);
    }
    let rhs = rhs.unwrap();

    let lhs = operands.pop();
    if lhs.is_none() {
        return Err(vec![Issue::NoOperandForOperator(
            op,
            pos,
            Some(LeftOrRight::Left),
        )]);
    }
    let lhs = lhs.unwrap();

    Ok(RvalueNode {
        position: pos,
        rvalue: Box::new(Rvalue::Binary {
            lhs,
            bin_op: match op {
                Operator::Plus => RichBinaryOperation::Add,
                Operator::Minus => RichBinaryOperation::Sub,
                Operator::Asterisk => RichBinaryOperation::Mul,
                Operator::Ampersand => RichBinaryOperation::BitwiseAnd,
                Operator::Binary(bin_op) => RichBinaryOperation::RegularBinary(bin_op),
                _ => unreachable!(),
            },
            rhs,
        }),
    })
}

fn free_ops_and_operands_stacks(
    ops: &mut Vec<(Operator, TokenPos)>,
    operands: &mut Vec<RvalueNode>,
) -> Result<RvalueNode, Vec<Issue>> {
    debug_assert!(!ops.is_empty() || !operands.is_empty());

    while ops.last().is_some() {
        let op_applied = apply_last_bin_op(ops, operands)?;
        operands.push(op_applied);
    }

    if operands.len() == 1 {
        Ok(operands.pop().unwrap())
    } else {
        Err(vec![Issue::UnexpectedOperand(operands[1].position)])
    }
}

fn unary_op_can_be_applied(
    op: Operator,
    rvalue_node: &RvalueNode,
    prefix_or_postfix: PrefixOrPostfix,
) -> bool {
    let check = if prefix_or_postfix == PrefixOrPostfix::Postfix {
        op_may_be_postfix
    } else {
        op_may_be_prefix
    };
    if !(check(op)) {
        return false;
    }

    match op {
        Operator::Plus | Operator::Minus | Operator::Asterisk | Operator::Unary(_) => true,
        Operator::Ampersand | Operator::IncDec(_) => {
            matches!(rvalue_node.rvalue.as_ref(), Rvalue::Lvalue(_))
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
    debug_assert!(unary_op_can_be_applied(op, &rvalue_node, prefix_or_postfix));

    let rvalue = match op {
        Operator::Plus => Rvalue::Unary(Unary::Plus, rvalue_node),

        Operator::Minus => Rvalue::Unary(Unary::Minus, rvalue_node),

        Operator::Asterisk => Rvalue::Lvalue(LvalueNode {
            position,
            lvalue: Lvalue::DerefRvalue(rvalue_node),
        }),

        Operator::Ampersand => Rvalue::TakeAddress(LvalueNode::try_from(&rvalue_node).unwrap()),

        Operator::Unary(unary_op) => match unary_op {
            UnaryOperation::LogicalNot => Rvalue::Unary(Unary::LogicalNot, rvalue_node),

            UnaryOperation::Complement => Rvalue::Unary(Unary::Complement, rvalue_node),
        },

        Operator::IncDec(inc_or_dec) => Rvalue::IncDec(IncDecNode {
            inc_or_dec,
            inc_dec_type: prefix_or_postfix,
            lvalue: LvalueNode::try_from(&rvalue_node).unwrap(),
        }),
        _ => unreachable!(),
    };

    RvalueNode {
        position,
        rvalue: Box::new(rvalue),
    }
}

fn try_to_apply_unary_op_to_last_elem_of_vec(
    op_and_pos: (Operator, TokenPos),
    vec: &mut Vec<TokenOrRvalueNode>,
    prefix_or_postfix: PrefixOrPostfix,
) -> Result<RvalueNode, Box<Issue>> {
    let (op, op_pos) = op_and_pos;
    match vec.last() {
        Some(TokenOrRvalueNode::RvalueNode(rvalue_node)) => {
            if unary_op_can_be_applied(op, rvalue_node, prefix_or_postfix) {
                let op_node = apply_unary_op(
                    op_and_pos,
                    if let Some(TokenOrRvalueNode::RvalueNode(rvalue_node)) = vec.pop() {
                        rvalue_node
                    } else {
                        unreachable!()
                    },
                    prefix_or_postfix,
                );
                Ok(op_node)
            } else {
                Err(Issue::OpCannotBeApplied {
                    op_pos,
                    expr_pos: Some(rvalue_node.position),
                }
                .into())
            }
        }
        Some(TokenOrRvalueNode::Token(t)) => Err(Issue::OpCannotBeApplied {
            op_pos,
            expr_pos: Some(t.pos),
        }
        .into()),
        None => Err(Issue::OpCannotBeApplied {
            op_pos,
            expr_pos: None,
        }
        .into()),
    }
}

fn parse_primary_expressions(input: &[Token]) -> Result<Vec<TokenOrRvalueNode>, Vec<Issue>> {
    enum TokenOrPrimaryExpr {
        Token(Token),
        PrimaryExpr(PrimaryExprAndPos),
    }

    impl TokenOrPrimaryExpr {
        fn is_lvalue(&self) -> bool {
            if let TokenOrPrimaryExpr::PrimaryExpr(PrimaryExprAndPos { prim_expr, .. }) = self {
                matches!(
                    **prim_expr,
                    PrimaryExpr::Name(_) | PrimaryExpr::Indexing { .. }
                )
            } else {
                false
            }
        }
    }

    impl TryFrom<TokenOrPrimaryExpr> for LvalueNode {
        type Error = ();

        fn try_from(x: TokenOrPrimaryExpr) -> Result<Self, Self::Error> {
            if let TokenOrPrimaryExpr::PrimaryExpr(PrimaryExprAndPos {
                prim_expr,
                position,
            }) = x
            {
                Ok(match *prim_expr {
                    PrimaryExpr::Name(name) => LvalueNode {
                        lvalue: Lvalue::Name(name),
                        position,
                    },
                    PrimaryExpr::Indexing { vector, index } => LvalueNode {
                        lvalue: Lvalue::Indexing {
                            vector: RvalueNode::from(vector),
                            index,
                        },
                        position,
                    },
                    _ => return Err(()),
                })
            } else {
                Err(())
            }
        }
    }

    impl From<TokenOrPrimaryExpr> for TokenOrRvalueNode {
        fn from(x: TokenOrPrimaryExpr) -> Self {
            if x.is_lvalue() {
                let lvalue_node = LvalueNode::try_from(x).unwrap();

                return TokenOrRvalueNode::RvalueNode(RvalueNode {
                    position: lvalue_node.position,
                    rvalue: Box::new(Rvalue::Lvalue(lvalue_node)),
                });
            }

            match x {
                TokenOrPrimaryExpr::Token(t) => TokenOrRvalueNode::Token(t),

                TokenOrPrimaryExpr::PrimaryExpr(prim_expr_and_pos) => {
                    let rvalue_node = RvalueNode::from(prim_expr_and_pos);
                    TokenOrRvalueNode::RvalueNode(rvalue_node)
                }
            }
        }
    }

    let mut toks_or_prim_or_br_exprs = vec![];

    let mut i: usize = 0;
    while let Some(t) = input.get(i) {
        let pos = t.pos;

        match &t.token {
            WrappedToken::Name(name) => {
                toks_or_prim_or_br_exprs.push(TokenOrPrimaryExpr::PrimaryExpr(PrimaryExprAndPos {
                    prim_expr: Box::new(PrimaryExpr::Name(name.clone())),
                    position: pos,
                }))
            }

            WrappedToken::Constant(constant) => {
                toks_or_prim_or_br_exprs.push(TokenOrPrimaryExpr::PrimaryExpr(PrimaryExprAndPos {
                    prim_expr: Box::new(PrimaryExpr::Constant(constant.clone())),
                    position: pos,
                }))
            }

            WrappedToken::Bracket(br) => {
                if br.bracket_type == BracketType::Curly {
                    return Err(vec![Issue::UnexpectedToken(pos)]);
                }

                let last_prim_expr_and_pos = match toks_or_prim_or_br_exprs.last() {
                    Some(TokenOrPrimaryExpr::PrimaryExpr(_)) => Some(
                        if let TokenOrPrimaryExpr::PrimaryExpr(prim_expr_and_pos) =
                            toks_or_prim_or_br_exprs.pop().unwrap()
                        {
                            prim_expr_and_pos
                        } else {
                            unreachable!()
                        },
                    ),
                    _ => None,
                };

                let (br_expr, adv) = BracketedExpr::parse(&input[i..])?;
                i += adv;

                if let Some(last_prim_expr_and_pos) = last_prim_expr_and_pos {
                    toks_or_prim_or_br_exprs.push(TokenOrPrimaryExpr::PrimaryExpr(
                        PrimaryExprAndPos {
                            prim_expr: match br_expr {
                                BracketedExpr::Round(single_arg) => Box::new(PrimaryExpr::FnCall {
                                    fn_name: last_prim_expr_and_pos.clone(),
                                    arguments: vec![single_arg],
                                }),

                                BracketedExpr::FunctionArgumentList(fn_args) => {
                                    Box::new(PrimaryExpr::FnCall {
                                        fn_name: last_prim_expr_and_pos.clone(),
                                        arguments: fn_args,
                                    })
                                }

                                BracketedExpr::Square(index) => Box::new(PrimaryExpr::Indexing {
                                    vector: last_prim_expr_and_pos.clone(),
                                    index,
                                }),
                            },
                            position: last_prim_expr_and_pos.position,
                        },
                    ));
                    continue;
                }

                if let BracketedExpr::Round(br_rvalue) = br_expr {
                    toks_or_prim_or_br_exprs.push(TokenOrPrimaryExpr::PrimaryExpr(
                        PrimaryExprAndPos {
                            position: br_rvalue.position,
                            prim_expr: Box::new(PrimaryExpr::BracketedRvalue(br_rvalue)),
                        },
                    ));
                } else {
                    // not_a_prim_expr [ ... ] | not_a_prim_expr ( ... , ... )
                    return Err(vec![Issue::ExpectedPrimaryExpr(input[i - adv].pos)]);
                }

                continue;
            }
            WrappedToken::Colon | WrappedToken::QuestionMark | WrappedToken::Operator(_) => {
                toks_or_prim_or_br_exprs.push(TokenOrPrimaryExpr::Token(t.clone()))
            }
            _ => return Err(vec![Issue::UnexpectedToken(t.pos)]),
        }
        i += 1;
    }

    let mut res = vec![];
    for x in toks_or_prim_or_br_exprs {
        res.push(TokenOrRvalueNode::from(x));
    }
    Ok(res)
}

fn parse_unary_operators_and_reverse(
    mut input: Vec<TokenOrRvalueNode>,
) -> Result<Vec<TokenOrRvalueNode>, Vec<Issue>> {
    let mut res_reversed = vec![];
    while let Some(tok_or_rvalue) = input.pop() {
        match &tok_or_rvalue {
            TokenOrRvalueNode::Token(t) => {
                match t.token {
                    WrappedToken::Operator(op) => {
                        if op_is_not_unary(op) {
                            res_reversed.push(tok_or_rvalue);
                            continue;
                        }

                        if op_may_be_both_binary_and_unary(op) {
                            let prev_tok = input.last();
                            match prev_tok {
                                None => (),
                                Some(TokenOrRvalueNode::RvalueNode(_)) => {
                                    res_reversed.push(tok_or_rvalue); // rvalue + ...
                                    continue;
                                }
                                Some(TokenOrRvalueNode::Token(prev_tok)) => {
                                    if let Ok(op) = Operator::try_from(prev_tok) {
                                        if op_may_be_postfix(op) {
                                            // a++ - ...
                                            res_reversed.push(tok_or_rvalue); // - is binary
                                            continue;
                                        }
                                    }
                                }
                            }
                        }

                        let op_node = (|| {
                            let mut errors = vec![];
                            if op_may_be_prefix(op) {
                                let prefix_or_postfix = PrefixOrPostfix::Prefix;
                                match try_to_apply_unary_op_to_last_elem_of_vec(
                                    (op, t.pos),
                                    &mut res_reversed,
                                    prefix_or_postfix,
                                ) {
                                    Err(err) => errors.push(*err),
                                    Ok(op_node) => {
                                        return Ok(op_node);
                                    }
                                }
                            }

                            if op_may_be_postfix(op) {
                                let prefix_or_postfix = PrefixOrPostfix::Postfix;
                                match try_to_apply_unary_op_to_last_elem_of_vec(
                                    (op, t.pos),
                                    &mut input,
                                    prefix_or_postfix,
                                ) {
                                    Err(err) => errors.push(*err),
                                    Ok(op_node) => {
                                        return Ok(op_node);
                                    }
                                }
                            }

                            Err(errors)
                        })();

                        res_reversed.push(TokenOrRvalueNode::RvalueNode(op_node?));
                    }
                    WrappedToken::Colon | WrappedToken::QuestionMark => {
                        res_reversed.push(tok_or_rvalue)
                    }
                    _ => unreachable!(),
                }
            }
            TokenOrRvalueNode::RvalueNode(_) => res_reversed.push(tok_or_rvalue),
        }
    }

    Ok(res_reversed)
}

fn parse_conditional_expressions_and_assignments(
    mut input: Vec<TokenOrRvalueNode>,
) -> Result<RvalueNode, Vec<Issue>> {
    debug_assert!(!input.is_empty());

    enum AssignOrRvalueNode {
        Assign(Assign, TokenPos),
        RvalueNode(RvalueNode),
    }

    fn parse_assignments(
        mut input: VecDeque<AssignOrRvalueNode>,
    ) -> Result<RvalueNode, Vec<Issue>> {
        debug_assert!(!input.is_empty());

        while input.len() > 1 {
            let rhs = match input.pop_back() {
                Some(AssignOrRvalueNode::RvalueNode(rv)) => rv,
                Some(AssignOrRvalueNode::Assign(_, pos)) => {
                    return Err(vec![Issue::UnexpectedToken(pos)]);
                }
                _ => unreachable!(),
            };

            let (assign, assign_pos) = match input.pop_back() {
                Some(AssignOrRvalueNode::Assign(assign, assign_pos)) => (assign, assign_pos),
                Some(AssignOrRvalueNode::RvalueNode(rv)) => {
                    return Err(vec![Issue::ExpectedTokenNotFound(rv.position)]);
                }
                _ => unreachable!(),
            };

            let (lhs_rv, lhs_pos) = match input.pop_back() {
                Some(AssignOrRvalueNode::RvalueNode(RvalueNode {
                    rvalue: lhs_rv,
                    position: lhs_pos,
                })) => (lhs_rv, lhs_pos),
                None => {
                    return Err(vec![Issue::NoOperandForOperator(
                        Operator::Assign(assign),
                        assign_pos,
                        Some(LeftOrRight::Left),
                    )]);
                }
                Some(AssignOrRvalueNode::Assign(_, pos)) => {
                    return Err(vec![Issue::UnexpectedToken(pos)]);
                }
            };

            if let Rvalue::Lvalue(lhs) = *lhs_rv {
                input.push_back(AssignOrRvalueNode::RvalueNode(RvalueNode {
                    position: assign_pos,
                    rvalue: Box::new(Rvalue::Assign { lhs, assign, rhs }),
                }));
            } else {
                return Err(vec![Issue::OpCannotBeApplied {
                    op_pos: assign_pos,
                    expr_pos: Some(lhs_pos),
                }]);
            }
        }

        match input.pop_back().unwrap() {
            AssignOrRvalueNode::RvalueNode(single_rv_node) => Ok(single_rv_node),
            AssignOrRvalueNode::Assign(_, pos) => Err(vec![Issue::UnexpectedToken(pos)]),
        }
    }

    let mut on_false_stack = Vec::<(RvalueNode, TokenPos)>::new();
    let mut buff = Cell::<VecDeque<AssignOrRvalueNode>>::default();

    while let Some(tok_or_rv_node) = input.pop() {
        match tok_or_rv_node {
            TokenOrRvalueNode::Token(Token {
                token: WrappedToken::Operator(Operator::Assign(assign)),
                pos,
                ..
            }) => buff
                .get_mut()
                .push_front(AssignOrRvalueNode::Assign(assign, pos)),

            TokenOrRvalueNode::RvalueNode(rv_node) => buff
                .get_mut()
                .push_front(AssignOrRvalueNode::RvalueNode(rv_node)),

            TokenOrRvalueNode::Token(t) => {
                match t.token {
                    WrappedToken::Colon => {
                        if buff.get_mut().len() != 1 {
                            // let len = buff.get_mut().len();
                            // match len {
                            // 0 => return Err(EmptyOnTrue { question_mark_pos: TokenPos {}, colon_pos: TokenPos {} }),
                            //     _ => (),
                            // }
                            todo!()
                        }

                        let next_rvalue = buff.get_mut().pop_front().unwrap();
                        match next_rvalue {
                            AssignOrRvalueNode::RvalueNode(rv_node) => {
                                on_false_stack.push((rv_node, t.pos));
                            }
                            AssignOrRvalueNode::Assign(_, pos) => {
                                return Err(vec![Issue::UnexpectedToken(pos)]);
                            }
                        }
                    }

                    WrappedToken::QuestionMark => {
                        if input.is_empty() {
                            // ? ... : ...
                            return Err(vec![Issue::NoCondition(t.pos)]);
                        }

                        let on_false_and_colon_pos = on_false_stack.pop();
                        if on_false_and_colon_pos.is_none() {
                            return Err(vec![Issue::NoColonInCond(t.pos)]);
                        }

                        let (on_false, colon_pos) = on_false_and_colon_pos.unwrap();
                        let on_true = parse_assignments(buff.take())?;

                        if let Some(TokenOrRvalueNode::RvalueNode(condition)) = input.pop() {
                            let condition = condition.into_truth_value();
                            input.push(TokenOrRvalueNode::RvalueNode(RvalueNode {
                                position: t.pos,
                                rvalue: Box::new(Rvalue::ConditionalExpression {
                                    condition,
                                    on_true,
                                    on_false,
                                    colon_pos,
                                }),
                            }));
                        }
                    }
                    _ => unreachable!(),
                }
            }
        }
    }

    if !on_false_stack.is_empty() {
        let (_, on_false_pos) = on_false_stack.pop().unwrap();
        return Err(vec![Issue::UnexpectedToken(on_false_pos)]);
    }

    match input.len() {
        0 => parse_assignments(buff.take()),
        1 => match input.pop() {
            Some(TokenOrRvalueNode::RvalueNode(single_rv_node)) => Ok(single_rv_node),
            Some(TokenOrRvalueNode::Token(t)) => Err(vec![Issue::UnexpectedToken(t.pos)]),
            _ => unreachable!(),
        },
        _ => Err(vec![Issue::UnexpectedToken(match input.pop().unwrap() {
            TokenOrRvalueNode::Token(t) => t.pos,
            TokenOrRvalueNode::RvalueNode(rv) => rv.position,
        })]),
    }
}

fn rev_parse_binary_operators_except_assign(
    mut reversed_input: Vec<TokenOrRvalueNode>,
) -> Result<Vec<TokenOrRvalueNode>, Vec<Issue>> {
    let bin_op_priority = HashMap::<Operator, i32>::from_iter(
        {
            fn map_priority(ops: Vec<Operator>, priority: i32) -> Vec<(Operator, i32)> {
                ops.into_iter().map(|op| (op, priority)).collect()
            }

            let mut res = Vec::<(Operator, i32)>::new();

            {
                let multiplicative_ops = map_priority(
                    vec![
                        Operator::Asterisk,
                        Operator::Binary(BinaryOperation::Div),
                        Operator::Binary(BinaryOperation::Mod),
                    ],
                    100,
                );

                res.extend(multiplicative_ops);
            }

            {
                let additive_ops = map_priority(
                    vec![
                        Operator::Plus,
                        Operator::Minus,
                        Operator::Binary(BinaryOperation::Mod),
                    ],
                    90,
                );

                res.extend(additive_ops);
            }

            {
                let shift_ops = map_priority(
                    vec![
                        Operator::Binary(BinaryOperation::Shift(LeftOrRight::Left)),
                        Operator::Binary(BinaryOperation::Shift(LeftOrRight::Right)),
                    ],
                    80,
                );

                res.extend(shift_ops);
            }

            {
                let rel_ops = map_priority(
                    vec![
                        Operator::Binary(BinaryOperation::Cmp(BinaryRelation::Lt)),
                        Operator::Binary(BinaryOperation::Cmp(BinaryRelation::Le)),
                        Operator::Binary(BinaryOperation::Cmp(BinaryRelation::Gt)),
                        Operator::Binary(BinaryOperation::Cmp(BinaryRelation::Ge)),
                    ],
                    70,
                );

                res.extend(rel_ops);
            }

            {
                let eq_ops = map_priority(
                    vec![
                        Operator::Binary(BinaryOperation::Cmp(BinaryRelation::Eq)),
                        Operator::Binary(BinaryOperation::Cmp(BinaryRelation::Ne)),
                    ],
                    60,
                );

                res.extend(eq_ops);
            }

            {
                let and_op = (Operator::Ampersand, 50);
                res.push(and_op);
            }

            {
                let xor_op = (Operator::Binary(BinaryOperation::Xor), 40);
                res.push(xor_op);
            }

            {
                let or_op = (Operator::Binary(BinaryOperation::Or), 30);
                res.push(or_op);
            }

            res
        }
        .into_iter(),
    );

    let mut bin_ops_and_positions = vec![];
    let mut operands = vec![];
    let mut res = vec![];

    while let Some(t_or_rv) = reversed_input.pop() {
        match t_or_rv {
            TokenOrRvalueNode::Token(t) => {
                let op = token_to_binary_operator(&t);

                if let Some(op) = op {
                    bin_ops_and_positions.push((op, t.pos));
                } else {
                    let ops_applied =
                        free_ops_and_operands_stacks(&mut bin_ops_and_positions, &mut operands)?;
                    res.push(TokenOrRvalueNode::RvalueNode(ops_applied));

                    res.push(TokenOrRvalueNode::Token(t));
                }
            }

            TokenOrRvalueNode::RvalueNode(rv_node) => {
                operands.push(rv_node);

                let next_op =
                    if let Some(TokenOrRvalueNode::Token(next_tok)) = reversed_input.last() {
                        token_to_binary_operator(next_tok)
                    } else {
                        None
                    };

                while let Some((curr_op, _)) = bin_ops_and_positions.last() {
                    if let Some(next_op) = next_op {
                        if bin_op_priority.get(&next_op) > bin_op_priority.get(curr_op) {
                            break;
                        } else {
                            let op_applied =
                                apply_last_bin_op(&mut bin_ops_and_positions, &mut operands)?;
                            operands.push(op_applied);
                        }
                    } else {
                        break;
                    }
                }
            }
        }
    }

    let ops_applied = free_ops_and_operands_stacks(&mut bin_ops_and_positions, &mut operands)?;
    res.push(TokenOrRvalueNode::RvalueNode(ops_applied));

    Ok(res)
}

impl Parse for BracketedExpr {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
    where
        Self: Sized,
    {
        debug_assert!(!input.is_empty());
        let pos = input[0].pos;
        return if let WrappedToken::Bracket(br) = input[0].token {
            let expr = extract_bracketed_expression(input)?;
            let adv = 1 + expr.len() + 1;

            match br.bracket_type {
                BracketType::Curly => Err(vec![Issue::UnexpectedToken(pos)]),
                BracketType::Round => {
                    let node = parse_round_brackets_content(expr)?;
                    Ok((node, adv))
                }
                BracketType::Square => {
                    let node = RvalueNode::parse_exact(expr)?;
                    Ok((BracketedExpr::Square(node), adv))
                }
            }
        } else {
            Err(vec![Issue::UnexpectedToken(pos)])
        };
    }
}

#[derive(Debug)]
enum TokenOrRvalueNode {
    Token(Token),
    RvalueNode(RvalueNode),
}

#[derive(Debug, Clone)]
enum PrimaryExpr {
    Name(String),
    Constant(Constant),
    BracketedRvalue(RvalueNode),
    Indexing {
        vector: PrimaryExprAndPos,
        index: RvalueNode,
    },
    FnCall {
        fn_name: PrimaryExprAndPos,
        arguments: Vec<RvalueNode>,
    },
}

#[derive(Debug, Clone)]
struct PrimaryExprAndPos {
    prim_expr: Box<PrimaryExpr>,
    position: TokenPos,
}

impl From<PrimaryExprAndPos> for RvalueNode {
    fn from(x: PrimaryExprAndPos) -> Self {
        let position = x.position;

        match *x.prim_expr {
            PrimaryExpr::BracketedRvalue(br_rvalue) => RvalueNode {
                position,
                rvalue: Box::new(Rvalue::BracketedExpression(br_rvalue)),
            },

            PrimaryExpr::Constant(constant) => RvalueNode {
                position,
                rvalue: Box::new(Rvalue::Constant(ConstantNode { constant, position })),
            },

            PrimaryExpr::FnCall { fn_name, arguments } => RvalueNode {
                position,
                rvalue: Box::new(Rvalue::FunctionCall(FunctionCallNode {
                    fn_name: RvalueNode::from(fn_name),
                    arguments,
                })),
            },

            PrimaryExpr::Name(name) => RvalueNode {
                position,
                rvalue: Box::new(Rvalue::Lvalue(LvalueNode {
                    position,
                    lvalue: Lvalue::Name(name),
                })),
            },

            PrimaryExpr::Indexing { vector, index } => RvalueNode {
                position,
                rvalue: Box::new(Rvalue::Lvalue(LvalueNode {
                    position,
                    lvalue: Lvalue::Indexing {
                        vector: RvalueNode::from(vector),
                        index,
                    },
                })),
            },
        }
    }
}

impl Parse for RvalueNode {
    fn parse(input: &[Token]) -> Result<(Self, usize), Vec<Issue>>
    where
        Self: Sized,
    {
        debug_assert!(!input.is_empty());
        let res = parse_primary_expressions(input)?;
        let res = parse_unary_operators_and_reverse(res)?;
        let res = rev_parse_binary_operators_except_assign(res)?;
        let res = parse_conditional_expressions_and_assignments(res)?;
        Ok((res, input.len()))
    }
}
