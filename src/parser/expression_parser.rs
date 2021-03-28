use std::collections::VecDeque;
use std::convert::TryFrom;

use token::{Token, TokenType};

use crate::lexical_analyzer::TokenPos;
use crate::token;

use super::ast::*;

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

            let last_rvalue = RvalueNode::parse_exact(&input[next_arg_idx..])?;
            return if arguments.is_empty() {
                Some(BracketedExpression::RoundBracketedExpression(last_rvalue))
            } else {
                arguments.push(last_rvalue);
                Some(BracketedExpression::FunctionArgumentList(arguments))
            };
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

impl From<PrimaryExpressionAndPos> for RvalueNode {
    fn from(x: PrimaryExpressionAndPos) -> Self {
        let position = x.position;
        match x.prim_expr {
            PrimaryExpression::BracketedRvalue(br_rvalue) =>
                RvalueNode::from((position, Rvalue::BracketedExpression(br_rvalue))),

            PrimaryExpression::Constant(constant) =>
                RvalueNode::from((position, Rvalue::Constant(constant))),

            PrimaryExpression::FnCall { fn_name, arguments } =>
                RvalueNode::from((position, Rvalue::FunctionCall {
                    fn_name: Box::new(RvalueNode::from(*fn_name)),
                    arguments,
                })),

            PrimaryExpression::Name(name) => {
                RvalueNode::from((position, Rvalue::Lvalue(Lvalue::Name(name))))
            }

            PrimaryExpression::Indexing { vector, index } => {
                RvalueNode::from((position, Rvalue::Lvalue(Lvalue::Indexing {
                    vector: Box::new(RvalueNode::from(*vector)),
                    index,
                })))
            }
        }
    }
}

impl Parse for RvalueNode {
    fn parse(input: &[Token]) -> Option<(Self, usize)>
        where Self: Sized {
        if input.is_empty() {
            return None;
        }

        use token::Operator;

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
                            RvalueNode::from((lvalue_and_pos.pos,
                                              Rvalue::Lvalue(lvalue_and_pos.lvalue)))
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

                        let last_prim_expr_and_pos =
                            match toks_or_prim_or_br_exprs.last() {
                                Some(TokenOrPrimaryOrBracketedExpression::PrimaryExpression(_)) =>
                                    Some(
                                        if let TokenOrPrimaryOrBracketedExpression::PrimaryExpression(
                                            prim_expr_and_pos
                                        ) = toks_or_prim_or_br_exprs.pop()? {
                                            prim_expr_and_pos
                                        } else {
                                            unreachable!()
                                        }
                                    ),
                                _ => None,
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
                                        position: br_rvalue.get_position(),
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

        fn parse_unary_operators_and_reverse(
            mut input: Vec<TokenOrRvalueNode>
        ) -> Option<Vec<TokenOrRvalueNode>> {
            fn op_is_not_unary(op: Operator) -> bool {
                match op {
                    Operator::Binary(_) | Operator::Assign(_) => true,
                    _ => false
                }
            }

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
                        RvalueNode::from((position, Rvalue::Unary(Unary::Plus,
                                                                  Box::new(rvalue_node)))),
                    Operator::Minus =>
                        RvalueNode::from((position, Rvalue::Unary(Unary::Minus,
                                                                  Box::new(rvalue_node)))),
                    Operator::Asterisk =>
                        RvalueNode::from((position, Rvalue::Lvalue(
                            Lvalue::DerefRvalue(Box::new(rvalue_node))
                        ))),
                    Operator::Ampersand =>
                        RvalueNode::from((position, Rvalue::TakeAddress(
                            Lvalue::try_from(rvalue_node).unwrap()
                        ))),
                    Operator::Unary(unary_op) =>
                        match unary_op {
                            UnaryOperation::LogicalNot =>
                                RvalueNode::from((position,
                                                  Rvalue::Unary(Unary::LogicalNot,
                                                                Box::new(rvalue_node)))),
                            UnaryOperation::Complement =>
                                RvalueNode::from((position,
                                                  Rvalue::Unary(Unary::Complement,
                                                                Box::new(rvalue_node)))),
                        },
                    Operator::Inc =>
                        RvalueNode::from((position,
                                          Rvalue::IncDec(
                                              IncDecNode::from(
                                                  (IncDec::Increment,
                                                   prefix_or_postfix,
                                                   Lvalue::try_from(rvalue_node).unwrap())
                                              ))
                        )),
                    Operator::Dec =>
                        RvalueNode::from((position,
                                          Rvalue::IncDec(
                                              IncDecNode::from(
                                                  (IncDec::Decrement,
                                                   prefix_or_postfix,
                                                   Lvalue::try_from(rvalue_node).unwrap())
                                              ))
                        )),
                    _ => unreachable!()
                }
            }

            fn try_to_apply_unary_op_to_last_elem_of_vec(
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

            let mut res_reversed = Vec::<TokenOrRvalueNode>::new();
            while let Some(tok_or_rvalue) = input.pop() {
                match &tok_or_rvalue {
                    TokenOrRvalueNode::Token(t) => {
                        match t.r#type {
                            TokenType::Operator(op) => {
                                if op_is_not_unary(op) {
                                    res_reversed.push(tok_or_rvalue);
                                    continue;
                                }

                                if op_may_be_both_binary_and_unary(op) {
                                    let prev_tok = input.last();
                                    match prev_tok {
                                        None => (),
                                        Some(TokenOrRvalueNode::RvalueNode(_)) => {
                                            res_reversed.push(tok_or_rvalue);  // rvalue + ...
                                            continue;
                                        }
                                        Some(TokenOrRvalueNode::Token(prev_tok)) => {
                                            if let Ok(op) = Operator::try_from(prev_tok) {
                                                if op_may_be_postfix(op) {  // a++ - ...
                                                    res_reversed.push(tok_or_rvalue);  // - is binary
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
                                        ) = try_to_apply_unary_op_to_last_elem_of_vec((op, t.pos),
                                                                                      &mut res_reversed,
                                                                                      prefix_or_postfix) {
                                            return Some(op_node);
                                        }
                                    }

                                    if op_may_be_postfix(op) {
                                        let prefix_or_postfix = PrefixOrPostfix::Postfix;
                                        if let Some(
                                            op_node
                                        ) = try_to_apply_unary_op_to_last_elem_of_vec((op, t.pos),
                                                                                      &mut input,
                                                                                      prefix_or_postfix) {
                                            return Some(op_node);
                                        }
                                    }

                                    None
                                })();

                                res_reversed.push(TokenOrRvalueNode::RvalueNode(op_node?));
                            }
                            TokenType::Colon | TokenType::QuestionMark => res_reversed.push(tok_or_rvalue),
                            _ => unreachable!()
                        }
                    }
                    TokenOrRvalueNode::RvalueNode(_) => res_reversed.push(tok_or_rvalue),
                }
            }

            Some(res_reversed)
        }

        let res = parse_unary_operators_and_reverse(res)?;

        fn rev_parse_binary_operators_except_assign(
            mut reversed_input: Vec<TokenOrRvalueNode>
        ) -> Option<Vec<TokenOrRvalueNode>> {
            use std::collections::HashMap;

            use Operator::*;
            use token::BinaryRelation::*;
            use token::BinaryOperation::*;
            use token::RichBinaryOperation::*;
            use token::LeftOrRight::*;

            let bin_op_priority = HashMap::<Operator, i32>::from((|| {
                fn map_priority(ops: Vec<Operator>, priority: i32) -> Vec<(Operator, i32)> {
                    ops.into_iter()
                        .map(|op| (op, priority))
                        .collect()
                }

                let mut res = Vec::<(Operator, i32)>::new();

                {
                    let mut multiplicative_ops = map_priority(
                        vec![Asterisk,
                             Binary(Div),
                             Binary(Mod)], 100);

                    res.append(&mut multiplicative_ops);
                }

                {
                    let mut additive_ops = map_priority(
                        vec![Plus,
                             Minus,
                             Binary(Mod)], 90);

                    res.append(&mut additive_ops);
                }

                {
                    let mut shift_ops = map_priority(
                        vec![Binary(Shift(Left)),
                             Binary(Shift(Right))], 80);

                    res.append(&mut shift_ops);
                }

                {
                    let mut rel_ops = map_priority(
                        vec![Binary(Cmp(Lt)),
                             Binary(Cmp(Le)),
                             Binary(Cmp(Gt)),
                             Binary(Cmp(Ge))], 70);

                    res.append(&mut rel_ops);
                }

                {
                    let mut eq_ops = map_priority(
                        vec![Binary(Cmp(Eq)),
                             Binary(Cmp(Ne))], 60);

                    res.append(&mut eq_ops);
                }

                {
                    let and_op = (Ampersand, 50);
                    res.push(and_op);
                }


                {
                    let xor_op = (Binary(Xor), 40);
                    res.push(xor_op);
                }


                {
                    let or_op = (Binary(Xor), 30);
                    res.push(or_op);
                }

                res
            })().into_iter().collect());

            fn token_to_binary_operator(x: &Token) -> Option<Operator> {
                if let Token { r#type: TokenType::Operator(op), .. } = x {
                    Some(
                        match op {
                            Plus | Minus | Asterisk | Ampersand | Binary(_) => *op,
                            Assign(_) => return None,
                            _ => unreachable!()
                        }
                    )
                } else {
                    None
                }
            }

            let mut bin_ops_and_positions = Vec::<(Operator, TokenPos)>::new();
            let mut operands = Vec::<RvalueNode>::new();
            let mut res = Vec::<TokenOrRvalueNode>::new();

            fn apply_last_bin_op(
                ops_and_positions: &mut Vec<(Operator, TokenPos)>,
                operands: &mut Vec<RvalueNode>,
            ) -> Result<RvalueNode, ()> {
                let last_op_and_pos = ops_and_positions.pop();
                if last_op_and_pos.is_none() {
                    return Err(());
                }

                let (op, pos) = last_op_and_pos.unwrap();

                let rhs =
                    if let Some(rvalue_node) = operands.pop() {
                        Box::new(rvalue_node)
                    } else {
                        return Err(());
                    };

                let lhs =
                    if let Some(rvalue_node) = operands.pop() {
                        Box::new(rvalue_node)
                    } else {
                        return Err(());
                    };

                Ok(RvalueNode::from(
                    (pos,
                     Rvalue::Binary {
                         lhs,
                         bin_op: match op {
                             Plus => Add,
                             Minus => Sub,
                             Asterisk => Mul,
                             Ampersand => BitwiseAnd,
                             Binary(bin_op) =>
                                 RegularBinary(bin_op),
                             _ => unreachable!()
                         },
                         rhs,
                     })
                ))
            }

            enum RvalueNodeOrUnit {
                RvalueNode(RvalueNode),
                Unit,
            }

            fn free_ops_and_operands_stacks(
                ops: &mut Vec::<(Operator, TokenPos)>,
                operands: &mut Vec::<RvalueNode>,
            ) -> Result<RvalueNodeOrUnit, ()> {
                if ops.is_empty() && operands.is_empty() {
                    return Ok(RvalueNodeOrUnit::Unit);
                }

                while let Some(_) = ops.last() {
                    let op_applied = apply_last_bin_op(ops, operands)?;
                    operands.push(op_applied);
                }

                if operands.len() == 1 {
                    Ok(RvalueNodeOrUnit::RvalueNode(operands.pop().unwrap()))
                } else {
                    Err(())
                }
            }

            while let Some(t_or_rv) = reversed_input.pop() {
                match t_or_rv {
                    TokenOrRvalueNode::Token(t) => {
                        let op = token_to_binary_operator(&t);

                        if let Some(op) = op {
                            bin_ops_and_positions.push((op, t.pos));
                        } else {
                            if let Ok(ops_applied)
                            = free_ops_and_operands_stacks(&mut bin_ops_and_positions,
                                                           &mut operands) {
                                if let RvalueNodeOrUnit::RvalueNode(
                                    ops_applied
                                ) = ops_applied {
                                    res.push(TokenOrRvalueNode::RvalueNode(ops_applied));
                                }
                            } else {  // error while freeing the stacks
                                return None;
                            }

                            res.push(TokenOrRvalueNode::Token(t));
                        }
                    }

                    TokenOrRvalueNode::RvalueNode(rv_node) => {
                        operands.push(rv_node);

                        let next_op =
                            if let Some(TokenOrRvalueNode::Token(
                                            next_tok)) = reversed_input.last() {
                                token_to_binary_operator(next_tok)
                            } else {
                                None
                            };

                        while let Some(curr_op) = bin_ops_and_positions.last() {
                            if let Some(next_op) = next_op {
                                if bin_op_priority.get(&next_op)
                                    > bin_op_priority.get(&curr_op.0) {
                                    break;
                                } else {
                                    if let Ok(op_applied)
                                    = apply_last_bin_op(&mut bin_ops_and_positions,
                                                        &mut operands) {
                                        operands.push(op_applied);
                                    } else {
                                        return None;
                                    }
                                }
                            } else {
                                break;
                            }
                        }
                    }
                }
            }

            if let Ok(RvalueNodeOrUnit::RvalueNode(ops_applied))
            = free_ops_and_operands_stacks(&mut bin_ops_and_positions,
                                           &mut operands) {
                res.push(TokenOrRvalueNode::RvalueNode(ops_applied));
            }

            Some(res)
        }

        let res = rev_parse_binary_operators_except_assign(res)?;

        fn parse_conditional_expressions_and_assignments(
            mut input: Vec<TokenOrRvalueNode>
        ) -> Option<RvalueNode> {
            if input.is_empty() {
                return None;
            }

            use token::Assign;

            enum AssignOrRvalueNode {
                Assign(Assign, TokenPos),
                RvalueNode(RvalueNode),
            }

            fn parse_assignments(
                input: &mut VecDeque<AssignOrRvalueNode>
            ) -> Option<RvalueNode> {
                if input.len() % 2 != 1 {  // the number of tokens is even
                    return None;
                }

                while input.len() > 1 {
                    let rhs = input.pop_back()?;
                    let assign = input.pop_back()?;
                    let lhs = input.pop_back()?;

                    if let AssignOrRvalueNode::RvalueNode(
                        RvalueNode {
                            rvalue: Rvalue::Lvalue(lhs), ..
                        }) = lhs {
                        if let AssignOrRvalueNode::Assign(assign,
                                                          position) = assign {
                            if let AssignOrRvalueNode::RvalueNode(rhs) = rhs {
                                input.push_back(AssignOrRvalueNode::RvalueNode(
                                    RvalueNode::from(
                                        (position,
                                         Rvalue::Assign {
                                             lhs,
                                             assign,
                                             rhs: Box::new(rhs),
                                         }))
                                ));
                                continue;
                            }
                        }
                    }

                    return None;
                }

                return if let AssignOrRvalueNode::RvalueNode(
                    single_rv_node
                ) = input.pop_back()? {
                    Some(single_rv_node)
                } else {
                    None
                };
            }

            let mut on_false_stack = Vec::<(RvalueNode, TokenPos)>::new();
            let mut buff = VecDeque::<AssignOrRvalueNode>::new();

            while let Some(tok_or_rv_node) = input.pop() {
                match tok_or_rv_node {
                    TokenOrRvalueNode::Token(
                        Token {
                            r#type: TokenType::Operator(Operator::Assign(assign)), pos, ..
                        }) =>
                        buff.push_front(AssignOrRvalueNode::Assign(assign, pos)),
                    TokenOrRvalueNode::RvalueNode(rv_node) =>
                        buff.push_front(AssignOrRvalueNode::RvalueNode(rv_node)),
                    TokenOrRvalueNode::Token(t) => {
                        match t.r#type {
                            TokenType::Colon => {
                                if buff.len() != 1 {
                                    return None;
                                }

                                let next_rvalue = buff.pop_front()?;
                                if let AssignOrRvalueNode::RvalueNode(
                                    rv_node
                                ) = next_rvalue {
                                    on_false_stack.push((rv_node, t.pos));
                                } else {
                                    return None;
                                }
                            }

                            TokenType::QuestionMark => {
                                if input.is_empty() {  // ? ... : ...
                                    return None;
                                }

                                let (on_false, colon_pos) = on_false_stack.pop()?;
                                let on_true = parse_assignments(&mut buff)?;
                                buff.clear();

                                if let Some(
                                    TokenOrRvalueNode::RvalueNode(condition)
                                ) = input.pop() {
                                    input.push(TokenOrRvalueNode::RvalueNode(
                                        RvalueNode::from(
                                            (t.pos,
                                             Rvalue::ConditionalExpression {
                                                 condition: Box::new(condition),
                                                 on_true: Box::new(on_true),
                                                 on_false: Box::new(on_false),
                                                 colon_pos,
                                             }))
                                    ));
                                }
                            }
                            _ => unreachable!()
                        }
                    }
                }
            }

            if !on_false_stack.is_empty() {
                return None;
            }

            if input.is_empty() {
                return parse_assignments(&mut buff);
            }

            if input.len() != 1 {
                return None;
            }

            return if let Some(
                TokenOrRvalueNode::RvalueNode(single_rv_node)
            ) = input.pop() {
                Some(single_rv_node)
            } else {
                None
            };
        }

        let res = parse_conditional_expressions_and_assignments(res)?;
        // dbg!(&res);
        Some((res, input.len()))
    }
}