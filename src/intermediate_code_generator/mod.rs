use std::*;
use collections::HashMap;
use convert::TryInto;
use cmp::max;
use ops::RangeInclusive;
use cell::Cell;

use crate::config::CompilerOptions;
use crate::tokenizer::token;
use token::*;
use crate::parser;
use parser::{ScopeTable, MultiMap};
use parser::analyzer::*;
use parser::ast;
use ast::flat_ast::*;
use ast::{Rvalue, ConstantNode, IncDecType, IncDecNode, Unary, FunctionCallNode};

#[derive(Debug)]
pub(crate) enum Ival {
    Name(Label),
    Number(u64),
    ReserveWords(u64),
}

#[derive(Debug)]
pub(crate) struct FnInfo {
    pub(crate) stack_size: u64,
}

#[derive(Debug)]
pub(crate) enum Label {
    Local(String),
    Global(String),
    PooledStr(u64),
}

#[derive(Debug)]
pub(crate) enum Lvalue {
    NthArg(u64),
    LocalVar(StackRange),
    Label(Label),
}

struct CaseWithVal {
    case_val: u64,
    label_no: u64,
}

type LocalVarToNoAndStackRange<'a> = MultiMap<Option<&'a String>, (u64, StackRange)>;

enum StmtRequiringEndMarker<'a> {
    If { cond_is_false_label: u64, after_both_branches: Option<u64> },
    While { check_condition_label: u64, after_last_stmt: u64 },
    Switch {
        regular_cases: Vec<CaseWithVal>,
        default_case_label: Option<u64>,
        after_last_stmt: u64,
    },
    Compound { parent_stack: LocalVarToNoAndStackRange<'a>, parent_stack_size: u64 },
}

#[derive(Debug)]
pub(crate) struct IncDec {
    pub(crate) inc_or_dec: IncOrDec,
    pub(crate) inc_dec_type: IncDecType,
}

#[derive(Debug)]
pub(crate) enum RvalueUnary {
    LogicalNot,
    InvertSign,
    Complement,
}

#[derive(Debug)]
pub(crate) enum BinaryOp {
    Div,
    Mod,
    Or,
    Xor,
    Shift(LeftOrRight),
    Cmp(BinaryRelation),
    Add,
    Sub,
    Mul,
    BitwiseAnd,
    LogicalAnd,
}

impl From<RichBinaryOperation> for BinaryOp {
    fn from(bin_op: RichBinaryOperation) -> Self {
        use BinaryOp::*;
        match bin_op {
            RichBinaryOperation::RegularBinary(bin_op) => {
                match bin_op {
                    BinaryOperation::Div => Div,
                    BinaryOperation::Mod => Mod,
                    BinaryOperation::Or => Or,
                    BinaryOperation::Xor => Xor,
                    BinaryOperation::Shift(l_o_r) => Shift(l_o_r),
                    BinaryOperation::Cmp(rel) => Cmp(rel),
                }
            }
            RichBinaryOperation::Add => Add,
            RichBinaryOperation::Sub => Sub,
            RichBinaryOperation::Mul => Mul,
            RichBinaryOperation::BitwiseAnd => BitwiseAnd,
            RichBinaryOperation::LogicalAnd => LogicalAnd,
        }
    }
}

#[derive(Debug)]
pub(crate) enum IntermRepr {
    FnDef(String, FnInfo),
    VarOrVecDef(String),
    Ival(Ival),
    LoadLvalue(Lvalue),
    WriteLvalue,
    LoadConstant(u64),
    Deref,
    DeclLabel(String),
    InternalLabel(u64),
    Jump(u64),
    JmpIfEq(u64),
    TestAndJumpIfZero(u64),
    CaseEq { case_val: u64 },
    Save,
    BinOp(BinaryOp),
    LvUnary(IncDec),
    RvUnary(RvalueUnary),
    Call { nargs: u64 },
    Nargs,
    Goto,
    Ret,
}

pub(crate) struct IntermediateCodeGenerator<'a> {
    pub(crate) compiler_options: CompilerOptions,
    label_counter: RangeInclusive<u64>,
    str_pool: Cell<HashMap<String, u64>>,
    global_definitions: &'a GlobalDefinitions,
    name_to_stack_range: LocalVarToNoAndStackRange<'a>,
    local_scope: Option<&'a LocalScope>,
    stmts_requiring_end_marker: Vec<StmtRequiringEndMarker<'a>>,
    last_breakable_stmt_idxs_stack: Vec<usize>,
    stack_size_in_words: u64,
    params_order: HashMap<&'a String, u64>,
}

#[derive(Debug, Clone)]
pub(crate) enum StackRange {
    Exact(u64),
    Span(RangeInclusive<u64>),
}

enum ShortCircuitOp {
    And,
    Or,
}

fn is_regular_node(node: &FlatNode) -> bool {
    use FlatNode::*;
    match node {
        Compound | If(_) | While(_) | Switch(_)
        | Def(_) | EndOfStmt { .. } => {
            false
        }
        _ => true
    }
}

impl<'a> IntermediateCodeGenerator<'a> {
    pub(crate) fn new(
        compiler_options: CompilerOptions,
        global_definitions: &'a GlobalDefinitions,
    ) -> Self {
        IntermediateCodeGenerator {
            compiler_options,
            label_counter: 0..=u64::MAX,
            str_pool: Default::default(),
            global_definitions,
            name_to_stack_range: Default::default(),
            local_scope: None,
            stmts_requiring_end_marker: Default::default(),
            last_breakable_stmt_idxs_stack: Default::default(),
            stack_size_in_words: 0,
            params_order: Default::default(),
        }
    }

    fn process_lvalue(&mut self, lv: &ast::Lvalue) -> Vec<IntermRepr> {
        use IntermRepr::*;
        match lv {
            ast::Lvalue::Name(name) => {
                vec![LoadLvalue(
                    if let Some(info) = self.local_scope.unwrap().get(name) {
                        match info.info {
                            ProcessedDeclInfo::AssumedExternFnCall(_)
                            | ProcessedDeclInfo::ExplicitExtern(_) => {
                                Lvalue::Label(Label::Global(name.clone()))
                            }
                            ProcessedDeclInfo::Auto { .. } => {
                                if let Some(
                                    (_, st)
                                ) = self.name_to_stack_range.get_last(&Some(name)) {
                                    Lvalue::LocalVar(st.clone())
                                } else {
                                    unreachable!()
                                }
                            }
                            ProcessedDeclInfo::Label => {
                                Lvalue::Label(Label::Local(name.clone()))
                            }
                            ProcessedDeclInfo::FnParameter { param_no } => {
                                Lvalue::NthArg(param_no)
                            }
                        }
                    } else {
                        unreachable!()
                    })]
            }
            ast::Lvalue::DerefRvalue(rv) => {
                self.process_rvalue(&rv.rvalue)
            }
            ast::Lvalue::Indexing { vector, index } => {
                let mut res = self.process_rvalue(&vector.rvalue);
                res.push(Save);
                res.append(&mut self.process_rvalue(&index.rvalue));
                res.push(BinOp(BinaryOp::Add));
                res
            }
        }
    }

    fn short_circuit(&mut self, op: ShortCircuitOp, out: u64) -> Vec<IntermRepr> {
        use IntermRepr::*;
        match op {
            ShortCircuitOp::And => {
                vec![TestAndJumpIfZero(out)]
            }
            ShortCircuitOp::Or => {
                let rhs_label = self.label_counter.next().unwrap();
                vec![TestAndJumpIfZero(rhs_label),
                     Jump(out),
                     InternalLabel(rhs_label)]
            }
        }
    }

    fn try_short_circuit(&mut self, op: RichBinaryOperation, rhs: &Rvalue) -> Vec<IntermRepr> {
        use IntermRepr::*;
        let mut res = vec![];
        let out = self.label_counter.next().unwrap();

        if self.compiler_options.short_circuit {
            let sc_op =
                match op {
                    RichBinaryOperation::RegularBinary(BinaryOperation::Or) =>
                        Some(ShortCircuitOp::Or),
                    RichBinaryOperation::LogicalAnd => Some(ShortCircuitOp::And),
                    _ => None,
                };
            if let Some(sc_op) = sc_op {
                self.short_circuit(sc_op, out);
            }
        }
        res.push(Save);
        res.append(&mut self.process_rvalue(rhs));
        res.push(BinOp(BinaryOp::from(op)));
        res.push(InternalLabel(out));
        res
    }

    fn process_rvalue(
        &mut self,
        rv: &Rvalue,
    ) -> Vec<IntermRepr> {
        use IntermRepr::*;
        match rv {
            Rvalue::Constant(c) => {
                match &c.constant {
                    token::Constant::Number(x) => {
                        vec![LoadConstant(*x)]
                    }
                    token::Constant::String(s) => {
                        let idx = self.try_add_to_str_pool(s);
                        vec![LoadLvalue(Lvalue::Label(Label::PooledStr(idx)))]
                    }
                }
            }
            Rvalue::Lvalue(lv) => {
                let mut res = self.process_lvalue(&lv.lvalue);
                if let ast::Lvalue::Name(name) = &lv.lvalue {
                    if let Some(
                        DeclInfoAndPos { info, .. }
                    ) = self.local_scope.unwrap().get(name) {
                        match info {
                            ProcessedDeclInfo::Auto { size_if_vec: Some(_) }
                            | ProcessedDeclInfo::ExplicitExtern(
                                DefInfoAndPos { info: DefInfo::Vector { .. }, .. }
                            ) => {
                                return res;  // the rvalue of a vector is its lvalue
                            }
                            ProcessedDeclInfo::AssumedExternFnCall(_)
                            | ProcessedDeclInfo::ExplicitExtern(
                                DefInfoAndPos { info: DefInfo::Function { .. }, .. }
                            ) => {
                                return res;  // the rvalue of a function is its lvalue
                            }
                            ProcessedDeclInfo::Label => {
                                return res;  // the rvalue of a label is its lvalue
                            }
                            _ => (),
                        }
                    } else {
                        unreachable!()
                    }
                }
                res.push(Deref);
                res
            }
            Rvalue::Assign { lhs, assign, rhs } => {
                let mut res = self.process_lvalue(&lhs.lvalue);
                res.push(Save);
                if let Some(bin_op) = assign.bin_op {
                    res.push(Deref);
                    res.append(&mut self.try_short_circuit(bin_op, &rhs.rvalue));
                } else {
                    res.append(&mut self.process_rvalue(&rhs.rvalue))
                }
                res.push(WriteLvalue);
                res
            }
            Rvalue::Binary { lhs, bin_op, rhs } => {
                let mut res = self.process_rvalue(&lhs.rvalue);
                res.append(&mut self.try_short_circuit(*bin_op, &rhs.rvalue));
                res
            }
            Rvalue::IncDec(
                IncDecNode {
                    inc_or_dec, inc_dec_type, lvalue
                }
            ) => {
                let mut res = self.process_lvalue(&lvalue.lvalue);
                res.push(LvUnary(IncDec {
                    inc_or_dec: *inc_or_dec,
                    inc_dec_type: *inc_dec_type,
                }));
                res
            }
            Rvalue::Unary(un, rv) => {
                let mut res = self.process_rvalue(&rv.rvalue);
                match un {
                    Unary::Minus => res.push(RvUnary(RvalueUnary::InvertSign)),
                    Unary::LogicalNot => res.push(RvUnary(RvalueUnary::LogicalNot)),
                    Unary::Complement => res.push(RvUnary(RvalueUnary::Complement)),
                    Unary::Plus => (),
                }
                res
            }
            Rvalue::TakeAddress(lv) => {
                self.process_lvalue(&lv.lvalue)
            }
            Rvalue::ConditionalExpression { condition, on_true, on_false, .. } => {
                let out = self.label_counter.next().unwrap();
                let on_false_label = self.label_counter.next().unwrap();
                let mut res = self.process_rvalue(&condition.rvalue);
                res.push(TestAndJumpIfZero(on_false_label));
                res.append(&mut self.process_rvalue(&on_true.rvalue));
                res.push(Jump(out));
                res.append(&mut self.process_rvalue(&on_false.rvalue));
                res.push(InternalLabel(out));
                res
            }
            Rvalue::BracketedExpression(rv) => {
                self.process_rvalue(&rv.rvalue)
            }
            Rvalue::FunctionCall(FunctionCallNode { fn_name, arguments }) => {
                if let Rvalue::Lvalue(
                    ast::LvalueNode { lvalue: ast::Lvalue::Name(fn_name), .. }
                ) = &*fn_name.rvalue {
                    if fn_name == "nargs" {
                        return vec![Nargs];
                    }
                }
                let mut res = vec![];
                for arg in arguments.iter().rev() {
                    res.append(&mut self.process_rvalue(&arg.rvalue));
                    res.push(Save);
                }
                res.append(&mut self.process_rvalue(&fn_name.rvalue));
                res.push(Call { nargs: arguments.len().try_into().unwrap() });
                res
            }
        }
    }

    fn process_regular_node(
        &mut self,
        curr_fn: &mut FnInfo,
        node: &'a FlatNode,
    ) -> Vec<IntermRepr> {
        use IntermRepr::*;
        debug_assert!(is_regular_node(node));
        match node {
            FlatNode::Decl(decl) => {
                match &decl.info {
                    FlatDeclarationNameInfo::Extern => vec![],
                    FlatDeclarationNameInfo::Label => {
                        vec![DeclLabel(decl.name.clone())]
                    }

                    FlatDeclarationNameInfo::Auto { size_if_vec } => {
                        let local_var_no = self.name_to_stack_range
                            .total_items()
                            .try_into()
                            .unwrap();
                        if let Some(
                            ConstantNode { constant: token::Constant::Number(size), .. }
                        ) = size_if_vec {
                            let upper = self.stack_size_in_words + size - 1;
                            let span = StackRange::Span(self.stack_size_in_words..=upper);

                            self.name_to_stack_range.insert(Some(&decl.name),
                                                            (local_var_no, span));
                            self.stack_size_in_words += size;
                        } else {
                            self.name_to_stack_range.insert(
                                Some(&decl.name),
                                (local_var_no, StackRange::Exact(self.stack_size_in_words)));
                            self.stack_size_in_words += 1;
                        }
                        curr_fn.stack_size = max(curr_fn.stack_size, self.stack_size_in_words);
                        vec![]
                    }
                }
            }
            FlatNode::Rvalue(rv) => {
                self.process_rvalue(rv)
            }
            FlatNode::Else => {
                if let Some(
                    StmtRequiringEndMarker::If {
                        cond_is_false_label: condition_is_false_label,
                        after_both_branches
                    }
                ) = self.stmts_requiring_end_marker.last_mut() {
                    let after_else_body = self.label_counter.next().unwrap();
                    debug_assert!(after_both_branches.is_none());
                    *after_both_branches = Some(after_else_body);
                    vec![
                        Jump(after_else_body),
                        InternalLabel(*condition_is_false_label),
                    ]
                } else {
                    unreachable!()
                }
            }
            FlatNode::Break => {
                let idx = self.last_breakable_stmt_idxs_stack.last().unwrap();
                let last_br_stmt = &self.stmts_requiring_end_marker[*idx];
                if let StmtRequiringEndMarker::Switch { after_last_stmt, .. }
                | StmtRequiringEndMarker::While { after_last_stmt, .. }
                = last_br_stmt {
                    vec![Jump(*after_last_stmt)]
                } else {
                    unreachable!()
                }
            }
            FlatNode::Goto(rv) => {
                let mut res = self.process_rvalue(rv);
                res.push(Goto);
                res
            }
            FlatNode::Case(constant) => {
                let idx = self.last_breakable_stmt_idxs_stack.last().unwrap();
                let switch = &mut self.stmts_requiring_end_marker[*idx];
                if let StmtRequiringEndMarker::Switch {
                    regular_cases, default_case_label, ..
                } = switch {
                    let label_no = self.label_counter.next().unwrap();
                    if let Some(
                        ConstantNode { constant: Constant::Number(case_val), .. }
                    ) = constant {
                        regular_cases.push(CaseWithVal {
                            case_val: *case_val,
                            label_no,
                        })
                    } else {
                        debug_assert!(default_case_label.is_none());
                        *default_case_label = Some(label_no);
                    }
                    vec![InternalLabel(label_no)]
                } else {
                    unreachable!()
                }
            }
            FlatNode::Return(rv) => {
                let mut res = vec![];
                if let Some(rv) = rv {
                    res.append(&mut self.process_rvalue(rv));
                }
                res.push(Ret);
                res
            }
            _ => unreachable!()
        }
    }

    fn process_slice(
        &mut self,
        curr_fn: &mut FnInfo,
        prog_slice: &'a [(FlatNodeAndPos, LocalScope)],
    ) -> (Vec<IntermRepr>, usize) {
        use IntermRepr::*;
        let mut res = vec![];
        let mut i: usize = 0;
        while let Some((node, sc)) = prog_slice.get(i) {
            self.local_scope = Some(sc);
            if is_regular_node(&node.node) {
                res.append(&mut self.process_regular_node(curr_fn, &node.node));
            } else {
                match &node.node {
                    FlatNode::Compound => {
                        self.stmts_requiring_end_marker.push(
                            StmtRequiringEndMarker::Compound {
                                parent_stack: self.name_to_stack_range.clone(),
                                parent_stack_size: self.stack_size_in_words,
                            });
                    }

                    FlatNode::If(rv) => {
                        res.append(&mut self.process_rvalue(rv));
                        let condition_is_false_label = self.label_counter.next().unwrap();
                        res.push(TestAndJumpIfZero(condition_is_false_label));
                        self.stmts_requiring_end_marker.push(
                            StmtRequiringEndMarker::If {
                                cond_is_false_label: condition_is_false_label,
                                after_both_branches: None,
                            });
                    }

                    FlatNode::While(rv) => {
                        let check_condition_label = self.label_counter.next().unwrap();
                        res.push(InternalLabel(check_condition_label));
                        let cond_is_false_label = self.label_counter.next().unwrap();
                        let r#while = StmtRequiringEndMarker::While {
                            check_condition_label,
                            after_last_stmt: cond_is_false_label,
                        };
                        res.append(&mut self.process_rvalue(rv));
                        res.push(TestAndJumpIfZero(cond_is_false_label));

                        self.last_breakable_stmt_idxs_stack.push(
                            self.stmts_requiring_end_marker.len());
                        self.stmts_requiring_end_marker.push(r#while)
                    }

                    FlatNode::Switch(rv) => {
                        res.append(&mut self.process_rvalue(rv));
                        self.last_breakable_stmt_idxs_stack.push(
                            self.stmts_requiring_end_marker.len());
                        self.stmts_requiring_end_marker.push(StmtRequiringEndMarker::Switch {
                            regular_cases: vec![],
                            default_case_label: None,
                            after_last_stmt: self.label_counter.next().unwrap(),
                        })
                    }

                    FlatNode::EndOfStmt { positions_away } => {
                        debug_assert_eq!(positions_away - i, 1);
                        match self.stmts_requiring_end_marker.pop().unwrap() {
                            StmtRequiringEndMarker::Compound {
                                parent_stack,
                                parent_stack_size
                            } => {
                                self.name_to_stack_range = parent_stack;
                                self.stack_size_in_words = parent_stack_size;
                            }

                            StmtRequiringEndMarker::If {
                                cond_is_false_label,
                                after_both_branches
                            } => {
                                if let Some(after_both_branches) = after_both_branches {
                                    res.push(InternalLabel(after_both_branches))
                                } else {
                                    res.push(InternalLabel(cond_is_false_label))
                                }
                            }
                            StmtRequiringEndMarker::While {
                                check_condition_label,
                                after_last_stmt
                            } => {
                                self.last_breakable_stmt_idxs_stack.pop();
                                res.push(Jump(check_condition_label));
                                res.push(InternalLabel(after_last_stmt));
                            }

                            StmtRequiringEndMarker::Switch {
                                regular_cases,
                                default_case_label,
                                after_last_stmt
                            } => {
                                self.last_breakable_stmt_idxs_stack.pop();
                                let mut case_table = vec![];
                                for CaseWithVal { case_val, label_no } in regular_cases {
                                    case_table.push(CaseEq { case_val });
                                    case_table.push(JmpIfEq(label_no));
                                }
                                if let Some(default_case_label) = default_case_label {
                                    case_table.push(Jump(default_case_label));
                                } else {
                                    case_table.push(Jump(after_last_stmt));
                                }
                                case_table.append(&mut res);
                                res = case_table;
                            }
                        }
                        return (res, i + 1);
                    }
                    _ => unreachable!()
                }
                let (mut nodes, adv)
                    = self.process_slice(
                    curr_fn,
                    &prog_slice[i + 1..]);
                res.append(&mut nodes);
                i += adv;
            }
            i += 1;
        }
        (res, i)
    }

    fn reset(&mut self) {
        *self = Self::from(Self::new(self.compiler_options, self.global_definitions));
    }

    fn process_function(
        &mut self,
        prog_slice: &'a [(FlatNodeAndPos, LocalScope)],
    ) -> (Vec<IntermRepr>, usize) {
        self.reset();
        let mut res = vec![];
        use IntermRepr::*;
        if let FlatNode::Def(
            FlatDefinition { name, info: FlatDefinitionInfo::Function { params } }
        ) = &prog_slice[0].0.node {
            self.params_order = params.iter().enumerate().map(|(i, (param, _))| {
                (param, i.try_into().unwrap())
            }).collect();
            res.push(FnDef(name.clone(), FnInfo { stack_size: 0 }))
        } else {
            unreachable!()
        }

        let curr_fn =
            if let FnDef(_, info) = &mut res[0] {
                info
            } else {
                unreachable!()
            };
        let (mut rest_of_nodes, adv) = self.process_slice(curr_fn, &prog_slice[1..]);
        res.append(&mut rest_of_nodes);
        res.push(Ret);
        (res, adv + 1)
    }

    fn try_add_to_str_pool(&mut self, s: &str) -> u64 {
        if let Some(idx) = self.str_pool.get_mut().get(s) {
            *idx
        } else {
            let len = self.str_pool.get_mut().len().try_into().unwrap();
            self.str_pool.get_mut().insert(s.to_string(), len);
            len
        }
    }

    fn name_or_constant_to_ival(&mut self, x: &NameOrConstant) -> Ival {
        match x {
            NameOrConstant::Name(name) => Ival::Name(Label::Global(name.clone())),
            NameOrConstant::Constant(token::Constant::Number(n)) =>
                Ival::Number(*n),
            NameOrConstant::Constant(token::Constant::String(s)) => {
                let idx = self.try_add_to_str_pool(s);
                Ival::Name(Label::PooledStr(idx))
            }
        }
    }

    pub(crate) fn run(
        &mut self,
        program: &'a ScopeTable,
    ) -> (Vec<IntermRepr>, HashMap<String, u64>) {
        let mut res = vec![];
        for (name, def) in &program.global {
            match &def.info {
                DefInfo::Variable(VarDefInfo::UserVar { ival }) => {
                    let ival =
                        if let Some(ival) = ival {
                            self.name_or_constant_to_ival(&ival.value)
                        } else {
                            Ival::Number(0)
                        };
                    res.push(IntermRepr::VarOrVecDef(name.clone()));
                    res.push(IntermRepr::Ival(ival))
                }
                DefInfo::Vector { size, ivals } => {
                    res.push(IntermRepr::VarOrVecDef(name.clone()));
                    let ivals: Vec<Ival> = ivals
                        .iter()
                        .map(|x| self.name_or_constant_to_ival(&x.value))
                        .collect();
                    let ivals_len = ivals.len().try_into().unwrap();
                    for iv in ivals {
                        res.push(IntermRepr::Ival(iv))
                    }
                    let unused_words = size.saturating_sub(ivals_len);
                    if unused_words > 0 {
                        res.push(IntermRepr::Ival(Ival::ReserveWords(unused_words)))
                    }
                }
                _ => (),
            }
        }
        let flat_nodes: Vec<&FlatNodeAndPos> = program.local.iter().map(|x| &x.0).collect();
        for rng in get_fns_ranges(flat_nodes.into_iter()) {
            let rng_len = rng.len();
            let (mut ir, adv)
                = self.process_function(&program.local[rng]);
            debug_assert_eq!(adv, rng_len);
            res.append(&mut ir);
        }
        (res, self.str_pool.take())
    }
}