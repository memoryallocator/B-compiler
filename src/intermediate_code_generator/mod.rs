use std::cell::Cell;
use std::cmp::max;
use std::collections::HashMap;
use std::ops::RangeInclusive;

use multimap::MultiMap;

use crate::parser;
use crate::tokenizer::token::{
    BinaryOperation, BinaryRelation, Constant, IncOrDec, LeftOrRight, RichBinaryOperation,
};
use crate::utils::CompilerOptions;

use analyzer::get_fns_ranges;
use analyzer::{DefInfo, GlobalDefinitions, LocalScope, ProcessedDeclInfo, ScopeTable, VarDefInfo};
use ast::flat_ast::{
    FlatDeclarationNameInfo, FlatDefinition, FlatDefinitionInfo, FlatNode, FlatNodeAndPos,
    NameOrConstant,
};
use ast::{
    ConstantNode, FunctionCallNode, IncDecNode, IncDecType, Lvalue as AstLvalue, Rvalue, Unary,
};
use parser::analyzer;
use parser::ast;
use parser::{DeclInfoAndPos, DefInfoAndPos};

#[derive(Debug)]
pub enum Ival {
    Name(Label),
    Number(u64),
    ReserveWords(u64),
}

#[derive(Debug)]
pub struct FnInfo {
    pub(crate) stack_size: u64,
    pub(crate) num_of_params: u64,
}

#[derive(Debug)]
pub enum Label {
    Local(String),
    Global(String),
    PooledStr(u64),
}

#[derive(Debug)]
pub enum Lvalue {
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
    If {
        cond_is_false_label: u64,
        after_both_branches: Option<u64>,
    },
    While {
        check_cond_label: u64,
        after_last_stmt: u64,
    },
    Switch {
        regular_cases: Vec<CaseWithVal>,
        default_case_label: Option<u64>,
        after_last_stmt: u64,
    },
    Compound {
        parent_stack: LocalVarToNoAndStackRange<'a>,
        parent_stack_size: u64,
    },
}

#[derive(Debug)]
pub struct IncDec {
    pub(crate) inc_or_dec: IncOrDec,
    pub(crate) inc_dec_type: IncDecType,
}

#[derive(Debug)]
pub enum RvalueUnary {
    LogicalNot,
    Minus,
    Complement,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
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
            RichBinaryOperation::RegularBinary(bin_op) => match bin_op {
                BinaryOperation::Div => Div,
                BinaryOperation::Mod => Mod,
                BinaryOperation::Or => Or,
                BinaryOperation::Xor => Xor,
                BinaryOperation::Shift(l_o_r) => Shift(l_o_r),
                BinaryOperation::Cmp(rel) => Cmp(rel),
            },
            RichBinaryOperation::Add => Add,
            RichBinaryOperation::Sub => Sub,
            RichBinaryOperation::Mul => Mul,
            RichBinaryOperation::BitwiseAnd => BitwiseAnd,
            RichBinaryOperation::LogicalAnd => LogicalAnd,
        }
    }
}

#[derive(Debug)]
pub enum IntermRepr {
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
    CaseJmpIfEq { case_val: u64, label_no: u64 },
    TestAndJumpIfZero(u64),
    Save,
    BinOp(BinaryOp),
    LvUnary(IncDec),
    RvUnary(RvalueUnary),
    Call { nargs: u64 },
    Goto,
    Ret,
}

pub struct IntermediateCodeGenerator<'a> {
    pub(crate) compiler_options: &'a CompilerOptions,
    label_counter: RangeInclusive<u64>,
    str_pool: Cell<HashMap<String, u64>>,
    global_definitions: &'a GlobalDefinitions,
    name_to_stack_range: LocalVarToNoAndStackRange<'a>,
    local_scope: Option<&'a LocalScope>,
    stmts_requiring_end_marker: Vec<StmtRequiringEndMarker<'a>>,
    last_breakable_stmt_idxs_stack: Vec<usize>,
    while_stmt_idxs_stack: Vec<usize>,
    stack_size_in_words: u64,
    params_order: HashMap<&'a String, u64>,
}

#[derive(Debug, Clone)]
pub enum StackRange {
    Exact(u64),
    Span(RangeInclusive<u64>),
}

fn is_regular_node(node: &FlatNode) -> bool {
    !matches!(
        node,
        FlatNode::Compound
            | FlatNode::If(_)
            | FlatNode::While(_)
            | FlatNode::Switch(_)
            | FlatNode::Def(_)
            | FlatNode::EndOfStmt { .. },
    )
}

impl<'a> IntermediateCodeGenerator<'a> {
    pub fn new(
        compiler_options: &'a CompilerOptions,
        global_definitions: &'a GlobalDefinitions,
    ) -> Self {
        IntermediateCodeGenerator {
            compiler_options,
            label_counter: 0..=u64::MAX,
            global_definitions,
            str_pool: Default::default(),
            name_to_stack_range: Default::default(),
            local_scope: None,
            stmts_requiring_end_marker: Default::default(),
            last_breakable_stmt_idxs_stack: Default::default(),
            while_stmt_idxs_stack: Default::default(),
            stack_size_in_words: 0,
            params_order: Default::default(),
        }
    }

    fn process_lvalue(&mut self, lv: &AstLvalue) -> Vec<IntermRepr> {
        match lv {
            AstLvalue::Name(name) => {
                vec![IntermRepr::LoadLvalue(
                    if let Some(info) = self.local_scope.unwrap().get(name) {
                        match info.info {
                            ProcessedDeclInfo::Label => Lvalue::Label(Label::Local(name.clone())),
                            ProcessedDeclInfo::FnParameter { param_no } => {
                                Lvalue::NthArg(1 + param_no)
                            }
                            ProcessedDeclInfo::AssumedExternFnCall(_)
                            | ProcessedDeclInfo::ExplicitExtern(_) => {
                                Lvalue::Label(Label::Global(name.clone()))
                            }
                            ProcessedDeclInfo::Auto { .. } => {
                                if let Some(stack_ranges) =
                                    self.name_to_stack_range.get_vec(&Some(name))
                                {
                                    Lvalue::LocalVar(stack_ranges.last().unwrap().1.clone())
                                } else {
                                    unreachable!()
                                }
                            }
                        }
                    } else {
                        unreachable!()
                    },
                )]
            }
            AstLvalue::DerefRvalue(rv) => self.process_rvalue(&rv.rvalue),
            AstLvalue::Indexing { vector, index } => {
                let mut res = self.process_rvalue(&vector.rvalue);
                res.push(IntermRepr::Save);
                res.extend(self.process_rvalue(&index.rvalue));
                res.push(IntermRepr::BinOp(BinaryOp::Add));
                res
            }
        }
    }

    fn process_rvalue(&mut self, rv: &Rvalue) -> Vec<IntermRepr> {
        match rv {
            Rvalue::Constant(c) => match &c.constant {
                Constant::Number(x) => {
                    vec![IntermRepr::LoadConstant(*x)]
                }
                Constant::String(s) => {
                    let idx = self.try_add_to_str_pool(s);
                    vec![IntermRepr::LoadLvalue(Lvalue::Label(Label::PooledStr(idx)))]
                }
            },
            Rvalue::Lvalue(lv) => {
                let mut res = self.process_lvalue(&lv.lvalue);
                if let ast::Lvalue::Name(name) = &lv.lvalue {
                    if let Some(DeclInfoAndPos { info, .. }) = self.local_scope.unwrap().get(name) {
                        match info {
                            ProcessedDeclInfo::Auto {
                                size_if_vec: Some(_),
                            }
                            | ProcessedDeclInfo::ExplicitExtern(DefInfoAndPos {
                                info: DefInfo::Vector { .. },
                                ..
                            })
                            | ProcessedDeclInfo::AssumedExternFnCall(_)
                            | ProcessedDeclInfo::ExplicitExtern(DefInfoAndPos {
                                info: DefInfo::Function { .. },
                                ..
                            })
                            | ProcessedDeclInfo::Label => {
                                return res;
                            }
                            _ => (),
                        }
                    } else {
                        unreachable!()
                    }
                }
                res.push(IntermRepr::Deref);
                res
            }
            Rvalue::Assign { lhs, assign, rhs } => {
                let mut res = self.process_lvalue(&lhs.lvalue);
                res.push(IntermRepr::Save);
                if let Some(bin_op) = assign.bin_op {
                    res.push(IntermRepr::Deref);
                    res.push(IntermRepr::Save);
                    res.extend(self.process_rvalue(rhs.rvalue.as_ref()));
                    res.push(IntermRepr::BinOp(BinaryOp::from(bin_op)));
                } else {
                    res.extend(self.process_rvalue(&rhs.rvalue))
                }
                res.push(IntermRepr::WriteLvalue);
                res
            }
            Rvalue::Binary { lhs, bin_op, rhs } => {
                let mut res = self.process_rvalue(&lhs.rvalue);
                res.push(IntermRepr::Save);
                res.extend(self.process_rvalue(&rhs.rvalue));
                res.push(IntermRepr::BinOp(BinaryOp::from(*bin_op)));
                res
            }
            Rvalue::IncDec(IncDecNode {
                inc_or_dec,
                inc_dec_type,
                lvalue,
            }) => {
                let mut res = self.process_lvalue(&lvalue.lvalue);
                res.push(IntermRepr::LvUnary(IncDec {
                    inc_or_dec: *inc_or_dec,
                    inc_dec_type: *inc_dec_type,
                }));
                res
            }
            Rvalue::Unary(un, rv) => {
                let mut res = self.process_rvalue(&rv.rvalue);
                match un {
                    Unary::Minus => res.push(IntermRepr::RvUnary(RvalueUnary::Minus)),
                    Unary::LogicalNot => res.push(IntermRepr::RvUnary(RvalueUnary::LogicalNot)),
                    Unary::Complement => res.push(IntermRepr::RvUnary(RvalueUnary::Complement)),
                    Unary::Plus => (),
                }
                res
            }
            Rvalue::TakeAddress(lv) => self.process_lvalue(&lv.lvalue),
            Rvalue::ConditionalExpression {
                condition,
                on_true,
                on_false,
                ..
            } => {
                let out = self.label_counter.next().unwrap();
                let on_false_label = self.label_counter.next().unwrap();
                let mut res = self.process_rvalue(&condition.rvalue);

                res.push(IntermRepr::TestAndJumpIfZero(on_false_label));
                res.extend(self.process_rvalue(&on_true.rvalue));
                res.push(IntermRepr::Jump(out));

                res.push(IntermRepr::InternalLabel(on_false_label));
                res.extend(self.process_rvalue(&on_false.rvalue));
                res.push(IntermRepr::InternalLabel(out));
                res
            }
            Rvalue::BracketedExpression(rv) => self.process_rvalue(&rv.rvalue),
            Rvalue::FunctionCall(FunctionCallNode { fn_name, arguments }) => {
                let mut res = vec![];
                for arg in arguments.iter().rev() {
                    res.extend(self.process_rvalue(&arg.rvalue));
                    res.push(IntermRepr::Save);
                }
                res.push(IntermRepr::LoadConstant(
                    arguments.len().try_into().unwrap(),
                ));
                res.push(IntermRepr::Save);
                res.extend(self.process_rvalue(&fn_name.rvalue));
                res.push(IntermRepr::Call {
                    nargs: arguments.len().try_into().unwrap(),
                });
                res
            }
        }
    }

    fn process_regular_node(
        &mut self,
        curr_fn: &mut FnInfo,
        node: &'a FlatNode,
    ) -> Vec<IntermRepr> {
        debug_assert!(is_regular_node(node));
        match node {
            FlatNode::Decl(decl) => match &decl.info {
                FlatDeclarationNameInfo::Extern => vec![],
                FlatDeclarationNameInfo::Label => {
                    vec![IntermRepr::DeclLabel(decl.name.clone())]
                }

                FlatDeclarationNameInfo::Auto {
                    specified_size_if_vec,
                } => {
                    let local_var_no = self
                        .name_to_stack_range
                        .iter_all()
                        .fold(0, |acc, (_, values)| acc + values.len())
                        .try_into()
                        .unwrap();
                    if let Some(ConstantNode {
                        constant: Constant::Number(spec_size),
                        ..
                    }) = specified_size_if_vec
                    {
                        let upper = self.stack_size_in_words + spec_size;
                        let span = StackRange::Span(self.stack_size_in_words..=upper);

                        self.name_to_stack_range
                            .insert(Some(&decl.name), (local_var_no, span));
                        self.stack_size_in_words = upper + 1;
                    } else {
                        self.name_to_stack_range.insert(
                            Some(&decl.name),
                            (local_var_no, StackRange::Exact(self.stack_size_in_words)),
                        );
                        self.stack_size_in_words += 1;
                    }
                    curr_fn.stack_size = max(curr_fn.stack_size, self.stack_size_in_words);
                    vec![]
                }
            },
            FlatNode::Rvalue(rv) => self.process_rvalue(rv),
            FlatNode::Else => {
                if let Some(StmtRequiringEndMarker::If {
                    cond_is_false_label: condition_is_false_label,
                    after_both_branches,
                }) = self.stmts_requiring_end_marker.last_mut()
                {
                    let after_else_body = self.label_counter.next().unwrap();
                    debug_assert!(after_both_branches.is_none());
                    *after_both_branches = Some(after_else_body);
                    vec![
                        IntermRepr::Jump(after_else_body),
                        IntermRepr::InternalLabel(*condition_is_false_label),
                    ]
                } else {
                    unreachable!()
                }
            }
            FlatNode::Break => {
                let idx = self.last_breakable_stmt_idxs_stack.last().unwrap();
                let last_br_stmt = &self.stmts_requiring_end_marker[*idx];
                if let StmtRequiringEndMarker::Switch {
                    after_last_stmt, ..
                }
                | StmtRequiringEndMarker::While {
                    after_last_stmt, ..
                } = last_br_stmt
                {
                    vec![IntermRepr::Jump(*after_last_stmt)]
                } else {
                    unreachable!()
                }
            }
            FlatNode::Continue => {
                let idx = self.while_stmt_idxs_stack.last().unwrap();
                let last_wh_stmt = &self.stmts_requiring_end_marker[*idx];
                if let StmtRequiringEndMarker::While {
                    check_cond_label, ..
                } = last_wh_stmt
                {
                    vec![IntermRepr::Jump(*check_cond_label)]
                } else {
                    unreachable!()
                }
            }
            FlatNode::Goto(rv) => {
                let mut res = self.process_rvalue(rv);
                res.push(IntermRepr::Goto);
                res
            }
            FlatNode::Case(constant) => {
                let idx = self.last_breakable_stmt_idxs_stack.last().unwrap();
                let switch_stmt = &mut self.stmts_requiring_end_marker[*idx];
                if let StmtRequiringEndMarker::Switch {
                    regular_cases,
                    default_case_label,
                    ..
                } = switch_stmt
                {
                    let label_no = self.label_counter.next().unwrap();
                    if let Some(ConstantNode {
                        constant: Constant::Number(case_val),
                        ..
                    }) = constant
                    {
                        regular_cases.push(CaseWithVal {
                            case_val: *case_val,
                            label_no,
                        })
                    } else {
                        debug_assert!(default_case_label.is_none());
                        *default_case_label = Some(label_no);
                    }
                    vec![IntermRepr::InternalLabel(label_no)]
                } else {
                    unreachable!()
                }
            }
            FlatNode::Return(rv) => {
                let mut res = vec![];
                if let Some(rv) = rv {
                    res.extend(self.process_rvalue(rv));
                }
                res.push(IntermRepr::Ret);
                res
            }
            _ => unreachable!(),
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
                res.extend(self.process_regular_node(curr_fn, &node.node));
            } else {
                match &node.node {
                    FlatNode::Compound => {
                        self.stmts_requiring_end_marker
                            .push(StmtRequiringEndMarker::Compound {
                                parent_stack: self.name_to_stack_range.clone(),
                                parent_stack_size: self.stack_size_in_words,
                            });
                    }

                    FlatNode::If(rv) => {
                        res.extend(self.process_rvalue(rv));
                        let condition_is_false_label = self.label_counter.next().unwrap();
                        res.push(TestAndJumpIfZero(condition_is_false_label));
                        self.stmts_requiring_end_marker
                            .push(StmtRequiringEndMarker::If {
                                cond_is_false_label: condition_is_false_label,
                                after_both_branches: None,
                            });
                    }

                    FlatNode::While(rv) => {
                        let check_cond_label = self.label_counter.next().unwrap();
                        res.push(InternalLabel(check_cond_label));
                        let cond_is_false_label = self.label_counter.next().unwrap();
                        let r#while = StmtRequiringEndMarker::While {
                            check_cond_label,
                            after_last_stmt: cond_is_false_label,
                        };
                        res.extend(self.process_rvalue(rv));
                        res.push(TestAndJumpIfZero(cond_is_false_label));

                        let idx = self.stmts_requiring_end_marker.len();
                        self.while_stmt_idxs_stack.push(idx);
                        self.last_breakable_stmt_idxs_stack.push(idx);
                        self.stmts_requiring_end_marker.push(r#while)
                    }

                    FlatNode::Switch(rv) => {
                        res.extend(self.process_rvalue(rv));
                        self.last_breakable_stmt_idxs_stack
                            .push(self.stmts_requiring_end_marker.len());
                        self.stmts_requiring_end_marker
                            .push(StmtRequiringEndMarker::Switch {
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
                                parent_stack_size,
                            } => {
                                self.name_to_stack_range = parent_stack;
                                self.stack_size_in_words = parent_stack_size;
                            }

                            StmtRequiringEndMarker::If {
                                cond_is_false_label,
                                after_both_branches,
                            } => {
                                if let Some(after_both_branches) = after_both_branches {
                                    res.push(InternalLabel(after_both_branches))
                                } else {
                                    res.push(InternalLabel(cond_is_false_label))
                                }
                            }
                            StmtRequiringEndMarker::While {
                                check_cond_label: check_condition_label,
                                after_last_stmt,
                            } => {
                                let wh = self.while_stmt_idxs_stack.pop();
                                let br = self.last_breakable_stmt_idxs_stack.pop();
                                debug_assert!(wh.is_some() && br.is_some());
                                res.push(Jump(check_condition_label));
                                res.push(InternalLabel(after_last_stmt));
                            }

                            StmtRequiringEndMarker::Switch {
                                regular_cases,
                                default_case_label,
                                after_last_stmt,
                            } => {
                                self.last_breakable_stmt_idxs_stack.pop();
                                let mut case_table = vec![];
                                for CaseWithVal { case_val, label_no } in regular_cases {
                                    case_table.push(CaseJmpIfEq { case_val, label_no });
                                }
                                if let Some(default_case_label) = default_case_label {
                                    case_table.push(Jump(default_case_label));
                                } else {
                                    case_table.push(Jump(after_last_stmt));
                                }
                                res.splice(0..0, case_table);
                                res.push(InternalLabel(after_last_stmt));
                            }
                        }
                        return (res, i + 1);
                    }
                    _ => unreachable!(),
                }
                let (nodes, adv) = self.process_slice(curr_fn, &prog_slice[i + 1..]);
                res.extend(nodes);
                i += adv;
            }
            i += 1;
        }
        (res, i)
    }

    fn reset_everything_except_str_pool(&mut self) {
        let str_pool = self.str_pool.take();
        *self = Self::new(self.compiler_options, self.global_definitions);
        self.str_pool = Cell::new(str_pool);
    }

    fn process_function(
        &mut self,
        prog_slice: &'a [(FlatNodeAndPos, LocalScope)],
    ) -> (Vec<IntermRepr>, usize) {
        self.reset_everything_except_str_pool();
        let mut res = vec![];
        if let FlatNode::Def(FlatDefinition {
            name,
            info: FlatDefinitionInfo::Function { params },
        }) = &prog_slice[0].0.node
        {
            self.params_order = params
                .iter()
                .enumerate()
                .map(|(i, (param, _))| (param, i.try_into().unwrap()))
                .collect();
            res.push(IntermRepr::FnDef(
                name.clone(),
                FnInfo {
                    stack_size: 0,
                    num_of_params: params.len() as u64,
                },
            ))
        } else {
            unreachable!()
        }

        let curr_fn = if let IntermRepr::FnDef(_, info) = &mut res[0] {
            info
        } else {
            unreachable!()
        };
        let (rest_of_nodes, adv) = self.process_slice(curr_fn, &prog_slice[1..]);
        res.extend(rest_of_nodes);
        res.push(IntermRepr::Ret);
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
            NameOrConstant::Constant(Constant::Number(n)) => Ival::Number(*n),
            NameOrConstant::Constant(Constant::String(s)) => {
                let idx = self.try_add_to_str_pool(s);
                Ival::Name(Label::PooledStr(idx))
            }
        }
    }

    pub fn run(&mut self, program: &'a ScopeTable) -> (Vec<IntermRepr>, HashMap<String, u64>) {
        let mut res = vec![];
        for (name, def) in &program.global {
            match &def.info {
                DefInfo::Variable(VarDefInfo::UserVar { ival }) => {
                    let ival = if let Some(ival) = ival {
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

        for rng in get_fns_ranges(program.local.iter().map(|x| &x.0)) {
            let rng_len = rng.len();
            let (ir, adv) = self.process_function(&program.local[rng]);
            debug_assert_eq!(adv, rng_len);
            res.extend(ir);
        }
        (res, self.str_pool.take())
    }
}
