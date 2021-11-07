use cell::Cell;
use collections::HashMap;
use ops::Range;
use std::*;

use crate::tokenizer::token;
use token::{Constant, TokenPos};

use crate::config::*;
use crate::parser;
use parser::ast;
use parser::ast::flat_ast::*;

#[derive(Debug, Clone)]
pub(crate) enum ProcessedDeclInfo {
    AssumedExternFnCall(DefInfoAndPos),
    Auto { size_if_vec: Option<u64> },
    ExplicitExtern(DefInfoAndPos),
    Label,
    FnParameter { param_no: u64 },
}

#[derive(Debug, Clone)]
pub(crate) enum VarDefInfo {
    StdVar { ival: i64 },
    UserVar { ival: Option<Ival> },
}

#[derive(Debug, Clone)]
pub(crate) enum DefInfo {
    Variable(VarDefInfo),
    Vector { size: u64, ivals: Vec<Ival> },
    Function(FnDef),
}

#[derive(Debug, Clone)]
pub(crate) struct DefInfoAndPos {
    pub(crate) pos_if_user_defined: Option<TokenPos>,
    pub(crate) info: DefInfo,
}

#[derive(Debug, Clone)]
pub(crate) struct DeclInfoAndPos {
    pub(crate) pos: TokenPos,
    pub(crate) info: ProcessedDeclInfo,
}

pub(crate) type LocalScope = HashMap<String, DeclInfoAndPos>;
pub(crate) type GlobalDefinitions = HashMap<String, DefInfoAndPos>;

#[derive(Debug)]
pub(crate) struct ScopeTable {
    pub(crate) global: GlobalDefinitions,
    pub(crate) local: Vec<(FlatNodeAndPos, LocalScope)>,
}

pub(crate) struct Analyzer<'a> {
    pub(crate) source_code: &'a str,
    global_scope: Cell<GlobalDefinitions>,
}

type FunctionNodesIndexesRanges = Vec<Range<usize>>;

pub(crate) fn get_fns_ranges<'a, I: ExactSizeIterator>(program: I) -> FunctionNodesIndexesRanges
where
    I: Iterator<Item = &'a FlatNodeAndPos>,
{
    let mut res = vec![];
    let mut fn_begins_at = None;

    let len = program.len();
    for (i, node) in program.enumerate() {
        if let FlatNode::Def(def) = &node.node {
            if let Some(fn_begins_at) = fn_begins_at {
                res.push(fn_begins_at..i);
            }
            fn_begins_at = None;

            if let FlatDefinitionInfo::Function { .. } = def.info {
                debug_assert!(fn_begins_at.is_none());
                fn_begins_at = Some(i);
            }
        }
    }
    if let Some(fn_begins_at) = fn_begins_at {
        // the last fn, if there was at least one
        res.push(fn_begins_at..len);
    }
    res
}

impl<'a> Analyzer<'a> {
    pub(crate) fn new(source_code: &'a str) -> Self {
        Analyzer {
            source_code,
            global_scope: Default::default(),
        }
    }

    fn add_explicit_decl_to_scope(
        &mut self,
        decl: &FlatDeclaration,
        pos: TokenPos,
        local_scope: &mut LocalScope,
        curr_fn: (&str, TokenPos),
        block_starts_at: TokenPos,
        issues: &mut Vec<Issue>,
    ) {
        use Issue::*;
        let name = &decl.name;
        enum ExplicitDeclType {
            Auto,
            Extern,
            Label,
        }
        let global_def = self.global_scope.get_mut().get(name);
        let explicit_decl_type;
        let decl_info;
        match &decl.info {
            FlatDeclarationNameInfo::Auto {
                specified_size_if_vec: size_if_vec,
            } => {
                explicit_decl_type = ExplicitDeclType::Auto;
                decl_info = ProcessedDeclInfo::Auto {
                    size_if_vec: {
                        if let Some(vec_sz) = size_if_vec {
                            let s = &vec_sz.constant;
                            Some(match s {
                                Constant::Number(nmb) => *nmb,
                                Constant::String(..) => {
                                    issues.push(VecSizeIsNotANumber {
                                        name: name.clone(),
                                        pos,
                                    });
                                    0
                                }
                            })
                        } else {
                            None
                        }
                    },
                }
            }
            FlatDeclarationNameInfo::Extern => {
                if global_def.is_none() {
                    issues.push(ExternSymbolNotFound {
                        name: name.clone(),
                        extern_pos: pos,
                    });
                    return;
                }
                explicit_decl_type = ExplicitDeclType::Extern;
                decl_info = ProcessedDeclInfo::ExplicitExtern(global_def.unwrap().clone());
            }
            FlatDeclarationNameInfo::Label => {
                explicit_decl_type = ExplicitDeclType::Label;
                decl_info = ProcessedDeclInfo::Label;
            }
        };
        let decl_info_and_pos = DeclInfoAndPos {
            pos,
            info: decl_info,
        };

        if let Some(global_def) = global_def {
            if let ExplicitDeclType::Auto | ExplicitDeclType::Label = explicit_decl_type {
                issues.push(DeclShadowsGlobalDef {
                    decl: (name.clone(), decl_info_and_pos.clone()),
                    global_def: global_def.clone(),
                })
            }
        }
        if let Some(prev_decl) = local_scope.insert(name.clone(), decl_info_and_pos.clone()) {
            use ProcessedDeclInfo::*;
            match prev_decl.info {
                ExplicitExtern(_) | AssumedExternFnCall(_) | Auto { .. } => {
                    let prev_decl_is_in_this_block = prev_decl.pos >= block_starts_at;
                    if let AssumedExternFnCall(_) | ExplicitExtern(_) = prev_decl.info {
                        if let ExplicitDeclType::Extern = explicit_decl_type {
                            issues.push(UnnecessaryImport {
                                curr_decl: (name.clone(), pos),
                                prev_import_pos: prev_decl.pos,
                            });
                            return;
                        }
                    }
                    if prev_decl_is_in_this_block {
                        issues.push(NameRedeclared {
                            decl: (name.clone(), decl_info_and_pos),
                            prev_decl,
                        })
                    } else {
                        issues.push(DeclShadowsPrevious {
                            decl: (name.clone(), decl_info_and_pos),
                            prev_decl,
                        })
                    }
                }
                ProcessedDeclInfo::Label => issues.push(NameRedeclared {
                    decl: (name.clone(), decl_info_and_pos),
                    prev_decl,
                }),
                ProcessedDeclInfo::FnParameter { .. } => issues.push(DeclShadowsFnParameter {
                    param_pos: decl_info_and_pos.pos,
                    decl: (name.clone(), decl_info_and_pos),
                    fn_def: (curr_fn.0.to_string(), curr_fn.1),
                }),
            }
        }
    }

    fn set_global_scope(&mut self, program: &[FlatNodeAndPos], issues: &mut Vec<Issue>) {
        let mut names_needed_for_init = HashMap::<&str, TokenPos>::new();
        let standard_scope = Cell::new(
            get_standard_library_names()
                .into_iter()
                .map(|(name, info)| {
                    (
                        name.to_string(),
                        DefInfoAndPos {
                            pos_if_user_defined: None,
                            info: match info {
                                StdNameInfo::Variable { ival } => {
                                    DefInfo::Variable(VarDefInfo::StdVar { ival })
                                }
                                StdNameInfo::Function(fn_def) => DefInfo::Function(fn_def),
                            },
                        },
                    )
                })
                .collect(),
        );
        self.global_scope = standard_scope;

        for node in program.iter() {
            if let FlatNode::Def(def) = &node.node {
                let pos = node.pos;
                let name = &def.name;
                let curr_def = (name.clone(), pos);
                let info = match &def.info {
                    FlatDefinitionInfo::Variable { ival } => {
                        if let Some(Ival {
                            pos: ival_pos,
                            value: NameOrConstant::Name(init_name),
                        }) = ival
                        {
                            if !names_needed_for_init.contains_key(init_name as &str) {
                                names_needed_for_init.insert(init_name, *ival_pos);
                            }
                        }
                        DefInfo::Variable(VarDefInfo::UserVar { ival: ival.clone() })
                    }

                    FlatDefinitionInfo::Vector {
                        ivals,
                        specified_size,
                    } => {
                        for ival in ivals {
                            if let NameOrConstant::Name(init_name) = &ival.value {
                                if !names_needed_for_init.contains_key(init_name as &str) {
                                    names_needed_for_init.insert(init_name, ival.pos);
                                }
                            }
                        }
                        let ivals_len = ivals.len();

                        let specified_size = if let Some(specified_size) = specified_size {
                            let s = &specified_size.constant;
                            match s {
                                Constant::Number(nmb) => Some(nmb),
                                Constant::String(..) => {
                                    issues.push(Issue::VecSizeIsNotANumber {
                                        name: name.clone(),
                                        pos,
                                    });
                                    None
                                }
                            }
                        } else {
                            None
                        };

                        let actual_size = if let Some(specified_size) = specified_size {
                            let specified_size_plus_1 = (1 + specified_size).try_into().unwrap();
                            if ivals_len > specified_size_plus_1 {
                                issues.push(Issue::VecTooManyIvals {
                                    vec_def: (name.clone(), pos),
                                    ivals_len,
                                    specified_size_plus_1,
                                })
                            }
                            cmp::max(ivals_len, specified_size_plus_1)
                        } else {
                            if ivals_len == 0 {
                                issues.push(Issue::VecWithNoSizeAndIvals(name.clone(), pos));
                            }
                            cmp::max(ivals_len, 1)
                        };
                        DefInfo::Vector {
                            size: actual_size.try_into().unwrap(),
                            ivals: ivals.clone(),
                        }
                    }

                    FlatDefinitionInfo::Function { params } => DefInfo::Function(FnDef {
                        num_of_params: NumberOfParameters::Exact(params.len()),
                    }),
                };
                let info = DefInfoAndPos {
                    pos_if_user_defined: Some(pos),
                    info,
                };
                if let Some(prev_def) = self.global_scope.get_mut().insert(name.clone(), info) {
                    issues.push(Issue::NameRedefined { curr_def, prev_def })
                }
            }
        }

        for (name, pos) in names_needed_for_init {
            if !self.global_scope.get_mut().contains_key(name) {
                issues.push(Issue::NameNotDefined {
                    name: name.to_string(),
                    pos,
                })
            }
        }
    }

    fn process_lvalue<'b>(
        &self,
        lv: &'b ast::LvalueNode,
        local_scope: &mut LocalScope,
        issues: &mut Vec<Issue>,
    ) -> Vec<&'b ast::RvalueNode> {
        match &lv.lvalue {
            ast::Lvalue::Name(name) => {
                let info = local_scope.get(name);
                if info.is_none() {
                    // TODO: suggestions
                    issues.push(Issue::NameNotDefined {
                        name: name.clone(),
                        pos: lv.position,
                    })
                }
                vec![]
            }
            ast::Lvalue::DerefRvalue(rv) => {
                vec![rv]
            }
            ast::Lvalue::Indexing { vector, index } => {
                vec![vector, index]
            }
        }
    }

    fn process_rvalue(
        &mut self,
        rv: &ast::Rvalue,
        local_scope: &mut LocalScope,
        issues: &mut Vec<Issue>,
    ) {
        use ast::Rvalue::*;
        use ast::*;

        let rvalues_to_process = match rv {
            Constant(_) => vec![],
            Lvalue(lv) => self.process_lvalue(lv, local_scope, issues),
            Assign { lhs, rhs, .. } => {
                let mut rvs_to_pr = self.process_lvalue(lhs, local_scope, issues);
                rvs_to_pr.push(rhs);
                rvs_to_pr
            }
            IncDec(node) => self.process_lvalue(&node.lvalue, local_scope, issues),

            Unary(_, rv) => vec![rv],
            TakeAddress(lv) => self.process_lvalue(lv, local_scope, issues),
            Binary { lhs, rhs, .. } => vec![lhs, rhs],
            ConditionalExpression {
                condition,
                on_true,
                on_false,
                ..
            } => {
                vec![condition, on_true, on_false]
            }

            BracketedExpression(expr) => vec![expr],
            FunctionCall(fn_call) => {
                let mut rvalue_to_process = vec![];
                if let Rvalue::Lvalue(LvalueNode {
                    position,
                    lvalue: ast::Lvalue::Name(name),
                }) = &*fn_call.fn_name.rvalue
                {
                    let global_name_info = self.global_scope.get_mut().get(name);
                    if !local_scope.contains_key(name) {
                        if let Some(global_name_info) = global_name_info {
                            local_scope.insert(
                                name.clone(),
                                DeclInfoAndPos {
                                    pos: *position,
                                    info: ProcessedDeclInfo::AssumedExternFnCall(
                                        global_name_info.clone(),
                                    ),
                                },
                            );
                        } else {
                            issues.push(Issue::NameNotDefined {
                                name: name.clone(),
                                pos: *position,
                            })
                        }
                    }
                    if let Some(DeclInfoAndPos { info, .. }) = local_scope.get(name) {
                        if let ProcessedDeclInfo::AssumedExternFnCall(def)
                        | ProcessedDeclInfo::ExplicitExtern(def) = info
                        {
                            if let DefInfoAndPos {
                                info: DefInfo::Function(FnDef { num_of_params, .. }),
                                ..
                            } = def
                            {
                                let actual_param_count = fn_call.arguments.len();
                                let fn_pos = position;
                                let invalid_param_count = match num_of_params {
                                    NumberOfParameters::Exact(n) => actual_param_count != *n,
                                    NumberOfParameters::AtLeast(n) => actual_param_count < *n,
                                };
                                if invalid_param_count {
                                    issues.push(Issue::InvalidParameterCount {
                                        expected: *num_of_params,
                                        actual: actual_param_count,
                                        pos: *fn_pos,
                                    })
                                }
                            }
                        }
                    }
                } else {
                    rvalue_to_process.push(&fn_call.fn_name)
                }
                rvalue_to_process.append(&mut fn_call.arguments.iter().collect());
                rvalue_to_process
            }
        };
        for x in rvalues_to_process {
            self.process_rvalue(&x.rvalue, local_scope, issues)
        }
    }

    fn validate_function(
        &mut self,
        function: &[FlatNodeAndPos],
        issues: &mut Vec<Issue>,
    ) -> Vec<LocalScope> {
        let fn_name = if let FlatNode::Def(FlatDefinition {
            name,
            info: FlatDefinitionInfo::Function { .. },
        }) = &function[0].node
        {
            name.as_str()
        } else {
            unreachable!()
        };
        let fn_starts_at = function[0].pos;

        let mut local_scope = LocalScope::new();
        function
            .iter()
            .filter_map(|node| {
                if let FlatNode::Decl(
                    decl
                    @
                    FlatDeclaration {
                        info: FlatDeclarationNameInfo::Label,
                        ..
                    },
                ) = &node.node
                {
                    Some((decl, node.pos))
                } else {
                    None
                }
            })
            .for_each(|(label_decl, pos)| {
                self.add_explicit_decl_to_scope(
                    label_decl,
                    pos,
                    &mut local_scope,
                    (fn_name, function[0].pos),
                    function[0].pos,
                    issues,
                )
            });

        let mut res = vec![];
        let mut scope_stack = vec![];
        let mut breakable_stmt_stack = vec![];
        let mut while_stmt_stack = vec![];
        let mut compound_stack = vec![];
        let mut cases_stack = vec![];

        for (i, node) in function.iter().enumerate() {
            let block_starts_at = *compound_stack.last().unwrap_or(&function[0].pos);
            let local_scope = &mut local_scope;

            match &node.node {
                FlatNode::Else => (),
                FlatNode::Break => {
                    if breakable_stmt_stack.is_empty() {
                        issues.push(Issue::UnexpectedKeyword(node.pos))
                    }
                }
                FlatNode::Continue => {
                    if while_stmt_stack.is_empty() {
                        issues.push(Issue::UnexpectedKeyword(node.pos))
                    }
                }

                FlatNode::EndOfStmt { positions_away } => {
                    let restore_because_this_stmt_ended = &function[i - positions_away];

                    if Some(&restore_because_this_stmt_ended.pos) == breakable_stmt_stack.last() {
                        match restore_because_this_stmt_ended.node {
                            FlatNode::Switch(_) => {
                                let cases_for_last_switch = cases_stack.pop();
                                debug_assert!(cases_for_last_switch.is_some());
                            }
                            FlatNode::While(_) => {
                                let last_while = while_stmt_stack.pop();
                                debug_assert!(last_while.is_some());
                            }
                            _ => (),
                        }
                        breakable_stmt_stack.pop();
                    } else if let FlatNode::Compound = restore_because_this_stmt_ended.node {
                        let last_comp_pos = compound_stack.pop();
                        debug_assert!(last_comp_pos.is_some());
                        let parent_scope = scope_stack.pop().unwrap();
                        *local_scope = parent_scope;
                    }
                }

                FlatNode::Decl(decl) => match decl.info {
                    FlatDeclarationNameInfo::Label => (),
                    _ => self.add_explicit_decl_to_scope(
                        decl,
                        node.pos,
                        local_scope,
                        (fn_name, fn_starts_at),
                        block_starts_at,
                        issues,
                    ),
                },
                FlatNode::Compound => {
                    compound_stack.push(node.pos);
                    scope_stack.push(local_scope.clone())
                }
                FlatNode::Rvalue(rv) => {
                    self.process_rvalue(rv, local_scope, issues);
                }
                FlatNode::If(cond) => {
                    self.process_rvalue(cond, local_scope, issues);
                }
                FlatNode::While(cond) => {
                    self.process_rvalue(cond, local_scope, issues);
                    breakable_stmt_stack.push(node.pos);
                    while_stmt_stack.push(node.pos);
                }
                FlatNode::Goto(goto) => {
                    self.process_rvalue(goto, local_scope, issues);
                }
                FlatNode::Switch(var) => {
                    breakable_stmt_stack.push(node.pos);
                    self.process_rvalue(var, local_scope, issues);
                    cases_stack.push(HashMap::new());
                }
                FlatNode::Case(case_val) => {
                    if let Some(cases) = cases_stack.last_mut() {
                        let case_const = match case_val {
                            None => (None, node.pos),
                            Some(c) => (Some(c.constant.clone()), c.position),
                        };
                        let prev = cases.insert(case_const.0, case_const.1);
                        if let Some(prev) = prev {
                            issues.push(Issue::CaseEncounteredTwice {
                                curr: case_const.1,
                                prev,
                            })
                        }
                    } else {
                        issues.push(Issue::UnexpectedKeyword(node.pos))
                    }
                }
                FlatNode::Return(ret) => {
                    if let Some(val) = ret {
                        self.process_rvalue(val, local_scope, issues);
                    }
                }
                FlatNode::Def(FlatDefinition {
                    info: FlatDefinitionInfo::Function { params },
                    ..
                }) => {
                    for (i, (param_name, param_pos)) in params.iter().enumerate() {
                        let decl = DeclInfoAndPos {
                            pos: *param_pos,
                            info: ProcessedDeclInfo::FnParameter {
                                param_no: i.try_into().unwrap(),
                            },
                        };
                        local_scope.insert(param_name.clone(), decl.clone());

                        if let Some(global_def) = self.global_scope.get_mut().get(param_name) {
                            issues.push(Issue::DeclShadowsGlobalDef {
                                decl: (param_name.clone(), decl),
                                global_def: global_def.clone(),
                            });
                        }
                    }
                }
                _ => unreachable!(),
            }
            debug_assert!(i == res.len());
            res.push(local_scope.clone());
        }
        res
    }

    pub(crate) fn run(
        &mut self,
        program: &[FlatNodeAndPos],
        issues: &mut Vec<Issue>,
    ) -> ScopeTable {
        self.set_global_scope(program, issues);
        if !self.global_scope.get_mut().contains_key("main") {
            issues.push(Issue::NoMainFn)
        }
        let mut node_to_scope = vec![LocalScope::default(); program.len()];
        for rng in get_fns_ranges(program.iter()) {
            let local_scopes = self.validate_function(&program[rng.clone()], issues);
            rng.zip(local_scopes.into_iter())
                .for_each(|(idx, local_scope)| node_to_scope[idx] = local_scope);
        }
        ScopeTable {
            global: self.global_scope.take(),
            local: program
                .iter()
                .cloned()
                .zip(node_to_scope.into_iter())
                .collect(),
        }
    }
}
