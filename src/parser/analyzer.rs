use std::*;
use collections::{HashMap, HashSet};
use ops::Range;
use convert::TryInto;

use crate::lexical_analyzer::token;
use token::{TokenPos, Constant};

use crate::config::*;
use crate::parser;
use parser::ast::flat_ast::*;
use parser::FlatAst;

#[derive(Debug, Clone)]
enum ProcessedDeclInfo {
    AssumedExternFnCall,
    Auto { size_if_vec: Option<u64> },
    ExplicitExtern,
    Label,
    FnParameter,
}

#[derive(Debug, Clone)]
enum VarDefInfo {
    StdVar { ival: isize },
    UserVar { ival: Option<Ival> },
}

#[derive(Debug, Clone)]
enum DefInfo {
    Variable(VarDefInfo),
    Vector { size: usize, ivals: Vec<Ival> },
    Function { num_of_params: NumberOfParameters },
}

#[derive(Debug, Clone)]
pub(crate) struct DefInfoAndPos {
    pub(crate) pos_if_user_defined: Option<TokenPos>,
    info: DefInfo,
}

#[derive(Debug, Clone)]
pub(crate) struct DeclInfoAndPos {
    pos: TokenPos,
    info: ProcessedDeclInfo,
}

type LocalScope = HashMap<String, DeclInfoAndPos>;
type GlobalDefinitions = HashMap<String, DefInfoAndPos>;

#[derive(Debug)]
pub(crate) struct ScopeTable {
    global: GlobalDefinitions,
    local: Vec<(FlatNodeAndPos, LocalScope)>,
}

pub(crate) struct Analyzer<'a> {
    pub(crate) source_code: &'a str,
}

type FunctionNodesIndexesRanges = Vec<Range<usize>>;

fn add_explicit_decl_to_scope(
    decl: &FlatDeclaration, pos: TokenPos,
    local_scope: &mut LocalScope, global_scope: &GlobalDefinitions,
    curr_fn: (&str, TokenPos), block_starts_at: TokenPos,
    issues: &mut Vec<Issue>,
) {
    use Issue::*;
    let name = &decl.name;
    enum ExplicitDeclType {
        Auto,
        Extern,
        Label,
    }
    let global_def = global_scope.get(name);
    let explicit_decl_type;
    let decl_info;
    match &decl.info {
        FlatDeclarationNameInfo::Auto { size_if_vec } => {
            explicit_decl_type = ExplicitDeclType::Auto;
            decl_info = ProcessedDeclInfo::Auto {
                size_if_vec: {
                    if let Some(vec_sz) = size_if_vec {
                        let s = &vec_sz.constant;
                        Some(match s {
                            Constant::Number(nmb) => *nmb,
                            Constant::Char(s) | Constant::String(s) => {
                                issues.push(VecSizeIsNotANumber {
                                    vec_def: (name.clone(), pos),
                                    size: s.clone(),
                                });
                                0
                            }
                        })
                    } else {
                        None
                    }
                }
            }
        }
        FlatDeclarationNameInfo::Extern => {
            if global_def.is_none() {
                issues.push(ExternSymbolNotFound { name: name.clone(), extern_pos: pos })
            }
            explicit_decl_type = ExplicitDeclType::Extern;
            decl_info = ProcessedDeclInfo::ExplicitExtern;
        }
        FlatDeclarationNameInfo::Label => {
            explicit_decl_type = ExplicitDeclType::Label;
            decl_info = ProcessedDeclInfo::Label;
        }
    };
    let decl_info_and_pos = DeclInfoAndPos { pos, info: decl_info };

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
            ExplicitExtern | AssumedExternFnCall | Auto { .. } => {
                let prev_decl_is_in_this_block = prev_decl.pos >= block_starts_at;
                if let AssumedExternFnCall | ExplicitExtern = prev_decl.info {
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
                    issues.push(DeclarationShadowsPrevious {
                        decl: (name.clone(), decl_info_and_pos),
                        prev_decl,
                    })
                }
            }
            ProcessedDeclInfo::Label => {
                issues.push(NameRedeclared {
                    decl: (name.clone(), decl_info_and_pos),
                    prev_decl,
                })
            }
            ProcessedDeclInfo::FnParameter => {
                issues.push(DeclarationShadowsFnParameter {
                    param_pos: decl_info_and_pos.pos,
                    decl: (name.clone(), decl_info_and_pos),
                    fn_def: (curr_fn.0.to_string(), curr_fn.1),
                })
            }
        }
    }
}

impl Analyzer<'_> {
    fn get_global_scope_and_fns_ranges(
        program: &FlatAst,
        issues: &mut Vec<Issue>,
    ) -> (GlobalDefinitions, FunctionNodesIndexesRanges) {
        let mut names_needed_for_init = HashMap::<&str, TokenPos>::new();
        let mut fn_nodes_indexes_ranges = vec![];
        let mut fn_begins_at = None;

        let standard_scope = get_standard_library_names()
            .into_iter()
            .map(|(name, info)|
                (name.to_string(),
                 DefInfoAndPos {
                     pos_if_user_defined: None,
                     info: match info {
                         StdNameInfo::Variable { ival } =>
                             DefInfo::Variable(VarDefInfo::StdVar { ival }),
                         StdNameInfo::Function { num_of_params } =>
                             DefInfo::Function { num_of_params }
                     },
                 }
                ))
            .collect();
        let mut global_scope: GlobalDefinitions = standard_scope;

        for (i, node) in program.iter().enumerate() {
            if let FlatNode::Def(def) = &node.node {
                if let Some(fn_begins_at) = fn_begins_at {
                    fn_nodes_indexes_ranges.push(fn_begins_at..i);
                }
                fn_begins_at = None;

                let pos = node.pos;
                let name = &def.name;
                let curr_def = (name.clone(), pos);
                let info = match &def.info {
                    FlatDefinitionNameInfo::Variable { ival } => {
                        if let Some(
                            Ival { pos: ival_pos, value: NameOrConstant::Name(init_name) }
                        ) = ival {
                            if !names_needed_for_init.contains_key(&init_name as &str) {
                                names_needed_for_init.insert(init_name, ival_pos.clone());
                            }
                        }
                        DefInfo::Variable(VarDefInfo::UserVar { ival: ival.clone() })
                    }

                    FlatDefinitionNameInfo::Vector { ivals, specified_size } => {
                        for ival in ivals {
                            if let NameOrConstant::Name(init_name) = &ival.value {
                                if !names_needed_for_init.contains_key(init_name as &str) {
                                    names_needed_for_init.insert(init_name, ival.pos);
                                }
                            }
                        }
                        let ivals_len = ivals.len();

                        let specified_size =
                            if let Some(specified_size) = specified_size {
                                let s = &specified_size.constant;
                                match s {
                                    Constant::Number(nmb) => Some(nmb),
                                    Constant::Char(s) | Constant::String(s) => {
                                        issues.push(Issue::VecSizeIsNotANumber {
                                            vec_def: (name.clone(), pos),
                                            size: s.clone(),
                                        });
                                        None
                                    }
                                }
                            } else {
                                None
                            };

                        let actual_size =
                            if let Some(specified_size) = specified_size {
                                let specified_size_plus_1 = (1 + specified_size)
                                    .try_into()
                                    .unwrap();
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
                                    issues.push(Issue::VecWithNoSizeAndInits(name.clone(), pos));
                                }
                                cmp::max(ivals_len, 1)
                            };
                        DefInfo::Vector {
                            size: actual_size,
                            ivals: ivals.clone(),
                        }
                    }

                    FlatDefinitionNameInfo::Function { params } => {
                        assert!(fn_begins_at.is_none());
                        fn_begins_at = Some(i);
                        DefInfo::Function {
                            num_of_params: NumberOfParameters::Exact(params.len()),
                        }
                    }
                };
                let info = DefInfoAndPos { pos_if_user_defined: Some(pos), info };
                if let Some(prev_def) = global_scope.insert(name.clone(), info) {
                    issues.push(Issue::NameRedefined { curr_def, prev_def })
                }
            }
        }
        if let Some(fn_begins_at) = fn_begins_at {  // the last fn, if there was at least one
            fn_nodes_indexes_ranges.push(fn_begins_at..program.len());
        }
        drop(fn_begins_at);

        for (name, pos) in names_needed_for_init {
            if !global_scope.contains_key(name) {
                issues.push(Issue::NameNotDefined { name: name.to_string(), pos })
            }
        }
        (global_scope, fn_nodes_indexes_ranges)
    }

    fn process_rvalue(
        rv: &parser::ast::Rvalue,
        local_scope: &mut LocalScope,
        global_scope: &GlobalDefinitions,
        issues: &mut Vec<Issue>,
    ) {
        use parser::ast;
        use ast::*;
        use ast::Rvalue::*;

        fn process_lvalue<'a>(lv: &'a LvalueNode,
                              local_scope: &mut LocalScope,
                              _global_scope: &GlobalDefinitions,
                              issues: &mut Vec<Issue>,
        ) -> Vec<&'a RvalueNode> {
            match &lv.lvalue {
                ast::Lvalue::Name(name) => {
                    if !local_scope.contains_key(name.as_str()) {
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

        let rvalues_to_process =
            match rv {
                Constant(_) => vec![],
                Lvalue(lv) => {
                    process_lvalue(&lv, local_scope, global_scope, issues)
                }
                Assign { lhs, rhs, .. } => {
                    let mut rvs_to_pr = process_lvalue(&lhs, local_scope, global_scope, issues);
                    rvs_to_pr.push(rhs);
                    rvs_to_pr
                }
                IncDec(node) => {
                    process_lvalue(&node.lvalue, local_scope, global_scope, issues)
                }

                Unary(_, rv) => vec![rv],
                TakeAddress(lv) => {
                    process_lvalue(&lv, local_scope, global_scope, issues)
                }
                Binary { lhs, rhs, .. } => vec![lhs, rhs],
                ConditionalExpression { condition, on_true, on_false, .. } => {
                    vec![condition, on_true, on_false]
                        .into_iter()
                        .collect()
                }

                BracketedExpression(expr) => vec![expr],
                FunctionCall(fn_call) => {
                    let mut rvs_to_pr = vec![];
                    if let Rvalue::Lvalue(
                        LvalueNode {
                            position,
                            lvalue: ast::Lvalue::Name(name),
                        }
                    ) = &*fn_call.fn_name.rvalue {
                        if !local_scope.contains_key(name.as_str()) {
                            if global_scope.contains_key(name.as_str()) {
                                local_scope.insert(name.clone(),
                                                   DeclInfoAndPos {
                                                       pos: *position,
                                                       info: ProcessedDeclInfo::AssumedExternFnCall,
                                                   });
                            } else {
                                issues.push(Issue::NameNotDefined {
                                    name: name.clone(),
                                    pos: *position,
                                })
                            }
                        }
                    } else {
                        rvs_to_pr.push(&fn_call.fn_name)
                    }
                    rvs_to_pr.append(
                        &mut fn_call.arguments
                            .iter()
                            .collect());
                    rvs_to_pr
                }
            };
        for x in rvalues_to_process {
            Analyzer::process_rvalue(&*x.rvalue, local_scope, global_scope, issues)
        }
    }

    fn validate_function(
        function: &[FlatNodeAndPos],
        global_scope: &GlobalDefinitions,
        issues: &mut Vec<Issue>,
    ) -> Vec<LocalScope> {
        let fn_name =
            if let FlatNode::Def(
                FlatDefinition {
                    name,
                    info: FlatDefinitionNameInfo::Function { .. }
                }
            ) = &function[0].node {
                name.as_str()
            } else {
                unreachable!()
            };
        let fn_starts_at = function[0].pos;

        let mut local_scope = LocalScope::new();
        let labels: Vec<(&FlatDeclaration, TokenPos)> = function
            .iter()
            .filter_map(|node|
                if let FlatNode::Decl(
                    decl @ FlatDeclaration { info: FlatDeclarationNameInfo::Label, .. }
                ) = &node.node {
                    Some((decl, node.pos))
                } else {
                    None
                })
            .collect();

        for (label_decl, pos) in labels.into_iter() {
            add_explicit_decl_to_scope(label_decl, pos,
                                       &mut local_scope, global_scope,
                                       (fn_name, function[0].pos), function[0].pos,
                                       issues);
        }

        let mut res = vec![];
        let mut scope_stack = vec![];
        let mut breakable_stmt_stack = vec![];
        let mut compound_stack = vec![];
        let mut cases_stack = vec![];

        for (i, node) in function.iter().enumerate() {
            let block_starts_at = *compound_stack.last().unwrap_or(&function[0].pos);
            let local_scope = &mut local_scope;

            match &node.node {
                FlatNode::Else | FlatNode::Break => (),

                FlatNode::EndOfStmt { positions_away } => {
                    let restore_because_this_stmt_ended = &function[i - positions_away];

                    if Some(&restore_because_this_stmt_ended.pos) == breakable_stmt_stack.last() {
                        if let FlatNode::Switch(_) = restore_because_this_stmt_ended.node {
                            assert!(cases_stack.pop().is_some());
                        }
                        breakable_stmt_stack.pop();
                    } else if let FlatNode::Compound = restore_because_this_stmt_ended.node {
                        assert!(compound_stack.pop().is_some());
                        let parent_scope = scope_stack.pop().unwrap();
                        *local_scope = parent_scope;
                    }
                }

                FlatNode::Decl(decl) => {
                    match decl.info {
                        FlatDeclarationNameInfo::Label => continue,
                        _ => {
                            add_explicit_decl_to_scope(decl, node.pos,
                                                       local_scope, global_scope,
                                                       (fn_name, fn_starts_at),
                                                       block_starts_at,
                                                       issues)
                        }
                    }
                }
                FlatNode::Compound => {
                    compound_stack.push(node.pos);
                    scope_stack.push(local_scope.clone())
                }
                FlatNode::Rvalue(rv) => {
                    Analyzer::process_rvalue(rv, local_scope, global_scope, issues);
                }
                FlatNode::If(cond) => {
                    Analyzer::process_rvalue(cond, local_scope, global_scope, issues);
                }
                FlatNode::While(cond) => {
                    Analyzer::process_rvalue(cond, local_scope, global_scope, issues);
                    breakable_stmt_stack.push(node.pos);
                }
                FlatNode::Goto(goto) => {
                    Analyzer::process_rvalue(goto, local_scope, global_scope, issues);
                }
                FlatNode::Switch(var) => {
                    Analyzer::process_rvalue(var, local_scope, global_scope, issues);
                    cases_stack.push(HashSet::new());
                }
                FlatNode::Case(case_val) => {
                    if let Some(cases) = cases_stack.last_mut() {
                        if !cases.insert(case_val) {
                            issues.push(Issue::CaseEncounteredTwice(
                                if let Some(cs) = case_val {
                                    cs.position
                                } else {
                                    node.pos
                                }
                            ))
                        }
                    } else {
                        issues.push(Issue::UnexpectedKeyword(node.pos))
                    }
                }
                FlatNode::Return(ret) => {
                    if let Some(val) = ret {
                        Analyzer::process_rvalue(val, local_scope, global_scope, issues);
                    }
                }
                FlatNode::Def(
                    FlatDefinition {
                        info: FlatDefinitionNameInfo::Function { params }, ..
                    }
                ) => {
                    for (param_name, param_pos) in params {
                        let decl = DeclInfoAndPos {
                            pos: *param_pos,
                            info: ProcessedDeclInfo::FnParameter,
                        };
                        local_scope.insert(param_name.clone(), decl.clone());

                        if let Some(global_def) = global_scope.get(param_name) {
                            issues.push(Issue::DeclShadowsGlobalDef {
                                decl: (param_name.clone(), decl),
                                global_def: global_def.clone(),
                            });
                        }
                    }
                }
                _ => unreachable!()
            }
            res.push(local_scope.clone());
        }
        res
    }

    pub(crate) fn run(&self, program: FlatAst, issues: &mut Vec<Issue>) -> ScopeTable {
        let (global_scope,
            fn_nodes_indexes_ranges) = Analyzer::get_global_scope_and_fns_ranges(&program,
                                                                                 issues);
        let mut local_scopes = vec![LocalScope::default(); program.len()];
        for rng in fn_nodes_indexes_ranges {
            rng.clone().zip(Analyzer::validate_function(&program[rng], &global_scope, issues)
                .into_iter())
                .for_each(|(idx, local_scope)| {
                    local_scopes[idx] = local_scope
                });
        }
        ScopeTable {
            global: global_scope,
            local: program.into_iter()
                .zip(local_scopes.into_iter())
                .collect(),
        }
    }
}