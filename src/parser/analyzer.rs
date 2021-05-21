use std::*;
use collections::HashMap;

use crate::parser::ast::flat_ast::*;
use crate::parser::{FlatAst, DefinedOrImportedHere};

use crate::config::{Issue, VarOrVecOrFnInfo, get_standard_library_names, NumberOfParameters};
use crate::lexical_analyzer::token::{TokenPos, ConstantType};

pub(crate) struct NameInfo {
    defined_or_imported_here: DefinedOrImportedHere,
    info: VarOrVecOrFnInfo,
}

impl From<&DefinedOrImportedHere> for Option<TokenPos> {
    fn from(x: &DefinedOrImportedHere) -> Self {
        if let DefinedOrImportedHere::Node(node) = x {
            Some(node.pos)
        } else {
            None
        }
    }
}

type Scope = HashMap<String, NameInfo>;

pub(crate) type ScopeTable = Vec<(FlatNodeAndPos, Scope)>;

pub(crate) struct Analyzer<'a> {
    pub(crate) source_code: &'a str,
}

type FunctionsStartAt = Vec<usize>;

impl Analyzer<'_> {
    fn get_global_scope(program: &FlatAst) -> (Vec<Issue>, Scope, FunctionsStartAt) {
        let mut issues = Vec::<Issue>::new();
        let mut names_needed_for_init = HashMap::<&str, TokenPos>::new();
        let mut functions_start_at = FunctionsStartAt::new();

        let standard_scope = get_standard_library_names();
        let mut scope: Scope = standard_scope
            .into_iter()
            .map(|x| (x.name,
                      NameInfo {
                          defined_or_imported_here: DefinedOrImportedHere::StandardLibrary,
                          info: x.info,
                      }))
            .collect();

        for (i, node) in program.iter().enumerate() {
            if let FlatNode::Def(def) = &node.node {
                let pos = node.pos;
                let (name, info) = match def {
                    FlatDefinition::VarDef { name, initial_value } => {
                        if let Some(
                            Ival { value: NameOrConstant::Name(init_name), pos }
                        ) = initial_value {
                            if !names_needed_for_init.contains_key(init_name as &str) {
                                names_needed_for_init.insert(init_name, *pos);
                            }
                        }
                        (name, VarOrVecOrFnInfo::Variable)
                    }

                    FlatDefinition::VecDef { name, initial_values, specified_size } => {
                        for ival in initial_values {
                            if let NameOrConstant::Name(init_name) = &ival.value {
                                if !names_needed_for_init.contains_key(init_name as &str) {
                                    names_needed_for_init.insert(init_name, ival.pos);
                                }
                            }
                        }
                        let initial_values_len = initial_values.len();

                        let specified_size =
                            if let Some(specified_size) = specified_size {
                                let s = &specified_size.constant;
                                match s.constant_type {
                                    ConstantType::Octal => {
                                        assert_eq!(s.value.chars().nth(0), Some('0'));
                                        if let Ok(size) = usize::from_str_radix(&s.value, 8) {
                                            Some(size)
                                        } else {
                                            issues.push(Issue::IntegerConstantIsTooLong(
                                                s.value.clone(), specified_size.position));
                                            None
                                        }
                                    }
                                    ConstantType::Decimal => {
                                        if let Ok(size) = s.value.parse() {
                                            Some(size)
                                        } else {
                                            issues.push(Issue::IntegerConstantIsTooLong(
                                                s.value.clone(), specified_size.position));
                                            None
                                        }
                                    }
                                    _ => {
                                        issues.push(Issue::VecSizeIsNotANumber {
                                            vec_def: (name.clone(), pos),
                                            size: s.value.clone(),
                                        });
                                        None
                                    }
                                }
                            } else {
                                None
                            };

                        let actual_size =
                            if let Some(specified_size) = specified_size {
                                if initial_values_len > 1 + specified_size {
                                    issues.push(Issue::VecTooManyIvals {
                                        vec_def: (name.clone(), pos),
                                        ivals_len: initial_values_len,
                                        specified_size_plus_1: 1 + specified_size,
                                    })
                                }
                                cmp::max(initial_values_len, 1 + specified_size)
                            } else {
                                if initial_values_len == 0 {
                                    issues.push(Issue::VecWithNoSizeAndInits(name.clone(), pos));
                                }
                                cmp::max(initial_values_len, 1)
                            };
                        (name, VarOrVecOrFnInfo::Vector { size: actual_size })
                    }

                    FlatDefinition::FnDef { name, parameters } => {
                        functions_start_at.push(i);
                        (name, VarOrVecOrFnInfo::Function {
                            number_of_parameters: NumberOfParameters::Exact(parameters.len())
                        })
                    }
                };
                let name_info = NameInfo {
                    defined_or_imported_here: DefinedOrImportedHere::Node(node.clone()),
                    info,
                };
                if let Some(info) = scope.insert(name.clone(), name_info) {
                    issues.push(Issue::NameRedefined {
                        curr_def: (name.clone(), pos),
                        prev_def_pos: Option::from(&info.defined_or_imported_here),
                    })
                }
            }
        }
        for (name, pos) in names_needed_for_init {
            if !scope.contains_key(name) {
                issues.push(Issue::NameNotDefined { name: name.to_string(), pos })
            }
        }
        (issues, scope, functions_start_at)
    }

    fn validate_function(
        program: &FlatAst,
        fn_starts_at: usize,
        global_scope: &Scope,
    ) -> (Vec<Issue>, ScopeTable) {
        todo!()
    }

    pub(crate) fn run(&self, program: &FlatAst) -> (Vec<Issue>, Result<ScopeTable, ()>) {
        let (issues, global_scope, fns_start) = Analyzer::get_global_scope(program);
        todo!()
    }
}