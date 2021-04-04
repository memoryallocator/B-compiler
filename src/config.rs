use std::*;
use collections::{HashMap, HashSet};
use fmt;
use rc::Rc;
use borrow::Borrow;

use crate::parser;
use parser::ast::*;
use crate::lexical_analyzer::token;
use token::*;
use crate::code_generator::GlobalDefinition;

fn global_definition_source<T: Borrow<Option<TokenPos>>>(x: T) -> String {
    if let Some(x) = x.borrow() {
        x.to_string()
    } else {
        "the standard library".to_string()
    }
}

impl ToString for GlobalDefinition {
    fn to_string(&self) -> String {
        match &self {
            GlobalDefinition::Variable => "variable",
            GlobalDefinition::Function { .. } => "function",
        }.to_string()
    }
}

pub(crate) enum VecDeclOrDef {
    Decl(Rc<AutoDeclarationNode>),
    Def(Rc<VectorDefinitionNode>),
}

pub(crate) enum Error {
    BracketNotOpened(TokenPos),
    BracketNotClosed(TokenPos),
    EmptyTokenStream,
    ParsingError,
    NameNotDefined {
        name: Rc<String>,
        pos: TokenPos,
    },

    NameRedefined { name: Rc<String>, curr_def_pos: TokenPos, prev_def_pos: Option<TokenPos> },
    // NameDeclaredTwice {
    //     curr_decl: Option<Rc<DeclarationNode>>,
    //     prev_decl: Option<Rc<DeclarationNode>>,
    // },

    VecSizeIsString(VecDeclOrDef),
    // LabelRedeclared {
    //     fn_name: Rc<String>,
    //     curr_decl: Rc<LabelDeclarationNode>,
    //     prev_decl: Rc<LabelDeclarationNode>,
    // },
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg =
            match self {
                Error::BracketNotOpened(pos) =>
                    format!("closing bracket in {}, doesn't have a matching opening bracket", pos),
                Error::BracketNotClosed(pos) =>
                    format!("bracket opened in {}, not closed", pos),

                Error::EmptyTokenStream => format!("failed to recognise a single token"),
                Error::ParsingError => format!("failed to parse"),

                Error::NameNotDefined { name, pos } => {
                    let err_msg = format!("name {}, used in {}, is not defined", name, pos);

                    //     if let Some(
                    //         (suggestion, pos)
                    //     ) = suggestion_and_pos {
                    //         err_msg += format!("note: there is a {} {} defined in {}",
                    //                            suggestion.to_string(),
                    //                            name,
                    //                            global_definition_source(pos)).as_str();
                    //     }
                    //
                    err_msg
                }

                _ => todo!()
            };

        write!(f, "{}", msg)
    }
}

pub(crate) enum Warning {
    InitVarWithItself(Rc<DefinitionNode>, TokenPos),
    StandardNameRedefined(Rc<DefinitionNode>),
    VecWithNoSizeAndInits(Rc<VectorDefinitionNode>),
    VecSizeIsNotANumber(Rc<VectorDefinitionNode>),
    FnBodyIsNullStatement(Rc<FunctionDefinitionNode>),
    EmptyCompound(Rc<CompoundStatementNode>),
    // ShadowingGlobalName(Rc<AutoDeclaration>, (GlobalDefinition, Option<TokenPos>)),
    // ShadowingLocalVariable { curr_decl: Rc<AutoDeclaration>, prev_decl: Rc<AutoDeclaration> },
    // ShadowingParameter { curr_decl: Rc<DeclarationNode>, curr_fn: Rc<FunctionDefinitionNode> },
}

impl fmt::Display for Warning {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let msg =
            match self {
                Warning::InitVarWithItself(def_node, same_name_pos) => {
                    match def_node.as_ref() {
                        DefinitionNode::Variable(var_def) => {
                            let var_name = &var_def.name;
                            format!("Variable {}, defined in {}, is initialized with itself at {}",
                                    var_name, var_def.get_position(), same_name_pos)
                        }

                        DefinitionNode::Vector(vec_def) => {
                            let vec_name = &vec_def.name;
                            format!("Vector {}, defined in {}, has its name as one of its initial values ({})",
                                    vec_name, vec_def.position, same_name_pos)
                        }

                        DefinitionNode::Function(_) => unreachable!(),
                    }
                }

                Warning::StandardNameRedefined(def_node) => {
                    let name = def_node.get_name();
                    let name = name.as_str();
                    format!("Standard name {} is redefined in {}", name, def_node.get_position())
                }

                Warning::VecWithNoSizeAndInits(vec_def) => {
                    let name = vec_def.name.as_ref();
                    format!("Vector {}, defined in {}, has neither the size nor the initial values specified",
                            name, vec_def.position)
                }

                Warning::VecSizeIsNotANumber(vec_def) => {
                    let name = vec_def.name.as_ref();
                    format!("The size of vector {}, specified in {}, is not a number",
                            name, vec_def.specified_size.as_ref().unwrap().position)
                }

                Warning::FnBodyIsNullStatement(fn_def) => {
                    let name = fn_def.name.as_ref();
                    format!("Function {}, defined in {}, has empty body",
                            name, fn_def.position)
                }

                Warning::EmptyCompound(comp_stmt) => {
                    format!("The compound statement at {} is empty", comp_stmt.position)
                }

                // Warning::ShadowingGlobalName(def, gl_def_and_pos) => {
                //     let (gl_def, gl_def_pos) = gl_def_and_pos;
                //
                //     format!("Name {}, declared at {}, shadows global name {}, defined in {}",
                //             def.name.as_str(), def.position,
                //             gl_def.to_string(), global_definition_source(gl_def_pos))
                // }
                //
                // Warning::ShadowingLocalVariable { curr_decl, prev_decl } => {
                //     format!("Name {}, declared at {}, shadows local name {}, defined in {}",
                //             curr_decl.name.as_str(), curr_decl.position,
                //             prev_decl.name.as_str(), prev_decl.position)
                // }
            };

        write!(f, "{}", msg)
    }
}

#[allow(non_camel_case_types)]
#[derive(Copy, Clone)]
pub(crate) enum Arch {
    x86_32,
    x86_64,
}

impl fmt::Display for Arch {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arch::x86_32 => write!(f, "x86-32"),
            Arch::x86_64 => write!(f, "x86-64"),
        }
    }
}

#[warn(non_camel_case_types)]
#[derive(Copy, Clone, PartialEq)]
pub(crate) enum PlatformName {
    Linux,
    Bsd,
    Windows,
    MacOs,
}

impl fmt::Display for PlatformName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlatformName::Linux => write!(f, "Linux"),
            PlatformName::Bsd => write!(f, "BSD"),
            PlatformName::Windows => write!(f, "Windows"),
            PlatformName::MacOs => write!(f, "macOS"),
        }
    }
}

#[derive(Copy, Clone)]
pub(crate) struct TargetPlatform {
    pub(crate) platform_name: PlatformName,
    pub(crate) arch: Arch,
}

impl TargetPlatform {
    pub(crate) fn native() -> Self {
        TargetPlatform {
            platform_name: if cfg!(any
            (target_os = "freebsd",
            target_os = "dragonfly",
            target_os = "openbsd",
            target_os = "netbsd")
            ) {
                PlatformName::Bsd
            } else if cfg!(target_os = "linux") {
                PlatformName::Linux
            } else if cfg!(target_os = "windows") {
                PlatformName::Windows
            } else if cfg!(target_os = "macos") {
                PlatformName::MacOs
            } else {
                let default_platform = TargetPlatform::default().platform_name;
                println!("Failed to determine the native OS. Assuming it's {}", default_platform);
                default_platform
            },
            arch: {
                if cfg!(target_pointer_width = "32") {
                    Arch::x86_32
                } else if cfg!(target_pointer_width = "64") {
                    Arch::x86_64
                } else {
                    let default_arch = TargetPlatform::default().arch;
                    println!("Failed to determine the native architecture. Assuming it's {}", default_arch);
                    default_arch
                }
            },
        }
    }
}

impl Default for TargetPlatform {
    fn default() -> Self {
        TargetPlatform {
            platform_name: PlatformName::Linux,
            arch: Arch::x86_64,
        }
    }
}

#[derive(Default, Copy, Clone)]
pub(crate) struct CompilerOptions {
    pub(crate) target_platform: TargetPlatform,
}

pub(crate) fn get_escape_sequences() -> HashMap<String, String> {
    vec![('0', '\0'),
         ('e', 4 as char),  // ASCII EOT, B end of string),
         ('(', '{'),
         (')', '}'),
         ('t', '\t'),
         ('*', '*'),
         ('\'', '\''),
         ('"', '"'),
         ('n', '\n')]
        .into_iter()
        .map(|x| (format!("*{}", x.0), x.1.to_string()))
        .collect()
}

pub(crate) type ReservedSymbolsTable = HashMap<String, ReservedName>;

pub(crate) fn get_reserved_symbols() -> ReservedSymbolsTable {
    use DeclarationSpecifier::*;
    use ControlStatementIdentifier::*;

    vec![
        ("auto", ReservedName::DeclarationSpecifier(Auto)),
        ("extrn", ReservedName::DeclarationSpecifier(Extrn)),
        ("goto", ReservedName::ControlStatement(Goto)),
        ("switch", ReservedName::ControlStatement(Switch)),
        ("case", ReservedName::ControlStatement(Case)),
        ("return", ReservedName::ControlStatement(Return)),
        ("if", ReservedName::ControlStatement(If)),
        ("else", ReservedName::ControlStatement(Else)),
        ("while", ReservedName::ControlStatement(While)),
        ("break", ReservedName::ControlStatement(Break)),
        ("default", ReservedName::ControlStatement(Default)),
        // ("for", SymbolType::Reserved::ControlStatement(For))),
    ].into_iter().map(|x| (x.0.to_string(), x.1)).collect()
}

#[derive(Eq, PartialEq, Hash)]
pub(crate) enum StandardLibraryName {
    Function { fn_name: Rc<String>, parameter_list_length: Option<usize> },
    Variable { var_name: Rc<String> },
}

pub(crate) fn get_standard_library_names() -> HashSet<StandardLibraryName> {
    let mut std_lib_fns = vec![];

    std_lib_fns.append(&mut (|| {
        let io_routines_without_printf = vec![
            ("getchar", 0),
            ("putchar", 1),
            ("openr", 2),
            ("openw", 2),
            ("getstr", 1),
            ("putstr", 1),
            ("system", 1),
            ("close", 1),
            ("flush", 0),
            ("reread", 0)
        ].into_iter()
            .map(|x| (x.0, Some(x.1)))
            .collect();

        let printf = ("printf", None);

        let mut io_routines: Vec<(&str, Option<usize>)> = io_routines_without_printf;
        io_routines.push(printf);

        io_routines
    })());

    std_lib_fns.push(("ioerrors", Some(1)));

    std_lib_fns.append(&mut (|| {
        let mut str_manip: Vec<(&str, Option<usize>)> = vec![
            ("char", 2),
            ("lchar", 3),
            ("getarg", 3),
        ].into_iter()
            .map(|x| (x.0, Some(x.1)))
            .collect();

        str_manip.push(("concat", None));
        str_manip
    })());

    std_lib_fns.append(&mut (|| {
        let other_functions: Vec<(&str, Option<usize>)> = vec![
            ("getvec", 1),
            ("rlsevec", 2),
            ("nargs", 0),
            ("exit", 0)
        ].into_iter()
            .map(|x| (x.0, Some(x.1)))
            .collect();

        other_functions
    })());

    let std_lib_vars: HashSet<StandardLibraryName> = vec!["wr.unit", "rd.unit"].into_iter()
        .map(|var_name|
            StandardLibraryName::Variable { var_name: Rc::new(var_name.to_string()) })
        .collect();

    let mut res = std_lib_vars;
    res
        .extend(
            std_lib_fns.into_iter()
                .map(|x| StandardLibraryName::Function {
                    fn_name: Rc::new(x.0.to_string()),
                    parameter_list_length: x.1,
                }));
    res
}