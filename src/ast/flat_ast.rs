use std::*;

use crate::ast::*;
use crate::lexical_analyzer::token;
use token::TokenPos;

enum NameOrConstant {
    Name(String),
    Constant(token::Constant),
}

struct Ival {
    value: NameOrConstant,
    pos: TokenPos,
}

enum FlatDefinition {
    VarDef { name: String, initial_value: Option<Ival> },
    VecDef {
        name: String,
        specified_size: Option<token::Constant>,
        initial_values: Option<Vec<Ival>>,
    },
    FnDef {
        name: String,
        parameters: HashMap<String, usize>,
    },
}

enum FlatDeclarationType {
    Auto,
    Extern,
    Label,
}

enum FlatNode {
    RestoreScope,
    Def(FlatDefinition),
    Decl(FlatDeclarationType, String),
    Compound,
    Rvalue(RvalueNode),
    If,
    While,
    Switch,
    EndOfControlStatementBody,
    Break,
    Goto,
    Case(Option<token::Constant>),
    Return { return_last_rvalue: bool },
}

struct FlatNodeAndPos {
    node: FlatNode,
    pos: TokenPos,
}