use std::collections::HashMap;

use crate::token::*;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) enum GrammarSymbol {
    Terminal(TokenType),
    NonTerminal(NonTerminal),
    // Epsilon,
    RightEndmarker,
}

type GrammarString = Vec<GrammarSymbol>;

// type Terminal = Token;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) enum DataType {
    Variable,
    Vector,
    Function,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) enum And {
    Conditional,
    Logical,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) enum IncDec {
    Increment,
    Decrement,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) enum DeclarationType {
    Label,
    Auto,
    Extern,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub(crate) enum NonTerminal {
    Definitions,
    Definition(DataType),
    Declaration(DeclarationType),
    And(And),
    Ival,
    IvalList,
    Statement,
    Lvalue,
    Rvalue,
    Assign,
    IncDec,
    Unary,
    Binary,
    TruthValue,
    ConditionalOperator,
    Name,
    Constant,
    Dereference,
    TakeAddress,
}

type GrammarProduction = GrammarString;
pub(crate) type ProductionsTable = HashMap<NonTerminal, Vec<GrammarProduction>>;

pub(crate) struct Grammar {
    pub(crate) start_symbol: NonTerminal,
    pub(crate) productions_table: ProductionsTable,
}