use std::ops::RangeInclusive;

use crate::token::{DeclarationSpecifier, TokenType};

#[derive(Clone)]
pub(crate) enum ParameterListLength {
    Precise(usize),
    Variable(Option<RangeInclusive<usize>>),
}

// pub(crate) enum Declared {
//     Parameter,
//     Extern,
//     Auto
// }

#[derive(Clone)]
pub(crate) struct VectorInfo {
    declaration_specifier: DeclarationSpecifier,
    size: usize,
}

#[derive(Clone)]
pub(crate) enum SymbolType {
    Variable,
    Vector(VectorInfo),
    Function(ParameterListLength),
    Label,
    Reserved(TokenType),
}