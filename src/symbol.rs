use std::ops::RangeInclusive;

use crate::lexical_analyzer::TokenPos;
use crate::token::{DeclarationSpecifier, TokenType};

// pub(crate) enum Declared {
//     Parameter,
//     Extern,
//     Auto
// }

#[derive(Clone, Debug)]
pub(crate) enum SymbolType {
    Variable {
        declaration_specifier: Option<DeclarationSpecifier>,
        first_occurrence: Option<TokenPos>,
    },
    Vector {
        declaration_specifier: Option<DeclarationSpecifier>,
        size: usize,
        first_occurrence: Option<TokenPos>,
    },
    Function {
        par_list_len: Option<RangeInclusive<usize>>,
        first_decl: Option<TokenPos>,
    },
    Label {
        first_occurrence: TokenPos,
    },
    Reserved(TokenType),
}