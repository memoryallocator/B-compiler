use std::fmt;

use crate::lexical_analyzer::TokenPos;

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum BracketType {
    Round,
    Curly,
    Square,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum LeftOrRight {
    Left,
    Right,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) struct Bracket {
    pub(crate) left_or_right: LeftOrRight,
    pub(crate) bracket_type: BracketType,
}

impl Bracket {
    pub(crate) const fn left_round_bracket() -> Bracket {
        Bracket {
            left_or_right: LeftOrRight::Left,
            bracket_type: BracketType::Round,
        }
    }

    pub(crate) const fn right_round_bracket() -> Bracket {
        Bracket {
            left_or_right: LeftOrRight::Right,
            bracket_type: BracketType::Round,
        }
    }

    pub(crate) const fn left_square_bracket() -> Bracket {
        Bracket {
            left_or_right: LeftOrRight::Left,
            bracket_type: BracketType::Square,
        }
    }

    pub(crate) const fn right_square_bracket() -> Bracket {
        Bracket {
            left_or_right: LeftOrRight::Right,
            bracket_type: BracketType::Square,
        }
    }

    pub(crate) fn paired_bracket(b: &Bracket) -> Bracket {
        use LeftOrRight::*;

        Bracket {
            left_or_right: if b.left_or_right == Left { Right } else { Left },
            bracket_type: b.bracket_type,
        }
    }

    pub(crate) fn from_char_unchecked(c: char) -> Bracket {
        Bracket::from_char(c).unwrap()
    }

    pub(crate) fn from_char(c: char) -> Option<Bracket> {
        use LeftOrRight::*;
        use BracketType::*;

        match c {
            '(' | '[' | '{' => Some(Bracket {
                left_or_right: Left,
                bracket_type: match c {
                    '(' => Round,
                    '[' => Square,
                    '{' => Curly,
                    _ => unimplemented!()
                },
            }),
            ')' | ']' | '}' => Some(Bracket {
                left_or_right: Right,
                bracket_type: match c {
                    ')' => Round,
                    ']' => Square,
                    '}' => Curly,
                    _ => unimplemented!()
                },
            }),
            _ => None
        }
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum Constant {
    Octal,
    Decimal,
    Char,
    String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum UnaryOperation {
    LogicalNot,
    Complement,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum BinaryRelation {
    Gt,
    Eq,
    Lt,
    Ge,
    Le,
    Ne,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum BinaryOperation {
    Div,
    Mod,
    Or,
    Xor,
    Shift(LeftOrRight),
    Cmp(BinaryRelation),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum RichBinaryOperation {
    RegularBinary(BinaryOperation),
    Add,
    Sub,
    Mul,
    And,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum ControlStatementIdentifier {
    If,
    Else,
    Goto,
    Switch,
    Case,
    While,
    // For,
    Break,
    Return,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum DeclarationSpecifier {
    Auto,
    Extrn,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub(crate) enum Operator {
    Plus,
    Minus,
    Asterisk,
    Ampersand,
    Unary(UnaryOperation),
    Binary(BinaryOperation),
    Assign(Option<RichBinaryOperation>),
    Inc,
    Dec,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub(crate) enum TokenType {
    ControlStatement(ControlStatementIdentifier),
    DeclarationSpecifier(DeclarationSpecifier),
    Operator(Operator),
    Name,
    Constant(Constant),
    Comma,
    Semicolon,
    Colon,
    Bracket(Bracket),
    QuestionMark,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // let mut value;
        // match self {
        //     TokenType::Unary(_) => value = "Unary",
        //     TokenType::Binary(_) => value = "Binary",
        //     TokenType::Assign(_) => value = "Assign",
        //     TokenType::Name => value = "Name",
        //     TokenType::Constant(_) => value = "Constant",
        //     TokenType::Bracket(_) => {}
        //     TokenType::Comma => {}
        //     Semicolon => {}
        //     TokenType::Colon => {}
        //     TokenType::Asterisk => {}
        //     TokenType::Ampersand => {}
        //     TokenType::Plus => {}
        //     TokenType::Minus => {}
        //     TokenType::ControlStatement(_) => {}
        //     TokenType::DeclarationSpecifier(_) => {}
        //     TokenType::QuestionMark => {}
        // }
        write!(f, "{:?}", self)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Hash)]
pub(crate) struct Token {
    pub(crate) r#type: TokenType,
    pub(crate) val: Option<String>,
    pub(crate) pos: TokenPos,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
