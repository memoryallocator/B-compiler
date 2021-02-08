use std::fmt;

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
    pub(crate) fn paired_bracket(b: &Bracket) -> Bracket {
        use LeftOrRight::*;

        Bracket {
            left_or_right: if b.left_or_right == Left { Right } else { Left },
            bracket_type: b.bracket_type,
        }
    }

    pub(crate) fn from(c: char) -> Option<Bracket> {
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
pub(crate) enum UnaryOperationSymbol {
    LogicalNot,
    Complement,  // ~
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
pub(crate) enum PunctuationSymbol {
    Comma,
    Semicolon,
    Colon,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub(crate) enum RelationSymbol {
    Greater,
    Less,
    Equal,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub(crate) enum Character {
    Plus,
    Minus,
    Slash,
    Punctuation(PunctuationSymbol),
    Or,
    ExclusiveOr,
    Asterisk,
    Ampersand,
    Bracket(Bracket),
    Unary(UnaryOperationSymbol),
    Relation(RelationSymbol),
    QuestionMark,
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub(crate) enum TokenType {
    ControlStatement(ControlStatementIdentifier),
    DeclarationSpecifier(DeclarationSpecifier),
    Character(Character),
    Name,
    Constant(Constant),
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
    pub(crate) token_type: TokenType,
    pub(crate) value: Option<Vec<u8>>,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Character {
    pub(crate) fn from_u8(c: u8) -> Option<Self> {
        use crate::token::PunctuationSymbol::*;
        use crate::token::RelationSymbol::*;
        use crate::token::UnaryOperationSymbol::*;

        let v = match c {
            b',' => Character::Punctuation(Comma),
            b';' => Character::Punctuation(Semicolon),
            b'*' => Character::Asterisk,
            b':' => Character::Punctuation(Colon),
            b'(' | b'{' | b'[' |
            b')' | b'}' | b']' => Character::Bracket(Bracket::from(c as char)?),
            b'+' => Character::Plus,
            b'-' => Character::Minus,
            b'!' => Character::Unary(LogicalNot),
            b'~' => Character::Unary(Complement),
            b'&' => Character::Ampersand,
            b'?' => Character::QuestionMark,
            b'=' => Character::Relation(Equal),
            b'<' => Character::Relation(Less),
            b'>' => Character::Relation(Greater),
            b'|' => Character::Or,
            b'^' => Character::ExclusiveOr,
            _ => return None,
        };
        Some(v)
    }
}