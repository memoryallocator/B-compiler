use borrow::Borrow;
use fmt::{Display, Formatter};
use std::*;

#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub(crate) enum BracketType {
    Round,
    Curly,
    Square,
}

#[derive(Eq, PartialEq, Clone, Copy, Debug, Hash)]
pub(crate) struct Bracket {
    pub(crate) left_or_right: LeftOrRight,
    pub(crate) bracket_type: BracketType,
    pub(crate) pair_pos: Option<TokenPos>,
}

impl Bracket {
    pub(crate) fn is_pair<T: Borrow<Bracket>>(&self, br: T) -> bool {
        let br = br.borrow();
        self.left_or_right != br.left_or_right && self.bracket_type == br.bracket_type
    }

    pub(crate) fn from_char_unchecked(c: char) -> Bracket {
        Bracket::from_char(c).unwrap()
    }

    pub(crate) fn from_char(c: char) -> Option<Bracket> {
        use BracketType::*;
        use LeftOrRight::*;

        match c {
            '(' | '[' | '{' => Some(Bracket {
                left_or_right: Left,
                bracket_type: match c {
                    '(' => Round,
                    '[' => Square,
                    '{' => Curly,
                    _ => unimplemented!(),
                },
                pair_pos: None,
            }),
            ')' | ']' | '}' => Some(Bracket {
                left_or_right: Right,
                bracket_type: match c {
                    ')' => Round,
                    ']' => Square,
                    '}' => Curly,
                    _ => unimplemented!(),
                },
                pair_pos: None,
            }),
            _ => None,
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub(crate) enum LeftOrRight {
    Left,
    Right,
}

impl Display for LeftOrRight {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LeftOrRight::Left => "left",
                LeftOrRight::Right => "right",
            }
        )
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub(crate) enum Constant {
    Number(u64),
    String(String),
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) enum UnaryOperation {
    LogicalNot,
    Complement,
}

impl Display for UnaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOperation::LogicalNot => "!",
                UnaryOperation::Complement => "~",
            }
        )
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub(crate) enum BinaryRelation {
    Gt,
    Eq,
    Lt,
    Ge,
    Le,
    Ne,
}

impl Display for BinaryRelation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryRelation::Gt => ">",
                BinaryRelation::Eq => "==",
                BinaryRelation::Lt => "<",
                BinaryRelation::Ge => ">=",
                BinaryRelation::Le => "<=",
                BinaryRelation::Ne => "!=",
            }
        )
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub(crate) enum BinaryOperation {
    Div,
    Mod,
    Or,
    Xor,
    Shift(LeftOrRight),
    Cmp(BinaryRelation),
}

impl Display for BinaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinaryOperation::Div => "/".to_owned(),
                BinaryOperation::Mod => "%".to_owned(),
                BinaryOperation::Or => "|".to_owned(),
                BinaryOperation::Xor => "^".to_owned(),
                BinaryOperation::Shift(direction) => {
                    match direction {
                        LeftOrRight::Left => "<<",
                        LeftOrRight::Right => ">>",
                    }
                    .to_owned()
                }
                BinaryOperation::Cmp(cmp) => {
                    format!("{}", cmp)
                }
            }
        )
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub(crate) enum RichBinaryOperation {
    RegularBinary(BinaryOperation),
    Add,
    Sub,
    Mul,
    BitwiseAnd,
    LogicalAnd,
}

impl Display for RichBinaryOperation {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                RichBinaryOperation::RegularBinary(b) => {
                    format!("{}", b)
                }
                RichBinaryOperation::Add => "+".to_owned(),
                RichBinaryOperation::Sub => "-".to_owned(),
                RichBinaryOperation::Mul => "*".to_owned(),
                RichBinaryOperation::BitwiseAnd | RichBinaryOperation::LogicalAnd => "&".to_owned(),
            }
        )
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum CtrlStmtIdent {
    If,
    Else,
    Goto,
    Switch,
    Case,
    While,
    // For,
    Break,
    Return,
    Default,
    Continue,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Copy, Debug, Hash)]
pub(crate) enum DeclarationSpecifier {
    Auto,
    Extrn,
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub(crate) struct Assign {
    pub(crate) bin_op: Option<RichBinaryOperation>,
}

impl From<Option<RichBinaryOperation>> for Assign {
    fn from(bin_op: Option<RichBinaryOperation>) -> Self {
        Assign { bin_op }
    }
}

impl From<RichBinaryOperation> for Assign {
    fn from(bin_op: RichBinaryOperation) -> Self {
        Assign::from(Some(bin_op))
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum IncOrDec {
    Increment,
    Decrement,
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum Operator {
    Plus,
    Minus,
    Asterisk,
    Ampersand,
    Unary(UnaryOperation),
    Binary(BinaryOperation),
    Assign(Assign),
    IncDec(IncOrDec),
}

impl TryFrom<&Token> for Operator {
    type Error = ();

    fn try_from(x: &Token) -> Result<Self, Self::Error> {
        if let Token {
            token: WrappedToken::Operator(op),
            ..
        } = x
        {
            return Ok(*op);
        }
        Err(())
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Operator::Plus => "+".to_owned(),
                Operator::Minus => "-".to_owned(),
                Operator::Asterisk => "*".to_owned(),
                Operator::Ampersand => "&".to_owned(),
                Operator::Unary(un) => {
                    format!("{}", un)
                }
                Operator::Binary(bin) => {
                    format!("{}", bin)
                }
                Operator::Assign(a) => {
                    "=".to_owned()
                        + &match a.bin_op {
                            None => "".to_owned(),
                            Some(bin_op) => {
                                format!("{}", bin_op)
                            }
                        }
                }
                Operator::IncDec(id) => {
                    match id {
                        IncOrDec::Increment => "++",
                        IncOrDec::Decrement => "--",
                    }
                    .to_owned()
                }
            }
        )
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum ReservedName {
    CtrlStmt(CtrlStmtIdent),
    DeclarationSpecifier(DeclarationSpecifier),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) enum WrappedToken {
    ReservedName(ReservedName),
    Operator(Operator),
    Comma,
    Semicolon,
    Colon,
    Bracket(Bracket),
    QuestionMark,
    Name(String),
    Constant(Constant),
}

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash, PartialOrd)]
pub(crate) struct TokenPos {
    pub(crate) line: usize,
    pub(crate) column: usize,
}

impl From<(usize, usize)> for TokenPos {
    fn from(line_and_column: (usize, usize)) -> Self {
        let (line, column) = line_and_column;
        TokenPos { line, column }
    }
}

impl TryFrom<&str> for TokenPos {
    type Error = ();

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        let line_and_col: Vec<&str> = s.split(',').collect();

        if line_and_col.len() != 2 {
            return Err(());
        }

        if let [line, col] = line_and_col[..2] {
            let line = line.parse::<usize>();
            let column = col.parse::<usize>();

            if let (Ok(line), Ok(column)) = (line, column) {
                return Ok(TokenPos { line, column });
            }
        }

        Err(())
    }
}

impl fmt::Display for TokenPos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, column {}", self.line, self.column)
    }
}

#[derive(Clone, Debug, Hash, Eq, PartialEq)]
pub struct Token {
    pub(crate) token: WrappedToken,
    pub(crate) pos: TokenPos,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
