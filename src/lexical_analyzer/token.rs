use std::*;
use convert::TryFrom;
use borrow::Borrow;

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
        self.left_or_right != br.left_or_right
            && self.bracket_type == br.bracket_type
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
                pair_pos: None,
            }),
            ')' | ']' | '}' => Some(Bracket {
                left_or_right: Right,
                bracket_type: match c {
                    ')' => Round,
                    ']' => Square,
                    '}' => Curly,
                    _ => unimplemented!()
                },
                pair_pos: None,
            }),
            _ => None
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub(crate) enum LeftOrRight {
    Left,
    Right,
}

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub(crate) enum ConstantType {
    Octal,
    Decimal,
    Char,
    String,
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
pub(crate) struct Constant {
    pub(crate) constant_type: ConstantType,
    pub(crate) value: String,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub(crate) enum UnaryOperation {
    LogicalNot,
    Complement,
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

#[derive(PartialEq, Eq, Clone, Copy, Debug, Hash)]
pub(crate) enum BinaryOperation {
    Div,
    Mod,
    Or,
    Xor,
    Shift(LeftOrRight),
    Cmp(BinaryRelation),
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
    Default,
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
pub(crate) enum IncDec {
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
    IncDec(IncDec),
}

impl TryFrom<&Token> for Operator {
    type Error = ();

    fn try_from(x: &Token) -> Result<Self, Self::Error> {
        if let Token { token: WrappedToken::Operator(op), .. } = x {
            return Ok(*op);
        }
        Err(())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub(crate) enum ReservedName {
    ControlStatement(ControlStatementIdentifier),
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

#[derive(Eq, PartialEq, Copy, Clone, Debug, Hash)]
pub(crate) struct TokenPos {
    line: usize,
    column: usize,
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
        let line_and_col: Vec<&str> =
            s.split(',').collect();

        if line_and_col.len() != 2 {
            return Err(());
        }

        match line_and_col[..2] {
            [line, col] => {
                let line = line.parse::<usize>();
                let column = col.parse::<usize>();

                if line.is_ok() && column.is_ok() {
                    let line = line.unwrap();
                    let column = column.unwrap();
                    return Ok(TokenPos { line, column });
                }
            }
            _ => unreachable!()
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
