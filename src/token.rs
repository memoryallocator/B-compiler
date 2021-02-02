#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum BracketType {
    Round,
    Curly,
    Square,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum LeftOrRight {
    Left,
    Right,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct Bracket {
    pub left_or_right: LeftOrRight,
    pub bracket_type: BracketType,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug, Copy)]
pub enum ConstantType {
    Octal,
    Decimal,
    Char,
    String,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub struct Constant {
    pub(crate) value: Vec<u8>,
    pub(crate) constant_type: ConstantType,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum Binary {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Ne,
    Le,
    Ge,
    Less,
    Greater,
    Or,
    And,
    Xor,
    Shift(LeftOrRight),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum Unary {
    Increment,
    Decrement,
    LogicalNot,
    Complement,  // ~
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum ControlStatement {
    If,
    Else,
    Goto,
    Switch,
    Case,
    While,
    For,
    Break,
    Return,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Hash, Debug)]
pub enum DeclarationSpecifier {
    Auto,
    Extrn,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
pub enum Token {
    Unary(Unary),
    Binary(Binary),
    Assign(Option<Binary>),
    Id(Vec<u8>),
    Constant(Constant),
    Bracket(Bracket),
    Comma,
    Semicolon,
    Colon,
    Asterisk,
    Ampersand,
    Plus,
    Minus,
    Dot,
    ControlStatement(ControlStatement),
    DeclarationSpecifier(DeclarationSpecifier),
    QuestionMark,
}