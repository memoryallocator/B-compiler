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
    left_or_right: LeftOrRight,
    bracket_type: BracketType,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Clone, Debug)]
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
    Div,
    Mod,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    Or,
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
    Bracket(BracketType),
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
}