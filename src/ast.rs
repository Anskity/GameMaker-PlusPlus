use crate::{parser_macros::impl_enum_equal, tokenizer::TokenStruct};
#[derive(Debug, Clone)]
pub enum Node {
    //Statements
    Program(Vec<Box<Node>>),
    VariableDeclaration(DeclarationType, Vec<Box<Node>>),
    VariableDeclarationPart(Box<Node>, Option<Box<Node>>),
    VariableSet(Box<Node>, Box<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    Else(Box<Node>),
    For(Box<Node>, Box<Node>, Box<Node>, Box<Node>),
    While(Box<Node>, Box<Node>),
    With(Box<Node>, Box<Node>),
    Do(Box<Node>, Box<Node>),
    Until(Box<Node>),
    ModifyBy(Box<Node>, OperatorType, Box<Node>),
    IncrementBy(Box<Node>, Box<Node>),
    DecrementBy(Box<Node>, Box<Node>),
    MultiplyBy(Box<Node>, Box<Node>),
    DivideBy(Box<Node>, Box<Node>),
    FunctionDeclaration(String, Vec<Box<Node>>, Box<Node>),
    FunctionConstructorDeclaration(String, Vec<Box<Node>>, Box<Node>),
    Return(Option<Box<Node>>),
    EnumDeclaration(String, Vec<Box<Node>>),
    EnumVariant(String),

    //Symbols
    BinaryOperator(OperatorType),

    //Expressions
    BinaryExpr(Box<Node>, OperatorType, Box<Node>),
    Neg(Box<Node>),
    Not(Box<Node>),
    BitwiseOr(Box<Node>, Box<Node>),
    BitwiseAnd(Box<Node>, Box<Node>),
    BitwiseXor(Box<Node>, Box<Node>),
    BitwiseNot(Box<Node>),
    PostIncrement(Box<Node>),
    PreIncrement(Box<Node>),
    PostDecrement(Box<Node>),
    PreDecrement(Box<Node>),
    Identifier(String),
    NumericLiteral(u32),
    String(String),
    ArrayAccess(Box<Node>, Box<Node>),
    ArrayConstructor(Vec<Box<Node>>),
    FunctionCall(Box<Node>, Vec<Box<Node>>),
    FunctionParemeter(Box<Node>, Option<Box<Node>>),
    AnonymousFunctionDeclaration(Vec<Box<Node>>, Box<Node>),
    Struct(Vec<Box<Node>>),
    StructAttribute(Box<Node>, Box<Node>),
    StructAttributePredefined(Box<Node>),
    StructAccess(Box<Node>, Box<Node>),
    Ternary(Box<Node>, Box<Node>, Box<Node>),
    DsListAccess(Box<Node>, Box<Node>),
    DsMapAccess(Box<Node>, Box<Node>),
    DsGridAccess(Box<Node>, Box<Node>, Box<Node>),
    StructKeyAccess(Box<Node>, Box<Node>),
}

impl_enum_equal!(Node);

impl Node {
    pub fn to_box(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug)]
pub struct TextData {
    pub start: usize,
    pub end: usize,
    pub line: usize,
}
impl TextData {
    pub fn new(start: usize, end: usize, line: usize) -> Self {
        TextData { start, end, line }
    }
    pub fn from_tokens(tokens: &[TokenStruct]) -> Self {
        let start = tokens[0].text_data.start;
        let end = tokens.last().unwrap().text_data.end;
        let line = tokens[0].text_data.line;
        TextData { start, end, line }
    }

    pub fn from_pair(left: &TextData, right: &TextData) -> Self {
        TextData {
            start: left.start,
            end: right.end,
            line: left.line,
        }
    }
}

#[derive(Debug, Clone)]
pub enum DeclarationType {
    Var,
    Const,
    Let,
}

#[derive(Debug, Clone, PartialEq)]
pub enum OperatorType {
    Add,
    Sub,
    Mul,
    Div,

    Gt,
    Lt,
    GtE,
    LtE,
    Equals,
    NotEquals,

    And,
    Or,
    Xor,

    BitwiseAnd,
    BitwiseOr,
    BitwiseXor,
    BitwiseShiftLeft,
    BitwiseShiftRight,
}
