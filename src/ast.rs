#[derive(Debug)]
pub enum Node {
    //Statements
    Program(Vec<Box<Node>>),
    VariableDeclaration(Option<DeclarationType>, Box<Node>, Box<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    Else(Box<Node>),
    While(Box<Node>, Box<Node>),
    With(Box<Node>, Box<Node>),
    Do(Box<Node>, Box<Node>),
    Until(Box<Node>),
    ModifyBy(Box<Node>, OperatorType, Box<Node>),
    IncrementBy(Box<Node>, Box<Node>),
    DecrementBy(Box<Node>, Box<Node>),
    MultiplyBy(Box<Node>, Box<Node>),
    DivideBy(Box<Node>, Box<Node>),

    //Symbols
    BinaryOperator(char),

    //Expressions
    BinaryExpr(Box<Node>, char, Box<Node>),
    Neg(Box<Node>),
    Not(Box<Node>),
    BitwiseOr(Box<Node>, Box<Node>),
    BitwiseAnd(Box<Node>, Box<Node>),
    BitwiseXor(Box<Node>, Box<Node>),
    BitwiseNot(Box<Node>),
    PostIncrement(String),
    PreIncrement(String),
    PostDecrement(String),
    PreDecrement(String),
    Identifier(String),
    NumericLiteral(u32),
    String(String),
    ArrayAccess(Box<Node>, Box<Node>),
    ArrayConstructor(Vec<Box<Node>>),
    FunctionCall(Box<Node>, Vec<Box<Node>>),
    FunctionParemeter(Box<Node>),
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

impl Node {
    pub fn to_box(self) -> Box<Self> {
        Box::new(self)
    }
}

#[derive(Debug)]
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
}
