#[derive(Debug, Clone)]
pub enum Node {
    //Statements
    Program(Vec<Box<Node>>),
    VariableDeclaration(Box<Node>, Box<Node>),
    If(Box<Node>, Box<Node>, Option<Box<Node>>),
    Else(Box<Node>),
    While(Box<Node>, Box<Node>),
    With(Box<Node>, Box<Node>),

    //Expressions
    BinaryExpr(Box<Node>, char, Box<Node>),
    Identifier(String),
    NumericLiteral(u32),
    String(String),
    BinaryOperator(char),
    ArrayAccess(Box<Node>, Box<Node>),
    ArrayConstructor(Vec<Box<Node>>),
    FunctionCall(Box<Node>, Vec<Box<Node>>),
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
