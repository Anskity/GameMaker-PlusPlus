#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    OpenParenthesis,
    CloseParenthesis,
    OpenCurly,
    CloseCurly,
    Comma,
    Dot,
    Equals,
    GreaterThan,
    LessThan,
    Semilicon,
    QuotationMarks,

    Identifier(String),
    NumericLiteral(u32),

    BinaryOperator(char),

    Function,
    Return,
    Constructor,
    While,
    Do,
    Until,
    Loop,
    For,
}

struct TokenConsumeMessage(Vec<Token>, usize); //(new_tokens, consumed_characters)

trait TokenRecognizer {
    fn is_valid(&self, current_char: &char) -> bool;
    fn consume(&self, left_code: &&str) -> TokenConsumeMessage;
}

struct IdentifierRecognizer {}

impl TokenRecognizer for IdentifierRecognizer {
    fn is_valid(&self, current_char: &char) -> bool {
        current_char.is_lowercase() != current_char.is_uppercase()
    }

    fn consume(&self, code_left: &&str) -> TokenConsumeMessage {
        let identifier: String = code_left
            .chars()
            .into_iter()
            .take_while(|chr| self.is_valid(&chr))
            .collect();
        let consumed = identifier.chars().count();

        let token = match identifier.as_str() {
            "function" => Token::Function,
            "return" => Token::Return,
            "constructor" => Token::Constructor,
            "while" => Token::While,
            "do" => Token::Do,
            "until" => Token::Until,
            "for" => Token::For,
            _ => Token::Identifier(identifier),
        };

        TokenConsumeMessage(vec![token], consumed)
    }
}

struct SkippableRecognizer {}
impl TokenRecognizer for SkippableRecognizer {
    fn is_valid(&self, chr: &char) -> bool {
        [' ', '\t', '\n'].iter().any(|skchr| skchr == chr)
    }

    fn consume(&self, code_left: &&str) -> TokenConsumeMessage {
        TokenConsumeMessage(
            Vec::new(),
            code_left
                .chars()
                .into_iter()
                .take_while(|code_char| self.is_valid(&code_char))
                .count(),
        )
    }
}

struct SingleCharRecognizer {
    char_map: std::collections::HashMap<char, Token>,
}
impl SingleCharRecognizer {
    fn new() -> Self {
        let mut char_map = std::collections::HashMap::<char, Token>::new();
        char_map.insert(';', Token::Semilicon);
        char_map.insert('=', Token::Equals);
        char_map.insert('+', Token::BinaryOperator('+'));
        char_map.insert('-', Token::BinaryOperator('-'));
        char_map.insert('*', Token::BinaryOperator('*'));
        char_map.insert('/', Token::BinaryOperator('/'));
        char_map.insert('(', Token::OpenParenthesis);
        char_map.insert(')', Token::CloseParenthesis);
        char_map.insert('{', Token::OpenCurly);
        char_map.insert('}', Token::CloseCurly);
        char_map.insert(',', Token::Comma);
        char_map.insert('.', Token::Dot);
        char_map.insert('<', Token::LessThan);
        char_map.insert('>', Token::GreaterThan);
        char_map.insert('"', Token::QuotationMarks);

        SingleCharRecognizer { char_map }
    }
}
impl TokenRecognizer for SingleCharRecognizer {
    fn is_valid(&self, chr: &char) -> bool {
        self.char_map.contains_key(&chr)
    }

    fn consume(&self, code_left: &&str) -> TokenConsumeMessage {
        let token: Token = self
            .char_map
            .get(&code_left.chars().next().unwrap())
            .unwrap()
            .clone();

        TokenConsumeMessage(vec![token], 1)
    }
}

struct NumericLiteralRecognizer;
impl TokenRecognizer for NumericLiteralRecognizer {
    fn is_valid(&self, chr: &char) -> bool {
        chr.is_numeric()
    }

    fn consume(&self, code_left: &&str) -> TokenConsumeMessage {
        let identifier: String = code_left
            .chars()
            .into_iter()
            .take_while(|chr| self.is_valid(&chr))
            .collect();
        let consumed: usize = identifier.chars().count();

        TokenConsumeMessage(
            vec![Token::NumericLiteral(
                identifier.parse().expect("INVALID NUMERIC LITERAL"),
            )],
            consumed,
        )
    }
}

pub fn tokenize(code: &&str) -> Vec<Token> {
    let mut ptr: usize = 0;
    let mut tokens = Vec::<Token>::new();
    let recognizers: Vec<Box<dyn TokenRecognizer>> = vec![
        Box::new(IdentifierRecognizer {}),
        Box::new(SingleCharRecognizer::new()),
        Box::new(NumericLiteralRecognizer {}),
        Box::new(SkippableRecognizer {}),
    ];

    while ptr < code.chars().count() {
        let chr = code
            .chars()
            .nth(ptr)
            .expect("ERROR GETTING CHARACTER IN CODE");

        let token_match = recognizers
            .iter()
            .find(|recognizer| recognizer.is_valid(&chr));

        let code_left = &code[ptr..];
        let consume_message = token_match
            .expect("Unexpected token while tokenizing")
            .consume(&code_left);

        let mut new_tokens = consume_message.0;
        let consumed_chars = consume_message.1;
        tokens.append(&mut new_tokens);
        ptr += consumed_chars;

        if consumed_chars < 1 {
            panic!("INVALID AMOUNT OF CONSUMED CHARACTERS");
        }
    }

    tokens
}
