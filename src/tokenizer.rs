use crate::parser_macros::impl_enum_equal;
use std::ops::Range;

use crate::ast::OperatorType;

#[derive(Debug, Clone)]
pub enum Token {
    OpenParenthesis,
    CloseParenthesis,
    OpenCurly,
    CloseCurly,
    OpenBracket,
    CloseBracket,
    Comma,
    Dot,
    Colon,
    QuestionMark,
    Equals,
    Semilicon,
    Bar,
    HashTag,
    Dollar,

    Identifier(String),
    NumericLiteral(u32),
    String(String),

    BinaryOperator(OperatorType),
    SingleIncrement,
    SingleDecrement,
    Exclamation,
    Tilde,

    IncrementBy,
    DecrementBy,
    MultiplyBy,
    DivideBy,

    Function,
    Return,
    Constructor,
    While,
    Do,
    Until,
    Loop,
    For,
    If,
    Else,
    With,

    Var,
    Const,
    Let,

    EOF,
}

impl_enum_equal!(Token);

struct TokenConsumeMessage(Vec<Token>, usize); //(new_tokens, consumed_characters)

trait TokenRecognizer {
    fn is_valid(&self, left_code: &str) -> bool;
    fn consume(&self, left_code: &str) -> TokenConsumeMessage;
}

struct IdentifierRecognizer {}

impl TokenRecognizer for IdentifierRecognizer {
    fn is_valid(&self, left_code: &str) -> bool {
        let current_char: &char = &left_code.chars().nth(0).unwrap();
        (current_char.is_lowercase() != current_char.is_uppercase()) || *current_char == '_'
    }

    fn consume(&self, code_left: &str) -> TokenConsumeMessage {
        let identifier: String = code_left
            .chars()
            .into_iter()
            .take_while(|chr| self.is_valid(chr.to_string().as_str()) || chr.is_numeric())
            .collect();
        let consumed = identifier.chars().count();

        let token = match identifier.as_str() {
            "function" => Token::Function,
            "return" => Token::Return,
            "constructor" => Token::Constructor,
            "if" => Token::If,
            "else" => Token::Else,
            "with" => Token::With,
            "while" => Token::While,
            "do" => Token::Do,
            "until" => Token::Until,
            "for" => Token::For,
            "var" => Token::Var,
            "const" => Token::Const,
            "let" => Token::Let,
            _ => Token::Identifier(identifier),
        };

        TokenConsumeMessage(vec![token], consumed)
    }
}

struct SkippableRecognizer {}
impl TokenRecognizer for SkippableRecognizer {
    fn is_valid(&self, left_code: &str) -> bool {
        [' ', '\t', '\n']
            .iter()
            .any(|skchr| *skchr == left_code.chars().nth(0).unwrap())
    }

    fn consume(&self, code_left: &str) -> TokenConsumeMessage {
        TokenConsumeMessage(
            Vec::new(),
            code_left
                .chars()
                .into_iter()
                .take_while(|code_char| self.is_valid(code_char.to_string().as_str()))
                .count(),
        )
    }
}

struct SubstrRecognizer {
    char_map: std::collections::HashMap<&'static str, Token>,
}
impl SubstrRecognizer {
    fn new() -> Self {
        let char_map = std::collections::HashMap::<&str, Token>::from([
            (";", Token::Semilicon),
            ("=", Token::Equals),
            ("+", Token::BinaryOperator(OperatorType::Add)),
            ("-", Token::BinaryOperator(OperatorType::Sub)),
            ("*", Token::BinaryOperator(OperatorType::Mul)),
            ("/", Token::BinaryOperator(OperatorType::Div)),
            (">", Token::BinaryOperator(OperatorType::Gt)),
            ("<", Token::BinaryOperator(OperatorType::Lt)),
            (">=", Token::BinaryOperator(OperatorType::GtE)),
            ("<=", Token::BinaryOperator(OperatorType::LtE)),
            ("==", Token::BinaryOperator(OperatorType::Equals)),
            ("!=", Token::BinaryOperator(OperatorType::NotEquals)),
            ("&&", Token::BinaryOperator(OperatorType::And)),
            ("||", Token::BinaryOperator(OperatorType::Or)),
            ("^^", Token::BinaryOperator(OperatorType::Xor)),
            (">>", Token::BinaryOperator(OperatorType::BitwiseShiftRight)),
            ("<<", Token::BinaryOperator(OperatorType::BitwiseShiftLeft)),
            ("&", Token::BinaryOperator(OperatorType::BitwiseAnd)),
            ("|", Token::BinaryOperator(OperatorType::BitwiseOr)),
            ("^", Token::BinaryOperator(OperatorType::BitwiseXor)),
            ("(", Token::OpenParenthesis),
            (")", Token::CloseParenthesis),
            ("{", Token::OpenCurly),
            ("}", Token::CloseCurly),
            ("[", Token::OpenBracket),
            ("]", Token::CloseBracket),
            (",", Token::Comma),
            (".", Token::Dot),
            (":", Token::Colon),
            ("?", Token::QuestionMark),
            ("|", Token::Bar),
            ("#", Token::HashTag),
            ("$", Token::Dollar),
            ("!", Token::Exclamation),
            ("~", Token::Tilde),
            ("++", Token::SingleIncrement),
            ("--", Token::SingleDecrement),
            ("+=", Token::IncrementBy),
            ("-=", Token::DecrementBy),
            ("*=", Token::MultiplyBy),
            ("/=", Token::DivideBy),
        ]);

        Self { char_map }
    }
}

impl TokenRecognizer for SubstrRecognizer {
    fn is_valid(&self, left_code: &str) -> bool {
        self.char_map
            .iter()
            .any(|(key, _)| left_code.starts_with(key))
    }

    fn consume(&self, code_left: &str) -> TokenConsumeMessage {
        let mut as_vec: Vec<(&str, Token)> = self.char_map.clone().into_iter().collect();
        as_vec.sort_by(|(prev_key, _), (key, _)| key.len().cmp(&prev_key.len()));

        let idx: usize = as_vec
            .iter()
            .position(|(key, _)| code_left.starts_with(key))
            .unwrap();

        let (key, token) = as_vec[idx].clone();

        TokenConsumeMessage(vec![token], key.len())
    }
}

struct NumericLiteralRecognizer;
impl TokenRecognizer for NumericLiteralRecognizer {
    fn is_valid(&self, left_code: &str) -> bool {
        left_code.chars().nth(0).unwrap().is_numeric()
    }

    fn consume(&self, code_left: &str) -> TokenConsumeMessage {
        let identifier: String = code_left
            .chars()
            .into_iter()
            .take_while(|chr| self.is_valid(chr.to_string().as_str()))
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

struct StringRecognizer {}
impl TokenRecognizer for StringRecognizer {
    fn is_valid(&self, current_char: &str) -> bool {
        current_char.chars().nth(0).unwrap() == '"'
    }
    fn consume(&self, left_code: &str) -> TokenConsumeMessage {
        let mut qmark_count = 0 as usize;
        let mut txt_range: Option<Range<usize>> = None;
        let mut consumed: Option<usize> = None;

        for (i, chr) in left_code.chars().enumerate() {
            if chr == '"' && left_code.chars().nth(i.max(1) - 1).unwrap_or('\0') != '\\' {
                qmark_count += 1;
            }

            if qmark_count == 2 {
                txt_range = Some(1..i);
                consumed = Some(i + 1);
                break;
            }
        }

        if txt_range.is_none() || consumed.is_none() {
            panic!("ERROR TOKENIZING STRING");
        }

        let mut str_text = left_code[txt_range.unwrap()].to_string();
        str_text = str_text.replace("\\\"", "\"");
        str_text = str_text.replace("\\\\", "\\");
        str_text = str_text.replace("\\n", "\n");

        let token = Token::String(str_text);
        TokenConsumeMessage(vec![token], consumed.unwrap())
    }
}

struct SingleIncrementDecrementRecognizer;
impl TokenRecognizer for SingleIncrementDecrementRecognizer {
    fn is_valid(&self, left_code: &str) -> bool {
        match (
            left_code.chars().nth(0).unwrap(),
            left_code.chars().nth(1).unwrap_or('\0'),
        ) {
            ('+', '+') | ('-', '-') => true,
            _ => false,
        }
    }

    fn consume(&self, left_code: &str) -> TokenConsumeMessage {
        match (
            left_code.chars().nth(0).unwrap(),
            left_code.chars().nth(1).unwrap_or('\0'),
        ) {
            ('+', '+') => TokenConsumeMessage(vec![Token::SingleIncrement], 2),
            ('-', '-') => TokenConsumeMessage(vec![Token::SingleDecrement], 2),
            _ => panic!("COULDNT CONSUME SINGLE INCREMENT/DECREMENT"),
        }
    }
}

pub fn tokenize(code: &str) -> Vec<Token> {
    let mut ptr: usize = 0;
    let mut tokens = Vec::<Token>::new();
    let recognizers: Vec<Box<dyn TokenRecognizer>> = vec![
        Box::new(IdentifierRecognizer {}),
        Box::new(SingleIncrementDecrementRecognizer {}),
        Box::new(SubstrRecognizer::new()),
        Box::new(NumericLiteralRecognizer {}),
        Box::new(SkippableRecognizer {}),
        Box::new(StringRecognizer {}),
    ];

    while ptr < code.chars().count() {
        let chr = code
            .chars()
            .nth(ptr)
            .expect("ERROR GETTING CHARACTER IN CODE");

        if chr == '/' && code.chars().nth(ptr + 1).is_some_and(|c| c == '/') {
            while !code.chars().nth(ptr).is_some_and(|c| c == '\n') {
                ptr += 1;
            }
            continue;
        }

        if chr == '/' && code.chars().nth(ptr + 1).is_some_and(|c| c == '*') {
            ptr += 2;
            while !(code.chars().nth(ptr - 1).is_some_and(|c| c == '*')
                && code.chars().nth(ptr).is_some_and(|c| c == '/'))
                && ptr < code.len()
            {
                ptr += 1;
            }
            ptr += 1;
            continue;
        }

        let token_match = recognizers
            .iter()
            .find(|recognizer| recognizer.is_valid(&code[ptr..]));

        let code_left = &code[ptr..];
        let consume_message = token_match
            .expect(format!("Unexpected token while tokenizing: {}", chr).as_str())
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
