use std::io::Error;

use crate::{
    ast::TextData,
    parser_macros::{assert_or, throw_parse_err},
    tokenizer::{Token, TokenStruct},
};

#[derive(Debug, Clone)]
pub enum DataType {
    Number,
    String,
    Array(Box<DataType>),
    RawStruct,
    Struct(Vec<(String, Box<DataType>)>),
}

impl DataType {
    pub fn to_box(self) -> Box<Self> {
        Box::new(self)
    }
}

pub fn parse_type(tokens: &[TokenStruct]) -> Result<DataType, Error> {
    assert_or!(!tokens.is_empty());
    if let Token::Identifier(id) = &tokens[0].token {
        match id.as_str() {
            "string" => return Ok(DataType::String),
            "number" => return Ok(DataType::Number),
            "struct" => return Ok(DataType::RawStruct),
            "array" => {
                let pair = tokens.len() - 1;
                let tks = &tokens[2..pair];
                if tks.len() > 1 {
                    let text_data = TextData::from_tokens(tks);
                    throw_parse_err!(text_data, "More arguments enoughs than required");
                }

                let arr_type = parse_type(tks)?;

                return Ok(DataType::Array(arr_type.to_box()));
            }
            _ => panic!("Invalid type"),
        }
    }

    todo!()
}
