use std::io::{Error, ErrorKind};

use crate::{
    parser_macros::throw_parse_err,
    tokenizer::{Token, TokenStruct},
};

pub struct CodeContainerManager {
    pub parenthesis_value: usize,
    pub bracket_value: usize,
    pub curly_value: usize,

    pub check_parenthesis: bool,
    pub check_bracket: bool,
    pub check_curly: bool,
}

impl CodeContainerManager {
    pub fn new_ext(check_parenthesis: bool, check_bracket: bool, check_curly: bool) -> Self {
        CodeContainerManager {
            bracket_value: 0,
            parenthesis_value: 0,
            curly_value: 0,

            check_parenthesis,
            check_curly,
            check_bracket,
        }
    }

    pub fn new() -> Self {
        CodeContainerManager::new_ext(true, true, true)
    }
    pub fn is_free(&self) -> bool {
        self.bracket_value == 0 && self.parenthesis_value == 0 && self.curly_value == 0
    }

    pub fn get_value(token: &Token) -> (i8, i8, i8) {
        match *token {
            Token::OpenParenthesis => (1, 0, 0),
            Token::OpenBracket => (0, 1, 0),
            Token::OpenCurly => (0, 0, 1),
            Token::CloseParenthesis => (-1, 0, 0),
            Token::CloseBracket => (0, -1, 0),
            Token::CloseCurly => (0, 0, -1),
            _ => (0, 0, 0),
        }
    }

    pub fn check_safe(&self, token: &TokenStruct) -> Result<(i8, i8, i8), Error> {
        let (paren, brack, curly) = Self::get_value(&token.token);

        if paren < 0 && self.parenthesis_value == 0 {
            throw_parse_err!(token.text_data, format!("No matching parenthesis at"));
        }
        if brack < 0 && self.bracket_value == 0 {
            throw_parse_err!(token.text_data, format!("No matching bracket"));
        }
        if curly < 0 && self.curly_value == 0 {
            throw_parse_err!(token.text_data, format!("No matching curly brace"));
        }

        Ok((paren, brack, curly))
    }
    pub fn check_safe_reverse(&self, token: &TokenStruct) -> Result<(i8, i8, i8), Error> {
        let (paren, brack, curly) = Self::get_value(&token.token);
        let (paren, brack, curly) = (-paren, -brack, -curly);

        if paren < 0 && self.parenthesis_value == 0 {
            throw_parse_err!(token.text_data, format!("No matching parenthesis at"));
        }
        if brack < 0 && self.bracket_value == 0 {
            throw_parse_err!(token.text_data, format!("No matching bracket at"));
        }
        if curly < 0 && self.curly_value == 0 {
            throw_parse_err!(token.text_data, format!("No matching curly brace"));
        }

        Ok((paren, brack, curly))
    }

    pub fn apply(&mut self, values: (i8, i8, i8)) {
        let (paren, brack, curly) = values;
        self.parenthesis_value = ((self.parenthesis_value as i8) + paren) as usize;
        self.bracket_value = ((self.bracket_value as i8) + brack) as usize;
        self.curly_value = ((self.curly_value as i8) + curly) as usize;
    }

    pub fn check(&mut self, tk: &TokenStruct) -> Result<(), Error> {
        let value = self.check_safe(tk);

        if value.is_err() {
            return Err(value.unwrap_err());
        }

        self.apply(value.unwrap());
        Ok(())
    }
    pub fn check_reverse(&mut self, tk: &TokenStruct) -> Result<(), Error> {
        let value = self.check_safe_reverse(tk);

        if value.is_err() {
            return Err(value.unwrap_err());
        }

        self.apply(value.unwrap());
        Ok(())
    }

    pub fn is_safe(&self, tk: &TokenStruct) -> bool {
        self.check_safe(tk).is_ok()
    }
}
