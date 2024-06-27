use crate::tokenizer::Token;

pub struct CodeContainerManager {
    pub bracket_value: usize,
    pub parenthesis_value: usize,
    pub curly_value: usize,

    pub check_bracket: bool,
    pub check_parenthesis: bool,
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
        CodeContainerManager {
            bracket_value: 0,
            parenthesis_value: 0,
            curly_value: 0,

            check_parenthesis: true,
            check_curly: true,
            check_bracket: true,
        }
    }
    pub fn is_free(&self) -> bool {
        self.bracket_value == 0 && self.parenthesis_value == 0 && self.curly_value == 0
    }
    pub fn check(&mut self, chr: &Token) {
        match *chr {
            Token::OpenParenthesis => {
                self.parenthesis_value += if self.check_parenthesis { 1 } else { 0 }
            }
            Token::CloseParenthesis => {
                if !self.check_parenthesis {
                    return;
                }

                if self.parenthesis_value > 0 {
                    self.parenthesis_value -= 1
                } else {
                    panic!("PARENTHESIS VALUE IS LOWER THAN ZERO");
                }
            }
            Token::OpenCurly => self.curly_value += if self.check_curly { 1 } else { 0 },
            Token::CloseCurly => {
                if !self.check_curly {
                    return;
                }

                if self.curly_value > 0 {
                    self.curly_value -= 1
                } else {
                    panic!("CURLY VALUE IS LOWER THAN ZERO");
                }
            }
            Token::OpenBracket => self.bracket_value += if self.check_bracket { 1 } else { 0 },
            Token::CloseBracket => {
                if !self.check_bracket {
                    return;
                }

                if self.bracket_value > 0 {
                    self.bracket_value -= 1
                } else {
                    panic!("BRACKET VALUE IS LOWER THAN ZERO");
                }
            }

            _ => {}
        }
    }
}
