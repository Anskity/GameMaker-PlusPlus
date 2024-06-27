use crate::tokenizer::Token;

pub struct CodeContainerManager {
    pub bracket_value: usize,
    pub parenthesis_value: usize,
    pub curly_value: usize,
    pub ternary_value: usize,

    pub check_bracket: bool,
    pub check_parenthesis: bool,
    pub check_curly: bool,
    pub check_ternary: bool,
}

impl CodeContainerManager {
    pub fn new_ext(
        check_parenthesis: bool,
        check_bracket: bool,
        check_curly: bool,
        check_ternary: bool,
    ) -> Self {
        CodeContainerManager {
            bracket_value: 0,
            parenthesis_value: 0,
            curly_value: 0,
            ternary_value: 0,

            check_parenthesis,
            check_curly,
            check_bracket,
            check_ternary,
        }
    }

    pub fn new() -> Self {
        CodeContainerManager::new_ext(true, true, true, true)
    }
    pub fn is_free(&self) -> bool {
        self.bracket_value == 0
            && self.parenthesis_value == 0
            && self.curly_value == 0
            && self.ternary_value == 0
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

            Token::QuestionMark => self.ternary_value += if self.check_ternary { 1 } else { 0 },
            Token::Colon => {
                if !self.check_ternary {
                    return;
                }

                if self.ternary_value > 0 {
                    self.ternary_value -= 1
                } else {
                    panic!("TERNARY VALUE IS LOWER THAN ZERO");
                }
            }

            _ => {}
        }
    }

    pub fn reset(&mut self) {
        self.parenthesis_value = 0;
        self.bracket_value = 0;
        self.curly_value = 0;
    }
}
