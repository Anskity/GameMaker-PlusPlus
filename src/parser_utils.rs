use crate::{code_container::CodeContainerManager, tokenizer::Token};
pub fn amount_of_tokens(tokens: &[Token], search: &Token) -> usize {
    let mut container_manager = CodeContainerManager::new();
    let mut count: usize = 0;

    for tk in tokens {
        container_manager.check(tk);

        if !container_manager.is_free() {
            continue;
        }

        if search == tk {
            count += 1;
        }
    }

    count
}

pub fn find_free_token(tokens: &[Token], search: &Token, offset: usize) -> Option<usize> {
    let mut idx: usize = 0;
    let mut container_manager = CodeContainerManager::new_ext(true, true, true);

    for (i, tk) in tokens.iter().enumerate() {
        container_manager.check(tk);

        if !container_manager.is_free() {
            continue;
        }

        if tk != search {
            continue;
        }

        if idx == offset {
            return Some(i);
        }

        idx += 1;
    }

    None
}

pub fn find_pair_container(tokens: &[Token], idx: usize) -> Option<usize> {
    let start_tk = &tokens[idx];
    let going_forward = match *start_tk {
        Token::OpenParenthesis | Token::OpenCurly | Token::OpenBracket => true,
        Token::CloseParenthesis | Token::CloseCurly | Token::CloseBracket => false,
        _ => panic!("Unexpected token finding a pair: {:?}", start_tk),
    };
    let mut ptr = idx;
    let mut container = CodeContainerManager::new();

    loop {
        if going_forward {
            container.check(&tokens[ptr]);
        } else {
            container.check_reverse(&tokens[ptr]);
        }

        if container.is_free() {
            return Some(ptr);
        }

        if (ptr == 0 && !going_forward) || (ptr == tokens.len() - 1 && going_forward) {
            break;
        }

        ptr = (ptr as isize + if going_forward { 1 } else { -1 }) as usize;
    }

    None
}
