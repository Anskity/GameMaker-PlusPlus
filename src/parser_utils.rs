use crate::{code_container::CodeContainerManager, tokenizer::Token};
pub fn amount_of_tokens(tokens: &Vec<Token>, search: Token) -> usize {
    let mut container_manager = CodeContainerManager::new();
    let mut count: usize = 0;

    for tk in tokens {
        container_manager.check(tk);

        if !container_manager.is_free() {
            continue;
        }

        if search == *tk {
            count += 1;
        }
    }

    count
}

pub fn find_free_token(
    tokens: &Vec<Token>,
    search: Token,
    offset: usize,
    check_ternary: bool,
) -> Option<usize> {
    let mut idx: usize = 0;
    let mut container_manager = CodeContainerManager::new_ext(true, true, true, check_ternary);

    for (i, tk) in tokens.iter().enumerate() {
        container_manager.check(tk);

        if !container_manager.is_free() {
            continue;
        }

        if *tk != search {
            continue;
        }

        if idx == offset {
            return Some(i);
        }

        idx += 1;
    }

    None
}
