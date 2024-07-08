pub fn apply_macros(code: &mut String) {
    let mut user_macros: Vec<(String, String)> = Vec::new();

    while code.contains("#macro") {
        let start = code.find("#macro").unwrap();

        let mut ptr = start + "#macro".len();

        while code.chars().nth(ptr).is_some_and(|c| c == ' ') {
            ptr += 1;
        }

        let mut macro_name = String::new();
        while code
            .chars()
            .nth(ptr)
            .is_some_and(|c| c.is_alphabetic() || c == '_')
        {
            macro_name.push(code.chars().nth(ptr).unwrap());
            ptr += 1;
        }

        let mut macro_code = String::new();
        while code.chars().nth(ptr).is_some_and(|c| c != '\n') {
            macro_code.push(code.chars().nth(ptr).unwrap());
            ptr += 1;
        }

        println!("{:?}", macro_name);
        println!("{:?}", macro_code);

        code.replace_range(start..=ptr, "");

        user_macros.push((macro_name, macro_code));
    }

    println!("{:?}", user_macros);

    for (macro_name, macro_code) in user_macros {
        let search_code = code.clone();
        let matches = search_code.match_indices(&macro_name).collect::<Vec<_>>();
        let mut amount = matches.len();
        while amount > 0 {
            let (pos, _) = matches[amount - 1];

            if (pos == 0
                || code
                    .chars()
                    .nth(pos - 1)
                    .is_some_and(|c| !(c.is_alphabetic() || c == '_')))
                && code
                    .chars()
                    .nth(pos + macro_name.len())
                    .is_some_and(|c| !(c.is_alphabetic() || c == '_'))
            {
                let range = pos..pos + macro_name.len();
                code.replace_range(range, &macro_code);
            }

            amount -= 1;
        }
    }
}
