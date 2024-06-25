use gamemaker_plus::parser::parse;
use gamemaker_plus::tokenizer::tokenize;
use std::fs;

fn main() {
    let mut args: Vec<String> = std::env::args().collect();
    let code: String = if args.len() < 2 {
        fs::read_to_string("code.gmpp").unwrap()
    } else {
        args.remove(1)
    };

    let tokens = tokenize(&code.as_str());
    let syntax_tree = parse(&tokens);

    dbg!(syntax_tree);
}
