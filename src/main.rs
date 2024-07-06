use gamemaker_plus::compiler::type_check;
use gamemaker_plus::parser::core::parse;
use gamemaker_plus::parser::expr::parse_expr;
use gamemaker_plus::tokenizer::tokenize;
use std::fs;

fn main() {
    if !std::path::Path::new("code.gmpp").exists() {
        panic!("Please create a code.gmpp file");
    }

    let mut args: Vec<String> = std::env::args().collect();
    let code: String = if args.len() < 2 {
        fs::read_to_string("code.gmpp").expect("ERROR TRYING TO ACCESS FILE: code.gmpp")
    } else {
        args.remove(1)
    };

    let tokens = tokenize(code.as_str());
    let ast = parse(&tokens);
}
