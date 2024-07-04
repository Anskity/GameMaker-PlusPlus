use gamemaker_plus::parser::core::parse;
use gamemaker_plus::parser::expr::parse_expr;
use gamemaker_plus::tokenizer::tokenize;
use std::fs;

fn main() {
    expr_parsing_impl();
}

#[allow(dead_code)]
fn expr_parsing_impl() {
    let mut args: Vec<String> = std::env::args().collect();
    let code: String = if args.len() < 2 {
        fs::read_to_string("code.gmpp").expect("ERROR TRYING TO ACCESS FILE: code.gmpp")
    } else {
        args.remove(1)
    };

    let tokens = tokenize(&code.as_str());

    let syntax_tree = parse_expr(&tokens.to_vec());

    dbg!(syntax_tree);
}

#[allow(dead_code)]
fn tokenizing_impl() {
    let mut args: Vec<String> = std::env::args().collect();
    let code: String = if args.len() < 2 {
        fs::read_to_string("code.gmpp").expect("ERROR TRYING TO ACCESS FILE: code.gmpp")
    } else {
        args.remove(1)
    };

    let tokens = tokenize(&code.as_str());

    dbg!(tokens);
}

#[allow(dead_code)]
fn parsing_impl() {
    let mut args: Vec<String> = std::env::args().collect();
    let code: String = if args.len() < 2 {
        fs::read_to_string("code.gmpp").expect("ERROR TRYING TO ACCESS FILE: code.gmpp")
    } else {
        args.remove(1)
    };

    let tokens = tokenize(&code.as_str());
    let syntax_tree = parse(&tokens);

    dbg!(syntax_tree);
}
