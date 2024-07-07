use gamemaker_plus::parser::core::parse;
use gamemaker_plus::tokenizer::tokenize;

fn main() {
    if !std::path::Path::new("code.gmpp").exists() {
        panic!("Please create a code.gmpp file");
    }

    let args: Vec<String> = std::env::args().collect();
    let code = if args.len() > 1 {
        args[1].clone()
    } else {
        std::fs::read_to_string("code.gmpp").unwrap()
    };

    let tokens = tokenize(&code);
    println!("{:?}", tokens);
    let ast = parse(&tokens);

    dbg!(ast);
}
