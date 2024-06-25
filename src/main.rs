use gamemaker_plus::parser::parse;
use gamemaker_plus::tokenizer::tokenize;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        panic!("PLEASE PROVIDE AN ARGUMENT");
    }

    let code = &args[1];

    let tokens = tokenize(&code.as_str());

    let expr = parse(&tokens);

    dbg!(expr);
}
