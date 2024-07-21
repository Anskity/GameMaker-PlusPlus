use gamemaker_plus::macros::apply_macros;
use gamemaker_plus::parser::core::parse;
use gamemaker_plus::tokenizer::tokenize;
use std::io::Error;

fn main() -> Result<(), Error> {
    if !std::path::Path::new("code.gmpp").exists() {
        panic!("Please create a code.gmpp file");
    }

    let args: Vec<String> = std::env::args().collect();
    let mut code = if args.len() > 1 {
        args[1].clone()
    } else {
        std::fs::read_to_string("code.gmpp").unwrap()
    };

    apply_macros(&mut code);

    let tokens = tokenize(&code);
    let ast = parse(&tokens);
    dbg!(ast);

    Ok(())
}
