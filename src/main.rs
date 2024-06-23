use gamemaker_plus::parser::parse_expr;
use gamemaker_plus::tokenizer::tokenize;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        panic!("PLEASE PROVIDE AN ARGUMENT");
    }

    let code = &args[1];

    let tokens = tokenize(&code.as_str());

    let expr = parse_expr(&tokens.iter().map(|tk| Box::from(tk.clone())).collect(), &0);

    expr.display(0);
}
