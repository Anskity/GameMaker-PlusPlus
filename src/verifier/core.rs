use std::io::Error;

use crate::{ast::Node, parser_macros::throw_err};
pub fn verify_code(code: Node) -> Result<(), Error> {
    match code {
        Node::Program(_) => {}
        _ => {
            throw_err!("Can't verify a node that isn't a program node");
        }
    }

    Ok(())
}
