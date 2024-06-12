#![allow(unused_crate_dependencies)]
#![doc = include_str!("./doc.md")]

fn main() -> sessionizer::Result<()> {
    let action = sessionizer::CliAction::cli();
    sessionizer::run(action)
}
