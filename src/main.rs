#![allow(unused_crate_dependencies)]
#![doc = include_str!("./doc.md")]

fn main() -> kommandozeile::Result<()> {
    let args = sessionizer::Args::init();
    sessionizer::run(&args)
}
