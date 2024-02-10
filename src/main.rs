//! ## Select a new tmux session from a list of running sessions or a selection of projects.
//!
//! # Installation
//!
//! ```sh
//! $ cargo install sessionizer
//! ```
//!
//! # Usage
//!
//! ```sh
//! $ sessionizer --help
//! ```

#![allow(unused_crate_dependencies)]

fn main() -> kommandozeile::Result<()> {
    let args = sessionizer::Args::init();
    sessionizer::run(&args)
}
