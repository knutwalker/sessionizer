# List available recipes
default:
    @just --justfile {{justfile()}} --list --unsorted

# Run a lox script
run example:
    cargo run --release --example {{example}}

# Compile and lint checking
check:
    cargo check
    cargo clippy

# Run all tests
test:
    cargo test

# Run all tests with nextest
nextest:
    cargo nextest run

# Continuously test
ctest:
    cargo watch -x test

# Continuously run nextest
cntest:
    cargo watch -x 'nextest run'

# Generate the README file
readme:
    cargo doc2readme --expand-macros

# aliases
alias c := check
alias t := test
alias nt := nextest
