
default:
  @just --list

# Build the whole project
build:
	cargo build

# Run example file with v2 compilation flag set
example file: build
	RUST_BACKTRACE=1 cargo run --package classyc -- --use-v2 --file="./examples/{{file}}.clss"
