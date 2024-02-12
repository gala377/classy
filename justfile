
build:
	cargo build

example file: build
	RUST_BACKTRACE=1 cargo run --package classyc -- --use-v2 --file="./examples/{{file}}.clss"

default:
  @just --list