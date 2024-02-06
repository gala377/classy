
buildall:
	cargo build

runexample: buildall
	RUST_BACKTRACE=1 cargo run --package classyc -- --use-v2 --file="./examples/$(file).clss"
