
default:
    @just --list


miri_quick:
    RUST_BACKTRACE=1 MIRIFLAGS=-Zmiri-backtrace=full cargo +nightly miri run -- --page-size 2048 --page-align 8 --pages-count 2 --allocate-integers 10 --threads 3