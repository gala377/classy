
export RUST_BACKTRACE := "1"
export MIRIFLAGS := "-Zmiri-backtrace=full" 

default:
    @just --list

check_flags:
    echo $RUST_BACKTRACE
    echo $MIRIFLAGS

miri_quick:
    cargo +nightly miri run -- \
        --page-size 2048 \
        --page-align 8 \
        --pages-count 10 \
        --allocate-integers 10000 \
        --threads 3 \
        --create-handle-every 1

mtest:
    cargo +nightly miri test 

run_release:
    cargo run --release -- \
        --page-size 524288 \
        --page-align 4096 \
        --pages-count 20 \
        --allocate-integers 65000 \
        --threads 20