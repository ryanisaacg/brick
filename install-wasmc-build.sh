#!/bin/sh

set -euo pipefail

cargo build -p brick-browser-runtime --target wasm32-unknown-unknown --release
wasm-opt -O4 target/wasm32-unknown-unknown/release/brick_browser_runtime.wasm -o target/wasm32-unknown-unknown/release/brick_browser_runtime.wasm
cargo install --path brick-wasmc --locked
