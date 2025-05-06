#!/bin/sh

set -eou pipefail

cargo fmt --check
cargo clippy --all -- -D warnings
cargo nextest run --workspace
cd tree-sitter-brick/ && tree-sitter generate && tree-sitter test && cd ../
# funny regex hack to avoid this instance of the forbidden string from blocking merges
! rg 'DON[T]MERGE'
