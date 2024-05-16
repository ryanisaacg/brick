use brick_wasm_backend::compile;

fn main() {
    let module = compile(
        "main",
        "example.brick",
        std::fs::read_to_string("example.brick").unwrap(),
        true,
    )
    .unwrap();
    std::fs::write("example.wasm", module.as_slice()).unwrap();
}
