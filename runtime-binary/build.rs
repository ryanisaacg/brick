use std::process::Command;

// Example custom build script.
fn main() {
    let mut crate_path = std::env::current_dir().unwrap().canonicalize().unwrap();
    crate_path.push("../brick-runtime");
    let crate_path = crate_path.canonicalize().unwrap();

    let mut target_path = crate_path.clone();
    target_path.push("target");

    let exit_code = Command::new("cargo")
        .current_dir(crate_path)
        .env("CARGO_TARGET_DIR", target_path)
        .arg("rustc")
        .arg("--crate-type")
        .arg("cdylib")
        .arg("--target")
        .arg("wasm32-unknown-unknown")
        .arg("--release")
        .spawn()
        .expect("failed to start wasm build")
        .wait()
        .expect("wasm build failed");

    if !exit_code.success() {
        panic!("wasm build failed: {exit_code}");
    }
}
