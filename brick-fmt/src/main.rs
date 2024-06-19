use std::io::Read;

use brick_fmt::format_str;

fn main() {
    let mut input = Vec::new();
    let stdin = std::io::stdin();
    let mut handle = stdin.lock();
    handle.read_to_end(&mut input).unwrap();

    let input = String::from_utf8(input).unwrap();
    let output = format_str(&input).unwrap();

    println!("{output}");
}
