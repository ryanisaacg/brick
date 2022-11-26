use brick::parser::tokenize;

fn main() {
    for token in tokenize("1 + 2") {
        println!("{}", token.unwrap());
    }
}
