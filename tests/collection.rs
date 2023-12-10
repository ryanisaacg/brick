use brick::compile_file;

#[tokio::test]
async fn arrays() {
    compile_file(
        "test",
        "test",
        r#"
let array = [1, 2, 3, 4];
let index = 0;
let total = 0;
while index < 4 {
    total += array[index];
}
total
"#.to_string(),
    ).unwrap();
}

#[tokio::test]
#[should_panic]
async fn bad_arrays() {
    compile_file(
        "test",
        "test",
        r#"
let array = [1, 2, 3, 4];
array[2] = "test";
"#.to_string(),
    ).unwrap();
}
