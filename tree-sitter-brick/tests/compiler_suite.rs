use data_test_driver::{find_tests, load_test_contents, TestExpectation};

#[test]
fn data() {
    let mut test_dir = std::env::current_dir().unwrap();
    test_dir.pop();
    test_dir.push("tests");

    let test_contents: Vec<_> = find_tests(test_dir)
        .iter()
        .map(load_test_contents)
        .filter(|contents| {
            // Don't include tests with compilation errors, because they may have syntax errors
            contents.expectation != TestExpectation::DoesNotCompile
        })
        .collect();
    let mut failed_tests = Vec::new();
    for test_contents in test_contents {
        let mut parser = tree_sitter::Parser::new();
        parser
            .set_language(&tree_sitter_brick::LANGUAGE.into())
            .unwrap();
        let tree = parser.parse(&test_contents.contents, None).unwrap();
        let has_error = tree.root_node().has_error();
        if has_error {
            failed_tests.push(test_contents.name);
        }
    }
    if !failed_tests.is_empty() {
        println!("Failed tests:\n{}", failed_tests.join("\n"));
        panic!();
    }
}
