use std::{
    fmt::Display,
    fs,
    path::{Path, PathBuf},
};

use anyhow::Error;
use glob::glob;

#[derive(Clone, Debug, PartialEq)]
pub enum TestExpectation {
    Compiles,
    DoesNotCompile,
    Aborts,
    ProducesValue(TestValue),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TestValue {
    Void,
    Null,
    Nullable(Box<TestValue>),
    Int(i64),
    Float(f64),
    String(String),
}

#[derive(Debug)]
enum TestSuccessOrFailure {
    Succeeded(PathBuf),
    FailsToCompile(PathBuf, Error),
    CompiledButShouldnt(PathBuf),
    RanButShouldnt(PathBuf),
    MismatchedResult {
        path: PathBuf,
        expected: TestValue,
        received: TestValue,
    },
}

impl Display for TestSuccessOrFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestSuccessOrFailure::Succeeded(path) => write!(f, "{}: Succeeded", path_display(path)),
            TestSuccessOrFailure::FailsToCompile(path, error) => {
                write!(f, "{}: Compilation failed\n{error:?}\n", path_display(path))
            }
            TestSuccessOrFailure::CompiledButShouldnt(path) => write!(
                f,
                "{}: Compilation succeeded, expected failure",
                path_display(path)
            ),
            TestSuccessOrFailure::RanButShouldnt(path) => write!(
                f,
                "{}: Program compiled and ran, expected panic",
                path_display(path),
            ),
            TestSuccessOrFailure::MismatchedResult {
                path,
                expected,
                received,
            } => write!(
                f,
                "{}: Expected {expected:?}, received {received:?}",
                path_display(path)
            ),
        }
    }
}

fn path_display(path: &Path) -> String {
    let component_count = path.components().count();
    let mut last_two = path.components().skip(component_count - 2).take(2);
    let folder = last_two.next().unwrap().as_os_str().to_str().unwrap();
    let path = last_two.next().unwrap().as_os_str().to_str().unwrap();

    format!("{folder}/{path}")
}

pub fn test_folder(
    mut path: PathBuf,
    check_does_compile: impl Fn(&str) -> anyhow::Result<()> + Send + Sync,
    execute: impl Fn(&str, &TestValue) -> anyhow::Result<TestValue> + Send + Sync,
) {
    use rayon::prelude::*;

    path.push("**");
    path.push("*.brick");
    let paths: Vec<_> = glob(path.to_str().unwrap()).unwrap().collect();

    let results: Vec<_> = paths
        .into_par_iter()
        .map(|entry| {
            let path = entry.unwrap();
            let contents = fs::read_to_string(&path).unwrap();
            match parse_intended_result(&contents) {
                TestExpectation::Compiles => match check_does_compile(&contents) {
                    Ok(_) => TestSuccessOrFailure::Succeeded(path),
                    Err(error) => TestSuccessOrFailure::FailsToCompile(path, error),
                },
                TestExpectation::DoesNotCompile => match check_does_compile(&contents) {
                    Ok(_) => TestSuccessOrFailure::CompiledButShouldnt(path),
                    Err(_) => TestSuccessOrFailure::Succeeded(path),
                },
                TestExpectation::Aborts => {
                    if check_does_compile(&contents).is_ok()
                        && execute(&contents, &TestValue::Void).is_err()
                    {
                        TestSuccessOrFailure::Succeeded(path)
                    } else {
                        TestSuccessOrFailure::RanButShouldnt(path)
                    }
                }
                TestExpectation::ProducesValue(expected) => match execute(&contents, &expected) {
                    Ok(received) if expected == received => TestSuccessOrFailure::Succeeded(path),
                    Ok(received) => TestSuccessOrFailure::MismatchedResult {
                        path,
                        expected,
                        received,
                    },
                    Err(error) => TestSuccessOrFailure::FailsToCompile(path, error),
                },
            }
        })
        .collect();

    let success_count = results
        .iter()
        .filter(|test| matches!(test, TestSuccessOrFailure::Succeeded(_)))
        .count();

    println!("{success_count} tests passed");
    if success_count < results.len() {
        println!("Failed tests:");
        for result in results.iter() {
            if !matches!(result, TestSuccessOrFailure::Succeeded(_)) {
                println!("  {result}");
            }
        }
        panic!();
    }
}

fn parse_intended_result(contents: &str) -> TestExpectation {
    let first_line = contents.split('\n').next().unwrap();
    let first_line = first_line.replace("// ", "");
    let result = first_line.trim();
    match result {
        "Compile" => TestExpectation::Compiles,
        "NoCompile" => TestExpectation::DoesNotCompile,
        "Void" => TestExpectation::ProducesValue(TestValue::Void),
        "Abort" => TestExpectation::Aborts,
        results => {
            let mut components = results.split(" | ");
            let ty = components.next().unwrap();
            let value = components.next().unwrap();
            TestExpectation::ProducesValue(if let Some(ty) = ty.strip_prefix('?') {
                TestValue::Nullable(Box::new(parse_test_value(ty, value)))
            } else {
                parse_test_value(ty, value)
            })
        }
    }
}

fn parse_test_value(ty: &str, value: &str) -> TestValue {
    match ty {
        "Int" => TestValue::Int(value.parse().unwrap()),
        "Float" => TestValue::Float(value.parse().unwrap()),
        "String" => TestValue::String(value.to_string()),
        other => panic!("Unexpected return type marker: {other}"),
    }
}
