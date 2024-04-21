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
    Int(i64),
    Float(f64),
}

#[derive(Debug)]
pub enum TestResult {
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

impl Display for TestResult {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestResult::Succeeded(path) => write!(f, "{}: Succeeded", path_display(path)),
            TestResult::FailsToCompile(path, error) => {
                write!(f, "{}: Compilation failed\n{error}\n", path_display(path))
            }
            TestResult::CompiledButShouldnt(path) => write!(
                f,
                "{}: Compilation succeeded, expected failure",
                path_display(path)
            ),
            TestResult::RanButShouldnt(path) => write!(
                f,
                "{}: Program compiled and ran, expected panic",
                path_display(path),
            ),
            TestResult::MismatchedResult {
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

fn path_display(path: &Path) -> &str {
    path.file_name().unwrap().to_str().unwrap()
}

pub fn test_folder(
    mut path: PathBuf,
    check_does_compile: impl Fn(&str) -> anyhow::Result<()> + Send + Sync,
    execute: impl Fn(&str) -> anyhow::Result<TestValue> + Send + Sync,
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
                    Ok(_) => TestResult::Succeeded(path),
                    Err(error) => TestResult::FailsToCompile(path, error),
                },
                TestExpectation::DoesNotCompile => match check_does_compile(&contents) {
                    Ok(_) => TestResult::CompiledButShouldnt(path),
                    Err(_) => TestResult::Succeeded(path),
                },
                TestExpectation::Aborts => {
                    if check_does_compile(&contents).is_ok() && execute(&contents).is_err() {
                        TestResult::Succeeded(path)
                    } else {
                        TestResult::RanButShouldnt(path)
                    }
                }
                TestExpectation::ProducesValue(expected) => match execute(&contents) {
                    Ok(received) if expected == received => TestResult::Succeeded(path),
                    Ok(received) => TestResult::MismatchedResult {
                        path,
                        expected,
                        received,
                    },
                    Err(error) => TestResult::FailsToCompile(path, error),
                },
            }
        })
        .collect();

    let success_count = results
        .iter()
        .filter(|test| matches!(test, TestResult::Succeeded(_)))
        .count();

    println!("{success_count} tests passed");
    if success_count < results.len() {
        println!("Failed tests:");
        for result in results.iter() {
            if !matches!(result, TestResult::Succeeded(_)) {
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
            TestExpectation::ProducesValue(match ty {
                "Int" => TestValue::Int(value.parse().unwrap()),
                "Float" => TestValue::Float(value.parse().unwrap()),
                other => panic!("Unexpected return type marker: {other}"),
            })
        }
    }
}
