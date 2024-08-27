use std::{
    any::Any,
    collections::HashSet,
    fmt::Display,
    fs,
    panic::{self, RefUnwindSafe, UnwindSafe},
    path::{Path, PathBuf},
};

use anyhow::Error;
use glob::glob;

pub fn test_folder(
    path: PathBuf,
    check_does_compile: impl Fn(&[&'static str]) -> anyhow::Result<()>
        + Send
        + Sync
        + UnwindSafe
        + RefUnwindSafe,
    execute: impl (Fn(&[&'static str], &TestValue) -> anyhow::Result<TestValue>)
        + Send
        + Sync
        + UnwindSafe
        + RefUnwindSafe,
    should_fail: HashSet<&str>,
) {
    use rayon::prelude::*;

    let paths = find_tests(path);

    let results: Vec<_> = paths
        .into_par_iter()
        .map(|test_case| {
            let root = test_case.root.clone();
            let TestContents {
                expectation,
                sources,
                ..
            } = load_test_contents(&test_case);
            let result = panic::catch_unwind(|| match expectation {
                TestExpectation::Compiles => match check_does_compile(&sources) {
                    Ok(_) => TestSuccessOrFailure::Succeeded(root),
                    Err(error) => TestSuccessOrFailure::FailsToCompile(root, error),
                },
                TestExpectation::DoesNotCompile => match check_does_compile(&sources) {
                    Ok(_) => TestSuccessOrFailure::CompiledButShouldnt(root),
                    Err(_) => TestSuccessOrFailure::Succeeded(root),
                },
                TestExpectation::Aborts => {
                    if check_does_compile(&sources).is_ok()
                        && execute(&sources, &TestValue::Void).is_err()
                    {
                        TestSuccessOrFailure::Succeeded(root)
                    } else {
                        TestSuccessOrFailure::RanButShouldnt(root)
                    }
                }
                TestExpectation::ProducesValue(expected) => match execute(&sources, &expected) {
                    Ok(received) if expected == received => TestSuccessOrFailure::Succeeded(root),
                    Ok(received) => TestSuccessOrFailure::MismatchedResult {
                        path: root,
                        expected,
                        received,
                    },
                    Err(error) => TestSuccessOrFailure::ErroredWhenRun(root, error),
                },
            });
            let result = match result {
                Ok(result) => result,
                Err(panic) => TestSuccessOrFailure::PanickedWhenRun(test_case.root.clone(), panic),
            };
            (test_case.root, result)
        })
        .map(|(path, result)| {
            if should_fail.contains(&path_display(&path).as_str()) {
                match result {
                    TestSuccessOrFailure::Succeeded(path) => {
                        TestSuccessOrFailure::SucceededButMarkedAsShouldFail(path)
                    }
                    _ => TestSuccessOrFailure::Succeeded(path),
                }
            } else {
                result
            }
        })
        .collect();

    let success_count = results
        .iter()
        .filter(|test| matches!(test, TestSuccessOrFailure::Succeeded(_)))
        .count()
        - should_fail.len();
    let failed_count = results.len() - (success_count + should_fail.len());

    println!("Failed tests:");
    for result in results.iter() {
        if !matches!(result, TestSuccessOrFailure::Succeeded(_)) {
            println!("  {result}");
        }
    }
    println!("{success_count} tests passed");
    if !should_fail.is_empty() {
        println!("{} tests failed as expected", should_fail.len());
    }
    println!("{failed_count} tests failed");
    if failed_count > 0 {
        panic!();
    }
}

pub fn find_tests(test_root: PathBuf) -> Vec<TestCase> {
    let mut source_files = test_root.clone();
    source_files.push("**");
    source_files.push("*.brick");
    let mut paths: Vec<_> = glob(source_files.to_str().unwrap())
        .unwrap()
        .filter_map(|path| {
            let path = path.unwrap();
            if path.to_str().unwrap().contains("package") {
                None
            } else {
                Some(TestCase {
                    root: path.clone(),
                    sources: vec![path],
                })
            }
        })
        .collect();

    let mut packages = test_root.clone();
    packages.push("packages");
    packages.push("**");
    packages.push("main.brick");
    for root in glob(packages.to_str().unwrap()).unwrap() {
        let root = root.unwrap();
        let sources = std::fs::read_dir(root.parent().unwrap())
            .unwrap()
            .map(|entry| entry.unwrap().path())
            .collect();
        paths.push(TestCase { root, sources });
    }

    paths
}

pub fn load_test_contents(TestCase { root, sources }: &TestCase) -> TestContents {
    let contents = fs::read_to_string(root).unwrap();
    let sources: Vec<_> = sources
        .iter()
        .map(|path| path.to_str().unwrap().to_string().leak() as &'static str)
        .collect();
    let expectation = parse_intended_result(&contents);
    TestContents {
        name: path_display(root),
        expectation,
        sources,
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
                if value == "null" {
                    TestValue::Null
                } else {
                    TestValue::Nullable(Box::new(parse_test_value(ty, value)))
                }
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
        "Counter" => TestValue::Counter(value.parse().unwrap()),
        other => panic!("Unexpected return type marker: {other}"),
    }
}

pub struct TestCase {
    pub root: PathBuf,
    pub sources: Vec<PathBuf>,
}

#[derive(Debug)]
pub struct TestContents {
    pub name: String,
    pub expectation: TestExpectation,
    pub sources: Vec<&'static str>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TestExpectation {
    Compiles,
    DoesNotCompile,
    Aborts,
    ProducesValue(TestValue),
}

#[derive(Clone, Debug)]
pub enum TestValue {
    Void,
    Null,
    Nullable(Box<TestValue>),
    Int(i64),
    Float(f64),
    String(String),
    Counter(u32),
}

impl PartialEq for TestValue {
    fn eq(&self, other: &Self) -> bool {
        use TestValue::*;
        match (self, other) {
            (Void, Void) => true,
            (Null, Null) => true,
            (Nullable(a), Nullable(b)) => a == b,
            (Int(a), Int(b)) => a == b,
            (String(a), String(b)) => a == b,
            (Counter(a), Counter(b)) => a == b,
            (Float(a), Float(b)) => (a - b).abs() < 0.000001,
            _ => false,
        }
    }
}

#[derive(Debug)]
enum TestSuccessOrFailure {
    Succeeded(PathBuf),
    FailsToCompile(PathBuf, Error),
    ErroredWhenRun(PathBuf, Error),
    PanickedWhenRun(PathBuf, Box<dyn Any + Send>),
    CompiledButShouldnt(PathBuf),
    RanButShouldnt(PathBuf),
    MismatchedResult {
        path: PathBuf,
        expected: TestValue,
        received: TestValue,
    },
    SucceededButMarkedAsShouldFail(PathBuf),
}

impl Display for TestSuccessOrFailure {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TestSuccessOrFailure::Succeeded(path) => write!(f, "{}: Succeeded", path_display(path)),
            TestSuccessOrFailure::FailsToCompile(path, error) => {
                write!(f, "{}: Compilation failed\n{error:?}\n", path_display(path))
            }
            TestSuccessOrFailure::ErroredWhenRun(path, error) => {
                write!(f, "{}: Failed at runtime\n{error:?}\n", path_display(path))
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
            TestSuccessOrFailure::SucceededButMarkedAsShouldFail(path) => write!(
                f,
                "{}: Test succeeded, but it was marked as should fail",
                path_display(path)
            ),
            TestSuccessOrFailure::PanickedWhenRun(path, panic) => write!(
                f,
                "{}: Test panicked\n{}\n",
                path_display(path),
                panic
                    .downcast_ref::<&str>()
                    .unwrap_or(&"<No panic message provided>"),
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
