use std::sync::{Arc, Mutex};

use anyhow::bail;
use brick::{check_types, interpret_code, SourceFile, Value};
use brick_fmt::format_str;
use data_test_driver::TestValue;

#[test]
fn data() {
    let mut working_dir = std::env::current_dir().unwrap();
    working_dir.pop();
    working_dir.push("tests");
    data_test_driver::test_folder(
        working_dir,
        |contents| -> anyhow::Result<()> {
            // This doesn't support multiple modules in a single test yet
            let first_pass_contents = format_str(contents)?;
            let second_pass_contents = format_str(first_pass_contents.as_str())?;

            if first_pass_contents != second_pass_contents {
                bail!("Formatting differed in second pass")
            }

            check_types(&second_pass_contents)?;

            Ok(())
        },
        |contents, expected| -> anyhow::Result<TestValue> {
            // This doesn't support multiple modules in a single test yet
            let contents = format_str(contents)?;

            let counter = Arc::new(Mutex::new(0));

            let func_counter = counter.clone();
            let (mut results, memory) = interpret_code(
                vec![SourceFile {
                    module_name: "main",
                    filename: "main.brick",
                    contents: contents.to_string(),
                }],
                vec![(
                    "incr_test_counter",
                    Box::new(move |_, _| {
                        *func_counter.lock().unwrap() += 1;
                        None
                    }),
                )],
            )?;
            let counter = *counter.lock().unwrap();
            look_for_value(&mut results, &memory[..], expected, counter)
        },
        [
            // Coroutines temporarily don't compile
            "coroutine/count_up.brick",
            "coroutine/echo.brick",
            "coroutine/infinite.brick",
            "coroutine/multiple_generators.brick",
            "coroutine/mutable_ref.brick",
            "coroutine/mutable_ref_repeated.brick",
            "coroutine/nested_coroutines.brick",
            "coroutine/other_functions.brick",
            "coroutine/regression_test_branch_in_yielding_loop.brick",
            "coroutine/yield_basic.brick",
            "coroutine/yield_once.brick",
            "coroutine/yield_twice.brick",
        ]
        .into_iter()
        .collect(),
    );
}

fn look_for_value(
    results: &mut Vec<Value>,
    memory: &[u8],
    expected: &TestValue,
    counter: u32,
) -> anyhow::Result<TestValue> {
    match expected {
        TestValue::Void => {
            if results.is_empty() {
                Ok(TestValue::Void)
            } else {
                bail!("non-void result returned from test case");
            }
        }
        TestValue::Null => {
            if results.len() != 2 {
                bail!("wrong number of results returned: {results:?}, expected 2")
            } else if results[1] != Value::Byte(0) {
                bail!("non-null result: {results:?}")
            } else {
                Ok(TestValue::Null)
            }
        }
        TestValue::Nullable(expected) => {
            let first = results.pop();
            if first != Some(Value::Byte(1)) {
                bail!("expected non-null marker, found {first:?}");
            }
            Ok(TestValue::Nullable(Box::new(look_for_value(
                results, memory, expected, counter,
            )?)))
        }
        TestValue::Float(_) | TestValue::Int(_) => {
            if results.len() == 1 {
                Ok(value_to_test_value(results.remove(0)))
            } else {
                bail!(
                    "wrong number of results returned: {:?}, expected 1",
                    results
                )
            }
        }
        TestValue::String(_) => {
            if results.len() == 2 {
                let length = results.pop().unwrap();
                let Value::Size(length) = length else {
                    bail!("non-length type returned: {:?}", length);
                };
                let pointer = results.pop().unwrap();
                let Value::Size(pointer) = pointer else {
                    bail!("non-pointer type returned: {:?}", pointer);
                };
                let bytes = &memory[pointer..(pointer + length)];
                let string = std::str::from_utf8(bytes)?;
                Ok(TestValue::String(string.to_string()))
            } else {
                bail!(
                    "wrong number of results returned: {:?}, expected 2",
                    results
                )
            }
        }
        TestValue::Counter(_) => Ok(TestValue::Counter(counter)),
    }
}

fn value_to_test_value(val: Value) -> TestValue {
    match val {
        Value::FunctionID(_) => todo!(),
        Value::Size(val) => TestValue::Int(val as i64),
        Value::Byte(byte) => TestValue::Int(byte as i64),
        Value::Int32(val) => TestValue::Int(val as i64),
        Value::Int64(val) => TestValue::Int(val),
        Value::Float32(val) => TestValue::Float(val as f64),
        Value::Float64(val) => TestValue::Float(val),
    }
}
