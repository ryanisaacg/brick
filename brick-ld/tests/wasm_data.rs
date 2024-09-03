use std::sync::{Arc, Mutex};

use anyhow::{bail, Context};
use brick::SourceFile;
use brick_ld::InputModule;
use brick_wasm_backend::{compile, BackendOptions};
use data_test_driver::{find_tests, load_test_contents, TestExpectation, TestValue};
use rayon::prelude::*;
use runtime_binary::wasm_runtime;
use wasmtime::{Engine, Func, Linker, Memory, Module, Store, Val};

#[test]
fn data() {
    let mut test_dir = std::env::current_dir().unwrap();
    test_dir.pop();
    test_dir.push("tests");

    let test_contents: Vec<_> = find_tests(test_dir)
        .iter()
        .map(load_test_contents)
        .filter(|contents| {
            // Only include tests that meaningfully run - compiles/nocompiles/aborts all handled by
            // brick-wasmtime's tests
            matches!(contents.expectation, TestExpectation::ProducesValue(_))
        })
        .collect();
    test_contents
        .into_par_iter()
        // Chunked because of 100-table-limit from wasmtime. combining funcref tables would obviate
        // the need for chunking
        .chunks(99)
        .for_each(|test_contents| {
            // Compile all code in the chunk
            let binaries = test_contents
                .iter()
                .map(|contents| -> anyhow::Result<Vec<u8>> {
                    Ok(compile(
                        &contents
                            .sources
                            .iter()
                            .map(|path| SourceFile::from_filename(path).unwrap())
                            .collect::<Vec<_>>(),
                        BackendOptions {
                            include_start_marker: false,
                            top_level_name: contents.name.as_str(),
                        },
                    )?
                    .finish())
                })
                .collect::<Result<Vec<_>, _>>()
                .unwrap();
            // Link the code in the chunk
            let mut modules: Vec<_> = test_contents
                .iter()
                .zip(binaries.iter())
                .map(|(content, binary)| InputModule {
                    name: content.name.as_str(),
                    definition: binary,
                    public_exports: true,
                    is_start: false,
                })
                .collect();
            modules.push(InputModule {
                name: "brick-runtime",
                definition: wasm_runtime(),
                public_exports: false,
                is_start: false,
            });
            let test_binary = brick_ld::link(&modules).unwrap();

            // Create a WASM engine and instantiate the chunk once
            let engine = Engine::default();
            let module = Module::from_binary(&engine, test_binary.as_slice()).unwrap();

            // Run all test cases in the chunk
            for test_case in test_contents {
                let mut store = Store::new(&engine, ());
                let mut linker = Linker::new(&engine);
                let counter = Arc::new(Mutex::new(0));
                let fn_counter = counter.clone();
                linker
                    .func_wrap("bindings", "incr_test_counter", move || {
                        *fn_counter.lock().unwrap() += 1;
                    })
                    .unwrap();

                let TestExpectation::ProducesValue(test_value) = &test_case.expectation else {
                    continue;
                };
                let instance = linker.instantiate(&mut store, &module).unwrap();
                let Some(func) = instance.get_func(&mut store, &test_case.name) else {
                    panic!("test not found: {}", test_case.name);
                };
                let memory = instance
                    .get_memory(&mut store, "memory")
                    .context("failed to find memory")
                    .unwrap();
                look_for_value(&mut store, memory, func, test_value, counter).unwrap();
                println!("{}", test_case.name);
            }
        });
}

fn look_for_value(
    store: &mut Store<()>,
    memory: Memory,
    func: Func,
    expected: &TestValue,
    counter: Arc<Mutex<u32>>,
) -> anyhow::Result<TestValue> {
    match expected {
        TestValue::Void => {
            func.call(store, &[], &mut [])?;

            Ok(TestValue::Void)
        }
        TestValue::Null => {
            let mut results = [Val::I32(-1), Val::I32(-1)];
            func.call(store, &[], &mut results)?;

            if results[1].i32() == Some(0) {
                Ok(TestValue::Null)
            } else {
                bail!("wrong result returned: {:?}", results)
            }
        }
        TestValue::Int(_) => {
            let mut results = [Val::I32(-1)];
            func.call(store, &[], &mut results)?;

            if let Some(int_val) = results[0].i32() {
                Ok(TestValue::Int(int_val as i64))
            } else {
                bail!("wrong result returned: {:?}", results[0])
            }
        }
        TestValue::Float(_) => {
            let mut results = [Val::F32(f32::to_bits(-1.0))];
            func.call(store, &[], &mut results)?;

            if let Some(int_val) = results[0].f32() {
                Ok(TestValue::Float(int_val as f64))
            } else {
                bail!("wrong result returned: {:?}", results[0])
            }
        }
        TestValue::Nullable(expected) => {
            let mut results = [
                match expected.as_ref() {
                    TestValue::Int(_) => Val::I32(-1),
                    TestValue::Float(_) => Val::F32(0),
                    _ => todo!(),
                },
                Val::I32(-1),
            ];

            func.call(store, &[], &mut results)?;

            if results[1].unwrap_i32() == 0 {
                Ok(TestValue::Null)
            } else if results[1].unwrap_i32() == 1 {
                Ok(TestValue::Nullable(Box::new(match expected.as_ref() {
                    TestValue::Int(_) => TestValue::Int(results[0].unwrap_i32() as i64),
                    TestValue::Float(_) => TestValue::Float(results[0].unwrap_f32() as f64),
                    _ => todo!(),
                })))
            } else {
                bail!("non-binary result for null bit")
            }
        }
        TestValue::String(_) => {
            let mut results = [Val::I32(-1), Val::I32(-1)];
            func.call(&mut *store, &[], &mut results)?;
            let ptr = results[0].unwrap_i32() as usize;
            let len = results[1].unwrap_i32() as usize;
            let slice = &memory.data(&store)[ptr..(ptr + len)];
            let string = std::str::from_utf8(slice)?;
            Ok(TestValue::String(string.to_string()))
        }
        TestValue::Counter(_) => {
            func.call(store, &[], &mut [])?;
            let counter = *counter.lock().unwrap();
            Ok(TestValue::Counter(counter))
        }
    }
}
