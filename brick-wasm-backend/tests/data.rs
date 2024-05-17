use anyhow::{bail, Context};
use brick_wasm_backend::compile;
use data_test_driver::TestValue;
use wasmtime::{AsContextMut, Caller, Engine, Extern, Func, Linker, Memory, Module, Store, Val};

#[test]
fn data() {
    let mut working_dir = std::env::current_dir().unwrap();
    working_dir.pop();
    working_dir.push("tests");
    data_test_driver::test_folder(
        working_dir,
        |contents| -> anyhow::Result<()> {
            compile("main", "main.brick", contents.to_string(), false)?;
            Ok(())
        },
        |contents, expected| -> anyhow::Result<TestValue> {
            let binary = compile("main", "main.brick", contents.to_string(), false)?.finish();
            let engine = Engine::default();
            let module = Module::from_binary(&engine, binary.as_slice())?;
            let mut store = Store::new(&engine, ());
            let mut linker = Linker::new(&engine);
            linker.func_wrap(
                "brick-runtime",
                "brick_memcpy",
                |mut caller: Caller<'_, ()>, dest: i32, source: i32, len: i32| {
                    let Extern::Memory(mem) = caller.get_export("memory").unwrap() else {
                        unreachable!();
                    };
                    let store = caller.as_context_mut();
                    let mem_ptr = mem.data_ptr(store);
                    unsafe {
                        let dest = mem_ptr.add(dest as usize);
                        let source = mem_ptr.add(source as usize);
                        brick_runtime::brick_memcpy(dest, source, len as usize);
                    }
                },
            )?;
            let instance = linker.instantiate(&mut store, &module)?;
            let func = instance
                .get_func(&mut store, "main")
                .context("failed to find main")?;
            let memory = instance
                .get_memory(&mut store, "memory")
                .context("failed to find memory")?;

            look_for_value(store, memory, func, expected)
        },
        [
            "collections/append_to_array.brick",
            "collections/basic_dict_keys.brick",
            "collections/dict_contains.brick",
            "collections/insert_existing_dict.brick",
            "collections/insert_new_in_dict.brick",
            "collections/write_to_dict.brick",
            "coroutine/count_up.brick",
            "coroutine/echo.brick",
            "coroutine/infinite.brick",
            "coroutine/multiple_generators.brick",
            "coroutine/mutable_ref.brick",
            "coroutine/nested_coroutines.brick",
            "coroutine/other_functions.brick",
            "coroutine/regression_test_branch_in_yielding_loop.brick",
            "coroutine/yield_basic.brick",
            "coroutine/yield_once.brick",
            "coroutine/yield_twice.brick",
            "functions/can_assign_structs.brick",
            "interfaces/can_assign_structs.brick",
            "nullability/basic_null.brick",
            "nullability/null_coalesce.brick",
            "nullability/null_traverse_is_null.brick",
            "nullability/null_traverse_union.brick",
            "strings/concat.brick",
            "unions/basic_construction.brick",
            "unions/incorrect_access.brick",
            "unions/case_statement_extracts_value.brick",
            "unions/case_follows_or_pattern.brick",
        ]
        .into_iter()
        .collect(),
    );
}

fn look_for_value(
    mut store: Store<()>,
    memory: Memory,
    func: Func,
    expected: &TestValue,
) -> anyhow::Result<TestValue> {
    match expected {
        TestValue::Void => {
            func.call(store, &[], &mut [])?;

            Ok(TestValue::Void)
        }
        TestValue::Null => {
            let mut results = [Val::I32(-1)];
            func.call(store, &[], &mut results)?;

            if results[0].i32() == Some(0) {
                Ok(TestValue::Null)
            } else {
                bail!("wrong result returned: {:?}", results[0])
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
                Val::I32(-1),
                match expected.as_ref() {
                    TestValue::Int(_) => Val::I32(-1),
                    TestValue::Float(_) => Val::F32(0),
                    _ => todo!(),
                },
            ];

            func.call(store, &[], &mut results)?;

            if results[0].unwrap_i32() == 0 {
                Ok(TestValue::Null)
            } else if results[0].unwrap_i32() == 1 {
                Ok(TestValue::Nullable(Box::new(match expected.as_ref() {
                    TestValue::Int(_) => TestValue::Int(results[1].unwrap_i32() as i64),
                    TestValue::Float(_) => TestValue::Float(results[1].unwrap_f32() as f64),
                    _ => todo!(),
                })))
            } else {
                bail!("non-binary result for null bit")
            }
        }
        TestValue::String(_) => {
            let mut results = [Val::I32(-1), Val::I32(-1)];
            func.call(&mut store, &[], &mut results)?;
            let len = results[0].unwrap_i32() as usize;
            let ptr = results[1].unwrap_i32() as usize;
            let slice = &memory.data(&store)[ptr..(ptr + len)];
            let string = std::str::from_utf8(slice)?;
            Ok(TestValue::String(string.to_string()))
        }
    }
}
