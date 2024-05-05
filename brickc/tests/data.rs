use anyhow::{bail, Context};
use brickc::compile;
use data_test_driver::TestValue;
use wasmtime::{Engine, Func, Instance, Module, Store, Val};

#[test]
fn basic() -> anyhow::Result<()> {
    let binary = compile("main", "main.brick", "1 + 1".to_string())?.finish();
    let engine = Engine::default();
    let module = Module::from_binary(&engine, binary.as_slice())?;
    let mut store = Store::new(&engine, ());
    let imports = [];
    let instance = Instance::new(&mut store, &module, &imports)?;
    let func = instance
        .get_func(&mut store, "main")
        .context("failed to find main")?;
    func.call(store, &[], &mut [])?;

    Ok(())
}

fn data() {
    let mut working_dir = std::env::current_dir().unwrap();
    working_dir.pop();
    working_dir.push("tests");
    data_test_driver::test_folder(
        working_dir,
        |contents| -> anyhow::Result<()> {
            compile("main", "main.brick", contents.to_string())?;
            Ok(())
        },
        |contents, expected| -> anyhow::Result<TestValue> {
            let binary = compile("main", "main.brick", contents.to_string())?.finish();
            let engine = Engine::default();
            let module = Module::from_binary(&engine, binary.as_slice())?;
            let mut store = Store::new(&engine, ());
            let imports = [];
            let instance = Instance::new(&mut store, &module, &imports)?;
            let func = instance
                .get_func(&mut store, "main")
                .context("failed to find main")?;

            look_for_value(&mut store, func, expected)
        },
    );
}

fn look_for_value(
    store: &mut Store<()>,
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
        TestValue::Int(int_val) => {
            let mut results = [Val::I32(-1)];
            func.call(store, &[], &mut results)?;

            if results[0].i32() == Some(*int_val as i32) {
                Ok(TestValue::Null)
            } else {
                bail!("wrong result returned: {:?}", results[0])
            }
        }
        TestValue::Float(float_val) => {
            let mut results = [Val::F32(f32::to_bits(-1.0))];
            func.call(store, &[], &mut results)?;

            if results[0].f32() == Some(*float_val as f32) {
                Ok(TestValue::Null)
            } else {
                bail!("wrong result returned: {:?}", results[0])
            }
        }
        _ => bail!("todo"),
        /*TestValue::Nullable(expected) => {
            let first = results.pop();
            if first != Some(Value::Byte(1)) {
                bail!("expected non-null marker, found {first:?}");
            }
            Ok(TestValue::Nullable(Box::new(look_for_value(
                results, memory, expected,
            )?)))
        }
        TestValue::String(_) => {
            if results.len() == 2 {
                let pointer = results.pop().unwrap();
                let Value::Size(pointer) = pointer else {
                    bail!("non-pointer type returned: {:?}", pointer);
                };
                let length = results.pop().unwrap();
                let Value::Size(length) = length else {
                    bail!("non-length type returned: {:?}", length);
                };
                let bytes = &memory[pointer..(pointer + length)];
                let string = std::str::from_utf8(bytes)?;
                Ok(TestValue::String(string.to_string()))
            } else {
                bail!("wrong number of results returned: {}", results.len());
            }
        }*/
    }
}
