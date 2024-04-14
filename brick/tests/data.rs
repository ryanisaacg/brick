use brick::{check_types, eval, Value};
use data_test_driver::TestValue;

#[test]
fn data() {
    data_test_driver::test_folder(
        // TODO
        "/Users/ryanisaacg/git/ryanisaacg/ludus/tests",
        |contents| -> anyhow::Result<()> {
            check_types(contents)?;
            Ok(())
        },
        |contents| -> anyhow::Result<TestValue> {
            let mut results = eval(contents)?;
            if results.len() == 0 {
                Ok(TestValue::Void)
            } else if results.len() == 1 {
                let result = results.remove(0);
                Ok(value_to_test_value(result))
            } else if results.len() == 2 {
                // Maybe a nullable value
                if results[1] == Value::Byte(1) {
                    Ok(value_to_test_value(results.remove(0)))
                } else {
                    todo!()
                }
            } else {
                todo!()
            }
        },
    );
}

fn value_to_test_value(val: Value) -> TestValue {
    match val {
        Value::FunctionID(_) => todo!(),
        Value::Size(_) => todo!(),
        Value::Byte(byte) => TestValue::Int(byte as i64),
        Value::Int32(val) => TestValue::Int(val as i64),
        Value::Int64(val) => TestValue::Int(val),
        Value::Float32(val) => TestValue::Float(val as f64),
        Value::Float64(val) => TestValue::Float(val),
    }
}
