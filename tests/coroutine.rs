use brick::eval_types;

#[test]
fn count_up() {
    eval_types(
        r#"
gen fn infinite_seq(): generator[i32, void] {
    let current = 0;
    while true {
        yield current;
        current += 1;
    }
}

let seq = infinite_seq();
let value = seq() + seq() + seq();
value
"#,
    )
    .unwrap();
}

#[test]
fn echo() {
    eval_types(
        r#"
gen fn echo(initial: i32): generator[i32, i32] {
    let current = initial;
    while true {
        let next = yield current;
        current = next;
    }
}

let seq = echo(1);
seq(2) + seq(3) + seq(4)
"#,
    )
    .unwrap();
}

#[test]
fn no_yield_value() {
    eval_types(
        r#"
gen fn consume(): generator[void, i32] {
    let acc = 0;
    while true {
        let next = yield void;
        acc += next;
    }
}

let seq = consume();
seq(1);
seq(2);
seq(3);
"#,
    )
    .unwrap();
}
