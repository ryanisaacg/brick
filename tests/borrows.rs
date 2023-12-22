use brick::{eval, CompileError};

#[tokio::test]
async fn basic_moves() {
    let result = eval(
        r#"
struct Data {}

fn test(): Data {
    let x = Data{};
    let y = x;
    x
}
    "#,
    )
    .await;
    if let Err(CompileError::BorrowcheckError(error)) = result {
        assert_eq!(error.len(), 1);
    } else {
        panic!("{:?}", result);
    }
}
