use brick_lsp::ServerState;
use lsp_types::{
    DidChangeTextDocumentParams, DidOpenTextDocumentParams, GotoDefinitionParams,
    GotoDefinitionResponse, Location, PartialResultParams, Position, Range,
    TextDocumentContentChangeEvent, TextDocumentIdentifier, TextDocumentItem,
    TextDocumentPositionParams, Url, VersionedTextDocumentIdentifier, WorkDoneProgressParams,
};

fn file_uri(filename: &str) -> anyhow::Result<Url> {
    Ok(Url::parse(format!("file://{filename}").as_str())?)
}

fn test_doc(uri: Url, contents: &str) -> TextDocumentItem {
    TextDocumentItem {
        uri,
        language_id: "brick".to_string(),
        version: 0,
        text: contents.to_string(),
    }
}

#[test]
pub fn goto_definition() -> anyhow::Result<()> {
    let uri = file_uri("test.brick")?;
    let file = test_doc(
        uri.clone(),
        r#"
fn func() { }
// other contents
func();"#,
    );

    let mut server = ServerState::new();
    server.did_open_text_document(DidOpenTextDocumentParams {
        text_document: file,
    });
    let resp = server.goto_definition(GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 3,
                character: 1,
            },
        },
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
    })?;
    assert_eq!(
        resp,
        Some(GotoDefinitionResponse::Scalar(Location {
            uri,
            range: Range {
                start: Position {
                    line: 1,
                    character: 1
                },
                end: Position {
                    line: 1,
                    character: 12
                },
            },
        }))
    );

    Ok(())
}

#[test]
pub fn goto_definition_file_change() -> anyhow::Result<()> {
    let uri = file_uri("test.brick")?;
    let file = test_doc(
        uri.clone(),
        r#"
fn func() { }
// other contents
func();"#,
    );

    let mut server = ServerState::new();
    server.did_open_text_document(DidOpenTextDocumentParams {
        text_document: file,
    });
    // Remove the reference
    server.did_change_text_document(DidChangeTextDocumentParams {
        text_document: VersionedTextDocumentIdentifier {
            uri: uri.clone(),
            version: 0,
        },
        content_changes: vec![TextDocumentContentChangeEvent {
            range: None,
            range_length: None,
            text: r#"
fn func() { }
// other contents
1 + 2"#
                .to_string(),
        }],
    });

    let resp = server.goto_definition(GotoDefinitionParams {
        text_document_position_params: TextDocumentPositionParams {
            text_document: TextDocumentIdentifier { uri: uri.clone() },
            position: Position {
                line: 3,
                character: 1,
            },
        },
        work_done_progress_params: WorkDoneProgressParams {
            work_done_token: None,
        },
        partial_result_params: PartialResultParams {
            partial_result_token: None,
        },
    })?;
    assert_eq!(resp, None,);

    Ok(())
}
