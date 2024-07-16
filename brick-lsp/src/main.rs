// Adapating from the example from the lsp-server repo

use lsp_types::notification::{
    DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
};
use lsp_types::request::{HoverRequest, Request as _, ShowMessageRequest};
use lsp_types::{request::GotoDefinition, InitializeParams};
use lsp_types::{MessageType, ShowMessageParams};

use brick_lsp::ServerState;
use lsp_server::{Connection, ExtractError, Message, Notification, Request, Response};

fn main() -> anyhow::Result<()> {
    eprintln!("brick-lsp booting up");

    let (connection, io_threads) = Connection::stdio();
    let server_capabilities = serde_json::to_value(ServerState::capabilities())?;
    let initialization_params = match connection.initialize(server_capabilities) {
        Ok(it) => it,
        Err(e) => {
            if e.channel_is_disconnected() {
                io_threads.join()?;
            }
            return Err(e.into());
        }
    };
    main_loop(connection, initialization_params)?;
    io_threads.join()?;

    eprintln!("shutting down server");
    Ok(())
}

fn main_loop(connection: Connection, params: serde_json::Value) -> anyhow::Result<()> {
    let mut server = ServerState::new();
    let _params: InitializeParams = serde_json::from_value(params)?;
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                match handle_request(&mut server, req) {
                    Ok(Some(resp)) => {
                        connection.sender.send(Message::Response(resp))?;
                    }
                    Ok(None) => {}
                    Err(err) => {
                        let message = ShowMessageParams {
                            typ: MessageType::ERROR,
                            message: err.to_string(),
                        };
                        let params = serde_json::to_value(&message)?;
                        let response = Request {
                            // TODO: super illegal
                            id: 0.into(),
                            method: ShowMessageRequest::METHOD.to_string(),
                            params,
                        };
                        connection.sender.send(Message::Request(response))?;
                    }
                }
            }
            Message::Response(_resp) => {}
            Message::Notification(not) => {
                handle_notification(&mut server, not)?;
            }
        }
    }
    Ok(())
}

fn handle_request(server: &mut ServerState, req: Request) -> anyhow::Result<Option<Response>> {
    Ok(match req.method.as_str() {
        GotoDefinition::METHOD => match req.extract(GotoDefinition::METHOD) {
            Ok((id, params)) => {
                eprintln!("got gotoDefinition request #{id}: {params:?}\n");
                let result = server.goto_definition(params)?;
                let result = serde_json::to_value(result)?;
                Some(Response {
                    id,
                    result: Some(result),
                    error: None,
                })
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(_)) => unreachable!(),
        },
        HoverRequest::METHOD => match req.extract(HoverRequest::METHOD) {
            Ok((id, params)) => {
                eprintln!("got gotoDefinition request #{id}: {params:?}\n");
                let result = server.hover(params)?;
                let result = serde_json::to_value(result)?;
                Some(Response {
                    id,
                    result: Some(result),
                    error: None,
                })
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(_)) => unreachable!(),
        },
        _ => None,
    })
}

fn handle_notification(server: &mut ServerState, notif: Notification) -> anyhow::Result<()> {
    match notif.method.as_str() {
        DidOpenTextDocument::METHOD => match notif.extract(DidOpenTextDocument::METHOD) {
            Ok(params) => {
                server.did_open_text_document(params)?;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(_)) => unreachable!(),
        },
        DidChangeTextDocument::METHOD => match notif.extract(DidChangeTextDocument::METHOD) {
            Ok(params) => {
                server.did_change_text_document(params)?;
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(_)) => unreachable!(),
        },
        DidCloseTextDocument::METHOD => match notif.extract(DidCloseTextDocument::METHOD) {
            Ok(params) => {
                server.did_close_text_document(params);
            }
            Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
            Err(ExtractError::MethodMismatch(_)) => unreachable!(),
        },
        _ => {}
    }
    Ok(())
}
