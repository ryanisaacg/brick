// Adapating from the example from the lsp-server repo

use lsp_types::request::{Request as _, ShowMessageRequest};
use lsp_types::{request::GotoDefinition, InitializeParams, ServerCapabilities};
use lsp_types::{MessageType, OneOf, ShowMessageParams};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};
use server_state::ServerState;

mod server_state;

fn main() -> anyhow::Result<()> {
    eprintln!("brick-lsp booting up");

    let (connection, io_threads) = Connection::stdio();
    let server_capabilities = serde_json::to_value(ServerCapabilities {
        definition_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })?;
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
        eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {req:?}");
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
            Message::Response(resp) => {
                eprintln!("got response: {resp:?}");
            }
            Message::Notification(not) => {
                eprintln!("got notification: {not:?}");
            }
        }
    }
    Ok(())
}

fn handle_request(server: &mut ServerState, req: Request) -> anyhow::Result<Option<Response>> {
    Ok(match req.method.as_str() {
        GotoDefinition::METHOD => match cast::<GotoDefinition>(req) {
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
        _ => None,
    })
}

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}
