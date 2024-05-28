// Adapating from the example from the lsp-server repo

use std::path::Path;

use brick::id::AnyID;
use brick::HirNodeValue;
use brick::SourceRange;
use lsp_types::{
    request::GotoDefinition, GotoDefinitionResponse, InitializeParams, ServerCapabilities,
};
use lsp_types::{GotoDefinitionParams, Location, OneOf, Position, Range};

use lsp_server::{Connection, ExtractError, Message, Request, RequestId, Response};

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
    let _params: InitializeParams = serde_json::from_value(params)?;
    for msg in &connection.receiver {
        eprintln!("got msg: {msg:?}");
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req)? {
                    return Ok(());
                }
                eprintln!("got request: {req:?}");
                match cast::<GotoDefinition>(req) {
                    Ok((id, params)) => {
                        eprintln!("got gotoDefinition request #{id}: {params:?}\n");
                        let provenance = find_definition(&params)?;
                        let result = provenance.map(|provenance| {
                            GotoDefinitionResponse::Scalar(Location {
                                uri: params
                                    .text_document_position_params
                                    .text_document
                                    .uri
                                    .clone(),
                                range: Range {
                                    start: Position {
                                        line: provenance.start_line - 1,
                                        character: provenance.start_offset - 1,
                                    },
                                    end: Position {
                                        line: provenance.end_line - 1,
                                        character: provenance.end_offset - 1,
                                    },
                                },
                            })
                        });
                        let result = serde_json::to_value(&result)?;
                        let resp = Response {
                            id,
                            result: Some(result),
                            error: None,
                        };
                        connection.sender.send(Message::Response(resp))?;
                        continue;
                    }
                    Err(err @ ExtractError::JsonError { .. }) => panic!("{err:?}"),
                    Err(ExtractError::MethodMismatch(req)) => req,
                };
                // ...
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

fn cast<R>(req: Request) -> Result<(RequestId, R::Params), ExtractError<Request>>
where
    R: lsp_types::request::Request,
    R::Params: serde::de::DeserializeOwned,
{
    req.extract(R::METHOD)
}

fn find_definition(params: &GotoDefinitionParams) -> anyhow::Result<Option<SourceRange>> {
    let path = Path::new(
        params
            .text_document_position_params
            .text_document
            .uri
            .path(),
    );
    let contents = std::fs::read_to_string(path)?;
    let position = params.text_document_position_params.position;

    let module_name = path.file_stem().unwrap().to_str().unwrap();
    let filename = path
        .file_name()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string()
        .leak();
    let (module, decls) = brick::typecheck_file(module_name, filename, contents)?;
    let mut found = None;
    module.visit(|_, node| {
        if found.is_some() {
            return;
        }
        let HirNodeValue::VariableReference(id) = &node.value else {
            return;
        };
        let Some(provenance) = &node.provenance else {
            return;
        };
        if provenance.contains(position.line + 1, position.character + 1) {
            found = Some(*id);
        }
    });

    Ok(match found {
        Some(id) => match id {
            AnyID::Function(fn_id) => decls.id_to_func[&fn_id].provenance.clone(),
            AnyID::Type(_) => todo!(),
            AnyID::Variable(_) => todo!(),
        },
        None => None,
    })
}
