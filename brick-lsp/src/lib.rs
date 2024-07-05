use std::collections::HashMap;

use brick::{id::AnyID, CompilationResults, HirNodeValue, SourceFile};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, Location, OneOf, Position, Range,
    ServerCapabilities, TextDocumentItem, TextDocumentSyncCapability, TextDocumentSyncKind,
    TextDocumentSyncOptions, Url,
};

#[derive(Default)]
pub struct ServerState {
    open_documents: HashMap<Url, SourceFile>,
}

impl ServerState {
    pub fn new() -> ServerState {
        ServerState {
            open_documents: HashMap::new(),
        }
    }

    pub fn capabilities() -> ServerCapabilities {
        ServerCapabilities {
            definition_provider: Some(OneOf::Left(true)),
            text_document_sync: Some(TextDocumentSyncCapability::Options(
                TextDocumentSyncOptions {
                    open_close: Some(true),
                    change: Some(TextDocumentSyncKind::FULL),
                    will_save: None,
                    will_save_wait_until: None,
                    save: None,
                },
            )),
            ..Default::default()
        }
    }

    pub fn did_open_text_document(
        &mut self,
        DidOpenTextDocumentParams { text_document }: DidOpenTextDocumentParams,
    ) {
        let TextDocumentItem {
            uri,
            language_id,
            version: _,
            text,
        } = text_document;
        eprintln!("{language_id}");
        let filename = uri.path().to_string().leak() as &'static str;
        let source_file = SourceFile::from_filename_and_contents(filename, text);
        self.open_documents.insert(uri, source_file);
    }

    pub fn did_change_text_document(
        &mut self,
        DidChangeTextDocumentParams {
            text_document,
            content_changes,
        }: DidChangeTextDocumentParams,
    ) {
        let Some(document) = self.open_documents.get_mut(&text_document.uri) else {
            // Should this report an error?
            return;
        };
        for change in content_changes {
            if let Some(range) = change.range {
                let start = position_to_idx(document.contents.as_str(), range.start) as usize;
                document
                    .contents
                    .replace_range(start..(start + change.text.len()), change.text.as_str());
            } else {
                document.contents = change.text;
            }
        }
    }

    pub fn did_close_text_document(
        &mut self,
        DidCloseTextDocumentParams { text_document }: DidCloseTextDocumentParams,
    ) {
        self.open_documents.remove(&text_document.uri);
    }

    pub fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> anyhow::Result<Option<GotoDefinitionResponse>> {
        let position = params.text_document_position_params.position;
        // TODO: be able to read sources from disk / package
        let CompilationResults {
            modules,
            declarations,
        } = brick::check_types(self.open_documents.values().cloned().collect())?;

        let mut found = None;
        for module in modules.values() {
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
        }

        let provenance = match found {
            Some(id) => match id {
                AnyID::Function(fn_id) => declarations.id_to_func[&fn_id].provenance.clone(),
                AnyID::Type(_) => todo!(),
                AnyID::Variable(_) => todo!(),
                AnyID::Constant(_) => todo!(),
            },
            None => None,
        };

        Ok(provenance.map(|provenance| {
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
        }))
    }
}

fn position_to_idx(source: &str, position: Position) -> u32 {
    let mut lines = 0;
    let mut idx = 0;
    for char in source.chars() {
        idx += 1;
        if char == '\n' {
            lines += 1;
            if lines >= position.line {
                break;
            }
        }
    }

    // TODO: https://github.com/rust-lang/rust-analyzer/issues/202
    idx + position.character
}
