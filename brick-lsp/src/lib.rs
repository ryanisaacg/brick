use std::{collections::HashMap, sync::OnceLock};

use brick::{
    diagnostic::{DiagnosticContents, DiagnosticMarker},
    id::AnyID,
    parse_file,
    parser::{AstArena, AstNode, AstNodeValue, ParsedFile},
    typecheck_node, CompileError, DeclarationContext, SourceFile, TypecheckContext,
};
use lsp_server::{Message, Notification};
use lsp_types::{
    notification::{Notification as _, PublishDiagnostics},
    Diagnostic, DiagnosticSeverity, PublishDiagnosticsParams,
};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
    HoverProviderCapability, Location, MarkedString, OneOf, Position, Range, ServerCapabilities,
    TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextDocumentSyncOptions, Url,
};
use serde::Serialize;

#[derive(Default)]
pub struct ServerState {
    open_documents: HashMap<Url, DocumentEntry>,
    server_driven_messages: Vec<Message>,
}

struct DocumentEntry {
    source: SourceFile,
    parsed: ParsedFile,
}

impl ServerState {
    pub fn new() -> ServerState {
        ServerState {
            open_documents: HashMap::new(),
            server_driven_messages: Vec::new(),
        }
    }

    pub fn capabilities() -> ServerCapabilities {
        ServerCapabilities {
            definition_provider: Some(OneOf::Left(true)),
            hover_provider: Some(HoverProviderCapability::Simple(true)),
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
    ) -> anyhow::Result<()> {
        let TextDocumentItem {
            uri,
            language_id,
            version: _,
            text,
        } = text_document;
        eprintln!("{language_id}");
        let filename = uri.path().to_string().leak() as &'static str;
        let source_file = SourceFile::from_filename_and_contents(filename, text);

        match parse_file(source_file.filename, source_file.contents.as_ref()) {
            Ok(parsed) => {
                // TODO: run full type checker async rather than sync
                if let Err(err) = brick::typecheck_module(&[(source_file.module_name, &parsed)]) {
                    self.generate_diagnostics(&uri, Some(err))?;
                } else {
                    self.generate_diagnostics(&uri, None)?;
                }
                self.open_documents.insert(
                    uri,
                    DocumentEntry {
                        source: source_file,
                        parsed,
                    },
                );
            }
            Err(error) => {
                self.generate_diagnostics(&uri, Some(error))?;
            }
        }

        Ok(())
    }

    pub fn did_change_text_document(
        &mut self,
        DidChangeTextDocumentParams {
            text_document,
            content_changes,
        }: DidChangeTextDocumentParams,
    ) -> anyhow::Result<()> {
        let Some(entry) = self.open_documents.get_mut(&text_document.uri) else {
            // Should this report an error?
            return Ok(());
        };
        let document = &mut entry.source;
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

        match parse_file(entry.source.filename, entry.source.contents.as_ref()) {
            Ok(contents) => {
                entry.parsed = contents;
                // TODO: run full type checker async rather than sync
                if let Err(err) =
                    brick::typecheck_module(&[(entry.source.module_name, &entry.parsed)])
                {
                    self.generate_diagnostics(&text_document.uri, Some(err))?;
                } else {
                    self.generate_diagnostics(&text_document.uri, None)?;
                }
            }
            Err(error) => {
                self.generate_diagnostics(&text_document.uri, Some(error))?;
            }
        }

        Ok(())
    }

    pub fn did_close_text_document(
        &mut self,
        DidCloseTextDocumentParams { text_document }: DidCloseTextDocumentParams,
    ) {
        self.open_documents.remove(&text_document.uri);
    }

    pub fn goto_definition(
        &mut self,
        GotoDefinitionParams {
            text_document_position_params,
            ..
        }: GotoDefinitionParams,
    ) -> anyhow::Result<Option<GotoDefinitionResponse>> {
        // TODO: be able to read sources from disk / package

        self.reset_types(&text_document_position_params.text_document.uri);
        let file = self
            .open_documents
            .get(&text_document_position_params.text_document.uri)
            .unwrap();
        let files: Vec<_> = self
            .open_documents
            .values()
            .map(|entry| (entry.source.module_name, &entry.parsed))
            .collect();
        let declarations = DeclarationContext::new(&files[..])?;

        let ctx = TypecheckContext::new(&file.parsed, file.source.module_name, &declarations)?;

        // Find the name currently under the cursor
        let position = text_document_position_params.position;
        let found = file
            .parsed
            .top_level
            .iter()
            .find(|node| {
                node.provenance
                    .contains(position.line + 1, position.character + 1)
            })
            .map(|node| -> anyhow::Result<Option<AnyID>> {
                eprintln!("{node:?}");
                let mut found = None;
                if node.ty.get().is_none() {
                    typecheck_node(
                        node,
                        &ctx,
                        &mut Default::default(),
                        &mut Default::default(),
                        &mut Default::default(),
                    )?;
                }
                find_id_at_position(&file.parsed.arena, &mut found, node, position);
                Ok(found)
            })
            .transpose()?
            .flatten();

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
                uri: text_document_position_params.text_document.uri.clone(),
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

    pub fn hover(
        &mut self,
        HoverParams {
            text_document_position_params:
                TextDocumentPositionParams {
                    text_document: TextDocumentIdentifier { uri },
                    position,
                },
            work_done_progress_params: _,
        }: HoverParams,
    ) -> anyhow::Result<Hover> {
        // TODO: be able to read sources from disk / package

        self.reset_types(&uri);
        let file = self.open_documents.get(&uri).unwrap();
        let files: Vec<_> = self
            .open_documents
            .values()
            .map(|entry| (entry.source.module_name, &entry.parsed))
            .collect();
        let declarations = DeclarationContext::new(&files[..])?;

        let ctx = TypecheckContext::new(&file.parsed, file.source.module_name, &declarations)?;

        // Find the name currently under the cursor
        let found = file
            .parsed
            .top_level
            .iter()
            .find(|node| {
                node.provenance
                    .contains(position.line + 1, position.character + 1)
            })
            .map(|node| -> anyhow::Result<Option<&AstNode>> {
                if node.ty.get().is_none() {
                    typecheck_node(
                        node,
                        &ctx,
                        &mut Default::default(),
                        &mut Default::default(),
                        &mut Default::default(),
                    )?;
                }
                let token = find_token_under_cursor(&file.parsed.arena, node, position);
                Ok(token)
            })
            .transpose()?
            .flatten();
        // TODO: are we hovering on a meaningless token?
        // find character under cursor
        // all characters in literals are meaningful

        let hover = format!("{:?}", found);

        Ok(Hover {
            contents: HoverContents::Scalar(MarkedString::String(hover)),
            range: None,
        })
    }

    /**
     * Reset all the OnceLock cells so we can run typechecking again
     */
    fn reset_types(&mut self, file: &Url) {
        let file = self.open_documents.get_mut(file).unwrap();
        for node in file.parsed.iter_mut() {
            node.ty = OnceLock::new();
            if let AstNodeValue::Name {
                value: _,
                referenced_id,
            } = &mut node.value
            {
                *referenced_id = OnceLock::new();
            }
        }
    }

    fn generate_diagnostics(
        &mut self,
        uri: &Url,
        error: Option<CompileError>,
    ) -> anyhow::Result<()> {
        let mut diagnostics = Vec::new();
        if let Some(error) = error {
            push_diagnostics(&mut diagnostics, error);
        }
        self.push_notification(
            PublishDiagnostics::METHOD,
            PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics,
                version: None,
            },
        )
    }

    pub fn server_messages(&mut self) -> impl Iterator<Item = Message> + '_ {
        self.server_driven_messages.drain(..)
    }

    fn push_notification(&mut self, method: &str, params: impl Serialize) -> anyhow::Result<()> {
        self.server_driven_messages
            .push(Message::Notification(Notification {
                method: method.to_string(),
                params: serde_json::to_value(params)?,
            }));

        Ok(())
    }
}

fn push_diagnostics(diagnostics: &mut Vec<Diagnostic>, error: CompileError) {
    match error {
        CompileError::ParseError(err) => push_diagnostic_contents(diagnostics, &err),
        CompileError::TypeValidationError(err) => push_diagnostic_contents(diagnostics, &err),
        CompileError::TypecheckError(err) => push_diagnostic_contents(diagnostics, &err),
        CompileError::LifetimeError(err) => push_diagnostic_contents(diagnostics, &err),
    }
}

fn push_diagnostic_contents(
    diagnostics: &mut Vec<Diagnostic>,
    diagnostic: &dyn brick::diagnostic::Diagnostic,
) {
    let contents = diagnostic.contents();
    match contents {
        DiagnosticContents::Scalar(marker) => diagnostics.push(error_diagnostic(marker)),
        DiagnosticContents::Vector(markers) => {
            for marker in markers {
                diagnostics.push(error_diagnostic(marker));
            }
        }
    }
}

fn error_diagnostic(
    DiagnosticMarker {
        range,
        message,
        severity,
    }: DiagnosticMarker,
) -> Diagnostic {
    Diagnostic {
        range: Range::new(
            Position::new(range.start_line - 1, range.start_offset - 1),
            Position::new(range.end_line - 1, range.end_offset - 1),
        ),
        severity: Some(match severity {
            brick::diagnostic::Severity::Error => DiagnosticSeverity::ERROR,
            brick::diagnostic::Severity::Info => DiagnosticSeverity::INFORMATION,
        }),
        code: None,
        code_description: None,
        source: Some("brick".to_string()),
        message: message.to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

fn find_id_at_position(
    ast: &AstArena,
    found: &mut Option<AnyID>,
    node: &AstNode,
    position: Position,
) {
    if let AstNodeValue::Name { referenced_id, .. } = &node.value {
        if node
            .provenance
            .contains(position.line + 1, position.character + 1)
        {
            *found = Some(*referenced_id.get().unwrap());
        }
    }

    node.children(ast, |node| {
        if found.is_none() {
            find_id_at_position(ast, found, node, position);
        }
    });
}

fn find_token_under_cursor<'a>(
    ast: &'a AstArena,
    node: &'a AstNode,
    position: Position,
) -> Option<&'a AstNode> {
    if !node
        .provenance
        .contains(position.line + 1, position.character + 1)
    {
        None
    } else {
        let mut more_specific = None;
        node.children(ast, |child| {
            if let Some(child_node) = find_token_under_cursor(ast, child, position) {
                more_specific = Some(child_node);
            }
        });
        Some(more_specific.unwrap_or(node))
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
