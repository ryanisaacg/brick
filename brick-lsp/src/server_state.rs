use std::path::Path;

use brick::{id::AnyID, CompilationResults, HirNodeValue, SourceFile};
use lsp_types::{GotoDefinitionParams, GotoDefinitionResponse, Location, Position, Range};

pub struct ServerState;

impl ServerState {
    pub fn new() -> ServerState {
        ServerState
    }

    pub fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> anyhow::Result<Option<GotoDefinitionResponse>> {
        let path = Path::new(
            params
                .text_document_position_params
                .text_document
                .uri
                .path(),
        );
        let position = params.text_document_position_params.position;

        let file =
            SourceFile::from_filename(path.to_str().unwrap().to_string().leak() as &'static str)?;

        let CompilationResults {
            modules,
            declarations,
        } = brick::check_types(vec![file])?;

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
