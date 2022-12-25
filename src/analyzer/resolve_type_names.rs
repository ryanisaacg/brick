use std::collections::HashMap;

use super::{IRContext, IRType, TypecheckError};

pub fn resolve_type_names(
    ir_context: &mut IRContext,
    declarations: &HashMap<String, usize>,
) -> Result<(), TypecheckError> {
    use IRType::*;

    let mut resolutions = HashMap::new();

    for (ptr, kind) in ir_context.types.iter().enumerate() {
        let Unresolved(name, provenance) = kind else { continue };
        let resolved_declaration = declarations
            .get(name)
            .ok_or_else(|| TypecheckError::UnknownName(name.clone(), *provenance))?;
        resolutions.insert(ptr, *resolved_declaration);
    }

    for decl in declarations.values() {
        match &mut ir_context.types[*decl] {
            Function {
                parameters,
                returns,
            } => {
                if let Some(replacement) = resolutions.get(returns) {
                    *returns = *replacement;
                }
                for param in parameters.iter_mut() {
                    if let Some(replacement) = resolutions.get(param) {
                        *param = *replacement;
                    }
                }
            }
            Struct { fields } => {
                for field in fields.values_mut() {
                    if let Some(replacement) = resolutions.get(field) {
                        *field = *replacement;
                    }
                }
            }
            Unique(child) | Shared(child) | Array(child) => {
                if let Some(replacement) = resolutions.get(child) {
                    *child = *replacement;
                }
            }
            Unresolved(..) | Void | Bool | Number(_) => {}
        }
    }

    Ok(())
}
