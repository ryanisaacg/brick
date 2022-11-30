use std::collections::HashMap;

use crate::{
    parser::{AstStatementValue, ParseTree},
    typecheck::{type_name_to_type, Type},
};

pub struct ScanResults {
    pub imports: Vec<String>,
    pub exports: HashMap<String, Type>,
}

pub fn scan_top_level(
    statements: impl Iterator<Item = usize>,
    parse_context: &ParseTree,
) -> ScanResults {
    let mut imports = Vec::new();
    let mut exports = HashMap::new();

    for statement in statements {
        let statement = parse_context.statement(statement);
        match &statement.value {
            AstStatementValue::Import(name) => imports.push(name.to_string()),
            AstStatementValue::FunctionDeclaration {
                name,
                params,
                returns,
                body: _,
            } => {
                let parameters = params
                    .iter()
                    .map(|param| type_name_to_type(param.kind.as_ref()))
                    .collect::<Vec<_>>();

                let returns = returns
                    .as_ref()
                    .map(|type_name| type_name_to_type(type_name.as_ref()))
                    .unwrap_or(Type::Void);

                exports.insert(
                    name.clone(),
                    Type::Function {
                        parameters,
                        returns: Box::new(returns.clone()),
                    },
                );
            }
            _ => {}
        }
    }

    ScanResults { imports, exports }
}
