use std::collections::HashMap;

use crate::{
    analyzer::{IRContext, IRType, NumericType, TypecheckError},
    parser::{AstStatementValue, AstType, AstTypeValue, ParseTree},
};

// TODO: scan for struct declarations

pub struct ScanResults {
    pub imports: Vec<String>,
    pub exports: HashMap<String, usize>,
}

pub fn scan_top_level(
    statements: impl Iterator<Item = usize>,
    parse_context: &ParseTree,
    ir_context: &mut IRContext,
) -> Result<ScanResults, TypecheckError> {
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
                    .map(|param| {
                        let kind = ast_type_to_ir(
                            parse_context.kind(param.kind),
                            parse_context,
                            ir_context,
                        )?;
                        Ok(ir_context.add_kind(kind))
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                let returns = returns
                    .as_ref()
                    .map(|kind| {
                        let kind =
                            ast_type_to_ir(parse_context.kind(*kind), parse_context, ir_context)?;
                        Ok(ir_context.add_kind(kind))
                    })
                    .unwrap_or(Ok(ir_context.add_kind(IRType::Void)))?;

                exports.insert(
                    name.clone(),
                    ir_context.add_kind(IRType::Function {
                        parameters,
                        returns,
                    }),
                );
            }
            _ => {}
        }
    }

    Ok(ScanResults { imports, exports })
}

pub fn ast_type_to_ir(
    ast_type: &AstType,
    parse_context: &ParseTree,
    ir_context: &mut IRContext,
) -> Result<IRType, TypecheckError> {
    use IRType::*;
    use NumericType::*;

    Ok(match &ast_type.value {
        AstTypeValue::Name(string) => match string.as_str() {
            "void" => Void,
            "bool" => Bool,
            "i64" => Number(Int64),
            "f64" => Number(Float64),
            _ => todo!(),
        },
        AstTypeValue::Unique(inner) => {
            let inner = parse_context.kind(*inner);
            let inner = ast_type_to_ir(inner, parse_context, ir_context)?;
            let inner = ir_context.add_kind(inner);

            IRType::Unique(inner)
        }
        AstTypeValue::Shared(inner) => {
            let inner = parse_context.kind(*inner);
            let inner = ast_type_to_ir(inner, parse_context, ir_context)?;
            let inner = ir_context.add_kind(inner);

            IRType::Shared(inner)
        }
    })
}
