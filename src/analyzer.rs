use crate::{
    id::{IDMap, ID},
    parser::{self, AstNode, AstNodeValue},
    provenance::Provenance,
};

pub struct ParserModule {
    pub imports: IDMap<Import>,
    pub functions: IDMap<parser::FunctionDeclarationValue>,
    pub externs: IDMap<parser::ExternFunctionBindingValue>,
    pub structs: IDMap<parser::StructDeclarationValue>,
}

pub struct Import {
    pub path: String,
}

pub enum ResolvedType {
    Integer,
    Float,
    Reference(ID),
    BrokenReference, // TODO: include error info
}

#[derive(Debug, thiserror::Error)]
pub enum ResolutionErrors {
    // TODO: better formatting
    #[error("unexpected top-level node {0:?} at {1}")]
    IllegalTopLevelNode(AstNode, Provenance),
}

pub fn top_levels_to_module(
    top_level_nodes: Vec<usize>,
    nodes: Vec<AstNode>,
) -> (ParserModule, Vec<ResolutionErrors>) {
    let mut module = ParserModule {
        imports: IDMap::new(),
        functions: IDMap::new(),
        externs: IDMap::new(),
        structs: IDMap::new(),
    };
    let mut errors = Vec::new();

    for node_ref in top_level_nodes {
        let node = &nodes[node_ref];
        match &node.value {
            AstNodeValue::FunctionDeclaration(val) => {
                let id = ID::new();
                module.functions.insert(id, val.clone());
            }
            AstNodeValue::ExternFunctionBinding(val) => {
                let id = ID::new();
                module.externs.insert(id, val.clone());
            }
            AstNodeValue::StructDeclaration(val) => {
                let id = ID::new();
                module.structs.insert(id, val.clone());
            }
            AstNodeValue::Import(path) => {
                // TODO: deduplicate imports at this stage?
                let id = ID::new();
                module.imports.insert(id, Import { path: path.clone() });
            }
            _ => {
                errors.push(ResolutionErrors::IllegalTopLevelNode(
                    node.clone(),
                    node.start,
                ));
            }
        }
    }

    (module, errors)
}
