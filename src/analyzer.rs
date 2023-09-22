use crate::{
    parser::{AstNode, AstNodeValue},
    provenance::Provenance,
};

pub struct Module {
    pub imports: Vec<Import>,
    pub functions: Vec<Function>,
    pub structs: Vec<Struct>,
}

// TODO: should this be an enum of Resolved vs Unresolved?
pub struct Import {
    pub path: String,
}

pub struct Function {
    pub name: String,
    pub params: Vec<NameAndType>,
    pub returns: MaybeResolvedType,
    pub body_node: usize,
    pub is_extern: bool,
}

pub struct ExternFunction {
    pub name: String,
    pub params: Vec<NameAndType>,
    pub returns: MaybeResolvedType,
}

pub struct Struct {
    pub name: String,
    pub fields: Vec<NameAndType>,
}

pub struct NameAndType {
    pub name: String,
    pub type_: MaybeResolvedType,
}

pub enum MaybeResolvedType {
    Unresolved { type_expr_node: usize },
}

#[derive(Debug, thiserror::Error)]
pub enum ResolutionErrors {
    // TODO: better formatting
    #[error("unexpected top-level node {0:?} at {1}")]
    IllegalTopLevelNode(AstNode, Provenance),
}

pub fn resolve_module(
    top_level_nodes: Vec<usize>,
    nodes: Vec<AstNode>,
) -> Result<Module, ResolutionErrors> {
    let module = Module {
        imports: Vec::new(),
        functions: Vec::new(),
        structs: Vec::new(),
    };

    for node_ref in top_level_nodes {
        let node = &nodes[node_ref];
        match &node.value {
            AstNodeValue::FunctionDeclaration {
                name,
                params,
                returns,
                body,
                is_extern,
            } => {}
            AstNodeValue::ExternFunctionBinding {
                name,
                params,
                returns,
            } => {}
            AstNodeValue::StructDeclaration { name, fields } => {}
            AstNodeValue::Import(path) => {}
            _ => {
                return Err(ResolutionErrors::IllegalTopLevelNode(
                    node.clone(),
                    node.start,
                ));
            }
        }
    }

    Ok(module)
}
