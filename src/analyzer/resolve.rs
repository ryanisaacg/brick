use std::collections::HashMap;

use crate::{
    id::{IDMap, ID},
    parser::{self, AstNode, AstNodeValue, NameAndType},
    provenance::{SourceMarker, SourceRange},
};

pub fn resolve_module(
    top_level_nodes: Vec<usize>,
    nodes: Vec<AstNode>,
) -> (ResolvedModule, Vec<ResolutionErrors>) {
    let (module, errors) = ParserModule::new(top_level_nodes, &nodes[..]);
    let ParserModule {
        imports,
        functions,
        externs,
        structs,
        names,
    } = module;

    let nodes = &nodes[..];
    let structs = structs
        .into_iter()
        .map(|(id, value)| {
            (
                id,
                ResolvedStruct {
                    name: value.name,
                    fields: resolve_names_and_types(nodes, value.fields, &names),
                },
            )
        })
        .collect();
    let functions = functions
        .into_iter()
        .map(|(id, value)| {
            (
                id,
                ResolvedFunction {
                    name: value.name,
                    params: resolve_names_and_types(nodes, value.params, &names),
                    returns: value
                        .returns
                        .map(|type_| resolve_type(nodes, type_, &names)),
                    body: value.body,
                    is_extern: value.is_extern,
                },
            )
        })
        .collect();
    let externs = externs
        .into_iter()
        .map(|(id, value)| {
            (
                id,
                ResolvedExternFunction {
                    name: value.name,
                    params: resolve_names_and_types(nodes, value.params, &names),
                    returns: value
                        .returns
                        .map(|type_| resolve_type(nodes, type_, &names)),
                },
            )
        })
        .collect();

    let module = ResolvedModule {
        imports,
        functions,
        externs,
        structs,
    };

    (module, errors)
}

fn resolve_names_and_types(
    ast_nodes: &[AstNode],
    params: Vec<NameAndType>,
    names: &HashMap<String, ID>,
) -> Vec<ResolvedNameAndType> {
    params
        .into_iter()
        .map(|param| ResolvedNameAndType {
            name: param.name,
            type_: resolve_type(ast_nodes, param.type_, names),
        })
        .collect()
}

fn resolve_type(ast_nodes: &[AstNode], type_: usize, names: &HashMap<String, ID>) -> ResolvedType {
    let type_node = &ast_nodes[type_];

    match &type_node.value {
        AstNodeValue::Name(name) if name == "int" => ResolvedType::Integer,
        AstNodeValue::Name(name) if name == "float" => ResolvedType::Float,
        AstNodeValue::Name(name) => match names.get(name) {
            Some(id) => ResolvedType::Reference(id.clone()),
            None => ResolvedType::BrokenReference(name.clone()),
        },
        _ => {
            unimplemented!("Type expressions not yet implemented");
        }
    }
}

pub struct ResolvedModule {
    pub imports: IDMap<Import>,
    pub functions: IDMap<ResolvedFunction>,
    pub externs: IDMap<ResolvedExternFunction>,
    pub structs: IDMap<ResolvedStruct>,
}

pub struct ResolvedFunction {
    pub name: String,
    pub params: Vec<ResolvedNameAndType>,
    pub returns: Option<ResolvedType>,
    pub body: usize,
    /**
     * Whether this function is available to extern. Distinct from declaring an extern function
     * is available in the environment
     */
    pub is_extern: bool,
}

pub struct ResolvedExternFunction {
    name: String,
    params: Vec<ResolvedNameAndType>,
    returns: Option<ResolvedType>,
}

pub struct ResolvedStruct {
    name: String,
    fields: Vec<ResolvedNameAndType>,
}

pub struct ResolvedNameAndType {
    pub name: String,
    pub type_: ResolvedType,
}

pub struct Import {
    pub path: String,
}

pub enum ResolvedType {
    Integer,
    Float,
    Reference(ID),
    BrokenReference(String),
}

#[derive(Debug, thiserror::Error)]
pub enum ResolutionErrors {
    // TODO: better formatting
    #[error("unexpected top-level node {0:?} at {1}")]
    IllegalTopLevelNode(AstNodeValue, SourceMarker),
    #[error("duplicate name {0} at {1}")] // TODO: figure out where the other name was
    DuplicateNameDefinition(String, SourceRange),
}

struct ParserModule {
    imports: IDMap<Import>,
    functions: IDMap<parser::FunctionDeclarationValue>,
    externs: IDMap<parser::ExternFunctionBindingValue>,
    structs: IDMap<parser::StructDeclarationValue>,
    names: HashMap<String, ID>,
}

impl ParserModule {
    fn new(
        top_level_nodes: Vec<usize>,
        nodes: &[AstNode],
    ) -> (ParserModule, Vec<ResolutionErrors>) {
        let mut module = ParserModule {
            imports: IDMap::new(),
            functions: IDMap::new(),
            externs: IDMap::new(),
            structs: IDMap::new(),
            names: HashMap::new(),
        };
        let mut errors = Vec::new();

        for node_ref in top_level_nodes {
            let node = &nodes[node_ref];
            match &node.value {
                AstNodeValue::FunctionDeclaration(val) => {
                    if module.names.contains_key(&val.name) {
                        errors.push(ResolutionErrors::DuplicateNameDefinition(
                            val.name.clone(),
                            node.provenance.clone(),
                        ));
                    } else {
                        let id = ID::new();
                        module.functions.insert(id.clone(), val.clone());
                        module.names.insert(val.name.clone(), id);
                    }
                }

                AstNodeValue::ExternFunctionBinding(val) => {
                    if module.names.contains_key(&val.name) {
                        errors.push(ResolutionErrors::DuplicateNameDefinition(
                            val.name.clone(),
                            node.provenance.clone(),
                        ));
                    } else {
                        let id = ID::new();
                        module.externs.insert(id.clone(), val.clone());
                        module.names.insert(val.name.clone(), id);
                    }
                }

                AstNodeValue::StructDeclaration(val) => {
                    if module.names.contains_key(&val.name) {
                        errors.push(ResolutionErrors::DuplicateNameDefinition(
                            val.name.clone(),
                            node.provenance.clone(),
                        ));
                    } else {
                        let id = ID::new();
                        module.structs.insert(id.clone(), val.clone());
                        module.names.insert(val.name.clone(), id);
                    }
                }

                AstNodeValue::Import(path) => {
                    // TODO: error at duplicate imports?
                    let id = ID::new();
                    module
                        .imports
                        .insert(id.clone(), Import { path: path.clone() });
                    module.names.insert(path.clone(), id);
                }

                _ => {
                    errors.push(ResolutionErrors::IllegalTopLevelNode(
                        node.value.clone(),
                        node.provenance.start(),
                    ));
                    continue;
                }
            }
        }

        (module, errors)
    }
}
