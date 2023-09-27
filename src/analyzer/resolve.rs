use std::collections::HashMap;

use crate::{
    id::{IDMap, ID},
    parser::{
        AstNode, AstNodeValue, ExternFunctionBindingValue, FunctionDeclarationValue, NameAndType,
        ParsedSourceFile, StructDeclarationValue,
    },
};

pub struct ResolvedModule {
    pub nodes: IDMap<AstNode>,
    pub imports: IDMap<Import>,
    pub functions: IDMap<ResolvedCallable>,
    pub types: IDMap<ResolvedStruct>,
}

// TODO: resolve the IDMap based module
pub fn resolve_module(module: ParsedSourceFile) -> ResolvedModule {
    let names = name_to_id(&module);

    let ParsedSourceFile {
        mut nodes,
        imports,
        functions,
        types,
    } = module;

    let imports = imports
        .iter()
        .map(|id| {
            match nodes
                .remove(id)
                .expect("import should be found via ID")
                .value
            {
                AstNodeValue::Import(path) => (*id, Import { path }),
                node => panic!("ICE: unexpected node passed as an import: {:?}", node),
            }
        })
        .collect();
    let types = types
        .iter()
        .map(
            |id| match nodes.remove(id).expect("type should be found via ID").value {
                AstNodeValue::StructDeclaration(value) => (
                    *id,
                    ResolvedStruct {
                        name: value.name,
                        fields: resolve_names_and_types(&nodes, value.fields, &names),
                    },
                ),
                node => panic!("ICE: unexpected node type passed as a struct: {:?}", node),
            },
        )
        .collect();
    let functions = functions
        .iter()
        .map(
            |id| match nodes.remove(id).expect("func should be found via ID").value {
                AstNodeValue::FunctionDeclaration(value) => (
                    *id,
                    ResolvedCallable::Function(ResolvedFunction {
                        name: value.name,
                        params: resolve_names_and_types(&nodes, value.params, &names),
                        returns: value
                            .returns
                            .map(|type_| resolve_type(&nodes, type_, &names)),
                        body: value.body,
                        is_extern: value.is_extern,
                    }),
                ),
                AstNodeValue::ExternFunctionBinding(value) => (
                    *id,
                    ResolvedCallable::Extern(ResolvedExternFunction {
                        name: value.name,
                        params: resolve_names_and_types(&nodes, value.params, &names),
                        returns: value
                            .returns
                            .map(|type_| resolve_type(&nodes, type_, &names)),
                    }),
                ),
                node => panic!("ICE: unexpected node type passed as a struct: {:?}", node),
            },
        )
        .collect();

    let module = ResolvedModule {
        nodes,
        imports,
        functions,
        types,
    };

    module
}

fn name_to_id(module: &ParsedSourceFile) -> HashMap<String, ID> {
    let mut names = HashMap::new();

    for id in module.imports.iter() {
        let AstNodeValue::Import(path) = &module.nodes[id].value else {
        unreachable!();
    };
        names.insert(path.clone(), id.clone());
    }
    for id in module.functions.iter() {
        if let AstNodeValue::FunctionDeclaration(FunctionDeclarationValue { name, .. })
        | AstNodeValue::ExternFunctionBinding(ExternFunctionBindingValue { name, .. }) =
            &module.nodes[id].value
        {
            names.insert(name.clone(), id.clone());
        } else {
            unreachable!();
        };
    }
    for id in module.types.iter() {
        if let AstNodeValue::StructDeclaration(StructDeclarationValue { name, .. }) =
            &module.nodes[id].value
        {
            names.insert(name.clone(), id.clone());
        } else {
            unreachable!();
        };
    }

    names
}

fn resolve_names_and_types(
    ast_nodes: &IDMap<AstNode>,
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

fn resolve_type(
    ast_nodes: &IDMap<AstNode>,
    type_: ID,
    names: &HashMap<String, ID>,
) -> ResolvedType {
    let type_node = &ast_nodes[&type_];

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

pub enum ResolvedCallable {
    Function(ResolvedFunction),
    Extern(ResolvedExternFunction),
}

pub struct ResolvedFunction {
    pub name: String,
    pub params: Vec<ResolvedNameAndType>,
    pub returns: Option<ResolvedType>,
    pub body: ID,
    /**
     * Whether this function is available to extern. Distinct from declaring an extern function
     * is available in the environment
     */
    pub is_extern: bool,
}

pub struct ResolvedExternFunction {
    pub name: String,
    pub params: Vec<ResolvedNameAndType>,
    pub returns: Option<ResolvedType>,
}

pub struct ResolvedStruct {
    pub name: String,
    pub fields: Vec<ResolvedNameAndType>,
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
