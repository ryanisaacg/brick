use std::collections::HashMap;

use typed_arena::Arena;

use crate::{
    id::{IDMap, ID},
    parser::{
        AstNode, AstNodeValue, ExternFunctionBindingValue, FunctionDeclarationValue, NameAndType,
        ParsedSourceFile, StructDeclarationValue,
    },
};

pub struct ResolvedModule<'a> {
    pub imports: Vec<&'a AstNode<'a>>,
    pub functions: HashMap<ID, ResolvedCallable<'a>>,
    pub types: HashMap<ID, TypeDeclaration<'a>>,
}

// TODO: resolve the IDMap based module
pub fn resolve_module(module: ParsedSourceFile) -> ResolvedModule {
    let names = name_to_node(&module);

    let ParsedSourceFile {
        imports,
        functions,
        types,
    } = module;

    let types = types
        .iter()
        .map(|node| match node.value {
            AstNodeValue::StructDeclaration(value) => (
                value.name.clone(),
                ResolvedStruct {
                    name: value.name,
                    fields: resolve_names_and_types(&nodes, value.fields, &names),
                },
            ),
            node => panic!("ICE: unexpected node type passed as a struct: {:?}", node),
        })
        .collect();
    let functions = functions
        .iter()
        .map(|node| match node.value {
            AstNodeValue::FunctionDeclaration(value) => (
                value.name.clone(),
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
                value.name.clone(),
                ResolvedCallable::Extern(ResolvedExternFunction {
                    name: value.name,
                    params: resolve_names_and_types(&nodes, value.params, &names),
                    returns: value
                        .returns
                        .map(|type_| resolve_type(&nodes, type_, &names)),
                }),
            ),
            node => panic!("ICE: unexpected node type passed as a struct: {:?}", node),
        })
        .collect();

    ResolvedModule {
        imports,
        functions,
        types,
    }
}

fn name_to_node<'a>(module: &'a ParsedSourceFile) -> HashMap<String, &'a AstNode<'a>> {
    let mut names = HashMap::new();

    for node in module.imports.iter() {
        let AstNodeValue::Import(path) = node.value else {
            unreachable!();
        };
        names.insert(path.clone(), *node);
    }
    for node in module.functions.iter() {
        if let AstNodeValue::FunctionDeclaration(FunctionDeclarationValue { name, .. })
        | AstNodeValue::ExternFunctionBinding(ExternFunctionBindingValue { name, .. }) =
            node.value
        {
            names.insert(name.clone(), *node);
        } else {
            unreachable!();
        };
    }
    for node in module.types.iter() {
        if let AstNodeValue::StructDeclaration(StructDeclarationValue { name, .. }) = node.value {
            names.insert(name.clone(), node);
        } else {
            unreachable!();
        };
    }

    names
}

fn resolve_names_and_types(
    names: &HashMap<String, &AstNode>,
    params: Vec<NameAndType>,
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

pub enum ResolvedCallable<'a> {
    Function(ResolvedFunction<'a>),
    Extern(ResolvedExternFunction<'a>),
}

pub struct ResolvedFunction<'a> {
    pub name: String,
    pub params: Vec<ResolvedNameAndType<'a>>,
    pub returns: Option<ResolvedType<'a>>,
    pub body: ID,
    /**
     * Whether this function is available to extern. Distinct from declaring an extern function
     * is available in the environment
     */
    pub is_extern: bool,
}

pub struct ResolvedExternFunction<'a> {
    pub name: String,
    pub params: Vec<ResolvedNameAndType<'a>>,
    pub returns: Option<ResolvedType<'a>>,
}

pub struct ResolvedStruct<'a> {
    pub name: String,
    pub fields: Vec<ResolvedNameAndType<'a>>,
}

pub struct ResolvedNameAndType<'a> {
    pub name: String,
    pub type_: ResolvedType<'a>,
}

pub struct Import {
    pub path: String,
}

pub enum TypeDeclaration<'a> {
    Struct(ResolvedStruct<'a>),
}

pub enum ResolvedType<'a> {
    Integer,
    Float,
    Import(&'a AstNode<'a>),
    Reference(&'a TypeDeclaration<'a>),
    BrokenReference(String),
}
