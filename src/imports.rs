use crate::parser::{AstNode, AstNodeValue};

pub struct Import<'a> {
    pub name: &'a str,
    pub source_path: String,
}

pub fn find_imports<'a>(
    nodes: impl Iterator<Item = &'a AstNode<'a>>,
) -> impl Iterator<Item = Import<'a>> {
    nodes.filter_map(|node| match &node.value {
        AstNodeValue::Import(name) => Some(Import {
            name: name.as_str(),
            source_path: resolve_file_path(name.as_str()),
        }),
        _ => None,
    })
}

fn resolve_file_path(module_name: &str) -> String {
    format!("{}.brick", module_name)
}
