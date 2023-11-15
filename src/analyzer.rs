use crate::parser::{AstNodeValue, FunctionDeclarationValue, ParsedSourceFile};

use self::control_flow_graph::build_control_flow_graph;

mod control_flow_graph;
//mod resolve;

pub fn typecheck<'a>(file: ParsedSourceFile<'a>) {
    //let module = resolve_module(file);

    for function in file.functions.iter() {
        // TODO
        match &function.value {
            AstNodeValue::FunctionDeclaration(func) => typecheck_function(func),
            _ => unreachable!(),
        }
    }
}

fn typecheck_function<'a>(function: &'a FunctionDeclarationValue<'a>) {
    let cfg = build_control_flow_graph(&function.body);
}
