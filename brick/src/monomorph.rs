use std::collections::{HashMap, HashSet};

use crate::{
    declaration_context::FileDeclarations,
    hir::{HirFunction, HirNode},
    id::{AnyID, FunctionID},
    DeclarationContext, ExpressionType, HirNodeValue, TypeID,
};

pub fn monomorphize(
    decls: &mut DeclarationContext,
    top_level_statements: &mut Option<HirNode>,
    functions: &mut HashMap<FunctionID, HirFunction>,
) {
    let generic_functions: HashSet<FunctionID> = functions
        .keys()
        .copied()
        .filter(|id| !decls.id_to_func[id].type_parameters.is_empty())
        .collect();

    let mut controller = MonomorphizeController::new();
    let mut functions_to_examine: Vec<FunctionID> = functions.keys().copied().collect();

    if let Some(top_level_statements) = top_level_statements.as_mut() {
        request_monomorphs(&mut controller, &generic_functions, top_level_statements);
    }

    loop {
        // DONTMERGE: parallelize?
        for func_id in functions_to_examine.drain(..) {
            let function = functions.get_mut(&func_id).unwrap();
            request_monomorphs(&mut controller, &generic_functions, &mut function.body);
        }

        for (
            ParameterizedFunction {
                call,
                type_parameters: _,
            },
            id_to_assign,
        ) in controller.monomorph_backlog.drain(..)
        {
            let mut func_ty = decls.id_to_func[&call].clone();
            func_ty.id = id_to_assign;
            func_ty.type_parameters.clear();
            // TODO: fill in the types in the new parameters / return type
            decls.id_to_func.insert(id_to_assign, func_ty);
            let func = functions[&call].clone();
            // TODO: fill in the types with the new generic types
            functions.insert(id_to_assign, func);
            functions_to_examine.push(id_to_assign);
        }

        if functions_to_examine.is_empty() {
            break;
        }
    }
}

fn request_monomorphs(
    controller: &mut MonomorphizeController,
    generic_functions: &HashSet<FunctionID>,
    node: &mut HirNode,
) {
    node.visit_mut(|node| {
        let HirNodeValue::Call(func, _) = &mut node.value else {
            return;
        };
        // DONTMERGE: what other function call types are there?
        let HirNodeValue::VariableReference(AnyID::Function(func_id)) = &mut func.value else {
            return;
        };
        if generic_functions.contains(func_id) {
            // TODO: generate the type parameters
            let type_parameters = HashMap::new();
            let monomorph_id =
                controller.get_monomorph_id_for_parameters(*func_id, type_parameters);
            *func_id = monomorph_id;
        }
    });
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct ParameterizedFunction {
    call: FunctionID,
    type_parameters: Vec<(TypeID, ExpressionType)>,
}

// DONTMERGE: name
struct MonomorphizeController {
    // DONTMERGE: write these into the original modules
    file_decls: FileDeclarations,
    func_to_id: HashMap<ParameterizedFunction, FunctionID>,
    monomorph_backlog: Vec<(ParameterizedFunction, FunctionID)>,
}

impl MonomorphizeController {
    pub fn new() -> MonomorphizeController {
        MonomorphizeController {
            file_decls: FileDeclarations::new(),
            func_to_id: HashMap::new(),
            monomorph_backlog: Vec::new(),
        }
    }

    pub fn get_monomorph_id_for_parameters(
        &mut self,
        call: FunctionID,
        type_parameters: HashMap<TypeID, ExpressionType>,
    ) -> FunctionID {
        let key = ParameterizedFunction {
            call,
            type_parameters: type_parameters.into_iter().collect(),
        };
        let id = self.func_to_id.get(&key).cloned();
        match id {
            Some(id) => id,
            None => {
                let id = self.file_decls.new_func_id();
                self.monomorph_backlog.push((key.clone(), id));
                self.func_to_id.insert(key, id);
                id
            }
        }
    }
}
