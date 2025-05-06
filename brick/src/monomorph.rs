use std::collections::{HashMap, HashSet};

use crate::{
    declaration_context::FileDeclarations,
    hir::{HirFunction, HirNode},
    id::{AnyID, FunctionID},
    typecheck::{CollectionType, InterfaceType, StructType, UnionType},
    DeclarationContext, ExpressionType, HirNodeValue, TypeDeclaration, TypeID,
};

pub fn monomorphize(
    decls: &mut DeclarationContext,
    top_level_statements: &mut Option<HirNode>,
    functions: &mut HashMap<FunctionID, HirFunction>,
) {
    use rayon::prelude::*;
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

    let mut newly_created_functions = Vec::new();

    loop {
        for func_id in functions_to_examine.drain(..) {
            let function = functions.get_mut(&func_id).unwrap();
            request_monomorphs(&mut controller, &generic_functions, &mut function.body);
        }

        // We can expect most monomorphs to get caught on the first pass, and generating the
        // monomoprhs is an embarassingly parallel task.
        controller
            .monomorph_backlog
            .par_drain(..)
            .map(
                |(
                    ParameterizedFunction {
                        call,
                        type_parameters,
                    },
                    id_to_assign,
                )| {
                    let type_parameters: HashMap<_, _> = type_parameters.into_iter().collect();

                    // Replace the type parameters in the function header
                    let mut func_ty = decls.id_to_func[&call].clone();
                    func_ty.id = id_to_assign;
                    func_ty.type_parameters.clear();
                    for param in func_ty.params.iter_mut() {
                        monomorphize_type(&type_parameters, param);
                    }
                    monomorphize_type(&type_parameters, &mut func_ty.returns);

                    // Replace the type parameters in the function body
                    let mut func = functions[&call].clone();
                    func.id = id_to_assign;
                    func.body
                        .visit_mut(|node| monomorphize_node(decls, &type_parameters, node));

                    (func, func_ty)
                },
            )
            .collect_into_vec(&mut newly_created_functions);

        for (func, func_ty) in newly_created_functions.drain(..) {
            let id_to_assign = func_ty.id;
            functions.insert(id_to_assign, func);
            functions_to_examine.push(id_to_assign);
            decls.id_to_func.insert(id_to_assign, func_ty);
        }

        if functions_to_examine.is_empty() {
            break;
        }
    }

    // Monomorphization destroys the information of the original parameterized function.
    // This prevents backends from trying to compile a non-concrete function, which they cannot
    // do.
    for func_id in generic_functions {
        functions.remove(&func_id);
        decls.id_to_func.remove(&func_id);
    }
}

fn request_monomorphs(
    controller: &mut MonomorphizeController,
    generic_functions: &HashSet<FunctionID>,
    node: &mut HirNode,
) {
    node.visit_mut(|node| {
        let HirNodeValue::Call {
            func,
            args: _,
            type_parameter_resolutions,
        } = &mut node.value
        else {
            return;
        };
        let HirNodeValue::VariableReference(AnyID::Function(func_id)) = &mut func.value else {
            return;
        };
        if generic_functions.contains(func_id) {
            let monomorph_id = controller.get_monomorph_id_for_parameters(
                *func_id,
                std::mem::take(type_parameter_resolutions),
            );
            *func_id = monomorph_id;
        }
    });
}

fn monomorphize_node(
    decls: &DeclarationContext,
    type_parameters: &HashMap<TypeID, ExpressionType>,
    node: &mut HirNode,
) {
    if let HirNodeValue::TypeParameterCall(type_paramter_id, func_name, lhs, args) = &mut node.value
    {
        let concrete_ty_id = type_parameters[type_paramter_id as &TypeID].type_id().expect("ICE: only defined types may satisfy interface requirements to call functions on type parameter instances");
        let concrete_ty = &decls.id_to_decl[concrete_ty_id];
        match concrete_ty {
            TypeDeclaration::Module(_) | TypeDeclaration::TypeParameter(_) => unreachable!(
                "ICE: illegal concrete parameter for type parameter: {:?}",
                concrete_ty
            ),
            TypeDeclaration::Interface(InterfaceType {
                associated_functions,
                ..
            }) => {
                let fn_id = associated_functions[func_name.as_str()];
                node.value =
                    HirNodeValue::VtableCall(std::mem::take(lhs), fn_id, std::mem::take(args));
            }
            TypeDeclaration::Struct(StructType {
                associated_functions,
                ..
            })
            | TypeDeclaration::Union(UnionType {
                associated_functions,
                ..
            }) => {
                let fn_id = associated_functions[func_name.as_str()];
                let fn_ref = HirNode::autogenerated(
                    HirNodeValue::VariableReference(fn_id.into()),
                    ExpressionType::ReferenceToFunction(fn_id),
                );
                let mut args = std::mem::take(args);
                args.insert(0, std::mem::take(lhs));
                node.value = HirNodeValue::Call {
                    func: Box::new(fn_ref),
                    args,
                    type_parameter_resolutions: HashMap::new(),
                };
            }
        }
    }
    monomorphize_type(type_parameters, &mut node.ty);
}

pub fn monomorphize_type(
    type_parameters: &HashMap<TypeID, ExpressionType>,
    expr: &mut ExpressionType,
) {
    match expr {
        ExpressionType::Void
        | ExpressionType::Null
        | ExpressionType::Unreachable
        | ExpressionType::Primitive(_)
        | ExpressionType::ReferenceToFunction(_) => {}
        ExpressionType::InstanceOf(type_id) | ExpressionType::ReferenceToType(type_id) => {
            if let Some(monomorph_expr) = type_parameters.get(type_id) {
                *expr = monomorph_expr.clone();
            }
        }
        ExpressionType::Pointer(_, expr) | ExpressionType::Nullable(expr) => {
            monomorphize_type(type_parameters, expr);
        }
        ExpressionType::Collection(collection_type) => match collection_type {
            CollectionType::Array(expr)
            | CollectionType::ReferenceCounter(expr)
            | CollectionType::Cell(expr) => monomorphize_type(type_parameters, expr),
            CollectionType::Dict(key, val) => {
                monomorphize_type(type_parameters, key);
                monomorphize_type(type_parameters, val);
            }
            CollectionType::String => {}
        },
        ExpressionType::Generator {
            yield_ty: _,
            param_ty: _,
        }
        | ExpressionType::FunctionReference {
            parameters: _,
            returns: _,
        } => todo!("generators and generics don't mix yet"),
    }
}

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
struct ParameterizedFunction {
    call: FunctionID,
    type_parameters: Vec<(TypeID, ExpressionType)>,
}

struct MonomorphizeController {
    // SOMEDAY: Should these declarations get pushed to the correct file? does it matter?
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
