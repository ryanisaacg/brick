use crate::{
    multi_error::merge_results,
    parser::{AstNode, AstNodeValue},
    ExpressionType, TypecheckContext, TypecheckError,
};

use super::PointerKind;

// TODO: do we needd to recurse if unsafe is in fact permitted

pub fn check_safety(
    context: &TypecheckContext,
    node: &AstNode,
    is_unsafe_permitted: bool,
) -> Result<(), TypecheckError> {
    let mut result = Ok(());

    if !is_unsafe_permitted {
        match &node.value {
            AstNodeValue::Call(lhs, _) => {
                let lhs_ty = &context.ast.get(*lhs).ty.get().unwrap();
                match lhs_ty {
                    ExpressionType::ReferenceToFunction(fn_id) => {
                        let fn_ty = &context.declarations.id_to_func[fn_id];
                        if fn_ty.is_unsafe {
                            merge_results(
                                &mut result,
                                Err(TypecheckError::UnsafeFunctionCalledOutsideUnsafe(
                                    node.provenance.clone(),
                                )),
                            );
                        } else if fn_ty.is_extern {
                            merge_results(
                                &mut result,
                                Err(TypecheckError::ExternFunctionCalledOutsideUnsafe(
                                    node.provenance.clone(),
                                )),
                            );
                        }
                    }
                    // For now, generators and function references can't be unsafe
                    ExpressionType::Generator { .. } | ExpressionType::FunctionReference { .. } => {
                    }

                    _ => unreachable!("ICE: LHS of function call must be a function type"),
                }
            }
            AstNodeValue::Deref(lhs) => {
                let lhs_ty = &context.ast.get(*lhs).ty.get().unwrap();
                if let ExpressionType::Pointer(PointerKind::UniqueRaw | PointerKind::SharedRaw, _) =
                    lhs_ty
                {
                    merge_results(
                        &mut result,
                        Err(TypecheckError::RawPointerDereferencedOutsideUnsafe(
                            node.provenance.clone(),
                        )),
                    );
                }
            }
            _ => (),
        }
    }

    let is_unsafe_permitted =
        is_unsafe_permitted || matches!(&node.value, AstNodeValue::UnsafeBlock(_));

    node.children(context.ast, |child| {
        merge_results(
            &mut result,
            check_safety(context, child, is_unsafe_permitted),
        );
    });

    result
}
