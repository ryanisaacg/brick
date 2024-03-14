use crate::{
    id::AnyID,
    typecheck::{ExpressionType, PointerKind},
};

use super::{HirModule, HirNode, HirNodeValue};

// TODO: remove 'CallGenerator' as a concept? rework it?
pub fn rewrite_generator_calls(module: &mut HirModule) {
    module.visit_mut(|node| {
        let HirNodeValue::Call(func, args) = &mut node.value else {
            return;
        };
        let ExpressionType::Generator { yield_ty, .. } = &func.ty else {
            return;
        };
        let yield_ty = yield_ty.clone();

        let mut generator = HirNode::dummy();
        std::mem::swap(func.as_mut(), &mut generator);

        let gen_ty = generator.ty.clone();

        let function_id = HirNode::autogenerated(
            HirNodeValue::Access(Box::new(generator.clone()), "function".to_string()),
            // TODO: wrong expression type
            ExpressionType::FunctionReference {
                parameters: vec![ExpressionType::Pointer(
                    PointerKind::Unique,
                    Box::new(gen_ty.clone()),
                )], // todo: parameters
                returns: yield_ty,
            },
        );

        args.insert(
            0,
            HirNode::autogenerated(
                HirNodeValue::TakeUnique(Box::new(generator)),
                ExpressionType::Pointer(PointerKind::Unique, Box::new(gen_ty)),
            ),
        );

        **func = function_id;
    });
}

pub fn rewrite_yields(module: &mut HirModule) {
    let mut jump_index = 1;

    for func in module.functions.iter_mut() {
        let Some((generator_var_id, gen_ty)) = &func.generator else {
            continue;
        };
        let generator_var_id: AnyID = (*generator_var_id).into();
        func.body.visit_mut(|node| {
            let HirNodeValue::Yield(yielded) = &mut node.value else {
                return;
            };
            let returned_value = std::mem::take(yielded);
            *node = HirNode::autogenerated(
                HirNodeValue::Sequence(vec![
                    HirNode::autogenerated(
                        HirNodeValue::GeneratorSuspend(
                            Box::new(HirNode::autogenerated(
                                HirNodeValue::VariableReference(generator_var_id),
                                gen_ty.clone(),
                            )),
                            jump_index,
                        ),
                        ExpressionType::Void,
                    ),
                    HirNode::autogenerated(
                        HirNodeValue::Return(returned_value),
                        ExpressionType::Void,
                    ),
                    HirNode::autogenerated(
                        HirNodeValue::GotoLabel(jump_index),
                        ExpressionType::Void,
                    ),
                ]),
                ExpressionType::Void,
            );
            jump_index += 1;
        });
    }
}
