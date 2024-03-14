use crate::typecheck::{ExpressionType, PointerKind};

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
