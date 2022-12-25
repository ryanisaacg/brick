use std::collections::HashMap;

use crate::{
    analyzer::{BinOpComparison, BinOpNumeric, IRExpression, IRExpressionValue, NumericType},
    parser::{AstNode, AstNodeValue, BinOp},
};

use super::{
    FunDecl, FunctionParameter, IRContext, IRStatement, IRStatementValue, IRType, Scope,
    TypecheckError, BOOL_KIND, F32_KIND, F64_KIND, I32_KIND, I64_KIND, VOID_KIND,
};
// TODO: unification of separate IRContexts?
// TODO: initial header-file like parse

pub fn typecheck(
    statements: impl Iterator<Item = usize>,
    ir_context: &mut IRContext,
    parse_context: &[AstNode],
    scopes: &[Scope],
) -> Result<Vec<IRStatement>, TypecheckError> {
    // TODO: cons cell instead?
    let mut local_scope = Vec::with_capacity(1 + scopes.len());
    local_scope.push(Scope {
        declarations: HashMap::new(),
    });
    local_scope.extend(scopes.iter().cloned());

    let statements = statements
        .map(|statement| {
            let AstNode { value, start, end } = &parse_context[statement];
            let start = *start;
            let end = *end;

            let value = match value {
                AstNodeValue::Import(_) => {
                    // TODO: disallow inside functions?
                    return Ok(None);
                }
                AstNodeValue::StructDeclaration { .. } => {
                    // TODO: disallow inside functions?
                    return Ok(None);
                }
                AstNodeValue::Expression(expr) => {
                    let expr = typecheck_expression(
                        &parse_context[*expr],
                        parse_context,
                        ir_context,
                        &local_scope[..],
                    )?;
                    IRStatementValue::Expression(ir_context.add_expression(expr))
                }
                AstNodeValue::Declaration(name, expr) => {
                    let expr = typecheck_expression(
                        &parse_context[*expr],
                        parse_context,
                        ir_context,
                        &local_scope[..],
                    )?;
                    local_scope[0]
                        .declarations
                        .insert(name.to_string(), expr.kind);
                    IRStatementValue::Declaration(name.clone(), ir_context.add_expression(expr))
                }
                AstNodeValue::FunctionDeclaration {
                    name,
                    params,
                    returns,
                    body,
                } => {
                    let params = params
                        .iter()
                        .map(|param| {
                            let kind = resolve_ast_type(
                                &parse_context[param.kind],
                                parse_context,
                                ir_context,
                                &local_scope[..],
                            )?;
                            Ok(FunctionParameter {
                                name: param.name.to_string(),
                                kind,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let returns = returns
                        .as_ref()
                        .map(|type_name| {
                            resolve_ast_type(
                                &parse_context[*type_name],
                                parse_context,
                                ir_context,
                                &local_scope[..],
                            )
                        })
                        .unwrap_or(Ok(VOID_KIND))?;
                    let function_type = ir_context.add_kind(IRType::Function {
                        parameters: params.iter().map(|param| param.kind).collect(),
                        returns,
                    });

                    local_scope[0]
                        .declarations
                        .insert(name.clone(), function_type);

                    let mut local_scope = local_scope.clone();
                    local_scope.insert(
                        0,
                        Scope {
                            declarations: params
                                .iter()
                                .map(|FunctionParameter { name, kind }| (name.to_string(), *kind))
                                .collect(),
                        },
                    );
                    let body = typecheck_expression(
                        &parse_context[*body],
                        parse_context,
                        ir_context,
                        &local_scope[..],
                    )?;

                    // TODO: verify body actually returns that type
                    IRStatementValue::FunctionDeclaration(FunDecl {
                        name: name.to_string(),
                        params,
                        returns,
                        body: ir_context.add_expression(body),
                    })
                }
                _ => todo!("handle loose statements"),
            };

            Ok(Some(IRStatement { value, start, end }))
        })
        .filter_map(|x| x.transpose())
        .collect::<Result<Vec<IRStatement>, TypecheckError>>()?;

    Ok(statements)
}

// TODO: allow accessing the local environment
fn typecheck_expression(
    expression: &AstNode,
    parse_context: &[AstNode],
    ir_context: &mut IRContext,
    local_scope: &[Scope],
) -> Result<IRExpression, TypecheckError> {
    use AstNodeValue::*;
    let AstNode { value, start, end } = expression;
    let start = *start;
    let end = *end;

    // TODO: analyze if, block
    Ok(match value {
        ArrayLiteralLength(child, length) => {
            let child = typecheck_expression(
                &parse_context[*child],
                parse_context,
                ir_context,
                local_scope,
            )?;
            let kind = IRType::Array(child.kind);
            let kind = ir_context.add_kind(kind);
            let child = ir_context.add_expression(child);

            IRExpression {
                value: IRExpressionValue::ArrayLiteralLength(child, *length),
                kind,
                start,
                end,
            }
        }
        ArrayLiteral(_) => {
            todo!();
        }
        BinExpr(BinOp::Index, left, right) => {
            // TODO: apply auto-dereferencing
            let left = typecheck_expression(
                &parse_context[*left],
                parse_context,
                ir_context,
                local_scope,
            )?;
            let right = typecheck_expression(
                &parse_context[*right],
                parse_context,
                ir_context,
                local_scope,
            )?;
            let left_kind = ir_context.kind(left.kind);
            let IRType::Array(inner) = left_kind else {
                return Err(TypecheckError::IllegalNonArrayIndex(left_kind.clone(), left.end));
            };
            let right_kind = ir_context.kind(right.kind);
            if !matches!(
                right_kind,
                IRType::Number(NumericType::Int32 | NumericType::Int64)
            ) {
                return Err(TypecheckError::IllegalNonNumericIndex(
                    right_kind.clone(),
                    right.end,
                ));
            }

            let kind = *inner;
            let left = ir_context.add_expression(left);
            let right = ir_context.add_expression(right);

            IRExpression {
                value: IRExpressionValue::ArrayIndex(left, right),
                kind,
                start,
                end,
            }
        }
        StructLiteral { name, fields } => {
            let struct_type_idx = resolve(local_scope, name.as_str());
            let struct_type = struct_type_idx.map(|idx| ir_context.kind(idx));
            if let Some(IRType::Struct {
                fields: type_fields,
            }) = struct_type
            {
                let type_fields = type_fields.clone();
                let mut expr_fields = HashMap::new();
                let mut missing_fields = Vec::new();
                for (field, expected_type) in type_fields.iter() {
                    if let Some(provided_value) = fields.get(field) {
                        let mut provided_value = typecheck_expression(
                            &parse_context[*provided_value],
                            parse_context,
                            ir_context,
                            local_scope,
                        )?;
                        let expected_type = ir_context.kind(*expected_type);
                        let provided_type = ir_context.kind(provided_value.kind);
                        let derefs = derefs_for_parity(ir_context, expected_type, provided_type);
                        for _ in 0..derefs {
                            provided_value = maybe_dereference(provided_value, ir_context);
                        }
                        let provided_value = ir_context.add_expression(provided_value);
                        expr_fields.insert(field.clone(), provided_value);
                    } else {
                        missing_fields.push(field.clone());
                    }
                }

                let mut extra_fields = Vec::new();
                for field in fields.keys() {
                    if type_fields.get(field).is_none() {
                        extra_fields.push(field.clone());
                    }
                }

                IRExpression {
                    value: IRExpressionValue::StructLiteral(expr_fields),
                    kind: struct_type_idx.unwrap(),
                    start,
                    end,
                }
            } else {
                return Err(TypecheckError::UnknownName(name.clone(), start));
            }
        }
        BinExpr(BinOp::Dot, left, right) => {
            let mut left = typecheck_expression(
                &parse_context[*left],
                parse_context,
                ir_context,
                local_scope,
            )?;
            while is_pointer(&left, ir_context) {
                left = maybe_dereference(left, ir_context);
            }
            let left_kind = ir_context.kind(left.kind);
            let IRType::Struct { fields } = left_kind else {
                return Err(TypecheckError::IllegalLeftDotOperand(left_kind.clone(), start));
            };
            let AstNode{ value: Name(name), .. } = &parse_context[*right] else {
                return Err(TypecheckError::IllegalRightHandDotOperand(start));
            };
            let Some(field) = fields.get(name) else {
                return Err(TypecheckError::FieldNotFound(name.clone(), left_kind.clone(), start));
            };
            let kind = *field;
            let left = ir_context.add_expression(left);
            IRExpression {
                value: IRExpressionValue::Dot(left, name.clone()),
                kind,
                start,
                end,
            }
        }
        BinExpr(BinOp::Assignment, lvalue, rvalue) => {
            // TODO: provide more error diagnostics
            let mut lvalue = match &parse_context[*lvalue] {
                expr @ AstNode {
                    value: Name(_) | BinExpr(BinOp::Dot, _, _) | BinExpr(BinOp::Index, _, _),
                    ..
                } => typecheck_expression(expr, parse_context, ir_context, local_scope)?,
                AstNode { start, .. } => return Err(TypecheckError::IllegalLeftHandValue(*start)),
            };
            let rvalue = &parse_context[*rvalue];
            let mut rvalue = typecheck_expression(rvalue, parse_context, ir_context, local_scope)?;

            let l_derefs_required = derefs_for_parity(
                ir_context,
                ir_context.kind(rvalue.kind),
                ir_context.kind(lvalue.kind),
            );
            for _ in 0..l_derefs_required {
                lvalue = maybe_dereference(lvalue, ir_context);
            }
            let r_derefs_required = derefs_for_parity(
                ir_context,
                ir_context.kind(rvalue.kind),
                ir_context.kind(lvalue.kind),
            );
            for _ in 0..r_derefs_required {
                rvalue = maybe_dereference(rvalue, ir_context);
            }

            let l_kind = ir_context.kind(lvalue.kind);
            let r_kind = ir_context.kind(rvalue.kind);
            if !are_types_equal(ir_context, l_kind, r_kind) {
                return Err(TypecheckError::UnexpectedType {
                    found: r_kind.clone(),
                    expected: l_kind.clone(),
                    provenance: start,
                });
            }

            let lvalue = ir_context.add_expression(lvalue);
            let rvalue = ir_context.add_expression(rvalue);
            IRExpression {
                value: IRExpressionValue::Assignment(lvalue, rvalue),
                kind: ir_context.add_kind(IRType::Void),
                start,
                end,
            }
        }
        Call(function, arguments) => {
            let expr = &parse_context[*function];
            let function = typecheck_expression(expr, parse_context, ir_context, local_scope)?;
            let (parameter_types, returns) = match ir_context.kind(function.kind).clone() {
                IRType::Function {
                    parameters,
                    returns,
                } => (parameters, returns),
                other => return Err(TypecheckError::NonCallableExpression(other, function.end)),
            };

            if arguments.len() != parameter_types.len() {
                return Err(TypecheckError::WrongArgumentCount {
                    found: arguments.len(),
                    expected: parameter_types.len(),
                    provenance: expr.start,
                });
            }

            let arguments = arguments
                .iter()
                .enumerate()
                .map(|(index, argument)| {
                    let argument = &parse_context[*argument];
                    let mut argument =
                        typecheck_expression(argument, parse_context, ir_context, local_scope)?;
                    let argument_kind = ir_context.kind(argument.kind);
                    let parameter_kind = ir_context.kind(parameter_types[index]);
                    let derefs_required =
                        derefs_for_parity(ir_context, parameter_kind, argument_kind);
                    for _ in 0..derefs_required {
                        argument = maybe_dereference(argument, ir_context);
                    }
                    let argument_kind = ir_context.kind(argument.kind);
                    let parameter_kind = ir_context.kind(parameter_types[index]);
                    if are_types_equal(ir_context, argument_kind, parameter_kind) {
                        Ok(ir_context.add_expression(argument))
                    } else {
                        Err(TypecheckError::UnexpectedType {
                            found: argument_kind.clone(),
                            expected: parameter_kind.clone(),
                            provenance: argument.start,
                        })
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;

            let function = ir_context.add_expression(function);

            IRExpression {
                value: IRExpressionValue::Call(function, arguments),
                kind: returns,
                start,
                end,
            }
        }
        Name(name) => {
            let local_type = resolve(local_scope, name);
            match local_type {
                Some(local_type) => IRExpression {
                    value: IRExpressionValue::LocalVariable(name.clone()),
                    kind: local_type,
                    start,
                    end,
                },
                None => {
                    return Err(TypecheckError::UnknownName(name.clone(), start));
                }
            }
        }
        Bool(val) => IRExpression {
            value: IRExpressionValue::Bool(*val),
            kind: ir_context.add_kind(IRType::Bool),
            start,
            end,
        },
        Int(val) => IRExpression {
            value: IRExpressionValue::Int(*val),
            kind: ir_context.add_kind(IRType::Number(NumericType::Int64)),
            start,
            end,
        },
        Float(val) => IRExpression {
            value: IRExpressionValue::Float(*val),
            kind: ir_context.add_kind(IRType::Number(NumericType::Float64)),
            start,
            end,
        },
        TakeUnique(child) | TakeShared(child) => {
            let child = &parse_context[*child];
            let child = typecheck_expression(child, parse_context, ir_context, local_scope)?;
            let kind = ir_context.add_kind(match value {
                TakeUnique(_) => IRType::Unique(child.kind),
                TakeShared(_) => IRType::Shared(child.kind),
                _ => unreachable!(),
            });
            let child = ir_context.add_expression(child);
            IRExpression {
                value: match value {
                    TakeUnique(_) => IRExpressionValue::TakeUnique(child),
                    TakeShared(_) => IRExpressionValue::TakeShared(child),
                    _ => unreachable!(),
                },
                kind,
                start,
                end,
            }
        }
        If(predicate, block) => {
            let mut predicate = typecheck_expression(
                &parse_context[*predicate],
                parse_context,
                ir_context,
                local_scope,
            )?;
            while is_pointer(&predicate, ir_context) {
                predicate = maybe_dereference(predicate, ir_context);
            }
            if ir_context.kind(predicate.kind) != &IRType::Bool {
                todo!(); // compiler diagnostic
            }
            let block = typecheck_expression(
                &parse_context[*block],
                parse_context,
                ir_context,
                local_scope,
            )?;
            // TODO: if-else can return types
            IRExpression {
                value: IRExpressionValue::If(
                    ir_context.add_expression(predicate),
                    ir_context.add_expression(block),
                ),
                kind: ir_context.add_kind(IRType::Void),
                start,
                end,
            }
        }
        // TODO
        While(predicate, block) => {
            let predicate = typecheck_expression(
                &parse_context[*predicate],
                parse_context,
                ir_context,
                local_scope,
            )?;
            let predicate_type = ir_context.kind(predicate.kind);
            if predicate_type != &IRType::Bool {
                todo!(); // compiler diagnostic
            }
            let block = typecheck_expression(
                &parse_context[*block],
                parse_context,
                ir_context,
                local_scope,
            )?;
            // TODO: if-else can return types
            IRExpression {
                value: IRExpressionValue::While(
                    ir_context.add_expression(predicate),
                    ir_context.add_expression(block),
                ),
                kind: ir_context.add_kind(IRType::Void),
                start,
                end,
            }
        }
        Block(parse_statements) => {
            let ir_statements = typecheck(
                parse_statements.iter().cloned(),
                ir_context,
                parse_context,
                local_scope,
            )?;
            let ir_statements = ir_statements
                .into_iter()
                .map(|statement| ir_context.add_statement(statement))
                .collect();
            // TODO: support returning last element if non-semicolon
            IRExpression {
                value: IRExpressionValue::Block(ir_statements),
                kind: ir_context.add_kind(IRType::Void),
                start,
                end,
            }
        }
        BinExpr(
            op @ (BinOp::Add | BinOp::Subtract | BinOp::LessThan | BinOp::GreaterThan),
            left,
            right,
        ) => {
            let mut left = typecheck_expression(
                &parse_context[*left],
                parse_context,
                ir_context,
                local_scope,
            )?;
            let mut right = typecheck_expression(
                &parse_context[*right],
                parse_context,
                ir_context,
                local_scope,
            )?;
            while is_pointer(&left, ir_context) {
                left = maybe_dereference(left, ir_context);
            }
            while is_pointer(&right, ir_context) {
                right = maybe_dereference(right, ir_context);
            }
            let left_kind = ir_context.kind(left.kind).clone();
            let right_kind = ir_context.kind(right.kind).clone();
            if left_kind != right_kind
                || left_kind != IRType::Number(NumericType::Int64)
                    && left_kind != IRType::Number(NumericType::Float64)
                    && left_kind != IRType::Number(NumericType::Int32)
                    && left_kind != IRType::Number(NumericType::Float32)
            {
                return Err(TypecheckError::BinaryOperandMismatch(
                    left_kind, right_kind, start,
                ));
            } else {
                let left_type = left.kind;
                if *op == BinOp::Add || *op == BinOp::Subtract {
                    IRExpression {
                        value: IRExpressionValue::BinaryNumeric(
                            match op {
                                BinOp::Add => BinOpNumeric::Add,
                                BinOp::Subtract => BinOpNumeric::Subtract,
                                _ => unreachable!(),
                            },
                            ir_context.add_expression(left),
                            ir_context.add_expression(right),
                        ),
                        kind: left_type,
                        start,
                        end,
                    }
                } else {
                    IRExpression {
                        value: IRExpressionValue::Comparison(
                            match op {
                                BinOp::LessThan => BinOpComparison::LessThan,
                                BinOp::GreaterThan => BinOpComparison::GreaterThan,
                                _ => unreachable!(),
                            },
                            ir_context.add_expression(left),
                            ir_context.add_expression(right),
                        ),
                        kind: ir_context.add_kind(IRType::Bool),
                        start,
                        end,
                    }
                }
            }
        }
        other => todo!("nested top-level declarations, {:?}", other),
    })
}

// TODO: handle needing to reference
fn derefs_for_parity(ir_context: &IRContext, benchmark: &IRType, argument: &IRType) -> u32 {
    match (benchmark, argument) {
        (
            IRType::Unique(benchmark) | IRType::Shared(benchmark),
            IRType::Unique(argument) | IRType::Shared(argument),
        ) => derefs_for_parity(
            ir_context,
            ir_context.kind(*benchmark),
            ir_context.kind(*argument),
        ),
        (IRType::Unique(_benchmark) | IRType::Shared(_benchmark), _) => {
            0 // TODO: should this indicate an error state?
        }
        (benchmark, IRType::Unique(argument) | IRType::Shared(argument)) => {
            derefs_for_parity(ir_context, benchmark, ir_context.kind(*argument)) + 1
        }
        (_benchmark, _guardian) => 0,
    }
}

/**
 * Dereference the expression if it is a pointer type
 */
fn maybe_dereference(expression: IRExpression, ir_context: &mut IRContext) -> IRExpression {
    let kind = ir_context.kind(expression.kind);
    if let IRType::Unique(inner) | IRType::Shared(inner) = kind {
        let kind = *inner;
        let start = expression.start;
        let end = expression.end;
        let child = ir_context.add_expression(expression);
        IRExpression {
            value: IRExpressionValue::Dereference(child),
            kind,
            start,
            end,
        }
    } else {
        expression
    }
}

fn is_pointer(expression: &IRExpression, ir_context: &IRContext) -> bool {
    let kind = ir_context.kind(expression.kind);

    matches!(kind, IRType::Unique(_) | IRType::Shared(_))
}

fn resolve_ast_type(
    ast_type: &AstNode,
    parse_context: &[AstNode],
    ir_context: &mut IRContext,
    scope: &[Scope],
) -> Result<usize, TypecheckError> {
    Ok(match &ast_type.value {
        AstNodeValue::Name(string) => match string.as_str() {
            "void" => VOID_KIND,
            "bool" => BOOL_KIND,
            "i64" => I64_KIND,
            "f64" => F64_KIND,
            "i32" => I32_KIND,
            "f32" => F32_KIND,
            name => resolve(scope, name)
                .ok_or_else(|| TypecheckError::UnknownName(name.to_string(), ast_type.start))?,
        },
        pointer @ (AstNodeValue::Unique(inner)
        | AstNodeValue::Shared(inner)
        | AstNodeValue::Array(inner)) => {
            let inner = &parse_context[*inner];
            let inner = resolve_ast_type(inner, parse_context, ir_context, scope)?;

            match pointer {
                AstNodeValue::Unique(_) => ir_context.add_kind(IRType::Unique(inner)),
                AstNodeValue::Shared(_) => ir_context.add_kind(IRType::Shared(inner)),
                AstNodeValue::Array(_) => ir_context.add_kind(IRType::Array(inner)),
                _ => unreachable!(),
            }
        }
        _ => todo!("ast type"),
    })
}

fn resolve(scope: &[Scope], name: &str) -> Option<usize> {
    for level in scope {
        if let Some(kind) = level.declarations.get(name) {
            return Some(*kind);
        }
    }

    None
}

fn are_types_equal(ir_context: &IRContext, a: &IRType, b: &IRType) -> bool {
    use IRType::*;
    match (a, b) {
        (Unique(a), Unique(b)) | (Shared(a), Shared(b)) => {
            are_types_equal(ir_context, ir_context.kind(*a), ir_context.kind(*b))
        }
        (
            Function {
                parameters: a_param,
                returns: a_returns,
            },
            Function {
                parameters: b_param,
                returns: b_returns,
            },
        ) => {
            a_param.len() == b_param.len()
                && a_param.iter().zip(b_param.iter()).all(|(a, b)| {
                    are_types_equal(ir_context, ir_context.kind(*a), ir_context.kind(*b))
                })
                && are_types_equal(
                    ir_context,
                    ir_context.kind(*a_returns),
                    ir_context.kind(*b_returns),
                )
        }
        (Struct { fields: a_fields }, Struct { fields: b_fields }) => {
            a_fields.iter().all(|(a_key, a_value)| {
                if let Some(b_value) = b_fields.get(a_key) {
                    are_types_equal(
                        ir_context,
                        ir_context.kind(*a_value),
                        ir_context.kind(*b_value),
                    )
                } else {
                    false
                }
            }) && b_fields.iter().all(|(b_key, b_value)| {
                if let Some(a_value) = a_fields.get(b_key) {
                    are_types_equal(
                        ir_context,
                        ir_context.kind(*b_value),
                        ir_context.kind(*a_value),
                    )
                } else {
                    false
                }
            })
        }
        (Void | Bool | Number(_), Void | Bool | Number(_)) => a == b,
        _ => false,
    }
}
