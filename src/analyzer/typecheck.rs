use std::collections::HashMap;

use crate::{
    analyzer::{BinOpComparison, BinOpNumeric, IRExpression, IRExpressionValue, NumericType},
    parser::{
        AstExpression, AstExpressionValue, AstStatement, AstStatementValue, BinOp, ParseTree,
    },
};

use super::{
    scan::ast_type_to_ir, FunDecl, FunctionParameter, IRContext, IRStatement, IRStatementValue,
    IRType, Scope, TypecheckError,
};
// TODO: unification of separate IRContexts?
// TODO: initial header-file like parse

pub fn typecheck(
    statements: impl Iterator<Item = usize>,
    ir_context: &mut IRContext,
    parse_context: &ParseTree,
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
            let AstStatement { value, start, end } = parse_context.statement(statement);
            let start = *start;
            let end = *end;

            let value = match value {
                AstStatementValue::Import(_) => {
                    // TODO: disallow inside functions?
                    return Ok(None);
                }
                AstStatementValue::StructDeclaration { .. } => {
                    // TODO: disallow inside functions?
                    return Ok(None);
                }
                AstStatementValue::Expression(expr) => {
                    let expr = typecheck_expression(
                        parse_context.expression(*expr),
                        parse_context,
                        ir_context,
                        &local_scope[..],
                    )?;
                    IRStatementValue::Expression(ir_context.add_expression(expr))
                }
                AstStatementValue::Declaration(name, expr) => {
                    let expr = typecheck_expression(
                        parse_context.expression(*expr),
                        parse_context,
                        ir_context,
                        &local_scope[..],
                    )?;
                    local_scope[0]
                        .declarations
                        .insert(name.to_string(), expr.kind);
                    IRStatementValue::Declaration(name.clone(), ir_context.add_expression(expr))
                }
                AstStatementValue::FunctionDeclaration {
                    name,
                    params,
                    returns,
                    body,
                } => {
                    let params = params
                        .iter()
                        .map(|param| {
                            let kind = ast_type_to_ir(
                                parse_context.kind(param.kind),
                                parse_context,
                                ir_context,
                            )?;
                            let kind = ir_context.add_kind(kind);
                            Ok(FunctionParameter {
                                name: param.name.to_string(),
                                kind,
                            })
                        })
                        .collect::<Result<Vec<_>, _>>()?;

                    let returns = returns
                        .as_ref()
                        .map(|type_name| {
                            ast_type_to_ir(
                                parse_context.kind(*type_name),
                                parse_context,
                                ir_context,
                            )
                        })
                        .unwrap_or(Ok(IRType::Void))?;
                    let returns = ir_context.add_kind(returns);
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
                        parse_context.expression(*body),
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
            };

            Ok(Some(IRStatement { value, start, end }))
        })
        .filter_map(|x| x.transpose())
        .collect::<Result<Vec<IRStatement>, TypecheckError>>()?;

    Ok(statements)
}

// TODO: allow accessing the local environment
fn typecheck_expression(
    expression: &AstExpression,
    parse_context: &ParseTree,
    ir_context: &mut IRContext,
    local_scope: &[Scope],
) -> Result<IRExpression, TypecheckError> {
    use AstExpressionValue::*;
    let AstExpression { value, start, end } = expression;
    let start = *start;
    let end = *end;

    // TODO: analyze if, block
    Ok(match value {
        Assignment(target, expr) => {
            // TODO: provide more error diagnostics
            // TODO: be able to assign to more things than just words
            let name = match parse_context.expression(*target) {
                AstExpression {
                    value: AstExpressionValue::Name(name),
                    ..
                } => name,
                AstExpression { start, .. } => {
                    return Err(TypecheckError::IllegalLeftHandValue(*start))
                }
            };
            let local_type = resolve(ir_context, local_scope, name).cloned();
            let expr = parse_context.expression(*expr);
            let expr = typecheck_expression(expr, parse_context, ir_context, local_scope)?;
            match local_type {
                None => todo!(),
                Some(local_type) => {
                    let expr_type = ir_context.kind(expr.kind);
                    if !are_types_equal(ir_context, &local_type, expr_type) {
                        todo!("elegant handling of type inequality in assignments");
                    }
                }
            }
            let expr = ir_context.add_expression(expr);
            IRExpression {
                value: IRExpressionValue::Assignment(name.clone(), expr),
                kind: ir_context.add_kind(IRType::Void),
                start,
                end,
            }
        }
        Call(function, arguments) => {
            let expr = parse_context.expression(*function);
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
                    let argument = parse_context.expression(*argument);
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
            let local_type = resolve(ir_context, local_scope, name);
            match local_type {
                Some(local_type) => IRExpression {
                    value: IRExpressionValue::LocalVariable(name.clone()),
                    kind: ir_context.add_kind(local_type.clone()),
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
            let child = parse_context.expression(*child);
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
                parse_context.expression(*predicate),
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
                parse_context.expression(*block),
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
                parse_context.expression(*predicate),
                parse_context,
                ir_context,
                local_scope,
            )?;
            let predicate_type = ir_context.kind(predicate.kind);
            if predicate_type != &IRType::Bool {
                todo!(); // compiler diagnostic
            }
            let block = typecheck_expression(
                parse_context.expression(*block),
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
                parse_context.expression(*left),
                parse_context,
                ir_context,
                local_scope,
            )?;
            let mut right = typecheck_expression(
                parse_context.expression(*right),
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
            todo!();
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

fn resolve<'a, 'b>(
    ir_context: &'a IRContext,
    scope: &'a [Scope],
    name: &'b str,
) -> Option<&'a IRType> {
    for level in scope {
        if let Some(kind) = level.declarations.get(name) {
            return Some(ir_context.kind(*kind));
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
        (Void | Bool | Number(_), Void | Bool | Number(_)) => a == b,
        _ => false,
    }
}