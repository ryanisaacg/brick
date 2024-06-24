use std::cmp::Ordering;

use brick::{
    parse_file,
    parser::{AstNode, AstNodeValue, BinOp, UnaryOp, UnionDeclarationVariant},
    CompileError,
};
use typed_arena::Arena;

pub fn format_str(source: &str) -> Result<String, CompileError> {
    let parse_arena = Arena::new();
    let mut ast = parse_file(&parse_arena, "input", source.to_string())?;

    Ok(format(&mut ast[..]))
}

pub fn format(nodes: &mut [AstNode]) -> String {
    rewrite(nodes);

    let mut output = String::new();
    for node in nodes.iter() {
        write_node(node, &mut output, 0);
        output.push('\n');
    }
    output
}

fn rewrite(nodes: &mut [AstNode]) {
    // Move imports to the top of the block
    nodes.sort_by(|a, b| match (&a.value, &b.value) {
        (AstNodeValue::Import(a_path), AstNodeValue::Import(b_path)) => a_path.cmp(b_path),
        (AstNodeValue::Import(_), _) => Ordering::Less,
        (_, AstNodeValue::Import(_)) => Ordering::Greater,
        (_, _) => Ordering::Equal,
    });
}

fn write_node(node: &AstNode, result: &mut String, indent: u32) {
    match &node.value {
        AstNodeValue::FunctionDeclaration(func) => {
            write_function_header(
                result,
                &func.name,
                func.params
                    .iter()
                    .map(|(_, param)| (param.name.as_str(), param.ty)),
                func.returns.as_deref(),
                func.is_extern,
                func.is_coroutine,
            );
            result.push(' ');
            write_node(func.body, result, indent);
            result.push('\n');
        }
        AstNodeValue::ExternFunctionBinding(func) => {
            write_function_header(
                result,
                func.name.as_str(),
                func.params
                    .iter()
                    .map(|param| (param.name.as_str(), param.ty)),
                func.returns.as_deref(),
                true,
                false,
            );
            result.push_str(";\n");
        }
        AstNodeValue::StructDeclaration(decl) => {
            result.push_str("struct ");
            result.push_str(decl.name.as_str());
            if !decl.properties.is_empty() {
                result.push(':');
                for (idx, property) in decl.properties.iter().enumerate() {
                    result.push(' ');
                    result.push_str(property.as_str());
                    if idx + 1 != decl.properties.len() {
                        result.push(',');
                    }
                }
            }
            result.push_str(" {\n");
            for field in decl.fields.iter() {
                do_indent(result, indent + 1);
                result.push_str(field.name.as_str());
                result.push_str(": ");
                write_node(field.ty, result, indent + 1);
                result.push_str(",\n");
            }
            if !decl.associated_functions.is_empty() {
                result.push('\n');
                for func in decl.associated_functions.iter() {
                    write_node(func, result, indent + 1);
                }
            }
            result.push_str("}\n");
        }
        AstNodeValue::UnionDeclaration(decl) => {
            result.push_str("union ");
            result.push_str(decl.name.as_str());
            if !decl.properties.is_empty() {
                result.push(':');
                for (idx, property) in decl.properties.iter().enumerate() {
                    result.push(' ');
                    result.push_str(property.as_str());
                    if idx + 1 != decl.properties.len() {
                        result.push(',');
                    }
                }
            }
            result.push_str(" {\n");
            for variant in decl.variants.iter() {
                do_indent(result, indent + 1);
                match variant {
                    UnionDeclarationVariant::WithValue(variant) => {
                        result.push_str(variant.name.as_str());
                        result.push('(');
                        write_node(variant.ty, result, indent + 1);
                        result.push(')');
                    }
                    UnionDeclarationVariant::WithoutValue(variant) => {
                        result.push_str(variant.as_str())
                    }
                }
                result.push_str(",\n");
            }
            result.push_str("}\n");
        }
        AstNodeValue::InterfaceDeclaration(decl) => {
            result.push_str("interface ");
            result.push_str(decl.name.as_str());
            result.push_str(" {\n");
            for func in decl.associated_functions.iter() {
                do_indent(result, indent + 1);
                write_node(func, result, indent + 1);
            }
            result.push_str("}\n");
        }
        AstNodeValue::RequiredFunction(func) => {
            write_function_header(
                result,
                func.name.as_str(),
                func.params
                    .iter()
                    .map(|param| (param.name.as_str(), param.ty)),
                func.returns.as_deref(),
                false,
                false,
            );
            result.push_str(",\n");
        }
        AstNodeValue::Declaration(var_name, type_hint, value, _) => {
            result.push_str("let ");
            result.push_str(var_name.as_str());
            if let Some(ty) = type_hint {
                result.push_str(": ");
                write_node(ty, result, indent);
            }
            result.push_str(" = ");
            write_node(value, result, indent);
            result.push(';');
        }
        AstNodeValue::ConstDeclaration {
            name,
            type_hint,
            value,
            variable_id: _,
        } => {
            result.push_str("const ");
            result.push_str(name.as_str());
            if let Some(ty) = type_hint {
                result.push_str(": ");
                write_node(ty, result, indent);
            }
            result.push_str(" = ");
            write_node(value, result, indent);
            result.push(';');
        }
        AstNodeValue::BorrowDeclaration(name, value, _) => {
            result.push_str("borrow ");
            result.push_str(name.as_str());
            result.push_str(" = ");
            write_node(value, result, indent);
            result.push(';');
        }
        AstNodeValue::Import(path) => {
            result.push_str("import ");
            for (idx, entry) in path.iter().enumerate() {
                result.push_str(entry);
                if idx + 1 != path.len() {
                    result.push('.');
                }
            }
            result.push(';');
        }
        AstNodeValue::Return(inner) => {
            result.push_str("return");
            if let Some(inner) = inner {
                result.push(' ');
                write_node(inner, result, indent);
            } else {
                result.push(';');
            }
        }
        AstNodeValue::Yield(inner) => {
            result.push_str("yield");
            if let Some(inner) = inner {
                result.push(' ');
                write_node(inner, result, indent);
            }
        }
        AstNodeValue::Statement(inner) => {
            do_indent(result, indent);
            write_node(inner, result, indent);
            result.push(';');
        }
        AstNodeValue::Name {
            value,
            referenced_id: _,
        } => result.push_str(value),
        AstNodeValue::Int(value) => result.push_str(value.to_string().as_str()),
        AstNodeValue::Float(value) => result.push_str(format!("{value:?}").as_str()),
        AstNodeValue::Bool(value) => result.push_str(value.to_string().as_str()),
        AstNodeValue::CharLiteral(chr) => {
            result.push('\'');
            result.push(*chr);
            result.push('\'');
        }
        AstNodeValue::StringLiteral(string) => {
            result.push('\"');
            result.push_str(string);
            result.push('\"');
        }
        AstNodeValue::Null => result.push_str("null"),
        AstNodeValue::UnaryExpr(op, operand) => {
            match op {
                UnaryOp::BooleanNot => result.push('!'),
            }
            write_node(operand, result, indent);
        }
        AstNodeValue::BinExpr(BinOp::Index, lhs, rhs) => {
            write_node(lhs, result, indent);
            result.push('[');
            write_node(rhs, result, indent);
            result.push(']');
        }
        AstNodeValue::BinExpr(BinOp::Dot, lhs, rhs) => {
            write_node(lhs, result, indent);
            result.push('.');
            write_node(rhs, result, indent);
        }
        AstNodeValue::BinExpr(BinOp::NullChaining, lhs, rhs) => {
            write_node(lhs, result, indent);
            result.push_str("?.");
            write_node(rhs, result, indent);
        }
        // TODO: paranthesize expressions as necessary
        AstNodeValue::BinExpr(op, lhs, rhs) => {
            let paren_left = if let AstNodeValue::BinExpr(lhs_op, _, _) = &lhs.value {
                op.binding_power() > lhs_op.binding_power()
            } else {
                false
            };
            if paren_left {
                result.push('(');
            }
            write_node(lhs, result, indent);
            if paren_left {
                result.push(')');
            }
            result.push(' ');
            result.push_str(match op {
                BinOp::NullChaining | BinOp::Dot | BinOp::Index => unreachable!(),
                BinOp::Concat => "++",
                BinOp::NullCoalesce => "??",
                BinOp::Add => "+",
                BinOp::Subtract => "-",
                BinOp::Multiply => "*",
                BinOp::Divide => "/",
                BinOp::LessThan => "<",
                BinOp::GreaterThan => ">",
                BinOp::LessEqualThan => "<=",
                BinOp::GreaterEqualThan => ">=",
                BinOp::EqualTo => "==",
                BinOp::NotEquals => "!=",
                BinOp::Assignment => "=",
                BinOp::AddAssign => "+=",
                BinOp::SubtractAssign => "-=",
                BinOp::MultiplyAssign => "*=",
                BinOp::DivideAssign => "/=",
                BinOp::BooleanAnd => "and",
                BinOp::BooleanOr => "or",
            });
            result.push(' ');
            let paren_right = if let AstNodeValue::BinExpr(rhs_op, _, _) = &rhs.value {
                op.binding_power() > rhs_op.binding_power()
            } else {
                false
            };
            if paren_right {
                result.push('(');
            }
            write_node(rhs, result, indent);
            if paren_right {
                result.push(')');
            }
        }
        AstNodeValue::If(decl) => {
            result.push_str("if ");
            write_node(decl.condition, result, indent);
            write_node(decl.if_branch, result, indent);
            // Remove the newline from the block output
            if let Some(else_branch) = decl.else_branch.as_ref() {
                result.push_str(" else ");
                write_node(else_branch, result, indent);
            }
        }
        AstNodeValue::While(cond, body) => {
            result.push_str("while ");
            write_node(cond, result, indent);
            write_node(body, result, indent);
        }
        AstNodeValue::Loop(body) => {
            result.push_str("loop ");
            write_node(body, result, indent);
        }
        AstNodeValue::Call(func, args) => {
            write_node(func, result, indent);
            result.push('(');
            for (idx, arg) in args.iter().enumerate() {
                write_node(arg, result, indent);
                if idx + 1 != args.len() {
                    result.push_str(", ");
                }
            }
            result.push(')');
        }
        AstNodeValue::RecordLiteral { name, fields } => {
            write_node(name, result, indent);
            result.push_str("{ ");
            let mut field_order: Vec<_> = fields.keys().collect();
            field_order.sort();
            for (idx, field) in field_order.iter().enumerate() {
                // TODO: unify { name: name }
                result.push_str(field);
                result.push_str(": ");
                write_node(&fields[*field], result, indent);
                if idx + 1 != field_order.len() {
                    result.push_str(", ");
                }
            }
            result.push('}');
        }
        AstNodeValue::DictLiteral(entries) => {
            result.push_str("dict{ ");
            for (idx, (key, value)) in entries.iter().enumerate() {
                result.push('[');
                write_node(key, result, indent);
                result.push_str("]: ");
                write_node(value, result, indent);
                if idx + 1 != entries.len() {
                    result.push_str(", ");
                }
            }
            result.push('}');
        }
        AstNodeValue::ArrayLiteral(entries) => {
            result.push_str("list[");
            for (idx, entry) in entries.iter().enumerate() {
                write_node(entry, result, indent);
                if idx + 1 != entries.len() {
                    result.push_str(", ");
                }
            }
            result.push(']');
        }
        AstNodeValue::ArrayLiteralLength(value, times) => {
            result.push_str("list[");
            write_node(value, result, indent);
            result.push_str("; ");
            write_node(times, result, indent);
            result.push(']');
        }
        AstNodeValue::ReferenceCountLiteral(inner) => {
            result.push_str("rc {");
            write_node(inner, result, indent);
            result.push_str(" }");
        }
        AstNodeValue::CellLiteral(inner) => {
            result.push_str("cell {");
            write_node(inner, result, indent);
            result.push_str(" }");
        }
        AstNodeValue::Block(values) => {
            result.push_str("{\n");
            for value in values.iter() {
                let indent = indent + 1;
                do_indent(result, indent);
                write_node(value, result, indent);
                result.push('\n');
            }
            do_indent(result, indent);
            result.push('}');
        }
        AstNodeValue::Deref(inner) => {
            result.push('*');
            write_node(inner, result, indent);
        }
        AstNodeValue::Match(decl) => {
            result.push_str("case ");
            write_node(decl.value, result, indent);
            result.push_str(" {\n");
            for case in decl.cases.iter() {
                do_indent(result, indent + 1);
                for (idx, variant) in case.variants.iter().enumerate() {
                    result.push_str(variant.name.as_str());
                    if !variant.bindings.is_empty() {
                        result.push('(');
                        for (idx, binding) in variant.bindings.iter().enumerate() {
                            result.push_str(binding.as_str());
                            if idx + 1 != variant.bindings.len() {
                                result.push_str(", ");
                            }
                        }
                        result.push(')');
                    }
                    if idx + 1 != case.variants.len() {
                        result.push_str(" | ");
                    }
                }
                result.push_str(" => ");
                write_node(&case.body, result, indent + 1);
                if !matches!(&case.body.value, AstNodeValue::Block(_)) {
                    result.push(',');
                }
                result.push('\n');
            }
            result.push('}');
        }
        AstNodeValue::VoidType => result.push_str("void"),
        AstNodeValue::TakeUnique(inner) | AstNodeValue::UniqueType(inner) => {
            result.push_str("unique ");
            write_node(inner, result, indent);
        }
        AstNodeValue::TakeRef(inner) | AstNodeValue::SharedType(inner) => {
            result.push_str("ref ");
            write_node(inner, result, indent);
        }
        AstNodeValue::ArrayType(inner) => {
            result.push_str("list[");
            write_node(inner, result, indent);
            result.push(']');
        }
        AstNodeValue::DictType(key_ty, value_ty) => {
            result.push_str("list[");
            write_node(key_ty, result, indent);
            result.push_str(", ");
            write_node(value_ty, result, indent);
            result.push(']');
        }
        AstNodeValue::RcType(inner) => {
            result.push_str("rc[");
            write_node(inner, result, indent);
            result.push(']');
        }
        AstNodeValue::CellType(inner) => {
            result.push_str("cell[");
            write_node(inner, result, indent);
            result.push(']');
        }
        AstNodeValue::NullableType(inner) => {
            write_node(inner, result, indent);
            result.push('?');
        }
        AstNodeValue::GeneratorType { yield_ty, param_ty } => {
            result.push_str("generator[");
            write_node(yield_ty, result, indent);
            result.push_str(", ");
            write_node(param_ty, result, indent);
            result.push(']');
        }
    }
}

fn write_function_header<'iter, 'node: 'iter>(
    result: &mut String,
    name: &str,
    params: impl Iterator<Item = (&'iter str, &'iter AstNode<'node>)>,
    returns: Option<&AstNode>,
    is_extern: bool,
    is_coroutine: bool,
) {
    if is_extern {
        result.push_str("extern ");
    }
    if is_coroutine {
        result.push_str("gen ");
    }
    result.push_str("fn ");
    result.push_str(name);
    result.push('(');
    let mut params = params.peekable();
    while let Some((name, ty)) = params.next() {
        result.push_str(name);
        result.push_str(": ");
        write_node(ty, result, 0);
        if params.peek().is_some() {
            result.push_str(", ");
        }
    }
    result.push(')');
    if let Some(returns) = &returns {
        result.push_str(": ");
        write_node(returns, result, 0);
    }
}

fn do_indent(string: &mut String, count: u32) {
    for _ in 0..count {
        string.push_str("    ");
    }
}
