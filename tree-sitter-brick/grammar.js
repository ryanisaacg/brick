/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

const ASSIGNMENT = 1;
const NULL_COALESCE = ASSIGNMENT + 1;
// bools
const BOOLEAN_OR = NULL_COALESCE + 1;
const BOOLEAN_AND = BOOLEAN_OR + 1;
const BOOLEAN_NOT = BOOLEAN_AND + 1;
const COMPARE = BOOLEAN_NOT + 1;
// math
const SUM = COMPARE + 1;
const FACTOR = SUM + 1;
// misc
const CONCAT = FACTOR + 1;
const REFERENCE = CONCAT + 1;
const CALL = REFERENCE + 1;
const NULL_CHAINING = CALL + 1;
const DOT = NULL_CHAINING + 1;

module.exports = grammar({
  name: "brick",

  extras: ($) => [$.line_comment, /\s/],

  rules: {
    source_file: ($) =>
      seq(
        repeat($._definition),
        optional(
          choice(
            seq(repeat1($._non_const_statement), optional($._expression)),
            $._expression
          )
        )
      ),

    _definition: ($) =>
      choice(
        $.function_definition,
        $.extern_function_binding,
        $.extern_function_definition,
        $.struct_declaration,
        $.union_declaration,
        $.interface_declaration,
        $.const_declaration,
        $.import
      ),

    // FUNCTIONS
    function_definition: ($) => seq($._function_header, $.block),

    extern_function_definition: ($) => seq("extern", $.function_definition),

    extern_function_binding: ($) => seq("extern", $._function_header, ";"),

    _function_header: ($) =>
      seq(
        "fn",
        field("name", $.identifier),
        "(",
        field(
          "params",
          seq(
            choice(
              seq(
                optional(choice("unique", "ref")),
                "self",
                repeat(seq(",", $.name_and_type))
              ),
              commaSep($.name_and_type)
            )
          )
        ),
        ")",
        field("return", optional(seq(":", $.type)))
      ),

    // DECLARATIONS
    struct_declaration: ($) =>
      seq(
        "struct",
        field("name", $.identifier),
        field("properties", optional($.type_properties)),
        "{",
        field("fields", seq(commaSep($.name_and_type))),
        field("associated_functions", repeat($.function_definition)),
        "}"
      ),
    union_declaration: ($) =>
      seq(
        "union",
        field("name", $.identifier),
        field("properties", optional($.type_properties)),
        "{",
        seq(commaSep1($.union_variant)),
        "}"
      ),
    union_variant: ($) =>
      choice(
        field("name", $.identifier),
        seq(
          field("name", $.identifier),
          "(",
          field("type", commaSep1($.type)),
          ")"
        )
      ),
    interface_declaration: ($) =>
      seq(
        "interface",
        field("name", $.identifier),
        "{",
        field("associated_functions", commaSep($.required_function)),
        "}"
      ),
    required_function: ($) => $._function_header,
    import: ($) => seq("import", periodSep1($.identifier), ";"),

    type_properties: ($) => seq(":", commaSep1($.identifier)),

    // TYPES
    type: ($) =>
      choice(
        "void",
        $.primitive_type,
        $.identifier,
        $.nullable_type,
        $._reference_type,
        $._generic_type
      ),
    primitive_type: (_) => choice("bool", "i32", "i64", "f32", "f64"),
    nullable_type: ($) => prec(2, seq($.type, "?")),
    _reference_type: ($) => prec(0, seq(choice("unique", "ref"), $.type)),
    _generic_type: ($) => prec(1, seq($.type, "[", commaSep1($.type), "]")),

    // EXPRESSIONS
    _statement: ($) => choice($._non_const_statement, $.const_declaration),
    _non_const_statement: ($) =>
      choice(
        seq(
          choice(
            $.return_statement,
            $.variable_declaration,
            $.borrow_declaration,
            $.assignment,
            $._expression
          ),
          ";"
        ),
        prec(2, $._block_expr)
      ),
    _expression: ($) => choice($._expression_not_literal, $.literal),
    _expression_not_literal: ($) =>
      prec(
        1,
        choice(
          $.identifier,
          $.number,
          $.bool_literal,
          $.null,
          $.not_expr,
          $.dot_expr,
          $.index_expr,
          $.null_coalesce,
          $.null_chaining,
          $.add_expr,
          $.mul_expr,
          $.compare_expr,
          $.boolean_and,
          $.boolean_or,
          $.deref_expr,
          $.call_expr,
          $.take_reference_expr,
          $._block_expr,
          $.char_literal,
          $.string_literal,
          $.concat_expr,
          seq("(", $._expression, ")")
        )
      ),
    _block_expr: ($) =>
      choice($.if_expr, $.while_expr, $.loop_expr, $.block, $.case_expr),
    block: ($) => seq("{", repeat($._statement), optional($._expression), "}"),

    return_statement: ($) =>
      seq(choice("yield", "return"), optional($._expression)),

    variable_declaration: ($) =>
      seq("let", $.identifier, optional($.type_hint), "=", $._expression),
    borrow_declaration: ($) =>
      seq("borrow", $.identifier, optional($.type_hint), "=", $._expression),
    const_declaration: ($) =>
      seq(
        "const",
        $.identifier,
        optional($.type_hint),
        "=",
        $._expression,
        ";"
      ),
    assignment: ($) =>
      prec(
        ASSIGNMENT,
        seq($._expression, choice("=", "+=", "-=", "*=", "/="), $._expression)
      ),

    type_hint: ($) => seq(":", $.type),

    name_and_type: ($) =>
      seq(field("name", $.identifier), ":", field("type", $.type)),

    identifier: ($) => $._name,
    _name: (_) => /[A-Za-z_][A-Za-z_0-9]*/,
    number: (_) => /-?\d+(.\d+)?/,
    bool_literal: (_) => choice("true", "false"),
    null: (_) => "null",
    char_literal: ($) => seq("'", choice($.escape_sequence, /[^\\']/), "'"),
    string_literal: ($) =>
      seq('"', repeat(choice($.escape_sequence, /[^"\n\\]+/)), '"'),
    escape_sequence: (_) => token.immediate(seq("\\", /./)),

    not_expr: ($) => prec(BOOLEAN_NOT, seq("!", $._expression)),
    dot_expr: ($) => prec(DOT, seq($._expression, ".", $.identifier)),
    index_expr: ($) => prec(CALL, seq($._expression, "[", $._expression, "]")),
    null_coalesce: ($) =>
      prec.left(NULL_COALESCE, seq($._expression, "??", $._expression)),
    null_chaining: ($) =>
      prec(NULL_CHAINING, seq($._expression, "?.", $.identifier)),
    add_expr: ($) =>
      prec.left(SUM, seq($._expression, choice("+", "-"), $._expression)),
    mul_expr: ($) =>
      prec.left(FACTOR, seq($._expression, choice("*", "/"), $._expression)),
    compare_expr: ($) =>
      prec.left(
        COMPARE,
        seq(
          $._expression,
          choice(">", ">=", "==", "<=", "<", "!="),
          $._expression
        )
      ),
    boolean_and: ($) =>
      prec.left(BOOLEAN_AND, seq($._expression, "and", $._expression)),
    boolean_or: ($) =>
      prec.left(BOOLEAN_OR, seq($._expression, "or", $._expression)),
    deref_expr: ($) => prec(REFERENCE, seq("*", $._expression)),
    concat_expr: ($) =>
      prec.left(CONCAT, seq($._expression, "++", $._expression)),

    call_expr: ($) =>
      prec(
        CALL,
        seq(field("function", $._expression), "(", commaSep($._expression), ")")
      ),
    take_reference_expr: ($) =>
      prec(REFERENCE, seq(choice("unique", "ref"), $._expression)),

    if_expr: ($) =>
      seq(
        "if",
        $._expression_not_literal,
        $.block,
        optional(seq("else", choice($.if_expr, $.block)))
      ),
    while_expr: ($) => seq("while", $._expression_not_literal, $.block),
    loop_expr: ($) => seq("loop", $.block),
    case_expr: ($) =>
      seq("case", $._expression_not_literal, "{", repeat1($.case_branch), "}"),
    case_branch: ($) =>
      seq(
        $.case_variant,
        repeat(seq("|", $.case_variant)),
        "=>",
        choice(seq($._expression, ","), $.block)
      ),
    case_variant: ($) =>
      choice(
        field("name", $.identifier),
        seq(field("name", $.identifier), "(", commaSep($.identifier), ")")
      ),

    literal: ($) =>
      choice(
        $._intrinsic_literal,
        $._list_literal,
        $._list_literal_length,
        $._record_literal
      ),
    _intrinsic_literal: ($) =>
      seq(
        field("type", choice("cell", "rc")),
        "{",
        field("value", $._expression),
        "}"
      ),
    _list_literal: ($) =>
      seq(
        field("type", "list"),
        "[",
        field("value", commaSep1($._expression)),
        "]"
      ),
    _list_literal_length: ($) =>
      seq(
        field("type", "list"),
        "[",
        field("value", seq($._expression, ";", $._expression)),
        "]"
      ),
    _record_literal: ($) =>
      seq(field("type", $.identifier), "{", commaSep($._record_entry), "}"),
    _record_entry: ($) =>
      choice($.record_key, seq($.record_key, ":", $._expression)),
    record_key: ($) => $._name,

    // copied from https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js
    line_comment: (_) => token(choice(seq("//", /(\\+(.|\r?\n)|[^\\\n])*/))),
  },
});

/**
 * @param rule {RuleOrLiteral}
 */
function commaSep1(rule) {
  return seq(rule, repeat(seq(",", rule)), optional(","));
}

/**
 * @param rule {RuleOrLiteral}
 */
function commaSep(rule) {
  return optional(commaSep1(rule));
}

/**
 * @param rule {RuleOrLiteral}
 */
function periodSep1(rule) {
  return seq(rule, repeat(seq(".", rule)));
}
