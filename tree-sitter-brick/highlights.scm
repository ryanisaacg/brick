; highlights.scm

"fn" @keyword.function
[
     "struct"
     "union"
     "interface"
] @keyword.type
[
    "return"
    "yield"
] @keyword.return
[
    "extern"
    "let"
    "borrow"
    "const"
] @keyword
[
    "unique"
    "ref"
    "and"
    "or"
] @keyword.operator
[
    "if"
    "else"
    "case"
] @keyword.conditional
[
    "while"
    "loop"
] @keyword.repeat
"import" @keyword.import

"self" @variable.builtin
(number) @number
(line_comment) @comment
[
     ","
     ";"
     ":"
     "|"
     "=>"
 ] @punctuation.delimiter
[
     "{"
     "}"
     "["
     "]"
 ] @punctuation.bracket
[
    "+"
    "-"
    "*"
    "/"
    "!"
    "."
    "??"
    "?."
    ">"
    ">="
    "=="
    "!="
    "<="
    "<"
    "!"
    "="
    "+="
    "-="
    "*="
    "/="
    "++"
] @operator
[
    "true"
    "false"
] @boolean
[
    "list"
    "cell"
    "rc"
] @type

(char_literal) @character
(string_literal) @string

(type) @type
(type (identifier) @type)
(type_properties (identifier) @attribute)
(struct_declaration
    name: (_) @type
    fields: (name_and_type
        name: (_) @variable.member
        type: (_) @type))
(union_declaration
    name: (_) @type)
(union_variant
    name: (_) @variable.member
    type: (_) @type)
(interface_declaration
    name: (_) @type)
(function_definition
    name: (_) @function
    params: (
      (name_and_type
            name: (_) @variable.parameter
            type: (_) @type)))
(extern_function_binding
    name: (_) @function
    params: (
      (name_and_type
            name: (_) @variable.parameter
            type: (_) @type)))
(required_function
    name: (_) @function
    params: (
      (name_and_type
            name: (_) @variable.parameter
            type: (_) @type)))
(call_expr
    function: (identifier) @function)
(call_expr
  function: (dot_expr _ "." (identifier) @function))
(literal
  type: (_) @type)
(record_key) @variable.member
(case_variant
  name: (_) @variable.member)
