import birdie
import glance
import gleam/string
import gleeunit
import gleeunit/should
import pprint
import simplifile

pub fn main() {
  gleeunit.main()
}

fn to_snapshot(src: String) -> String {
  let result = glance.module(src)
  let out = case result {
    Ok(ast) -> pprint.format(ast)
    Error(_) -> pprint.format(result)
  }
  src <> "\n\n---------------------------\n\n" <> out
}

type Entity {
  Import
  CustomType
  TypeAlias
  Constant
  Function
  Expression
  Pattern
}

fn check_location(src: String, entity: Entity) -> String {
  let assert Ok(module) = glance.module(src) as "Failed to parse module"
  let location = case entity {
    Constant -> {
      let assert [constant, ..] = module.constants as "Constant not found"
      constant.definition.location
    }
    CustomType -> {
      let assert [type_, ..] = module.custom_types as "Custom type not found"
      type_.definition.location
    }
    Function -> {
      let assert [function, ..] = module.functions as "Function not found"
      function.definition.location
    }
    Import -> {
      let assert [import_, ..] = module.imports as "Import not found"
      import_.definition.location
    }
    TypeAlias -> {
      let assert [alias, ..] = module.type_aliases as "Type alias not found"
      alias.definition.location
    }

    Expression -> {
      let assert [function, ..] = module.functions as "Function not found"
      let assert [statement, ..] = function.definition.body as "Body is empty"
      case statement {
        glance.Assert(location:, ..)
        | glance.Assignment(location:, ..)
        | glance.Use(location:, ..) -> location
        glance.Expression(expression) -> expression.location
      }
    }

    Pattern -> {
      let assert [function, ..] = module.functions as "Function not found"
      let assert [statement, ..] = function.definition.body as "Body is empty"
      let assert glance.Assignment(pattern:, ..) = statement
        as "Statement is not an assignment"
      pattern.location
    }
  }

  highlight_location(src, location, 0, "", "", False)
}

fn highlight_location(
  src: String,
  location: glance.Span,
  position: Int,
  text: String,
  next_line: String,
  underline_used: Bool,
) -> String {
  case src {
    "" ->
      case underline_used {
        False -> text
        True -> text <> "\n" <> next_line
      }
    "\n" <> src -> {
      let text = case underline_used {
        False -> text <> "\n"
        True -> text <> "\n" <> next_line <> "\n"
      }
      case position + 1 >= location.end {
        False ->
          highlight_location(src, location, position + 1, text, "", False)
        True -> text <> src
      }
    }
    _ ->
      case string.pop_grapheme(src) {
        Ok(#(char, src)) -> {
          let #(next_line, underline_used) = case position >= location.start {
            False -> #(next_line <> " ", False)
            True ->
              case position < location.end {
                False -> #(next_line, underline_used)
                True -> #(next_line <> "▔", True)
              }
          }
          highlight_location(
            src,
            location,
            position + string.byte_size(char),
            text <> char,
            next_line,
            underline_used,
          )
        }
        Error(_) -> text
      }
  }
}

pub fn parse_self_test() {
  let assert Ok(src) = simplifile.read("src/glance.gleam")
  let assert Ok(_) = glance.module(src)
}

pub fn empty_test() {
  ""
  |> glance.module
  |> should.equal(Ok(glance.Module([], [], [], [], [])))
}

pub fn public_enum_test() {
  "// comment
  pub type Cardinal {
    North East South West
  }"
  |> to_snapshot
  |> birdie.snap(title: "public_enum")
}

pub fn private_enum_test() {
  "type Cardinal {
    North East South West
  }"
  |> to_snapshot
  |> birdie.snap(title: "private_enum")
}

pub fn phantom_test() {
  "pub type Spooky(t) {
    Spooky
  }"
  |> to_snapshot
  |> birdie.snap(title: "phantom")
}

pub fn phantom_multiple_test() {
  "pub type Spooky(t, u) {
    Spooky
  }"
  |> to_snapshot
  |> birdie.snap(title: "phantom_multiple")
}

pub fn box_test() {
  "pub type Box(x) {
    Box(x)
  }"
  |> to_snapshot
  |> birdie.snap(title: "box")
}

pub fn multiple_fields_test() {
  "pub type Box(x, y, z) {
    Box(x, y, z)
  }"
  |> to_snapshot
  |> birdie.snap(title: "multiple_fields")
}

pub fn trailing_comma_in_parameters_test() {
  "pub type Box(x, ) {
    Box
  }"
  |> to_snapshot
  |> birdie.snap(title: "trailing_comma_in_parameters")
}

pub fn empty_parameter_list_test() {
  "pub type Box() {
    Box
  }"
  |> to_snapshot
  |> birdie.snap(title: "empty_parameter_list")
}

pub fn empty_fields_list_test() {
  "pub type Box {
    Box()
  }"
  |> to_snapshot
  |> birdie.snap(title: "empty_fields_list")
}

pub fn opaque_test() {
  "pub opaque type Box {
    Box()
  }"
  |> to_snapshot
  |> birdie.snap(title: "opaque")
}

pub fn fields_trailing_comma_test() {
  "pub type Box(a) {
    Box(a,)
  }"
  |> to_snapshot
  |> birdie.snap(title: "fields_trailing_comma")
}

pub fn labelled_fields_test() {
  "pub type Box(a) {
    Box(a: a, b: a)
  }"
  |> to_snapshot
  |> birdie.snap(title: "labelled_fields")
}

pub fn phantom_trailing_comma_test() {
  "pub type Spooky(t, u, ) {
    Spooky
  }"
  |> to_snapshot
  |> birdie.snap(title: "phantom_trailing_comma")
}

pub fn comment_discarding_test() {
  "pub type
    // Comment!
  Spooky(
    // one
    t, u,
    // two
     ) {
      // three
    Spooky
    // four
  }"
  |> to_snapshot
  |> birdie.snap(title: "comment_discarding")
}

pub fn alias_variable_test() {
  "pub type X = a"
  |> to_snapshot
  |> birdie.snap(title: "alias_variable")
}

pub fn alias_hole_test() {
  "pub type X = _whatever"
  |> to_snapshot
  |> birdie.snap(title: "alias_hole")
}

pub fn alias_named_test() {
  "pub type X = Y"
  |> to_snapshot
  |> birdie.snap(title: "alias_named")
}

pub fn alias_qualified_named_test() {
  "pub type X = wibble.Y"
  |> to_snapshot
  |> birdie.snap(title: "alias_qualified_named")
}

pub fn alias_tuple_test() {
  "pub type X = #(A, B)"
  |> to_snapshot
  |> birdie.snap(title: "alias_tuple")
}

pub fn alias_fn_test() {
  "pub type X = fn(A, B) -> C"
  |> to_snapshot
  |> birdie.snap(title: "alias_fn")
}

pub fn import_test() {
  "import one"
  |> to_snapshot
  |> birdie.snap(title: "import")
}

pub fn nested_import_test() {
  "import one/two/three"
  |> to_snapshot
  |> birdie.snap(title: "nested_import")
}

pub fn aliased_import_test() {
  "import one/two/three as four"
  |> to_snapshot
  |> birdie.snap(title: "aliased_import")
}

pub fn empty_unqualified_test() {
  "import one/two/three.{} as four"
  |> to_snapshot
  |> birdie.snap(title: "empty_unqualified")
}

pub fn unqualified_test() {
  "import one/two/three.{One, Two, three, four} as four"
  |> to_snapshot
  |> birdie.snap(title: "unqualified")
}

pub fn unqualified_type_test() {
  "import one/two/three.{type One, type Two, three, four, type Five as X, type Six as Y} as four"
  |> to_snapshot
  |> birdie.snap(title: "unqualified_type")
}

pub fn unqualified_aliased_test() {
  "import one/two/three.{One as Two, Three, four as five, six} as four"
  |> to_snapshot
  |> birdie.snap(title: "unqualified_aliased")
}

pub fn constant_int_test() {
  "const x = 123"
  |> to_snapshot
  |> birdie.snap(title: "constant_int")
}

pub fn constant_float_test() {
  "const x = 1.1"
  |> to_snapshot
  |> birdie.snap(title: "constant_float")
}

pub fn constant_string_test() {
  "const x = \"123\""
  |> to_snapshot
  |> birdie.snap(title: "constant_string")
}

pub fn constant_variable_test() {
  "const x = y"
  |> to_snapshot
  |> birdie.snap(title: "constant_variable")
}

pub fn constant_pub_int_test() {
  "pub const x = 123"
  |> to_snapshot
  |> birdie.snap(title: "constant_pub_int")
}

pub fn constant_annotated_int_test() {
  "pub const x: Int = 123"
  |> to_snapshot
  |> birdie.snap(title: "constant_annotated_int")
}

pub fn constant_tuple_test() {
  "const x = #(1, 2.0, 3)"
  |> to_snapshot
  |> birdie.snap(title: "constant_tuple")
}

pub fn constant_tuple_trailing_comma_test() {
  "const x = #(1, 2.0, 3,)"
  |> to_snapshot
  |> birdie.snap(title: "constant_tuple_trailing_comma")
}

pub fn constant_empty_tuple_test() {
  "const x = #()"
  |> to_snapshot
  |> birdie.snap(title: "constant_empty_tuple")
}

pub fn constant_list_test() {
  "const x = [1, 2.0, 3]"
  |> to_snapshot
  |> birdie.snap(title: "constant_list")
}

pub fn constant_list_trailing_comma_test() {
  "const x = [1, 2.0, 3,]"
  |> to_snapshot
  |> birdie.snap(title: "constant_list_trailing_comma")
}

pub fn constant_empty_list_test() {
  "const x = []"
  |> to_snapshot
  |> birdie.snap(title: "constant_empty_list")
}

pub fn constant_enum_constructor_test() {
  "const x = Nil"
  |> to_snapshot
  |> birdie.snap(title: "constant_enum_constructor")
}

pub fn constant_qualified_enum_constructor_test() {
  "const x = wibble.Nil"
  |> to_snapshot
  |> birdie.snap(title: "constant_qualified_enum_constructor")
}

pub fn constant_constructor_test() {
  "const x = Box(1, 2.0)"
  |> to_snapshot
  |> birdie.snap(title: "constant_constructor")
}

pub fn constant_labelled_constructor_test() {
  "const x = Box(1, wobber: 2.0)"
  |> to_snapshot
  |> birdie.snap(title: "constant_labelled_constructor")
}

pub fn constant_bit_string_test() {
  "const x = <<1:2, 2.0>>"
  |> to_snapshot
  |> birdie.snap(title: "constant_bit_string")
}

pub fn function_main_test() {
  "pub fn main() {}"
  |> to_snapshot
  |> birdie.snap(title: "function_main")
}

pub fn private_function_main_test() {
  "fn main() {}"
  |> to_snapshot
  |> birdie.snap(title: "private_function_main")
}

pub fn function_return_annotation_test() {
  "fn main() -> Nil {}"
  |> to_snapshot
  |> birdie.snap(title: "function_return_annotation")
}

pub fn function_parameters_test() {
  "fn main(a, b: #(), c d, e f: G, h _i, j _k: L) {}"
  |> to_snapshot
  |> birdie.snap(title: "function_parameters")
}

pub fn expression_int_test() {
  "pub fn main() { 1 2 3 }"
  |> to_snapshot
  |> birdie.snap(title: "expression_int")
}

pub fn expression_float_test() {
  "pub fn main() { 1.0 2.0 3.0 }"
  |> to_snapshot
  |> birdie.snap(title: "expression_float")
}

pub fn expression_string_test() {
  "pub fn main() { \"10\" \"20\" \"30\" }"
  |> to_snapshot
  |> birdie.snap(title: "expression_string")
}

pub fn expression_variable_test() {
  "pub fn main() { x y z }"
  |> to_snapshot
  |> birdie.snap(title: "expression_variable")
}

pub fn expression_panic_test() {
  "pub fn main() { panic panic }"
  |> to_snapshot
  |> birdie.snap(title: "expression_panic")
}

pub fn expression_negate_int_test() {
  "pub fn main() { -x }"
  |> to_snapshot
  |> birdie.snap(title: "expression_negate_int")
}

pub fn expression_negate_bool_test() {
  "pub fn main() { !x }"
  |> to_snapshot
  |> birdie.snap(title: "expression_negate_bool")
}

pub fn expression_block_test() {
  "pub fn main() { { x y z } }"
  |> to_snapshot
  |> birdie.snap(title: "expression_block")
}

pub fn expression_todo_test() {
  "pub fn main() { todo }"
  |> to_snapshot
  |> birdie.snap(title: "expression_todo")
}

pub fn expression_todo_message_test() {
  "pub fn main() { todo(\"huh\") }"
  |> to_snapshot
  |> birdie.snap(title: "expression_todo_message")
}

pub fn expression_tuple_test() {
  "pub fn main() { #(1, 2, 3) }"
  |> to_snapshot
  |> birdie.snap(title: "expression_tuple")
}

pub fn expression_tuple_trailing_comma_test() {
  "pub fn main() { #(1, 2, 3, ) }"
  |> to_snapshot
  |> birdie.snap(title: "expression_tuple_trailing_comma")
}

pub fn expression_list_test() {
  "pub fn main() { [1, 2, 3] }"
  |> to_snapshot
  |> birdie.snap(title: "expression_list")
}

pub fn expression_list_trailing_comma_test() {
  "pub fn main() { [1, 2, 3, ] }"
  |> to_snapshot
  |> birdie.snap(title: "expression_list_trailing_comma")
}

pub fn expression_list_prefix_test() {
  "pub fn main() { [1, 2, 3, ..x] }"
  |> to_snapshot
  |> birdie.snap(title: "expression_list_prefix")
}

pub fn expression_empty_list_prefix_test() {
  "pub fn main() { [..x] }"
  |> to_snapshot
  |> birdie.snap(title: "expression_empty_list_prefix")
}

pub fn expression_fn_test() {
  "pub fn main() { fn(x) { x } }"
  |> to_snapshot
  |> birdie.snap(title: "expression_fn")
}

pub fn expression_fn_return_test() {
  "pub fn main() { fn(x) -> a { 1 x } }"
  |> to_snapshot
  |> birdie.snap(title: "expression_fn_return")
}

pub fn expression_fn_annotated_parens_test() {
  "pub fn main() { fn(x: a) { x } }"
  |> to_snapshot
  |> birdie.snap(title: "expression_fn_annotated_parens")
}

pub fn expression_fn_discard_test() {
  "pub fn main() { fn(_x: a) { 1 } }"
  |> to_snapshot
  |> birdie.snap(title: "expression_fn_discard")
}

pub fn record_update_test() {
  "pub fn main() { Wibble(..wibble, one: 1, two: 2) }"
  |> to_snapshot
  |> birdie.snap(title: "record_update")
}

pub fn record_update_qualified_test() {
  "pub fn main() { wobble.Wibble(..wibble, one: 1, two: 2) }"
  |> to_snapshot
  |> birdie.snap(title: "record_update_qualified")
}

pub fn record_update_empty_test() {
  "pub fn main() { Wibble(..wibble) }"
  |> to_snapshot
  |> birdie.snap(title: "record_update_empty")
}

pub fn record_update_trailing_comma_test() {
  "pub fn main() { Wibble(..wibble, one: 1, two: 2,) }"
  |> to_snapshot
  |> birdie.snap(title: "record_update_trailing_comma")
}

pub fn record_update_empty_trailing_comma_test() {
  "pub fn main() { Wibble(..wibble,) }"
  |> to_snapshot
  |> birdie.snap(title: "record_update_empty_trailing_comma")
}

pub fn record_partial_destructure_trailing_comma_test() {
  "pub fn main(x) { let Wibble(y, ..,) = x }"
  |> to_snapshot
  |> birdie.snap(title: "record_partial_destructure_trailing_comma")
}

pub fn field_access_test() {
  "pub fn main() { wobble.wibble }"
  |> to_snapshot
  |> birdie.snap(title: "field_access")
}

pub fn field_access_upper_test() {
  "pub fn main() { wobble.Wibble }"
  |> to_snapshot
  |> birdie.snap(title: "field_access_upper")
}

pub fn field_access_recursive_test() {
  "pub fn main() { one.two.three.four }"
  |> to_snapshot
  |> birdie.snap(title: "field_access_recursive")
}

pub fn call_test() {
  "pub fn main() { wobble(1, 2, 3) }"
  |> to_snapshot
  |> birdie.snap(title: "call")
}

pub fn call_labelled_test() {
  "pub fn main() { wobble(1, one: 2, two: 3) }"
  |> to_snapshot
  |> birdie.snap(title: "call_labelled")
}

pub fn call_field_test() {
  "pub fn main() { wibble.wobble(1, 2, 3) }"
  |> to_snapshot
  |> birdie.snap(title: "call_field")
}

pub fn call_recursive_test() {
  "pub fn main() { wobble(1, 2, 3)()() }"
  |> to_snapshot
  |> birdie.snap(title: "call_recursive")
}

pub fn tuple_index_test() {
  "pub fn main() { wobble.12 }"
  |> to_snapshot
  |> birdie.snap(title: "tuple_index")
}

pub fn function_capture_pointless_test() {
  "pub fn main() { wibble(_) }"
  |> to_snapshot
  |> birdie.snap(title: "function_capture_pointless")
}

pub fn function_capture_before_test() {
  "pub fn main() { wibble(1,2,3,_) }"
  |> to_snapshot
  |> birdie.snap(title: "function_capture_before")
}

pub fn function_capture_after_test() {
  "pub fn main() { wibble(_,1,2,3) }"
  |> to_snapshot
  |> birdie.snap(title: "function_capture_after")
}

pub fn function_capture_after_trailing_comma_test() {
  "pub fn main() { wibble(_,1,2,3,) }"
  |> to_snapshot
  |> birdie.snap(title: "function_capture_after_trailing_comma")
}

pub fn function_capture_both_test() {
  "pub fn main() { wibble(1, 2, _, 3) }"
  |> to_snapshot
  |> birdie.snap(title: "function_capture_both")
}

pub fn function_capture_immediate_call_test() {
  "pub fn main() { wibble(_)() }"
  |> to_snapshot
  |> birdie.snap(title: "function_capture_immediate_call")
}

pub fn bit_string_empty_test() {
  "pub fn main() { <<>> }"
  |> to_snapshot
  |> birdie.snap(title: "bit_string_empty")
}

pub fn bit_string_numbers_test() {
  "pub fn main() { <<1, 2, 3>> }"
  |> to_snapshot
  |> birdie.snap(title: "bit_string_numbers")
}

pub fn bit_string_sizes_test() {
  "pub fn main() { <<1, 2:4, 5:8>> }"
  |> to_snapshot
  |> birdie.snap(title: "bit_string_sizes")
}

pub fn bit_string_value_sizes_test() {
  "pub fn main() { <<1, 2:size(5), 5:size(x)>> }"
  |> to_snapshot
  |> birdie.snap(title: "bit_string_value_sizes")
}

pub fn bit_string_units_test() {
  "pub fn main() { <<1, 2:unit(5), 5:unit(3)>> }"
  |> to_snapshot
  |> birdie.snap(title: "bit_string_units")
}

pub fn bit_string_others_test() {
  "pub fn main() { <<1, 2:
bytes-binary-int-float-bits-bit_string-utf8-utf16-utf32-utf8_codepoint-utf16_codepoint-utf32_codepoint-signed-unsigned-big-little-native
>> }"
  |> to_snapshot
  |> birdie.snap(title: "bit_string_others")
}

pub fn assignment_test() {
  "pub fn main() { let x = 1 2 }"
  |> to_snapshot
  |> birdie.snap(title: "assignment")
}

pub fn let_assert_test() {
  "pub fn main() { let assert x = 1 2 }"
  |> to_snapshot
  |> birdie.snap(title: "let_assert")
}

pub fn let_assert_with_message_test() {
  "pub fn main() { let assert x = 1 as \"This shouldn't fail\" }"
  |> to_snapshot
  |> birdie.snap(title: "let_assert_with_message")
}

pub fn let_assert_with_compound_message_test() {
  "pub fn main() { let assert x = 1 as { \"some\" <> \"message\" } }"
  |> to_snapshot
  |> birdie.snap(title: "let_assert_with_compound_message")
}

pub fn let_assert_todo_precedence_test() {
  "pub fn main() { let assert x = todo as \"This is the todo message\" }"
  |> to_snapshot
  |> birdie.snap(title: "let_assert_todo_precedence")
}

pub fn int_pattern_test() {
  "pub fn main() { let 123 = 1 }"
  |> to_snapshot
  |> birdie.snap(title: "int_pattern")
}

pub fn float_pattern_test() {
  "pub fn main() { let 1.3 = 1 }"
  |> to_snapshot
  |> birdie.snap(title: "float_pattern")
}

pub fn string_pattern_test() {
  "pub fn main() { let \"123\" = 1 }"
  |> to_snapshot
  |> birdie.snap(title: "string_pattern")
}

pub fn discard_pattern_test() {
  "pub fn main() { let _nah = 1 }"
  |> to_snapshot
  |> birdie.snap(title: "discard_pattern")
}

pub fn tuple_pattern_test() {
  "pub fn main() { let #(_, _) = 1 }"
  |> to_snapshot
  |> birdie.snap(title: "tuple_pattern")
}

pub fn tuple_pattern_trailing_comma_test() {
  "pub fn main() { let #(_, _,) = 1 }"
  |> to_snapshot
  |> birdie.snap(title: "tuple_pattern_trailing_comma")
}

pub fn bit_string_pattern_test() {
  "pub fn main() { let <<1, 2:4>> = 1 }"
  |> to_snapshot
  |> birdie.snap(title: "bit_string_pattern")
}

pub fn concatenate_discard_pattern_test() {
  "pub fn main() { let \"ok\" <> _nah = 1 }"
  |> to_snapshot
  |> birdie.snap(title: "concatenate_discard_pattern")
}

pub fn concatenate_pattern_test() {
  "pub fn main() { let \"ok\" <> yah = 1 }"
  |> to_snapshot
  |> birdie.snap(title: "concatenate_pattern")
}

pub fn concatenate_pattern_with_prefix_assignment_test() {
  "pub fn main() { let \"1\" as x <> y = \"\" }"
  |> to_snapshot
  |> birdie.snap(title: "concatenate_pattern_with_prefix_assignment")
}

pub fn assignment_pattern_test() {
  "pub fn main() { let x as y = 1 }"
  |> to_snapshot
  |> birdie.snap(title: "assignment_pattern")
}

pub fn list_pattern_test() {
  "pub fn main() { let [1, 2] = x }"
  |> to_snapshot
  |> birdie.snap(title: "list_pattern")
}

pub fn list_rest_pattern_test() {
  "pub fn main() { let [1, 2, ..y] = x }"
  |> to_snapshot
  |> birdie.snap(title: "list_rest_pattern")
}

pub fn empty_list_rest_pattern_test() {
  "pub fn main() { let [..] = x }"
  |> to_snapshot
  |> birdie.snap(title: "empty_list_rest_pattern")
}

pub fn constructor_pattern_test() {
  "pub fn main() { let None = x }"
  |> to_snapshot
  |> birdie.snap(title: "constructor_pattern")
}

pub fn constructor_pattern_args_test() {
  "pub fn main() { let Thing(1, 2) = x }"
  |> to_snapshot
  |> birdie.snap(title: "constructor_pattern_args")
}

pub fn constructor_pattern_spread_test() {
  "pub fn main() { let Thing(1, 2, ..) = x }"
  |> to_snapshot
  |> birdie.snap(title: "constructor_pattern_spread")
}

pub fn constructor_pattern_labels_test() {
  "pub fn main() { let Thing(1, x: 2, ..) = x }"
  |> to_snapshot
  |> birdie.snap(title: "constructor_pattern_labels")
}

pub fn constructor_pattern_qualified_test() {
  "pub fn main() { let wobble.Thing(1, x: 2, ..) = x }"
  |> to_snapshot
  |> birdie.snap(title: "constructor_pattern_qualified")
}

pub fn case_test() {
  "pub fn main() { case x { y -> 1 } }"
  |> to_snapshot
  |> birdie.snap(title: "case")
}

pub fn case_multi_test() {
  "pub fn main() { case x, y, z { a, b, c -> 1 } }"
  |> to_snapshot
  |> birdie.snap(title: "case_multi")
}

pub fn case_alternatives_test() {
  "pub fn main() { case x, y { a, b | c, d -> 1 } }"
  |> to_snapshot
  |> birdie.snap(title: "case_alternatives")
}

pub fn case_clauses_test() {
  "pub fn main() { case x, y { a, b | c, d -> 1 e, f -> 123 } }"
  |> to_snapshot
  |> birdie.snap(title: "case_clauses")
}

pub fn use_test() {
  "pub fn main() { use x <- y }"
  |> to_snapshot
  |> birdie.snap(title: "use")
}

pub fn use_none_test() {
  "
pub fn main() {
  use <- x
}
"
  |> to_snapshot
  |> birdie.snap(title: "use_none")
}

pub fn use_multiple_test() {
  "pub fn main() { use x, y, z <- f }"
  |> to_snapshot
  |> birdie.snap(title: "use_multiple")
}

pub fn addint_test() {
  "pub fn main() { x + y }"
  |> to_snapshot
  |> birdie.snap(title: "addint")
}

pub fn addint2_test() {
  "pub fn main() { x + y + z }"
  |> to_snapshot
  |> birdie.snap(title: "addint2")
}

pub fn and_test() {
  "pub fn main() { x && y }"
  |> to_snapshot
  |> birdie.snap(title: "and")
}

pub fn and2_test() {
  "pub fn main() { x && y && z }"
  |> to_snapshot
  |> birdie.snap(title: "and2")
}

pub fn mult_add_test() {
  "pub fn main() { x * y + z }"
  |> to_snapshot
  |> birdie.snap(title: "mult_add")
}

pub fn add_mult_test() {
  "pub fn main() { x + y * z }"
  |> to_snapshot
  |> birdie.snap(title: "add_mult")
}

pub fn add_mult_block_test() {
  "pub fn main() { { x + y } * z }"
  |> to_snapshot
  |> birdie.snap(title: "add_mult_block")
}

pub fn pipe_test() {
  "pub fn main() { x |> y(1) |> z(2, 3) }"
  |> to_snapshot
  |> birdie.snap(title: "pipe")
}

pub fn guard_test() {
  "pub fn main() { case x { y if z -> 1 } }"
  |> to_snapshot
  |> birdie.snap(title: "guard")
}

pub fn nil_test() {
  "pub fn main() { Nil }"
  |> to_snapshot
  |> birdie.snap(title: "nil")
}

pub fn attributes_test() {
  "
@thingbobby(erlang, \"one\", \"two\")
@thingbobby(javascript, \"three\", \"four\")
pub fn main() { Nil }
"
  |> to_snapshot
  |> birdie.snap(title: "attributes")
}

pub fn attributes_no_paren_test() {
  "
@thingbobby
pub fn main() { Nil }
"
  |> to_snapshot
  |> birdie.snap(title: "attributes_no_paren")
}

pub fn discard_list_rest_test() {
  "pub fn main() { case x { [x, ..] -> Nil } }
"
  |> to_snapshot
  |> birdie.snap(title: "discard_list_rest")
}

pub fn comments_test() {
  "/// Module comment

// Comment

/// Doc comment
pub fn main() { Nil }
"
  |> to_snapshot
  |> birdie.snap(title: "comments")
}

pub fn or_test() {
  "pub fn main() {
  x || y
}"
  |> to_snapshot
  |> birdie.snap(title: "or")
}

pub fn todo_as_test() {
  "pub fn main() {
  todo as \"oh no\"
}"
  |> to_snapshot
  |> birdie.snap(title: "todo_as")
}

pub fn todo_as_block_test() {
  "pub fn main() {
  todo as { \"oh no\" }
}"
  |> to_snapshot
  |> birdie.snap(title: "todo_as_block")
}

pub fn expression_panic_message_test() {
  "pub fn main() { panic(\"huh\") }"
  |> to_snapshot
  |> birdie.snap(title: "expression_panic_message")
}

pub fn panic_as_test() {
  "pub fn main() {
  panic as \"oh no\"
}"
  |> to_snapshot
  |> birdie.snap(title: "panic_as")
}

pub fn panic_as_block_test() {
  "pub fn main() {
  panic as { \"oh no\" }
}"
  |> to_snapshot
  |> birdie.snap(title: "panic_as_block")
}

pub fn label_capture_test() {
  "pub fn main() {
  wibble(x: _)
}"
  |> to_snapshot
  |> birdie.snap(title: "label_capture")
}

pub fn crash_test() {
  "pub fn main() {
  wibble(x: _, )
}"
  |> to_snapshot
  |> birdie.snap(title: "crash")
}

pub fn let_annotation_test() {
  "
pub fn main() {
  let _money: Int = 1
}
"
  |> to_snapshot
  |> birdie.snap(title: "let_annotation")
}

pub fn external_attribute_test() {
  "
@external(erlang, \"gb_trees\", \"empty\")
@external(javascript, \"./gb_trees.js\", \"empty\")
pub fn new_gb_tree() -> GbTree(k, v)
"
  |> to_snapshot
  |> birdie.snap(title: "external_attribute")
}

pub fn constuctorless_type_test() {
  "
pub type X
"
  |> to_snapshot
  |> birdie.snap(title: "constuctorless_type")
}

pub fn import_with_underscore_alias_test() {
  "
import gleam/list.{range} as _alias
"
  |> to_snapshot
  |> birdie.snap(title: "import_with_underscore_alias")
}

// https://github.com/lpil/glance/issues/10
pub fn negative_float_bug_test() {
  "
pub fn main() {
  -1.1
}
"
  |> to_snapshot
  |> birdie.snap(title: "negative_float_bug")
}

// https://github.com/lpil/glance/issues/10
pub fn negative_int_bug_test() {
  "
pub fn main() {
  -11
}
"
  |> to_snapshot
  |> birdie.snap(title: "negative_int_bug")
}

pub fn record_shorthand_test() {
  "
pub fn wibble() {
  Wobble(field:)
}
"
  |> to_snapshot
  |> birdie.snap(title: "record_shorthand")
}

pub fn const_record_shorthand_test() {
  "
const wibble = Wibble(field:)
"
  |> to_snapshot
  |> birdie.snap(title: "const_record_shorthand")
}

pub fn call_shorthand_test() {
  "
pub fn wibble() {
  wobble(field:)
}
"
  |> to_snapshot
  |> birdie.snap(title: "call_shorthand")
}

pub fn function_capture_shorthand_test() {
  "
pub fn wibble() {
  wobble(_, field:)
}
"
  |> to_snapshot
  |> birdie.snap(title: "function_capture_shorthand")
}

pub fn pattern_shorthand_test() {
  "
pub fn wibble() {
  case wobble {
    Wabble(field:) -> field
  }
}
"
  |> to_snapshot
  |> birdie.snap(title: "pattern_shorthand")
}

pub fn record_update_shorthand_test() {
  "
pub fn wibble() {
  Wobble(..wabble, field:)
}
"
  |> to_snapshot
  |> birdie.snap(title: "record_update_shorthand")
}

pub fn multiple_field_call_test() {
  "
pub fn wibble() {
  wobble(unlabelled, shorthand_mid:, non_shorthand: a, shorthand_end:)
}
"
  |> to_snapshot
  |> birdie.snap(title: "multiple_field_call")
}

pub fn multiple_field_record_update_test() {
  "
pub fn wibble() {
  Wobble(..wobble, shorthand_mid:, non_shorthand: a, shorthand_end:)
}
"
  |> to_snapshot
  |> birdie.snap(title: "multiple_field_record_update")
}

pub fn no_shorthand_in_constructors_test() {
  "
pub type Wibble {
  Wobble(int:)
}
"
  |> to_snapshot
  |> birdie.snap(title: "no_shorthand_in_constructors")
}

pub fn empty_block_test() {
  "
pub fn main() {
  {}
}
"
  |> to_snapshot
  |> birdie.snap(title: "empty_block")
}

pub fn deprecated_constructor_test() {
  "
pub type HashAlgorithm {
  @deprecated(\"Please upgrade to another algorithm\")
  Md5
  Sha224
  Sha512
}
"
  |> to_snapshot
  |> birdie.snap(title: "deprecated_constructor")
}

pub fn echo_test() {
  "
pub fn main() {
  echo 10
}
"
  |> to_snapshot
  |> birdie.snap(title: "echo")
}

pub fn echo_pipeline_test() {
  "
pub fn main() {
  10 |> echo
}
"
  |> to_snapshot
  |> birdie.snap(title: "echo_pipeline")
}

pub fn assert_test() {
  "
pub fn main() {
  assert is_even(10)
}
"
  |> to_snapshot
  |> birdie.snap(title: "assert")
}

pub fn assert_with_message_test() {
  "
pub fn main() {
  assert lock(directory) as \"Failed to lock directory\"
}
"
  |> to_snapshot
  |> birdie.snap(title: "assert_with_message")
}

pub fn assert_with_compound_message_test() {
  "
pub fn main() {
  assert lock(directory) as { some_message <> \"!\" }
}
"
  |> to_snapshot
  |> birdie.snap(title: "assert_with_compound_message")
}

pub fn assert_precedence_with_todo_test() {
  "
pub fn main() {
  assert todo as \"todo message\"
}
"
  |> to_snapshot
  |> birdie.snap(title: "assert_precedence_with_todo")
}

pub fn use_with_annotations_test() {
  "
fn x() {
  use x: Int <- todo
  todo
}
"
  |> to_snapshot
  |> birdie.snap(title: "use_with_annotations")
}

pub fn import_location_test() {
  "
import glance
"
  |> check_location(Import)
  |> birdie.snap(title: "import_location")
}

pub fn import_nested_location_test() {
  "
import gleam/io
"
  |> check_location(Import)
  |> birdie.snap(title: "import_nested_location")
}

pub fn import_unqualified_location_test() {
  "
import glance.{Module}
"
  |> check_location(Import)
  |> birdie.snap(title: "import_unqualified_location")
}

pub fn import_alias_location_test() {
  "
import glance.{Module} as parser
"
  |> check_location(Import)
  |> birdie.snap(title: "import_alias_location")
}

pub fn import_alias_discard_location_test() {
  "
import glance as _ignore
"
  |> check_location(Import)
  |> birdie.snap(title: "import_alias_discard_location")
}

pub fn external_type_location_test() {
  "
type Wibble
"
  |> check_location(CustomType)
  |> birdie.snap(title: "external_type_location")
}

pub fn public_external_type_location_test() {
  "
pub type Wibble
"
  |> check_location(CustomType)
  |> birdie.snap(title: "public_external_type_location")
}

pub fn external_type_with_parameters_location_test() {
  "
type Dict(key, value)
"
  |> check_location(CustomType)
  |> birdie.snap(title: "external_type_with_parameters_location")
}

pub fn custom_type_location_test() {
  "
type Result(v, e) {
  Ok(v)
  Error(e)
}
"
  |> check_location(CustomType)
  |> birdie.snap(title: "custom_type_location")
}

pub fn type_alias_location_test() {
  "
type Wibble = Wobble
"
  |> check_location(TypeAlias)
  |> birdie.snap(title: "type_alias_location")
}

pub fn public_type_alias_location_test() {
  "
pub type Wibble = Wobble
"
  |> check_location(TypeAlias)
  |> birdie.snap(title: "public_type_alias_location")
}

pub fn constant_location_test() {
  "
const pi = 3.14
"
  |> check_location(Constant)
  |> birdie.snap(title: "constant_location")
}

pub fn public_constant_location_test() {
  "
pub const pi = 3.14
"
  |> check_location(Constant)
  |> birdie.snap(title: "public_constant_location")
}

pub fn external_function_location_test() {
  "
fn something_external(args)
"
  |> check_location(Function)
  |> birdie.snap(title: "external_function_location")
}

pub fn external_function_with_return_location_test() {
  "
fn something_external(args) -> ReturnType
"
  |> check_location(Function)
  |> birdie.snap(title: "external_function_with_return_location")
}

pub fn function_location_test() {
  "
fn wibble(x) { x }
"
  |> check_location(Function)
  |> birdie.snap(title: "function_location")
}

pub fn public_function_location_test() {
  "
pub fn wibble(x) { x }
"
  |> check_location(Function)
  |> birdie.snap(title: "public_function_location")
}

pub fn let_location_test() {
  "
pub fn main() {
  let a = b
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "let_location")
}

pub fn let_assert_location_test() {
  "
pub fn main() {
  let assert Ok(a) = b
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "let_assert_location")
}

pub fn let_assert_message_location_test() {
  "
pub fn main() {
  let assert Ok(a) = b as \"Value was not Ok\"
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "let_assert_message_location")
}

pub fn assert_location_test() {
  "
pub fn main() {
  assert a == b
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "assert_location")
}

pub fn assert_message_location_test() {
  "
pub fn main() {
  assert a == b as \"Values are not equal\"
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "assert_message_location")
}

pub fn use_location_test() {
  "
pub fn main() {
  use x <- result.try(something)
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "use_location")
}

pub fn int_location_test() {
  "
pub fn main() {
  123_456
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "int_location")
}

pub fn float_location_test() {
  "
pub fn main() {
  3.1415
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "float_location")
}

pub fn string_location_test() {
  "
pub fn main() {
  \"Hello, world!\"
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "string_location")
}

pub fn variable_location_test() {
  "
pub fn main() {
  some_variable
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "variable_location")
}

pub fn negate_int_location_test() {
  "
pub fn main() {
  -123
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "negate_int_location")
}

pub fn negative_float_location_test() {
  "
pub fn main() {
  -123.456
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "negative_float_location")
}

pub fn block_location_test() {
  "
pub fn main() {
  {
    something
    something_else
  }
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "block_location")
}

pub fn panic_location_test() {
  "
pub fn main() {
  panic
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "panic_location")
}

pub fn panic_message_location_test() {
  "
pub fn main() {
  panic as \"Crash\"
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "panic_message_location")
}

pub fn todo_location_test() {
  "
pub fn main() {
  todo
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "todo_location")
}

pub fn todo_message_location_test() {
  "
pub fn main() {
  todo as \"Not implemented\"
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "todo_message_location")
}

pub fn tuple_location_test() {
  "
pub fn main() {
  #(1, 2, True)
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "tuple_location")
}

pub fn list_location_test() {
  "
pub fn main() {
  [a, b, c]
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "list_location")
}

pub fn list_spread_location_test() {
  "
pub fn main() {
  [a, b, c, ..efg]
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "list_spread_location")
}

pub fn field_access_location_test() {
  "
pub fn main() {
  something.wibble
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "field_access_location")
}

pub fn call_location_test() {
  "
pub fn main() {
  io.println(\"Hello!\")
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "call_location")
}

pub fn tuple_index_location_test() {
  "
pub fn main() {
  some_tuple.42
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "tuple_index_location")
}

pub fn case_location_test() {
  "
pub fn main() {
  case abc {
    1 -> 2
    3 -> 4
    _ -> 0
  }
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "case_location")
}

pub fn echo_location_test() {
  "
pub fn main() {
  echo some_variable
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "echo_location")
}

pub fn fn_location_test() {
  "
pub fn main() {
  fn(arg) {
    arg + 1
  }
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "fn_location")
}

pub fn record_update_location_test() {
  "
pub fn main() {
  Wibble(..w, wobble: 4)
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "record_update_location")
}

pub fn function_capture_location_test() {
  "
pub fn main() {
  add(_, 1)
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "function_capture_location")
}

pub fn bit_array_location_test() {
  "
pub fn main() {
  <<1, 2, 3, 4>>
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "bit_array_location")
}

pub fn binary_operator_location_test() {
  "
pub fn main() {
  1 + 2
}
"
  |> check_location(Expression)
  |> birdie.snap(title: "binary_operator_location")
}

pub fn int_pattern_location_test() {
  "
pub fn main() {
  let 0xABCD = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "int_pattern_location")
}

pub fn float_pattern_location_test() {
  "
pub fn main() {
  let 1.4e19 = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "float_pattern_location")
}

pub fn string_pattern_location_test() {
  "
pub fn main() {
  let \"Some string\" = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "string_pattern_location")
}

pub fn discard_pattern_location_test() {
  "
pub fn main() {
  let _ignored = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "discard_pattern_location")
}

pub fn variable_pattern_location_test() {
  "
pub fn main() {
  let something = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "variable_pattern_location")
}

pub fn tuple_pattern_location_test() {
  "
pub fn main() {
  let #(a, b, 4) = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "tuple_pattern_location")
}

pub fn list_pattern_location_test() {
  "
pub fn main() {
  let [1, 2, x] = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "list_pattern_location")
}

pub fn list_pattern_spread_location_test() {
  "
pub fn main() {
  let [1, 2, ..rest] = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "list_pattern_spread_location")
}

pub fn assignment_pattern_location_test() {
  "
pub fn main() {
  let 1 as x = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "assignment_pattern_location")
}

pub fn concatenate_pattern_location_test() {
  "
pub fn main() {
  let \"prefix\" <> suffix = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "concatenate_pattern_location")
}

pub fn concatenate_pattern_discard_location_test() {
  "
pub fn main() {
  let \"prefix\" <> _suffix = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "concatenate_pattern_discard_location")
}

pub fn concatenate_pattern_assignment_location_test() {
  "
pub fn main() {
  let \"prefix\" as prefix <> suffix = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "concatenate_pattern_assignment_location")
}

pub fn bit_array_pattern_location_test() {
  "
pub fn main() {
  let <<1, 2, 3, rest:bits>> = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "bit_array_pattern_location")
}

pub fn variant_pattern_location_test() {
  "
pub fn main() {
  let Something = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "variant_pattern_location")
}

pub fn constructor_pattern_location_test() {
  "
pub fn main() {
  let Wibble(x, wobble:, ..) = todo
}
"
  |> check_location(Pattern)
  |> birdie.snap(title: "constructor_pattern_location")
}
