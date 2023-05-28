import gleeunit
import gleeunit/should
import gleam/option.{None, Some}
import glance.{
  Block, Call, Constant, ConstantBitString, ConstantConstructor, ConstantFloat,
  ConstantInt, ConstantList, ConstantString, ConstantTuple, ConstantVariable,
  CustomType, DiscardedParameter, Expression, ExternalFunction, ExternalType,
  Field, FieldAccess, Float, Fn, FnCapture, FnParameter, Function,
  FunctionParameter, FunctionType, Import, Int, List, Module, NamedParameter,
  NamedType, NegateBool, NegateInt, Panic, Private, Public, RecordUpdate, String,
  Todo, Tuple, TupleIndex, TupleType, TypeAlias, UnqualifiedImport, Variable,
  VariableType, Variant,
}

pub fn main() {
  gleeunit.main()
}

pub fn empty_test() {
  ""
  |> glance.module()
  |> should.equal(Ok(Module([], [], [], [], [], [], [])))
}

pub fn public_enum_test() {
  "pub type Cardinal {
    North East South West
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Cardinal",
      publicity: Public,
      parameters: [],
      variants: [
        Variant("North", []),
        Variant("East", []),
        Variant("South", []),
        Variant("West", []),
      ],
    ),
  ])
}

pub fn private_enum_test() {
  "type Cardinal {
    North East South West
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Cardinal",
      publicity: Private,
      parameters: [],
      variants: [
        Variant("North", []),
        Variant("East", []),
        Variant("South", []),
        Variant("West", []),
      ],
    ),
  ])
}

pub fn phantom_test() {
  "pub type Spooky(t) {
    Spooky
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Spooky",
      publicity: Public,
      parameters: ["t"],
      variants: [Variant("Spooky", [])],
    ),
  ])
}

pub fn phantom_multiple_test() {
  "pub type Spooky(t, u) {
    Spooky
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Spooky",
      publicity: Public,
      parameters: ["t", "u"],
      variants: [Variant("Spooky", [])],
    ),
  ])
}

pub fn box_test() {
  "pub type Box(x) {
    Box(x)
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Box",
      publicity: Public,
      parameters: ["x"],
      variants: [Variant("Box", [Field(None, VariableType("x"))])],
    ),
  ])
}

pub fn multiple_fields_test() {
  "pub type Box(x, y, z) {
    Box(x, y, z)
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Box",
      publicity: Public,
      parameters: ["x", "y", "z"],
      variants: [
        Variant(
          "Box",
          [
            Field(None, VariableType("x")),
            Field(None, VariableType("y")),
            Field(None, VariableType("z")),
          ],
        ),
      ],
    ),
  ])
}

pub fn trailing_comma_in_parameters_test() {
  "pub type Box(x, ) {
    Box
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Box",
      publicity: Public,
      parameters: ["x"],
      variants: [Variant("Box", [])],
    ),
  ])
}

pub fn empty_parameter_list_test() {
  "pub type Box() {
    Box
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Box",
      publicity: Public,
      parameters: [],
      variants: [Variant("Box", [])],
    ),
  ])
}

pub fn empty_fields_list_test() {
  "pub type Box {
    Box()
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Box",
      publicity: Public,
      parameters: [],
      variants: [Variant("Box", [])],
    ),
  ])
}

pub fn fields_trailing_comma_test() {
  "pub type Box(a) {
    Box(a,)
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Box",
      publicity: Public,
      parameters: ["a"],
      variants: [Variant("Box", [Field(None, VariableType("a"))])],
    ),
  ])
}

pub fn labelled_fields_test() {
  "pub type Box(a) {
    Box(a: a, b: a)
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Box",
      publicity: Public,
      parameters: ["a"],
      variants: [
        Variant(
          "Box",
          [
            Field(Some("a"), VariableType("a")),
            Field(Some("b"), VariableType("a")),
          ],
        ),
      ],
    ),
  ])
}

pub fn phantom_trailing_comma_test() {
  "pub type Spooky(t, u, ) {
    Spooky
  }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Spooky",
      publicity: Public,
      parameters: ["t", "u"],
      variants: [Variant("Spooky", [])],
    ),
  ])
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
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.custom_types }
  |> should.equal([
    CustomType(
      name: "Spooky",
      publicity: Public,
      parameters: ["t", "u"],
      variants: [Variant("Spooky", [])],
    ),
  ])
}

pub fn alias_variable_test() {
  "pub type X = a"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.type_aliases }
  |> should.equal([
    TypeAlias(
      name: "X",
      publicity: Public,
      parameters: [],
      aliased: VariableType("a"),
    ),
  ])
}

pub fn alias_named_test() {
  "pub type X = Y"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.type_aliases }
  |> should.equal([
    TypeAlias(
      name: "X",
      publicity: Public,
      parameters: [],
      aliased: NamedType("Y", None, []),
    ),
  ])
}

pub fn alias_qualified_named_test() {
  "pub type X = wibble.Y"
  |> glance.module()
  |> should.be_ok
  |> fn(m: Module) { m.type_aliases }
  |> should.equal([
    TypeAlias(
      name: "X",
      publicity: Public,
      parameters: [],
      aliased: NamedType("Y", Some("wibble"), []),
    ),
  ])
}

pub fn alias_tuple_test() {
  "pub type X = #(A, B)"
  |> glance.module()
  |> should.be_ok
  |> fn(m: Module) { m.type_aliases }
  |> should.equal([
    TypeAlias(
      name: "X",
      publicity: Public,
      parameters: [],
      aliased: TupleType([NamedType("A", None, []), NamedType("B", None, [])]),
    ),
  ])
}

pub fn alias_fn_test() {
  "pub type X = fn(A, B) -> C"
  |> glance.module()
  |> should.be_ok
  |> fn(m: Module) { m.type_aliases }
  |> should.equal([
    TypeAlias(
      name: "X",
      publicity: Public,
      parameters: [],
      aliased: FunctionType(
        [NamedType("A", None, []), NamedType("B", None, [])],
        NamedType("C", None, []),
      ),
    ),
  ])
}

pub fn import_test() {
  "import one"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.imports }
  |> should.equal([Import("one", None, [])])
}

pub fn nested_import_test() {
  "import one/two/three"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.imports }
  |> should.equal([Import("one/two/three", None, [])])
}

pub fn aliased_import_test() {
  "import one/two/three as four"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.imports }
  |> should.equal([Import("one/two/three", Some("four"), [])])
}

pub fn empty_unqualified_test() {
  "import one/two/three.{} as four"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.imports }
  |> should.equal([Import("one/two/three", Some("four"), [])])
}

pub fn unqualified_test() {
  "import one/two/three.{One, Two, three, four} as four"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.imports }
  |> should.equal([
    Import(
      "one/two/three",
      Some("four"),
      [
        UnqualifiedImport("One", None),
        UnqualifiedImport("Two", None),
        UnqualifiedImport("three", None),
        UnqualifiedImport("four", None),
      ],
    ),
  ])
}

pub fn unqualified_aliased_test() {
  "import one/two/three.{One as Two, Three, four as five, six} as four"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.imports }
  |> should.equal([
    Import(
      "one/two/three",
      Some("four"),
      [
        UnqualifiedImport("One", Some("Two")),
        UnqualifiedImport("Three", None),
        UnqualifiedImport("four", Some("five")),
        UnqualifiedImport("six", None),
      ],
    ),
  ])
}

pub fn constant_int_test() {
  "const x = 123"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([Constant("x", Private, None, ConstantInt("123"))])
}

pub fn constant_float_test() {
  "const x = 1.1"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([Constant("x", Private, None, ConstantFloat("1.1"))])
}

pub fn constant_string_test() {
  "const x = \"123\""
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([Constant("x", Private, None, ConstantString("123"))])
}

pub fn constant_variable_test() {
  "const x = y"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([Constant("x", Private, None, ConstantVariable("y"))])
}

pub fn constant_pub_int_test() {
  "pub const x = 123"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([Constant("x", Public, None, ConstantInt("123"))])
}

pub fn constant_annotated_int_test() {
  "pub const x: Int = 123"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([
    Constant("x", Public, Some(NamedType("Int", None, [])), ConstantInt("123")),
  ])
}

pub fn constant_tuple_test() {
  "const x = #(1, 2.0, 3)"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([
    Constant(
      "x",
      Private,
      None,
      ConstantTuple([ConstantInt("1"), ConstantFloat("2.0"), ConstantInt("3")]),
    ),
  ])
}

pub fn constant_tuple_trailing_comma_test() {
  "const x = #(1, 2.0, 3,)"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([
    Constant(
      "x",
      Private,
      None,
      ConstantTuple([ConstantInt("1"), ConstantFloat("2.0"), ConstantInt("3")]),
    ),
  ])
}

pub fn constant_empty_tuple_test() {
  "const x = #()"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([Constant("x", Private, None, ConstantTuple([]))])
}

pub fn constant_list_test() {
  "const x = [1, 2.0, 3]"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([
    Constant(
      "x",
      Private,
      None,
      ConstantList([ConstantInt("1"), ConstantFloat("2.0"), ConstantInt("3")]),
    ),
  ])
}

pub fn constant_list_trailing_comma_test() {
  "const x = [1, 2.0, 3,]"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([
    Constant(
      "x",
      Private,
      None,
      ConstantList([ConstantInt("1"), ConstantFloat("2.0"), ConstantInt("3")]),
    ),
  ])
}

pub fn constant_empty_list_test() {
  "const x = []"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([Constant("x", Private, None, ConstantList([]))])
}

pub fn constant_enum_constructor_test() {
  "const x = Nil"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([
    Constant("x", Private, None, ConstantConstructor("Nil", None, [])),
  ])
}

pub fn constant_qualified_enum_constructor_test() {
  "const x = wibble.Nil"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([
    Constant("x", Private, None, ConstantConstructor("Nil", Some("wibble"), [])),
  ])
}

pub fn constant_constructor_test() {
  "const x = Box(1, 2.0)"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([
    Constant(
      "x",
      Private,
      None,
      ConstantConstructor(
        "Box",
        None,
        [Field(None, ConstantInt("1")), Field(None, ConstantFloat("2.0"))],
      ),
    ),
  ])
}

pub fn constant_labelled_constructor_test() {
  "const x = Box(1, wobber: 2.0)"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([
    Constant(
      "x",
      Private,
      None,
      ConstantConstructor(
        "Box",
        None,
        [
          Field(None, ConstantInt("1")),
          Field(Some("wobber"), ConstantFloat("2.0")),
        ],
      ),
    ),
  ])
}

pub fn constant_bit_string_test() {
  "const x = <<1, 2.0>>"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.constants }
  |> should.equal([Constant("x", Private, None, ConstantBitString)])
}

pub fn external_type_test() {
  "external type Wibble"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.external_types }
  |> should.equal([ExternalType("Wibble", Private, [])])
}

pub fn pub_external_type_test() {
  "pub external type Wibble"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.external_types }
  |> should.equal([ExternalType("Wibble", Public, [])])
}

pub fn parameter_external_type_test() {
  "external type Wibble(a, b, c)"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.external_types }
  |> should.equal([ExternalType("Wibble", Private, ["a", "b", "c"])])
}

pub fn trailing_comma_parameter_external_type_test() {
  "external type Wibble(a, b, c, )"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.external_types }
  |> should.equal([ExternalType("Wibble", Private, ["a", "b", "c"])])
}

pub fn external_function_test() {
  "external fn one(Nil, label: other.Two) -> Nil = \"one\" \"two\""
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.external_functions }
  |> should.equal([
    ExternalFunction(
      name: "one",
      publicity: Private,
      parameters: [
        Field(None, NamedType("Nil", None, [])),
        Field(Some("label"), NamedType("Two", Some("other"), [])),
      ],
      return: NamedType("Nil", None, []),
      module: "one",
      function: "two",
    ),
  ])
}

pub fn pub_external_function_test() {
  "pub external fn one(Nil, label: other.Two) -> Nil = \"one\" \"two\""
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.external_functions }
  |> should.equal([
    ExternalFunction(
      name: "one",
      publicity: Public,
      parameters: [
        Field(None, NamedType("Nil", None, [])),
        Field(Some("label"), NamedType("Two", Some("other"), [])),
      ],
      return: NamedType("Nil", None, []),
      module: "one",
      function: "two",
    ),
  ])
}

pub fn function_main_test() {
  "pub fn main() {}"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [],
    ),
  ])
}

pub fn private_function_main_test() {
  "fn main() {}"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Private,
      parameters: [],
      return: None,
      body: [],
    ),
  ])
}

pub fn function_return_annotation_test() {
  "fn main() -> Nil {}"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Private,
      parameters: [],
      return: Some(NamedType("Nil", None, [])),
      body: [],
    ),
  ])
}

pub fn function_parameters_test() {
  "fn main(a, b: #(), c d, e f: G, h _i, j _k: L) {}"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Private,
      parameters: [
        FunctionParameter(None, NamedParameter("a"), None),
        FunctionParameter(None, NamedParameter("b"), Some(TupleType([]))),
        FunctionParameter(Some("c"), NamedParameter("d"), None),
        FunctionParameter(
          Some("e"),
          NamedParameter("f"),
          Some(NamedType("G", None, [])),
        ),
        FunctionParameter(Some("h"), DiscardedParameter("i"), None),
        FunctionParameter(
          Some("j"),
          DiscardedParameter("k"),
          Some(NamedType("L", None, [])),
        ),
      ],
      return: None,
      body: [],
    ),
  ])
}

pub fn expression_int_test() {
  "pub fn main() { 1 2 3 }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(Int("1")), Expression(Int("2")), Expression(Int("3"))],
    ),
  ])
}

pub fn expression_float_test() {
  "pub fn main() { 1.0 2.0 3.0 }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Float("1.0")),
        Expression(Float("2.0")),
        Expression(Float("3.0")),
      ],
    ),
  ])
}

pub fn expression_string_test() {
  "pub fn main() { \"10\" \"20\" \"30\" }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(String("10")),
        Expression(String("20")),
        Expression(String("30")),
      ],
    ),
  ])
}

pub fn expression_variable_test() {
  "pub fn main() { x y z }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Variable("x")),
        Expression(Variable("y")),
        Expression(Variable("z")),
      ],
    ),
  ])
}

pub fn expression_panic_test() {
  "pub fn main() { panic panic }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(Panic), Expression(Panic)],
    ),
  ])
}

pub fn expression_negate_int_test() {
  "pub fn main() { -x }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(NegateInt(Variable("x")))],
    ),
  ])
}

pub fn expression_negate_bool_test() {
  "pub fn main() { !x }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(NegateBool(Variable("x")))],
    ),
  ])
}

pub fn expression_block_test() {
  "pub fn main() { { x y z } }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Block([
          Expression(Variable("x")),
          Expression(Variable("y")),
          Expression(Variable("z")),
        ])),
      ],
    ),
  ])
}

pub fn expression_todo_test() {
  "pub fn main() { todo }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(Todo(None))],
    ),
  ])
}

pub fn expression_todo_message_test() {
  "pub fn main() { todo(\"huh\") }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(Todo(Some("huh")))],
    ),
  ])
}

pub fn expression_tuple_test() {
  "pub fn main() { #(1, 2, 3) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(Tuple([Int("1"), Int("2"), Int("3")]))],
    ),
  ])
}

pub fn expression_tuple_trailing_comma_test() {
  "pub fn main() { #(1, 2, 3, ) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(Tuple([Int("1"), Int("2"), Int("3")]))],
    ),
  ])
}

pub fn expression_list_test() {
  "pub fn main() { [1, 2, 3] }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(List([Int("1"), Int("2"), Int("3")], None))],
    ),
  ])
}

pub fn expression_list_trailing_comma_test() {
  "pub fn main() { [1, 2, 3, ] }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(List([Int("1"), Int("2"), Int("3")], None))],
    ),
  ])
}

pub fn expression_list_prefix_test() {
  "pub fn main() { [1, 2, 3, ..x] }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(List([Int("1"), Int("2"), Int("3")], Some(Variable("x")))),
      ],
    ),
  ])
}

pub fn expression_fn_test() {
  "pub fn main() { fn(x) { x } }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Fn(
          [FnParameter(NamedParameter("x"), None)],
          None,
          [Expression(Variable("x"))],
        )),
      ],
    ),
  ])
}

pub fn expression_fn_return_test() {
  "pub fn main() { fn(x) -> a { 1 x } }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Fn(
          [FnParameter(NamedParameter("x"), None)],
          Some(VariableType("a")),
          [Expression(Int("1")), Expression(Variable("x"))],
        )),
      ],
    ),
  ])
}

pub fn expression_fn_annotated_parens_test() {
  "pub fn main() { fn(x: a) { x } }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Fn(
          [FnParameter(NamedParameter("x"), Some(VariableType("a")))],
          None,
          [Expression(Variable("x"))],
        )),
      ],
    ),
  ])
}

pub fn expression_fn_discard_test() {
  "pub fn main() { fn(_x: a) { 1 } }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Fn(
          [FnParameter(DiscardedParameter("x"), Some(VariableType("a")))],
          None,
          [Expression(Int("1"))],
        )),
      ],
    ),
  ])
}

pub fn record_update_test() {
  "pub fn main() { Wibble(..wibble, one: 1, two: 2) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(RecordUpdate(
          module: None,
          constructor: "Wibble",
          record: Variable("wibble"),
          fields: [#("one", Int("1")), #("two", Int("2"))],
        )),
      ],
    ),
  ])
}

pub fn record_update_qualified_test() {
  "pub fn main() { wobble.Wibble(..wibble, one: 1, two: 2) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(RecordUpdate(
          module: Some("wobble"),
          constructor: "Wibble",
          record: Variable("wibble"),
          fields: [#("one", Int("1")), #("two", Int("2"))],
        )),
      ],
    ),
  ])
}

pub fn record_update_empty_test() {
  "pub fn main() { Wibble(..wibble) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(RecordUpdate(
          module: None,
          constructor: "Wibble",
          record: Variable("wibble"),
          fields: [],
        )),
      ],
    ),
  ])
}

pub fn record_update_trailing_comma_test() {
  "pub fn main() { Wibble(..wibble, one: 1, two: 2,) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(RecordUpdate(
          module: None,
          constructor: "Wibble",
          record: Variable("wibble"),
          fields: [#("one", Int("1")), #("two", Int("2"))],
        )),
      ],
    ),
  ])
}

pub fn record_update_empty_trailing_comma_test() {
  "pub fn main() { Wibble(..wibble,) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(RecordUpdate(
          module: None,
          constructor: "Wibble",
          record: Variable("wibble"),
          fields: [],
        )),
      ],
    ),
  ])
}

pub fn field_access_test() {
  "pub fn main() { wobble.wibble }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(FieldAccess(container: Variable("wobble"), label: "wibble")),
      ],
    ),
  ])
}

pub fn field_access_upper_test() {
  "pub fn main() { wobble.Wibble }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(FieldAccess(container: Variable("wobble"), label: "Wibble")),
      ],
    ),
  ])
}

pub fn field_access_recursive_test() {
  "pub fn main() { one.two.three.four }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(FieldAccess(
          container: FieldAccess(
            container: FieldAccess(container: Variable("one"), label: "two"),
            label: "three",
          ),
          label: "four",
        )),
      ],
    ),
  ])
}

pub fn call_test() {
  "pub fn main() { wobble(1, 2, 3) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Call(
          function: Variable("wobble"),
          arguments: [
            Field(None, Int("1")),
            Field(None, Int("2")),
            Field(None, Int("3")),
          ],
        )),
      ],
    ),
  ])
}

pub fn call_labelled_test() {
  "pub fn main() { wobble(1, one: 2, two: 3) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Call(
          function: Variable("wobble"),
          arguments: [
            Field(None, Int("1")),
            Field(Some("one"), Int("2")),
            Field(Some("two"), Int("3")),
          ],
        )),
      ],
    ),
  ])
}

pub fn call_field_test() {
  "pub fn main() { wibble.wobble(1, 2, 3) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Call(
          function: FieldAccess(container: Variable("wibble"), label: "wobble"),
          arguments: [
            Field(None, Int("1")),
            Field(None, Int("2")),
            Field(None, Int("3")),
          ],
        )),
      ],
    ),
  ])
}

pub fn call_recursive_test() {
  "pub fn main() { wobble(1, 2, 3)()() }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Call(
          function: Call(
            function: Call(
              function: Variable("wobble"),
              arguments: [
                Field(None, Int("1")),
                Field(None, Int("2")),
                Field(None, Int("3")),
              ],
            ),
            arguments: [],
          ),
          arguments: [],
        )),
      ],
    ),
  ])
}

pub fn tuple_index_test() {
  "pub fn main() { wobble.12 }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [Expression(TupleIndex(tuple: Variable("wobble"), index: 12))],
    ),
  ])
}

pub fn function_capture_pointless_test() {
  "pub fn main() { wibble(_) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(FnCapture(
          arguments_before: [],
          arguments_after: [],
          function: Variable("wibble"),
        )),
      ],
    ),
  ])
}

pub fn function_capture_before_test() {
  "pub fn main() { wibble(1,2,3,_) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(FnCapture(
          arguments_before: [
            Field(None, Int("1")),
            Field(None, Int("2")),
            Field(None, Int("3")),
          ],
          arguments_after: [],
          function: Variable("wibble"),
        )),
      ],
    ),
  ])
}

pub fn function_capture_after_test() {
  "pub fn main() { wibble(_,1,2,3) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(FnCapture(
          arguments_before: [],
          arguments_after: [
            Field(None, Int("1")),
            Field(None, Int("2")),
            Field(None, Int("3")),
          ],
          function: Variable("wibble"),
        )),
      ],
    ),
  ])
}

pub fn function_capture_after_trailing_comma_test() {
  "pub fn main() { wibble(_,1,2,3,) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(FnCapture(
          arguments_before: [],
          arguments_after: [
            Field(None, Int("1")),
            Field(None, Int("2")),
            Field(None, Int("3")),
          ],
          function: Variable("wibble"),
        )),
      ],
    ),
  ])
}

pub fn function_capture_both_test() {
  "pub fn main() { wibble(1, 2, _, 3) }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(FnCapture(
          arguments_before: [Field(None, Int("1")), Field(None, Int("2"))],
          arguments_after: [Field(None, Int("3"))],
          function: Variable("wibble"),
        )),
      ],
    ),
  ])
}

pub fn function_capture_immediate_call_test() {
  "pub fn main() { wibble(_)() }"
  |> glance.module()
  |> should.be_ok
  |> fn(x: Module) { x.functions }
  |> should.equal([
    Function(
      name: "main",
      publicity: Public,
      parameters: [],
      return: None,
      body: [
        Expression(Call(
          function: FnCapture(
            arguments_before: [],
            arguments_after: [],
            function: Variable("wibble"),
          ),
          arguments: [],
        )),
      ],
    ),
  ])
}
