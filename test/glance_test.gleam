import gleeunit
import gleeunit/should
import gleam/option.{None, Some}
import glance.{
  Constant, ConstantFloat, ConstantInt, ConstantString, ConstantVariable,
  CustomType, Field, FunctionType, Import, Module, NamedType, Private, Public,
  TupleType, TypeAlias, UnqualifiedImport, VariableType, Variant,
}

pub fn main() {
  gleeunit.main()
}

pub fn empty_test() {
  ""
  |> glance.module()
  |> should.equal(Ok(Module([], [], [], [])))
}

pub fn public_enum_test() {
  "pub type Cardinal {
    North East South West
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
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
    ],
    [],
    [],
  )))
}

pub fn private_enum_test() {
  "type Cardinal {
    North East South West
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
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
    ],
    [],
    [],
  )))
}

pub fn phantom_test() {
  "pub type Spooky(t) {
    Spooky
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      CustomType(
        name: "Spooky",
        publicity: Public,
        parameters: ["t"],
        variants: [Variant("Spooky", [])],
      ),
    ],
    [],
    [],
  )))
}

pub fn phantom_multiple_test() {
  "pub type Spooky(t, u) {
    Spooky
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      CustomType(
        name: "Spooky",
        publicity: Public,
        parameters: ["t", "u"],
        variants: [Variant("Spooky", [])],
      ),
    ],
    [],
    [],
  )))
}

pub fn box_test() {
  "pub type Box(x) {
    Box(x)
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      CustomType(
        name: "Box",
        publicity: Public,
        parameters: ["x"],
        variants: [Variant("Box", [Field(None, VariableType("x"))])],
      ),
    ],
    [],
    [],
  )))
}

pub fn multiple_fields_test() {
  "pub type Box(x, y, z) {
    Box(x, y, z)
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
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
    ],
    [],
    [],
  )))
}

pub fn trailing_comma_in_parameters_test() {
  "pub type Box(x, ) {
    Box
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      CustomType(
        name: "Box",
        publicity: Public,
        parameters: ["x"],
        variants: [Variant("Box", [])],
      ),
    ],
    [],
    [],
  )))
}

pub fn empty_parameter_list_test() {
  "pub type Box() {
    Box
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      CustomType(
        name: "Box",
        publicity: Public,
        parameters: [],
        variants: [Variant("Box", [])],
      ),
    ],
    [],
    [],
  )))
}

pub fn empty_fields_list_test() {
  "pub type Box {
    Box()
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      CustomType(
        name: "Box",
        publicity: Public,
        parameters: [],
        variants: [Variant("Box", [])],
      ),
    ],
    [],
    [],
  )))
}

pub fn fields_trailing_comma_test() {
  "pub type Box(a) {
    Box(a,)
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      CustomType(
        name: "Box",
        publicity: Public,
        parameters: ["a"],
        variants: [Variant("Box", [Field(None, VariableType("a"))])],
      ),
    ],
    [],
    [],
  )))
}

pub fn labelled_fields_test() {
  "pub type Box(a) {
    Box(a: a, b: a)
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
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
    ],
    [],
    [],
  )))
}

pub fn phantom_trailing_comma_test() {
  "pub type Spooky(t, u, ) {
    Spooky
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      CustomType(
        name: "Spooky",
        publicity: Public,
        parameters: ["t", "u"],
        variants: [Variant("Spooky", [])],
      ),
    ],
    [],
    [],
  )))
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
  |> should.equal(Ok(Module(
    [],
    [
      CustomType(
        name: "Spooky",
        publicity: Public,
        parameters: ["t", "u"],
        variants: [Variant("Spooky", [])],
      ),
    ],
    [],
    [],
  )))
}

pub fn alias_variable_test() {
  "pub type X = a"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [],
    [
      TypeAlias(
        name: "X",
        publicity: Public,
        parameters: [],
        aliased: VariableType("a"),
      ),
    ],
    [],
  )))
}

pub fn alias_named_test() {
  "pub type X = Y"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [],
    [
      TypeAlias(
        name: "X",
        publicity: Public,
        parameters: [],
        aliased: NamedType("Y", None, []),
      ),
    ],
    [],
  )))
}

pub fn alias_qualified_named_test() {
  "pub type X = wibble.Y"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [],
    [
      TypeAlias(
        name: "X",
        publicity: Public,
        parameters: [],
        aliased: NamedType("Y", Some("wibble"), []),
      ),
    ],
    [],
  )))
}

pub fn alias_tuple_test() {
  "pub type X = #(A, B)"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [],
    [
      TypeAlias(
        name: "X",
        publicity: Public,
        parameters: [],
        aliased: TupleType([NamedType("A", None, []), NamedType("B", None, [])]),
      ),
    ],
    [],
  )))
}

pub fn alias_fn_test() {
  "pub type X = fn(A, B) -> C"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [],
    [
      TypeAlias(
        name: "X",
        publicity: Public,
        parameters: [],
        aliased: FunctionType(
          [NamedType("A", None, []), NamedType("B", None, [])],
          NamedType("C", None, []),
        ),
      ),
    ],
    [],
  )))
}

pub fn import_test() {
  "import one"
  |> glance.module()
  |> should.equal(Ok(Module([Import("one", None, [])], [], [], [])))
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
