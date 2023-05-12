import gleeunit
import gleeunit/should
import gleam/option.{None, Some}
import glance.{
  CustomType, FunctionType, Module, NamedType, Private, Public, TupleType,
  TypeAlias, VariableType, Variant,
}

pub fn main() {
  gleeunit.main()
}

pub fn empty_test() {
  ""
  |> glance.module()
  |> should.equal(Ok(Module([], [])))
}

pub fn public_enum_test() {
  "pub type Cardinal {
    North East South West
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
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
  )))
}

pub fn private_enum_test() {
  "type Cardinal {
    North East South West
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
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
  )))
}

pub fn phantom_test() {
  "pub type Spooky(t) {
    Spooky
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [
      CustomType(
        name: "Spooky",
        publicity: Public,
        parameters: ["t"],
        variants: [Variant("Spooky", [])],
      ),
    ],
    [],
  )))
}

pub fn phantom_multiple_test() {
  "pub type Spooky(t, u) {
    Spooky
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [
      CustomType(
        name: "Spooky",
        publicity: Public,
        parameters: ["t", "u"],
        variants: [Variant("Spooky", [])],
      ),
    ],
    [],
  )))
}

pub fn phantom_trailing_comma_test() {
  "pub type Spooky(t, u, ) {
    Spooky
  }"
  |> glance.module()
  |> should.equal(Ok(Module(
    [
      CustomType(
        name: "Spooky",
        publicity: Public,
        parameters: ["t", "u"],
        variants: [Variant("Spooky", [])],
      ),
    ],
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
    [
      CustomType(
        name: "Spooky",
        publicity: Public,
        parameters: ["t", "u"],
        variants: [Variant("Spooky", [])],
      ),
    ],
    [],
  )))
}

pub fn alias_variable_test() {
  "pub type X = a"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      TypeAlias(
        name: "X",
        publicity: Public,
        parameters: [],
        aliased: VariableType("a"),
      ),
    ],
  )))
}

pub fn alias_named_test() {
  "pub type X = Y"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      TypeAlias(
        name: "X",
        publicity: Public,
        parameters: [],
        aliased: NamedType("Y", None, []),
      ),
    ],
  )))
}

pub fn alias_qualified_named_test() {
  "pub type X = wibble.Y"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      TypeAlias(
        name: "X",
        publicity: Public,
        parameters: [],
        aliased: NamedType("Y", Some("wibble"), []),
      ),
    ],
  )))
}

pub fn alias_tuple_test() {
  "pub type X = #(A, B)"
  |> glance.module()
  |> should.equal(Ok(Module(
    [],
    [
      TypeAlias(
        name: "X",
        publicity: Public,
        parameters: [],
        aliased: TupleType([NamedType("A", None, []), NamedType("B", None, [])]),
      ),
    ],
  )))
}

pub fn alias_fn_test() {
  "pub type X = fn(A, B) -> C"
  |> glance.module()
  |> should.equal(Ok(Module(
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
  )))
}
