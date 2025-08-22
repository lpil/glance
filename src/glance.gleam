import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import gleam/string
import glexer.{type Position, Position as P}
import glexer/token.{type Token} as t

type Tokens =
  List(#(Token, Position))

pub type Definition(definition) {
  Definition(attributes: List(Attribute), definition: definition)
}

pub type Attribute {
  Attribute(name: String, arguments: List(Expression))
}

pub type Module {
  Module(
    imports: List(Definition(Import)),
    custom_types: List(Definition(CustomType)),
    type_aliases: List(Definition(TypeAlias)),
    constants: List(Definition(Constant)),
    functions: List(Definition(Function)),
  )
}

pub type Function {
  Function(
    location: Span,
    name: String,
    publicity: Publicity,
    parameters: List(FunctionParameter),
    return: Option(Type),
    body: List(Statement),
  )
}

pub type Span {
  /// A span within a file, indicated by byte offsets.
  Span(start: Int, end: Int)
}

pub type Statement {
  Use(location: Span, patterns: List(UsePattern), function: Expression)
  Assignment(
    location: Span,
    kind: AssignmentKind,
    pattern: Pattern,
    annotation: Option(Type),
    value: Expression,
  )
  Assert(location: Span, expression: Expression, message: Option(Expression))
  Expression(Expression)
}

pub type AssignmentKind {
  Let
  LetAssert(message: Option(Expression))
}

pub type UsePattern {
  UsePattern(pattern: Pattern, annotation: Option(Type))
}

pub type Pattern {
  PatternInt(location: Span, value: String)
  PatternFloat(location: Span, value: String)
  PatternString(location: Span, value: String)
  PatternDiscard(location: Span, name: String)
  PatternVariable(location: Span, name: String)
  PatternTuple(location: Span, elements: List(Pattern))
  PatternList(location: Span, elements: List(Pattern), tail: Option(Pattern))
  PatternAssignment(location: Span, attern: Pattern, name: String)
  PatternConcatenate(
    location: Span,
    prefix: String,
    prefix_name: Option(AssignmentName),
    rest_name: AssignmentName,
  )
  PatternBitString(
    location: Span,
    segments: List(#(Pattern, List(BitStringSegmentOption(Pattern)))),
  )
  PatternVariant(
    location: Span,
    module: Option(String),
    constructor: String,
    arguments: List(Field(Pattern)),
    with_spread: Bool,
  )
}

pub type Expression {
  Int(location: Span, value: String)
  Float(location: Span, value: String)
  String(location: Span, value: String)
  Variable(location: Span, name: String)
  NegateInt(location: Span, value: Expression)
  NegateBool(location: Span, value: Expression)
  Block(location: Span, statements: List(Statement))
  Panic(location: Span, message: Option(Expression))
  Todo(location: Span, message: Option(Expression))
  Tuple(location: Span, elements: List(Expression))
  List(location: Span, elements: List(Expression), rest: Option(Expression))
  Fn(
    location: Span,
    arguments: List(FnParameter),
    return_annotation: Option(Type),
    body: List(Statement),
  )
  RecordUpdate(
    location: Span,
    module: Option(String),
    constructor: String,
    record: Expression,
    fields: List(RecordUpdateField(Expression)),
  )
  FieldAccess(location: Span, container: Expression, label: String)
  Call(location: Span, function: Expression, arguments: List(Field(Expression)))
  TupleIndex(location: Span, tuple: Expression, index: Int)
  FnCapture(
    location: Span,
    label: Option(String),
    function: Expression,
    arguments_before: List(Field(Expression)),
    arguments_after: List(Field(Expression)),
  )
  BitString(
    location: Span,
    segments: List(#(Expression, List(BitStringSegmentOption(Expression)))),
  )

  Case(location: Span, subjects: List(Expression), clauses: List(Clause))
  BinaryOperator(
    location: Span,
    name: BinaryOperator,
    left: Expression,
    right: Expression,
  )
  Echo(location: Span, expression: Option(Expression))
}

pub type Clause {
  Clause(
    patterns: List(List(Pattern)),
    guard: Option(Expression),
    body: Expression,
  )
}

pub type BitStringSegmentOption(t) {
  BytesOption
  IntOption
  FloatOption
  BitsOption
  Utf8Option
  Utf16Option
  Utf32Option
  Utf8CodepointOption
  Utf16CodepointOption
  Utf32CodepointOption
  SignedOption
  UnsignedOption
  BigOption
  LittleOption
  NativeOption
  SizeValueOption(t)
  SizeOption(Int)
  UnitOption(Int)
}

pub type BinaryOperator {
  // Boolean logic
  And
  Or

  // Equality
  Eq
  NotEq

  // Order comparison
  LtInt
  LtEqInt
  LtFloat
  LtEqFloat
  GtEqInt
  GtInt
  GtEqFloat
  GtFloat

  // Functions
  Pipe

  // Maths
  AddInt
  AddFloat
  SubInt
  SubFloat
  MultInt
  MultFloat
  DivInt
  DivFloat
  RemainderInt

  // Strings
  Concatenate
}

pub fn precedence(operator: BinaryOperator) -> Int {
  // Ensure that this matches the other precedence function for guards
  case operator {
    Or -> 1
    And -> 2
    Eq | NotEq -> 3
    LtInt
    | LtEqInt
    | LtFloat
    | LtEqFloat
    | GtEqInt
    | GtInt
    | GtEqFloat
    | GtFloat -> 4
    Concatenate -> 5
    Pipe -> 6
    AddInt | AddFloat | SubInt | SubFloat -> 7
    MultInt | MultFloat | DivInt | DivFloat | RemainderInt -> 8
  }
}

pub type FnParameter {
  FnParameter(name: AssignmentName, type_: Option(Type))
}

pub type FunctionParameter {
  FunctionParameter(
    label: Option(String),
    name: AssignmentName,
    type_: Option(Type),
  )
}

pub type AssignmentName {
  Named(String)
  Discarded(String)
}

pub type Import {
  Import(
    location: Span,
    module: String,
    alias: Option(AssignmentName),
    unqualified_types: List(UnqualifiedImport),
    unqualified_values: List(UnqualifiedImport),
  )
}

pub type Constant {
  Constant(
    location: Span,
    name: String,
    publicity: Publicity,
    annotation: Option(Type),
    value: Expression,
  )
}

pub type UnqualifiedImport {
  UnqualifiedImport(name: String, alias: Option(String))
}

pub type Publicity {
  Public
  Private
}

pub type TypeAlias {
  TypeAlias(
    location: Span,
    name: String,
    publicity: Publicity,
    parameters: List(String),
    aliased: Type,
  )
}

pub type CustomType {
  CustomType(
    location: Span,
    name: String,
    publicity: Publicity,
    opaque_: Bool,
    parameters: List(String),
    variants: List(Variant),
  )
}

pub type Variant {
  Variant(name: String, fields: List(VariantField), attributes: List(Attribute))
}

pub type RecordUpdateField(t) {
  RecordUpdateField(label: String, item: Option(t))
}

pub type VariantField {
  LabelledVariantField(item: Type, label: String)
  UnlabelledVariantField(item: Type)
}

pub type Field(t) {
  LabelledField(label: String, item: t)
  ShorthandField(label: String)
  UnlabelledField(item: t)
}

pub type Type {
  NamedType(
    location: Span,
    name: String,
    module: Option(String),
    parameters: List(Type),
  )
  TupleType(location: Span, elements: List(Type))
  FunctionType(location: Span, parameters: List(Type), return: Type)
  VariableType(location: Span, name: String)
  HoleType(location: Span, name: String)
}

pub type Error {
  UnexpectedEndOfInput
  UnexpectedToken(token: Token, position: Position)
}

pub fn module(src: String) -> Result(Module, Error) {
  glexer.new(src)
  |> glexer.discard_comments
  |> glexer.discard_whitespace
  |> glexer.lex
  |> slurp(Module([], [], [], [], []), [], _)
}

fn push_constant(
  module: Module,
  attributes: List(Attribute),
  constant: Constant,
) -> Module {
  Module(..module, constants: [
    Definition(list.reverse(attributes), constant),
    ..module.constants
  ])
}

fn push_function(
  module: Module,
  attributes: List(Attribute),
  function: Function,
) -> Module {
  Module(..module, functions: [
    Definition(list.reverse(attributes), function),
    ..module.functions
  ])
}

fn push_custom_type(
  module: Module,
  attributes: List(Attribute),
  custom_type: CustomType,
) -> Module {
  let custom_type =
    CustomType(..custom_type, variants: list.reverse(custom_type.variants))
  Module(..module, custom_types: [
    Definition(list.reverse(attributes), custom_type),
    ..module.custom_types
  ])
}

fn push_type_alias(
  module: Module,
  attributes: List(Attribute),
  type_alias: TypeAlias,
) -> Module {
  Module(..module, type_aliases: [
    Definition(list.reverse(attributes), type_alias),
    ..module.type_aliases
  ])
}

fn push_variant(custom_type: CustomType, variant: Variant) -> CustomType {
  CustomType(..custom_type, variants: [variant, ..custom_type.variants])
}

fn expect(
  expected: Token,
  tokens: Tokens,
  next: fn(Position, Tokens) -> Result(t, Error),
) -> Result(t, Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(token, position), ..tokens] if token == expected ->
      next(position, tokens)
    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
  }
}

fn expect_upper_name(
  tokens: Tokens,
  next: fn(String, Int, Tokens) -> Result(t, Error),
) -> Result(t, Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.UpperName(name), P(end)), ..tokens] -> next(name, end, tokens)
    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
  }
}

fn expect_name(
  tokens: Tokens,
  next: fn(String, Tokens) -> Result(t, Error),
) -> Result(t, Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.Name(name), _), ..tokens] -> next(name, tokens)
    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
  }
}

fn until(
  limit: Token,
  acc: acc,
  tokens: Tokens,
  callback: fn(acc, Tokens) -> Result(#(acc, Tokens), Error),
) -> Result(#(acc, Int, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(token, P(i)), ..tokens] if token == limit ->
      Ok(#(acc, string_offset(i, t.to_source(token)), tokens))
    [_, ..] -> {
      case callback(acc, tokens) {
        Ok(#(acc, tokens)) -> until(limit, acc, tokens, callback)
        Error(error) -> Error(error)
      }
    }
  }
}

fn attribute(tokens: Tokens) -> Result(#(Attribute, Tokens), Error) {
  use #(name, tokens) <- result.try(case tokens {
    [#(t.Name(name), _), ..tokens] -> Ok(#(name, tokens))
    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
    [] -> Error(UnexpectedEndOfInput)
  })
  case tokens {
    [#(t.LeftParen, _), ..tokens] -> {
      let result = comma_delimited([], tokens, expression, t.RightParen)
      use #(parameters, _, tokens) <- result.try(result)
      Ok(#(Attribute(name, parameters), tokens))
    }
    _ -> {
      Ok(#(Attribute(name, []), tokens))
    }
  }
}

fn slurp(
  module: Module,
  attributes: List(Attribute),
  tokens: Tokens,
) -> Result(Module, Error) {
  case tokens {
    [#(t.At, _), ..tokens] -> {
      use #(attribute, tokens) <- result.try(attribute(tokens))
      slurp(module, [attribute, ..attributes], tokens)
    }

    [#(t.Import, P(start)), ..tokens] -> {
      let result = import_statement(module, attributes, tokens, start)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }

    [#(t.Pub, P(start)), #(t.Type, _), ..tokens] -> {
      let result =
        type_definition(module, attributes, Public, False, tokens, start)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }

    [#(t.Pub, P(start)), #(t.Opaque, _), #(t.Type, _), ..tokens] -> {
      let result =
        type_definition(module, attributes, Public, True, tokens, start)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }

    [#(t.Type, P(start)), ..tokens] -> {
      let result =
        type_definition(module, attributes, Private, False, tokens, start)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }

    [#(t.Pub, P(start)), #(t.Const, _), ..tokens] -> {
      let result = const_definition(module, attributes, Public, tokens, start)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }

    [#(t.Const, P(start)), ..tokens] -> {
      let result = const_definition(module, attributes, Private, tokens, start)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }

    [#(t.Pub, start), #(t.Fn, _), #(t.Name(name), _), ..tokens] -> {
      let P(start) = start
      let result =
        function_definition(module, attributes, Public, name, start, tokens)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }

    [#(t.Fn, start), #(t.Name(name), _), ..tokens] -> {
      let P(start) = start
      let result =
        function_definition(module, attributes, Private, name, start, tokens)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }

    [] -> Ok(module)
    tokens -> unexpected_error(tokens)
  }
}

fn import_statement(
  module: Module,
  attributes: List(Attribute),
  tokens: Tokens,
  start: Int,
) -> Result(#(Module, Tokens), Error) {
  use #(module_name, end, tokens) <- result.try(module_name("", 0, tokens))
  use UnqualifiedImports(ts, vs, end, tokens) <- result.try(
    optional_unqualified_imports(tokens, end),
  )
  let #(alias, end, tokens) = optional_module_alias(tokens, end)
  let span = Span(start, end)
  let import_ = Import(span, module_name, alias, ts, vs)
  let definition = Definition(list.reverse(attributes), import_)
  let module = Module(..module, imports: [definition, ..module.imports])
  Ok(#(module, tokens))
}

fn module_name(
  name: String,
  end: Int,
  tokens: Tokens,
) -> Result(#(String, Int, Tokens), Error) {
  case tokens {
    [#(t.Slash, _), #(t.Name(s), P(i)), ..tokens] if name != "" -> {
      let end = i + string.byte_size(s)
      module_name(name <> "/" <> s, end, tokens)
    }
    [#(t.Name(s), P(i)), ..tokens] if name == "" -> {
      let end = i + string.byte_size(s)
      module_name(s, end, tokens)
    }

    [] if name == "" -> Error(UnexpectedEndOfInput)
    [#(other, position), ..] if name == "" ->
      Error(UnexpectedToken(other, position))

    _ -> Ok(#(name, end, tokens))
  }
}

fn optional_module_alias(
  tokens: Tokens,
  end: Int,
) -> #(Option(AssignmentName), Int, Tokens) {
  case tokens {
    [#(t.As, _), #(t.Name(alias), P(alias_start)), ..tokens] -> #(
      Some(Named(alias)),
      string_offset(alias_start, alias),
      tokens,
    )
    [#(t.As, _), #(t.DiscardName(alias), P(alias_start)), ..tokens] -> #(
      Some(Discarded(alias)),
      string_offset(alias_start, alias) + 1,
      tokens,
    )
    _ -> #(None, end, tokens)
  }
}

type UnqualifiedImports {
  UnqualifiedImports(
    types: List(UnqualifiedImport),
    values: List(UnqualifiedImport),
    end: Int,
    remaining_tokens: Tokens,
  )
}

fn optional_unqualified_imports(
  tokens: Tokens,
  end: Int,
) -> Result(UnqualifiedImports, Error) {
  case tokens {
    [#(t.Dot, _), #(t.LeftBrace, _), ..tokens] ->
      unqualified_imports([], [], tokens)
    _ -> Ok(UnqualifiedImports([], [], end, tokens))
  }
}

fn unqualified_imports(
  types: List(UnqualifiedImport),
  values: List(UnqualifiedImport),
  tokens: Tokens,
) -> Result(UnqualifiedImports, Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)

    [#(t.RightBrace, P(end)), ..tokens] ->
      Ok(UnqualifiedImports(
        list.reverse(types),
        list.reverse(values),
        end + 1,
        tokens,
      ))

    // Aliased non-final value
    [
      #(t.UpperName(name), _),
      #(t.As, _),
      #(t.UpperName(alias), _),
      #(t.Comma, _),
      ..tokens
    ]
    | [
        #(t.Name(name), _),
        #(t.As, _),
        #(t.Name(alias), _),
        #(t.Comma, _),
        ..tokens
      ] -> {
      let import_ = UnqualifiedImport(name, Some(alias))
      unqualified_imports(types, [import_, ..values], tokens)
    }

    // Aliased final value
    [
      #(t.UpperName(name), _),
      #(t.As, _),
      #(t.UpperName(alias), _),
      #(t.RightBrace, P(end)),
      ..tokens
    ]
    | [
        #(t.Name(name), _),
        #(t.As, _),
        #(t.Name(alias), _),
        #(t.RightBrace, P(end)),
        ..tokens
      ] -> {
      let import_ = UnqualifiedImport(name, Some(alias))
      Ok(UnqualifiedImports(
        list.reverse(types),
        list.reverse([import_, ..values]),
        end + 1,
        tokens,
      ))
    }

    // Unaliased non-final value
    [#(t.UpperName(name), _), #(t.Comma, _), ..tokens]
    | [#(t.Name(name), _), #(t.Comma, _), ..tokens] -> {
      let import_ = UnqualifiedImport(name, None)
      unqualified_imports(types, [import_, ..values], tokens)
    }

    // Unaliased final value
    [#(t.UpperName(name), _), #(t.RightBrace, P(end)), ..tokens]
    | [#(t.Name(name), _), #(t.RightBrace, P(end)), ..tokens] -> {
      let import_ = UnqualifiedImport(name, None)
      Ok(UnqualifiedImports(
        list.reverse(types),
        list.reverse([import_, ..values]),
        end + 1,
        tokens,
      ))
    }

    // Aliased non-final type
    [
      #(t.Type, _),
      #(t.UpperName(name), _),
      #(t.As, _),
      #(t.UpperName(alias), _),
      #(t.Comma, _),
      ..tokens
    ] -> {
      let import_ = UnqualifiedImport(name, Some(alias))
      unqualified_imports([import_, ..types], values, tokens)
    }

    // Aliased final type
    [
      #(t.Type, _),
      #(t.UpperName(name), _),
      #(t.As, _),
      #(t.UpperName(alias), _),
      #(t.RightBrace, P(end)),
      ..tokens
    ] -> {
      let import_ = UnqualifiedImport(name, Some(alias))
      Ok(UnqualifiedImports(
        list.reverse([import_, ..types]),
        list.reverse(values),
        end + 1,
        tokens,
      ))
    }

    // Unaliased non-final type
    [#(t.Type, _), #(t.UpperName(name), _), #(t.Comma, _), ..tokens] -> {
      let import_ = UnqualifiedImport(name, None)
      unqualified_imports([import_, ..types], values, tokens)
    }

    // Unaliased final type
    [#(t.Type, _), #(t.UpperName(name), _), #(t.RightBrace, P(end)), ..tokens] -> {
      let import_ = UnqualifiedImport(name, None)
      Ok(UnqualifiedImports(
        list.reverse([import_, ..types]),
        list.reverse(values),
        end + 1,
        tokens,
      ))
    }
    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
  }
}

fn function_definition(
  module: Module,
  attributes: List(Attribute),
  publicity: Publicity,
  name: String,
  start: Int,
  tokens: Tokens,
) -> Result(#(Module, Tokens), Error) {
  // Parameters
  use _, tokens <- expect(t.LeftParen, tokens)

  let result = comma_delimited([], tokens, function_parameter, t.RightParen)
  use #(parameters, end, tokens) <- result.try(result)

  // Return type
  let result = optional_return_annotation(end, tokens)
  use #(return_type, end, tokens) <- result.try(result)

  // The function body
  use #(body, end, tokens) <- result.try(case tokens {
    [#(t.LeftBrace, _), ..tokens] -> statements([], tokens)
    _ -> Ok(#([], end, tokens))
  })

  let location = Span(start, end)
  let function =
    Function(location, name, publicity, parameters, return_type, body)
  let module = push_function(module, attributes, function)
  Ok(#(module, tokens))
}

fn optional_return_annotation(
  end: Int,
  tokens: Tokens,
) -> Result(#(Option(Type), Int, Tokens), Error) {
  case tokens {
    [#(t.RightArrow, _), ..tokens] -> {
      use #(return_type, tokens) <- result.try(type_(tokens))
      Ok(#(Some(return_type), return_type.location.end, tokens))
    }
    _ -> Ok(#(None, end, tokens))
  }
}

fn statements(
  acc: List(Statement),
  tokens: Tokens,
) -> Result(#(List(Statement), Int, Tokens), Error) {
  case tokens {
    [#(t.RightBrace, P(end)), ..tokens] ->
      Ok(#(list.reverse(acc), end + 1, tokens))
    _ -> {
      use #(statement, tokens) <- result.try(statement(tokens))
      statements([statement, ..acc], tokens)
    }
  }
}

fn statement(tokens: Tokens) -> Result(#(Statement, Tokens), Error) {
  case tokens {
    [#(t.Let, P(start)), #(t.Assert, _), ..tokens] ->
      assignment(LetAssert(None), tokens, start)
    [#(t.Let, P(start)), ..tokens] -> assignment(Let, tokens, start)
    [#(t.Use, P(start)), ..tokens] -> use_(tokens, start)
    [#(t.Assert, P(start)), ..tokens] -> assert_(tokens, start)
    tokens -> {
      use #(expression, tokens) <- result.try(expression(tokens))
      Ok(#(Expression(expression), tokens))
    }
  }
}

fn assert_(tokens: Tokens, start: Int) -> Result(#(Statement, Tokens), Error) {
  use #(subject, tokens) <- result.try(expression(tokens))
  case tokens {
    [#(t.As, _), ..tokens] ->
      case expression(tokens) {
        Error(error) -> Error(error)
        Ok(#(message, tokens)) -> {
          let statement =
            Assert(Span(start, message.location.end), subject, Some(message))
          Ok(#(statement, tokens))
        }
      }
    _ -> {
      let statement = Assert(Span(start, subject.location.end), subject, None)
      Ok(#(statement, tokens))
    }
  }
}

fn use_(tokens: Tokens, start: Int) -> Result(#(Statement, Tokens), Error) {
  use #(patterns, tokens) <- result.try(case tokens {
    [#(t.LeftArrow, _), ..] -> Ok(#([], tokens))
    _ -> delimited([], tokens, use_pattern, t.Comma)
  })

  use _, tokens <- expect(t.LeftArrow, tokens)
  use #(function, tokens) <- result.try(expression(tokens))
  Ok(#(Use(Span(start, function.location.end), patterns, function), tokens))
}

fn use_pattern(
  tokens: List(#(Token, Position)),
) -> Result(#(UsePattern, List(#(Token, Position))), Error) {
  use #(pattern, tokens) <- result.try(pattern(tokens))
  use #(annotation, tokens) <- result.try(optional_type_annotation(tokens))
  Ok(#(UsePattern(pattern:, annotation:), tokens))
}

fn assignment(
  kind: AssignmentKind,
  tokens: Tokens,
  start: Int,
) -> Result(#(Statement, Tokens), Error) {
  use #(pattern, tokens) <- result.try(pattern(tokens))
  use #(annotation, tokens) <- result.try(optional_type_annotation(tokens))
  use _, tokens <- expect(t.Equal, tokens)
  use #(value, tokens) <- result.try(expression(tokens))

  use #(kind, tokens, end) <- result.try(case kind, tokens {
    LetAssert(None), [#(t.As, _), ..tokens] -> {
      use #(message, tokens) <- result.map(expression(tokens))
      #(LetAssert(message: Some(message)), tokens, message.location.end)
    }
    LetAssert(_), _ | Let, _ -> Ok(#(kind, tokens, value.location.end))
  })

  let statement = Assignment(Span(start, end), kind, pattern, annotation, value)
  Ok(#(statement, tokens))
}

fn pattern_constructor(
  module: Option(String),
  constructor: String,
  tokens: Tokens,
  start: Int,
  name_start: Int,
) -> Result(#(Pattern, Tokens), Error) {
  case tokens {
    [#(t.LeftParen, _), ..tokens] -> {
      let result = pattern_constructor_arguments([], tokens)
      use PatternConstructorArguments(patterns, spread, end, tokens) <- result.try(
        result,
      )
      let arguments = list.reverse(patterns)
      let pattern =
        PatternVariant(Span(start, end), module, constructor, arguments, spread)
      Ok(#(pattern, tokens))
    }
    _ -> {
      let span = Span(start, string_offset(name_start, constructor))
      let pattern = PatternVariant(span, module, constructor, [], False)
      Ok(#(pattern, tokens))
    }
  }
}

type PatternConstructorArguments {
  PatternConstructorArguments(
    fields: List(Field(Pattern)),
    spread: Bool,
    end: Int,
    remaining_tokens: Tokens,
  )
}

fn pattern_constructor_arguments(
  arguments: List(Field(Pattern)),
  tokens: Tokens,
) -> Result(PatternConstructorArguments, Error) {
  case tokens {
    [#(t.RightParen, P(end)), ..tokens] ->
      Ok(PatternConstructorArguments(arguments, False, end + 1, tokens))

    [#(t.DotDot, _), #(t.Comma, _), #(t.RightParen, P(end)), ..tokens]
    | [#(t.DotDot, _), #(t.RightParen, P(end)), ..tokens] ->
      Ok(PatternConstructorArguments(arguments, True, end + 1, tokens))

    tokens -> {
      use #(pattern, tokens) <- result.try(field(tokens, pattern))
      let arguments = [pattern, ..arguments]

      case tokens {
        [#(t.RightParen, P(end)), ..tokens] ->
          Ok(PatternConstructorArguments(arguments, False, end + 1, tokens))

        [#(t.Comma, _), #(t.DotDot, _), #(t.RightParen, P(end)), ..tokens] ->
          Ok(PatternConstructorArguments(arguments, True, end + 1, tokens))

        [#(t.Comma, _), ..tokens] ->
          pattern_constructor_arguments(arguments, tokens)

        [#(token, position), ..] -> Error(UnexpectedToken(token, position))
        [] -> Error(UnexpectedEndOfInput)
      }
    }
  }
}

fn pattern(tokens: Tokens) -> Result(#(Pattern, Tokens), Error) {
  use #(pattern, tokens) <- result.try(case tokens {
    [#(t.UpperName(name), P(start)), ..tokens] ->
      pattern_constructor(None, name, tokens, start, start)
    [
      #(t.Name(module), P(start)),
      #(t.Dot, _),
      #(t.UpperName(name), P(name_start)),
      ..tokens
    ] -> pattern_constructor(Some(module), name, tokens, start, name_start)

    [
      #(t.String(v), P(start)),
      #(t.As, _),
      #(t.Name(l), _),
      #(t.LessGreater, _),
      #(t.Name(r), P(name_start)),
      ..tokens
    ] -> {
      let span = Span(start, string_offset(name_start, r))
      let pattern = PatternConcatenate(span, v, Some(Named(l)), Named(r))
      Ok(#(pattern, tokens))
    }
    [
      #(t.String(v), P(start)),
      #(t.As, _),
      #(t.DiscardName(l), _),
      #(t.LessGreater, _),
      #(t.Name(r), P(name_start)),
      ..tokens
    ] -> {
      let span = Span(start, string_offset(name_start, r))
      let pattern = PatternConcatenate(span, v, Some(Discarded(l)), Named(r))
      Ok(#(pattern, tokens))
    }
    [
      #(t.String(v), P(start)),
      #(t.LessGreater, _),
      #(t.Name(n), P(name_start)),
      ..tokens
    ] -> {
      let span = Span(start, string_offset(name_start, n))
      let pattern = PatternConcatenate(span, v, None, Named(n))
      Ok(#(pattern, tokens))
    }
    [
      #(t.String(v), P(start)),
      #(t.LessGreater, _),
      #(t.DiscardName(n), P(name_start)),
      ..tokens
    ] -> {
      let span = Span(start, string_offset(name_start, n) + 1)
      let pattern = PatternConcatenate(span, v, None, Discarded(n))
      Ok(#(pattern, tokens))
    }

    [#(t.Int(value), P(start)), ..tokens] ->
      Ok(#(PatternInt(span_from_string(start, value), value), tokens))
    [#(t.Float(value), P(start)), ..tokens] ->
      Ok(#(PatternFloat(span_from_string(start, value), value), tokens))
    [#(t.String(value), P(start)), ..tokens] ->
      Ok(#(
        PatternString(Span(start, string_offset(start, value) + 2), value),
        tokens,
      ))
    [#(t.DiscardName(name), P(start)), ..tokens] ->
      Ok(#(
        PatternDiscard(Span(start, string_offset(start, name) + 1), name),
        tokens,
      ))
    [#(t.Name(name), P(start)), ..tokens] ->
      Ok(#(PatternVariable(span_from_string(start, name), name), tokens))

    [#(t.LeftSquare, P(start)), ..tokens] -> {
      let result = list(pattern, Some(PatternDiscard(_, "")), [], tokens)
      use ParsedList(elements, rest, tokens, end) <- result.map(result)
      #(PatternList(Span(start, end), elements, rest), tokens)
    }

    [#(t.Hash, P(start)), #(t.LeftParen, _), ..tokens] -> {
      let result = comma_delimited([], tokens, pattern, t.RightParen)
      use #(patterns, end, tokens) <- result.try(result)
      Ok(#(PatternTuple(Span(start, end), patterns), tokens))
    }

    [#(t.LessLess, P(start)), ..tokens] -> {
      let parser = bit_string_segment(pattern, _)
      let result = comma_delimited([], tokens, parser, t.GreaterGreater)
      use #(segments, end, tokens) <- result.try(result)
      Ok(#(PatternBitString(Span(start, end), segments), tokens))
    }

    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
    [] -> Error(UnexpectedEndOfInput)
  })

  case tokens {
    [#(t.As, _), #(t.Name(name), P(name_start)), ..tokens] -> {
      let span = Span(pattern.location.start, string_offset(name_start, name))
      let pattern = PatternAssignment(span, pattern, name)
      Ok(#(pattern, tokens))
    }
    _ -> Ok(#(pattern, tokens))
  }
}

fn expression(tokens: Tokens) -> Result(#(Expression, Tokens), Error) {
  expression_loop(tokens, [], [], RegularExpressionUnit)
}

fn unexpected_error(tokens: Tokens) -> Result(a, Error) {
  case tokens {
    [#(token, position), ..] -> Error(UnexpectedToken(token, position))
    [] -> Error(UnexpectedEndOfInput)
  }
}

fn binary_operator(token: Token) -> Result(BinaryOperator, Nil) {
  case token {
    t.AmperAmper -> Ok(And)
    t.EqualEqual -> Ok(Eq)
    t.Greater -> Ok(GtInt)
    t.GreaterDot -> Ok(GtFloat)
    t.GreaterEqual -> Ok(GtEqInt)
    t.GreaterEqualDot -> Ok(GtEqFloat)
    t.Less -> Ok(LtInt)
    t.LessDot -> Ok(LtFloat)
    t.LessEqual -> Ok(LtEqInt)
    t.LessEqualDot -> Ok(LtEqFloat)
    t.LessGreater -> Ok(Concatenate)
    t.Minus -> Ok(SubInt)
    t.MinusDot -> Ok(SubFloat)
    t.NotEqual -> Ok(NotEq)
    t.Percent -> Ok(RemainderInt)
    t.VBarVBar -> Ok(Or)
    t.Pipe -> Ok(Pipe)
    t.Plus -> Ok(AddInt)
    t.PlusDot -> Ok(AddFloat)
    t.Slash -> Ok(DivInt)
    t.SlashDot -> Ok(DivFloat)
    t.Star -> Ok(MultInt)
    t.StarDot -> Ok(MultFloat)
    _ -> Error(Nil)
  }
}

fn pop_binary_operator(tokens: Tokens) -> Result(#(BinaryOperator, Tokens), Nil) {
  case tokens {
    [#(token, _), ..tokens] -> {
      use op <- result.map(binary_operator(token))
      #(op, tokens)
    }
    [] -> Error(Nil)
  }
}

fn expression_loop(
  tokens: List(#(Token, Position)),
  operators: List(BinaryOperator),
  values: List(Expression),
  context: ParseExpressionUnitContext,
) -> Result(#(Expression, Tokens), Error) {
  use #(expression, tokens) <- result.try(expression_unit(tokens, context))

  case expression {
    None -> unexpected_error(tokens)
    Some(e) -> {
      let values = [e, ..values]
      case pop_binary_operator(tokens) {
        Ok(#(operator, tokens)) -> {
          case handle_operator(Some(operator), operators, values) {
            #(Some(expression), _, _) -> Ok(#(expression, tokens))
            #(None, operators, values) ->
              expression_loop(tokens, operators, values, case operator {
                Pipe -> ExpressionUnitAfterPipe
                _ -> RegularExpressionUnit
              })
          }
        }
        _ ->
          case handle_operator(None, operators, values).0 {
            None -> unexpected_error(tokens)
            Some(expression) -> Ok(#(expression, tokens))
          }
      }
    }
  }
}

/// Simple-Precedence-Parser, handle seeing an operator or end
fn handle_operator(
  next: Option(BinaryOperator),
  operators: List(BinaryOperator),
  values: List(Expression),
) -> #(Option(Expression), List(BinaryOperator), List(Expression)) {
  case next, operators, values {
    Some(operator), [], _ -> #(None, [operator], values)

    Some(next), [previous, ..operators], [a, b, ..rest_values] -> {
      case precedence(previous) >= precedence(next) {
        True -> {
          let span = Span(b.location.start, a.location.end)
          let expression = BinaryOperator(span, previous, b, a)
          let values = [expression, ..rest_values]
          handle_operator(Some(next), operators, values)
        }
        False -> {
          #(None, [next, previous, ..operators], values)
        }
      }
    }

    None, [operator, ..operators], [a, b, ..values] -> {
      let values = [
        BinaryOperator(Span(b.location.start, a.location.end), operator, b, a),
        ..values
      ]
      handle_operator(None, operators, values)
    }

    None, [], [expression] -> #(Some(expression), operators, values)
    None, [], [] -> #(None, operators, values)
    _, _, _ -> panic as "parser bug, expression not full reduced"
  }
}

type ParseExpressionUnitContext {
  RegularExpressionUnit
  ExpressionUnitAfterPipe
}

fn span_from_string(start: Int, string: String) -> Span {
  Span(start:, end: start + string.byte_size(string))
}

fn expression_unit(
  tokens: Tokens,
  context: ParseExpressionUnitContext,
) -> Result(#(Option(Expression), Tokens), Error) {
  use #(parsed, tokens) <- result.try(case tokens {
    [
      #(t.Name(module), P(start)),
      #(t.Dot, _),
      #(t.UpperName(constructor), _),
      #(t.LeftParen, _),
      #(t.DotDot, _),
      ..tokens
    ] -> record_update(Some(module), constructor, tokens, start)
    [
      #(t.UpperName(constructor), P(start)),
      #(t.LeftParen, _),
      #(t.DotDot, _),
      ..tokens
    ] -> record_update(None, constructor, tokens, start)

    [#(t.UpperName(name), P(start)), ..tokens] ->
      Ok(#(Some(Variable(span_from_string(start, name), name)), tokens))

    [#(t.Int(value), P(start)), ..tokens] -> {
      let span = span_from_string(start, value)
      Ok(#(Some(Int(span, value)), tokens))
    }
    [#(t.Float(value), P(start)), ..tokens] -> {
      let span = span_from_string(start, value)
      Ok(#(Some(Float(span, value)), tokens))
    }
    [#(t.String(value), P(start)), ..tokens] -> {
      let span = Span(start, string_offset(start, value) + 2)
      Ok(#(Some(String(span, value)), tokens))
    }
    [#(t.Name(name), P(start)), ..tokens] -> {
      let span = span_from_string(start, name)
      Ok(#(Some(Variable(span, name)), tokens))
    }

    [#(t.Fn, P(start)), ..tokens] -> fn_(tokens, start)
    [#(t.Case, P(start)), ..tokens] -> case_(tokens, start)

    [#(t.Panic, P(start)), ..tokens] ->
      todo_panic(tokens, Panic, start, "panic")
    [#(t.Todo, P(start)), ..tokens] -> todo_panic(tokens, Todo, start, "todo")

    [#(t.LeftSquare, P(start)), ..tokens] -> {
      let result = list(expression, None, [], tokens)
      use ParsedList(elements, rest, tokens, end) <- result.map(result)
      #(Some(List(Span(start, end), elements, rest)), tokens)
    }

    [#(t.Hash, P(start)), #(t.LeftParen, _), ..tokens] -> {
      let result = comma_delimited([], tokens, expression, t.RightParen)
      use #(expressions, end, tokens) <- result.map(result)
      #(Some(Tuple(Span(start, end), expressions)), tokens)
    }

    [#(t.Bang, P(start)), ..tokens] -> {
      let unit = expression_unit(tokens, RegularExpressionUnit)
      use #(maybe_expression, tokens) <- result.try(unit)
      case maybe_expression {
        Some(expression) -> {
          let span = Span(start, expression.location.end)
          Ok(#(Some(NegateBool(span, expression)), tokens))
        }
        None -> unexpected_error(tokens)
      }
    }

    [#(t.Minus, P(start)), ..tokens] -> {
      let unit = expression_unit(tokens, RegularExpressionUnit)
      use #(maybe_expression, tokens) <- result.try(unit)
      case maybe_expression {
        Some(expression) -> {
          let span = Span(start, expression.location.end)
          Ok(#(Some(NegateInt(span, expression)), tokens))
        }
        None -> unexpected_error(tokens)
      }
    }

    [#(t.LeftBrace, P(start)), ..tokens] -> {
      use #(statements, end, tokens) <- result.map(statements([], tokens))
      #(Some(Block(Span(start, end), statements)), tokens)
    }

    [#(t.LessLess, P(start)), ..tokens] -> {
      let parser = bit_string_segment(expression, _)
      let result = comma_delimited([], tokens, parser, t.GreaterGreater)
      use #(segments, end, tokens) <- result.map(result)
      #(Some(BitString(Span(start, end), segments)), tokens)
    }

    [#(t.Echo, P(start)), ..tokens] ->
      case context {
        // `echo` in a pipeline doesn't have an expression after it
        ExpressionUnitAfterPipe -> {
          let span = span_from_string(start, "echo")
          Ok(#(Some(Echo(span, None)), tokens))
        }
        RegularExpressionUnit ->
          result.map(expression(tokens), fn(expression_and_tokens) {
            let #(expression, tokens) = expression_and_tokens
            let span = Span(start, expression.location.end)
            let expression = Echo(span, Some(expression))
            #(Some(expression), tokens)
          })
      }

    _ -> Ok(#(None, tokens))
  })

  case parsed {
    Some(expression) -> {
      case after_expression(expression, tokens) {
        Ok(#(expression, tokens)) -> Ok(#(Some(expression), tokens))
        Error(error) -> Error(error)
      }
    }
    None -> Ok(#(None, tokens))
  }
}

fn todo_panic(
  tokens: Tokens,
  constructor: fn(Span, Option(Expression)) -> Expression,
  start: Int,
  keyword_name: String,
) -> Result(#(Option(Expression), Tokens), Error) {
  case tokens {
    [#(t.As, _), ..tokens] -> {
      use #(reason, tokens) <- result.try(expression(tokens))
      let span = Span(start, reason.location.end)
      let expression = constructor(span, Some(reason))
      Ok(#(Some(expression), tokens))
    }
    _ -> {
      let span = span_from_string(start, keyword_name)
      let expression = constructor(span, None)
      Ok(#(Some(expression), tokens))
    }
  }
}

fn bit_string_segment(
  parser: fn(Tokens) -> Result(#(t, Tokens), Error),
  tokens: Tokens,
) -> Result(#(#(t, List(BitStringSegmentOption(t))), Tokens), Error) {
  use #(value, tokens) <- result.try(parser(tokens))
  let result = optional_bit_string_segment_options(parser, tokens)
  use #(options, tokens) <- result.try(result)
  Ok(#(#(value, options), tokens))
}

fn optional_bit_string_segment_options(
  parser: fn(Tokens) -> Result(#(t, Tokens), Error),
  tokens: Tokens,
) -> Result(#(List(BitStringSegmentOption(t)), Tokens), Error) {
  case tokens {
    [#(t.Colon, _), ..tokens] -> bit_string_segment_options(parser, [], tokens)
    _ -> Ok(#([], tokens))
  }
}

fn bit_string_segment_options(
  parser: fn(Tokens) -> Result(#(t, Tokens), Error),
  options: List(BitStringSegmentOption(t)),
  tokens: Tokens,
) -> Result(#(List(BitStringSegmentOption(t)), Tokens), Error) {
  use #(option, tokens) <- result.try(case tokens {
    // Size as just an int
    [#(t.Int(i), position), ..tokens] -> {
      case int.parse(i) {
        Ok(i) -> Ok(#(SizeOption(i), tokens))
        Error(_) -> Error(UnexpectedToken(t.Int(i), position))
      }
    }

    // Size as an expression
    [#(t.Name("size"), _), #(t.LeftParen, _), ..tokens] -> {
      use #(value, tokens) <- result.try(parser(tokens))
      use _, tokens <- expect(t.RightParen, tokens)
      Ok(#(SizeValueOption(value), tokens))
    }

    // Unit
    [
      #(t.Name("unit"), position),
      #(t.LeftParen, _),
      #(t.Int(i), _),
      #(t.RightParen, _),
      ..tokens
    ] -> {
      case int.parse(i) {
        Ok(i) -> Ok(#(UnitOption(i), tokens))
        Error(_) -> Error(UnexpectedToken(t.Int(i), position))
      }
    }

    [#(t.Name("bytes"), _), ..tokens] -> Ok(#(BytesOption, tokens))
    [#(t.Name("binary"), _), ..tokens] -> Ok(#(BytesOption, tokens))
    [#(t.Name("int"), _), ..tokens] -> Ok(#(IntOption, tokens))
    [#(t.Name("float"), _), ..tokens] -> Ok(#(FloatOption, tokens))
    [#(t.Name("bits"), _), ..tokens] -> Ok(#(BitsOption, tokens))
    [#(t.Name("bit_string"), _), ..tokens] -> Ok(#(BitsOption, tokens))
    [#(t.Name("utf8"), _), ..tokens] -> Ok(#(Utf8Option, tokens))
    [#(t.Name("utf16"), _), ..tokens] -> Ok(#(Utf16Option, tokens))
    [#(t.Name("utf32"), _), ..tokens] -> Ok(#(Utf32Option, tokens))
    [#(t.Name("utf8_codepoint"), _), ..tokens] ->
      Ok(#(Utf8CodepointOption, tokens))
    [#(t.Name("utf16_codepoint"), _), ..tokens] ->
      Ok(#(Utf16CodepointOption, tokens))
    [#(t.Name("utf32_codepoint"), _), ..tokens] ->
      Ok(#(Utf32CodepointOption, tokens))
    [#(t.Name("signed"), _), ..tokens] -> Ok(#(SignedOption, tokens))
    [#(t.Name("unsigned"), _), ..tokens] -> Ok(#(UnsignedOption, tokens))
    [#(t.Name("big"), _), ..tokens] -> Ok(#(BigOption, tokens))
    [#(t.Name("little"), _), ..tokens] -> Ok(#(LittleOption, tokens))
    [#(t.Name("native"), _), ..tokens] -> Ok(#(NativeOption, tokens))

    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
    [] -> Error(UnexpectedEndOfInput)
  })

  let options = [option, ..options]

  case tokens {
    [#(t.Minus, _), ..tokens] ->
      bit_string_segment_options(parser, options, tokens)
    _ -> Ok(#(list.reverse(options), tokens))
  }
}

fn string_offset(start: Int, string: String) -> Int {
  start + string.byte_size(string)
}

fn after_expression(
  parsed: Expression,
  tokens: Tokens,
) -> Result(#(Expression, Tokens), Error) {
  case tokens {
    // Record or module access
    [#(t.Dot, _), #(t.Name(label), P(label_start)), ..tokens]
    | [#(t.Dot, _), #(t.UpperName(label), P(label_start)), ..tokens] -> {
      let span = Span(parsed.location.start, string_offset(label_start, label))
      let expression = FieldAccess(span, parsed, label)
      after_expression(expression, tokens)
    }

    // Tuple index
    [#(t.Dot, _), #(t.Int(value) as token, position), ..tokens] -> {
      case int.parse(value) {
        Ok(i) -> {
          let end = string_offset(position.byte_offset, value)
          let span = Span(parsed.location.start, end)
          let expression = TupleIndex(span, parsed, i)
          after_expression(expression, tokens)
        }
        Error(_) -> Error(UnexpectedToken(token, position))
      }
    }

    // Function call
    [#(t.LeftParen, _), ..tokens] -> {
      call([], parsed, tokens)
    }

    _ -> Ok(#(parsed, tokens))
  }
}

fn call(
  arguments: List(Field(Expression)),
  function: Expression,
  tokens: Tokens,
) -> Result(#(Expression, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)

    [#(t.RightParen, P(end)), ..tokens] -> {
      let span = Span(function.location.start, end + 1)
      let call = Call(span, function, list.reverse(arguments))
      after_expression(call, tokens)
    }

    [
      #(t.Name(label), _),
      #(t.Colon, _),
      #(t.DiscardName(""), _),
      #(t.Comma, _),
      #(t.RightParen, P(end)),
      ..tokens
    ]
    | [
        #(t.Name(label), _),
        #(t.Colon, _),
        #(t.DiscardName(""), _),
        #(t.RightParen, P(end)),
        ..tokens
      ] -> {
      let span = Span(function.location.start, end + 1)
      let capture =
        FnCapture(span, Some(label), function, list.reverse(arguments), [])
      after_expression(capture, tokens)
    }

    [
      #(t.Name(label), _),
      #(t.Colon, _),
      #(t.DiscardName(""), _),
      #(t.Comma, _),
      ..tokens
    ]
    | [#(t.Name(label), _), #(t.Colon, _), #(t.DiscardName(""), _), ..tokens] -> {
      fn_capture(Some(label), function, list.reverse(arguments), [], tokens)
    }

    [#(t.DiscardName(""), _), #(t.Comma, _), #(t.RightParen, P(end)), ..tokens]
    | [#(t.DiscardName(""), _), #(t.RightParen, P(end)), ..tokens] -> {
      let span = Span(function.location.start, end + 1)
      let capture = FnCapture(span, None, function, list.reverse(arguments), [])
      after_expression(capture, tokens)
    }

    [#(t.DiscardName(""), _), #(t.Comma, _), ..tokens]
    | [#(t.DiscardName(""), _), ..tokens] -> {
      fn_capture(None, function, list.reverse(arguments), [], tokens)
    }

    _ -> {
      use #(argument, tokens) <- result.try(field(tokens, expression))
      let arguments = [argument, ..arguments]
      case tokens {
        [#(t.Comma, _), ..tokens] -> {
          call(arguments, function, tokens)
        }
        [#(t.RightParen, P(end)), ..tokens] -> {
          let span = Span(function.location.start, end + 1)
          let call = Call(span, function, list.reverse(arguments))
          after_expression(call, tokens)
        }
        [#(other, position), ..] -> {
          Error(UnexpectedToken(other, position))
        }
        [] -> Error(UnexpectedEndOfInput)
      }
    }
  }
}

fn fn_capture(
  label: Option(String),
  function: Expression,
  before: List(Field(Expression)),
  after: List(Field(Expression)),
  tokens: Tokens,
) -> Result(#(Expression, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)

    [#(t.RightParen, P(end)), ..tokens] -> {
      let span = Span(function.location.start, end + 1)
      let capture =
        FnCapture(span, label, function, before, list.reverse(after))
      after_expression(capture, tokens)
    }

    _ -> {
      use #(argument, tokens) <- result.try(field(tokens, expression))
      let after = [argument, ..after]
      case tokens {
        [#(t.Comma, _), ..tokens] -> {
          fn_capture(label, function, before, after, tokens)
        }
        [#(t.RightParen, P(end)), ..tokens] -> {
          let span = Span(function.location.start, end + 1)
          let call =
            FnCapture(span, label, function, before, list.reverse(after))
          after_expression(call, tokens)
        }
        [#(other, position), ..] -> {
          Error(UnexpectedToken(other, position))
        }
        [] -> Error(UnexpectedEndOfInput)
      }
    }
  }
}

fn record_update(
  module: Option(String),
  constructor: String,
  tokens: Tokens,
  start: Int,
) -> Result(#(Option(Expression), Tokens), Error) {
  use #(record, tokens) <- result.try(expression(tokens))

  case tokens {
    [#(t.RightParen, P(end)), ..tokens] -> {
      let span = Span(start, end + 1)
      let expression = RecordUpdate(span, module, constructor, record, [])
      Ok(#(Some(expression), tokens))
    }
    [#(t.Comma, _), ..tokens] -> {
      let result =
        comma_delimited([], tokens, record_update_field, t.RightParen)
      use #(fields, end, tokens) <- result.try(result)
      let span = Span(start, end)
      let expression = RecordUpdate(span, module, constructor, record, fields)
      Ok(#(Some(expression), tokens))
    }
    _ -> Ok(#(None, tokens))
  }
}

fn record_update_field(
  tokens: Tokens,
) -> Result(#(RecordUpdateField(Expression), Tokens), Error) {
  case tokens {
    [#(t.Name(name), _), #(t.Colon, _), ..tokens] ->
      case tokens {
        // Field is using shorthand (`value:` instead of `value: value`)
        [#(t.Comma, _), ..] | [#(t.RightParen, _), ..] ->
          Ok(#(RecordUpdateField(name, None), tokens))
        // Field is not using shorthand
        _ -> {
          use #(expression, tokens) <- result.try(expression(tokens))
          Ok(#(RecordUpdateField(name, Some(expression)), tokens))
        }
      }
    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
    [] -> Error(UnexpectedEndOfInput)
  }
}

fn case_(
  tokens: Tokens,
  start: Int,
) -> Result(#(Option(Expression), Tokens), Error) {
  use #(subjects, tokens) <- result.try(case_subjects([], tokens))
  use _, tokens <- expect(t.LeftBrace, tokens)
  use #(clauses, tokens, end) <- result.try(case_clauses([], tokens))
  Ok(#(Some(Case(Span(start, end), subjects, clauses)), tokens))
}

fn case_subjects(
  subjects: List(Expression),
  tokens: Tokens,
) -> Result(#(List(Expression), Tokens), Error) {
  use #(subject, tokens) <- result.try(expression(tokens))
  let subjects = [subject, ..subjects]
  case tokens {
    [#(t.Comma, _), ..tokens] -> case_subjects(subjects, tokens)
    _ -> Ok(#(list.reverse(subjects), tokens))
  }
}

fn case_clauses(
  clauses: List(Clause),
  tokens: Tokens,
) -> Result(#(List(Clause), Tokens, Int), Error) {
  use #(clause, tokens) <- result.try(case_clause(tokens))
  let clauses = [clause, ..clauses]
  case tokens {
    [#(t.RightBrace, P(end)), ..tokens] ->
      Ok(#(list.reverse(clauses), tokens, end + 1))
    _ -> case_clauses(clauses, tokens)
  }
}

fn case_clause(tokens: Tokens) -> Result(#(Clause, Tokens), Error) {
  let multipatterns = delimited([], _, pattern, t.Comma)
  let result = delimited([], tokens, multipatterns, t.VBar)
  use #(patterns, tokens) <- result.try(result)
  use #(guard, tokens) <- result.try(optional_clause_guard(tokens))
  use _, tokens <- expect(t.RightArrow, tokens)
  use #(expression, tokens) <- result.map(expression(tokens))
  #(Clause(patterns, guard, expression), tokens)
}

fn optional_clause_guard(
  tokens: Tokens,
) -> Result(#(Option(Expression), Tokens), Error) {
  case tokens {
    [#(t.If, _), ..tokens] -> {
      use #(expression, tokens) <- result.try(expression(tokens))
      Ok(#(Some(expression), tokens))
    }
    _ -> Ok(#(None, tokens))
  }
}

fn delimited(
  acc: List(t),
  tokens: Tokens,
  parser: fn(Tokens) -> Result(#(t, Tokens), Error),
  delimeter: Token,
) -> Result(#(List(t), Tokens), Error) {
  use #(t, tokens) <- result.try(parser(tokens))
  let acc = [t, ..acc]
  case tokens {
    [#(token, _), ..tokens] if token == delimeter ->
      delimited(acc, tokens, parser, delimeter)
    _ -> Ok(#(list.reverse(acc), tokens))
  }
}

fn fn_(
  tokens: Tokens,
  start: Int,
) -> Result(#(Option(Expression), Tokens), Error) {
  // Parameters
  use _, tokens <- expect(t.LeftParen, tokens)
  let result = comma_delimited([], tokens, fn_parameter, t.RightParen)
  use #(parameters, _, tokens) <- result.try(result)

  // Return type
  use #(return, _, tokens) <- result.try(optional_return_annotation(0, tokens))

  // The function body
  use _, tokens <- expect(t.LeftBrace, tokens)
  use #(body, end, tokens) <- result.try(statements([], tokens))

  Ok(#(Some(Fn(Span(start, end), parameters, return, body)), tokens))
}

type ParsedList(ast_node) {
  ParsedList(
    values: List(ast_node),
    spread: Option(ast_node),
    remaining_tokens: Tokens,
    end: Int,
  )
}

fn list(
  parser: fn(Tokens) -> Result(#(t, Tokens), Error),
  discard: Option(fn(Span) -> t),
  acc: List(t),
  tokens: Tokens,
) -> Result(ParsedList(t), Error) {
  case tokens {
    [#(t.RightSquare, P(end)), ..tokens] ->
      Ok(ParsedList(list.reverse(acc), None, tokens, end + 1))

    [#(t.Comma, _), #(t.RightSquare, P(end)), ..tokens] if acc != [] ->
      Ok(ParsedList(list.reverse(acc), None, tokens, end + 1))
    [#(t.DotDot, P(start)), #(t.RightSquare, P(end)) as close, ..tokens] -> {
      case discard {
        None -> unexpected_error([close, ..tokens])
        Some(discard) -> {
          let value = discard(Span(start, start + 1))
          let parsed_list =
            ParsedList(list.reverse(acc), Some(value), tokens, end + 1)
          Ok(parsed_list)
        }
      }
    }

    [#(t.DotDot, _), ..tokens] -> {
      use #(rest, tokens) <- result.try(parser(tokens))
      use P(end), tokens <- expect(t.RightSquare, tokens)
      Ok(ParsedList(list.reverse(acc), Some(rest), tokens, end + 1))
    }
    _ -> {
      use #(element, tokens) <- result.try(parser(tokens))
      let acc = [element, ..acc]
      case tokens {
        [#(t.RightSquare, P(end)), ..tokens]
        | [#(t.Comma, _), #(t.RightSquare, P(end)), ..tokens] ->
          Ok(ParsedList(list.reverse(acc), None, tokens, end + 1))

        [
          #(t.Comma, _),
          #(t.DotDot, P(start)),
          #(t.RightSquare, P(end)) as close,
          ..tokens
        ] -> {
          case discard {
            None -> unexpected_error([close, ..tokens])
            Some(discard) -> {
              let value = discard(Span(start, start + 1))
              let parsed_list =
                ParsedList(list.reverse(acc), Some(value), tokens, end + 1)
              Ok(parsed_list)
            }
          }
        }

        [#(t.Comma, _), #(t.DotDot, _), ..tokens] -> {
          use #(rest, tokens) <- result.try(parser(tokens))
          use P(end), tokens <- expect(t.RightSquare, tokens)
          Ok(ParsedList(list.reverse(acc), Some(rest), tokens, end + 1))
        }

        [#(t.Comma, _), ..tokens] -> list(parser, discard, acc, tokens)

        [#(other, position), ..] -> Error(UnexpectedToken(other, position))
        [] -> Error(UnexpectedEndOfInput)
      }
    }
  }
}

fn fn_parameter(tokens: Tokens) -> Result(#(FnParameter, Tokens), Error) {
  use #(name, tokens) <- result.try(case tokens {
    [#(t.Name(name), _), ..tokens] -> {
      Ok(#(Named(name), tokens))
    }
    [#(t.DiscardName(name), _), ..tokens] -> {
      Ok(#(Discarded(name), tokens))
    }
    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
    [] -> Error(UnexpectedEndOfInput)
  })

  use #(type_, tokens) <- result.try(optional_type_annotation(tokens))
  Ok(#(FnParameter(name, type_), tokens))
}

fn function_parameter(
  tokens: Tokens,
) -> Result(#(FunctionParameter, Tokens), Error) {
  use #(label, parameter, tokens) <- result.try(case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.Name(label), _), #(t.DiscardName(name), _), ..tokens] -> {
      Ok(#(Some(label), Discarded(name), tokens))
    }
    [#(t.DiscardName(name), _), ..tokens] -> {
      Ok(#(None, Discarded(name), tokens))
    }
    [#(t.Name(label), _), #(t.Name(name), _), ..tokens] -> {
      Ok(#(Some(label), Named(name), tokens))
    }
    [#(t.Name(name), _), ..tokens] -> {
      Ok(#(None, Named(name), tokens))
    }
    [#(token, position), ..] -> Error(UnexpectedToken(token, position))
  })

  // Annotation
  use #(type_, tokens) <- result.try(optional_type_annotation(tokens))

  Ok(#(FunctionParameter(label, parameter, type_), tokens))
}

fn const_definition(
  module: Module,
  attributes: List(Attribute),
  publicity: Publicity,
  tokens: Tokens,
  start: Int,
) -> Result(#(Module, Tokens), Error) {
  // name
  use name, tokens <- expect_name(tokens)

  // Optional type annotation
  use #(annotation, tokens) <- result.try(optional_type_annotation(tokens))

  // = Expression
  use _, tokens <- expect(t.Equal, tokens)

  use #(expression, tokens) <- result.try(expression(tokens))

  let constant =
    Constant(
      Span(start, expression.location.end),
      name,
      publicity,
      annotation,
      expression,
    )
  let module = push_constant(module, attributes, constant)
  Ok(#(module, tokens))
}

fn optional_type_annotation(
  tokens: Tokens,
) -> Result(#(Option(Type), Tokens), Error) {
  case tokens {
    [#(t.Colon, _), ..tokens] -> {
      use #(annotation, tokens) <- result.map(type_(tokens))
      #(Some(annotation), tokens)
    }
    _ -> Ok(#(None, tokens))
  }
}

fn comma_delimited(
  items: List(t),
  tokens: Tokens,
  parse parser: fn(Tokens) -> Result(#(t, Tokens), Error),
  until final: t.Token,
) -> Result(#(List(t), Int, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)

    [#(token, P(token_start)), ..tokens] if token == final -> {
      Ok(#(
        list.reverse(items),
        string_offset(token_start, t.to_source(token)),
        tokens,
      ))
    }

    _ -> {
      use #(element, tokens) <- result.try(parser(tokens))
      case tokens {
        [#(t.Comma, _), ..tokens] -> {
          comma_delimited([element, ..items], tokens, parser, final)
        }
        [#(token, P(token_start)), ..tokens] if token == final -> {
          let offset = string_offset(token_start, t.to_source(token))
          Ok(#(list.reverse([element, ..items]), offset, tokens))
        }
        [#(other, position), ..] -> {
          Error(UnexpectedToken(other, position))
        }
        [] -> Error(UnexpectedEndOfInput)
      }
    }
  }
}

fn type_definition(
  module: Module,
  attributes: List(Attribute),
  publicity: Publicity,
  opaque_: Bool,
  tokens: Tokens,
  start: Int,
) -> Result(#(Module, Tokens), Error) {
  // Name(a, b, c)
  use name_value, name_start, tokens <- expect_upper_name(tokens)
  use #(parameters, end, tokens) <- result.try(case tokens {
    [#(t.LeftParen, _), ..tokens] ->
      comma_delimited([], tokens, name, until: t.RightParen)
    _ -> Ok(#([], string_offset(name_start, name_value), tokens))
  })

  case tokens {
    [#(t.Equal, _), ..tokens] -> {
      type_alias(
        module,
        attributes,
        name_value,
        parameters,
        publicity,
        start,
        tokens,
      )
    }
    [#(t.LeftBrace, _), ..tokens] -> {
      module
      |> custom_type(
        attributes,
        name_value,
        parameters,
        publicity,
        opaque_,
        tokens,
        start,
      )
    }
    _ -> {
      let span = Span(start, end)
      let ct = CustomType(span, name_value, publicity, opaque_, parameters, [])
      let module = push_custom_type(module, attributes, ct)
      Ok(#(module, tokens))
    }
  }
}

fn type_alias(
  module: Module,
  attributes: List(Attribute),
  name: String,
  parameters: List(String),
  publicity: Publicity,
  start: Int,
  tokens: Tokens,
) -> Result(#(Module, Tokens), Error) {
  use #(type_, tokens) <- result.try(type_(tokens))
  let span = Span(start, type_.location.end)
  let alias = TypeAlias(span, name, publicity, parameters, type_)
  let module = push_type_alias(module, attributes, alias)
  Ok(#(module, tokens))
}

fn type_(tokens: Tokens) -> Result(#(Type, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.Fn, P(i)), #(t.LeftParen, _), ..tokens] -> {
      fn_type(i, tokens)
    }
    [#(t.Hash, P(i)), #(t.LeftParen, _), ..tokens] -> {
      tuple_type(i, tokens)
    }
    [
      #(t.Name(module), P(start)),
      #(t.Dot, _),
      #(t.UpperName(name), P(end)),
      ..tokens
    ] -> {
      named_type(name, Some(module), tokens, start, end)
    }
    [#(t.UpperName(name), P(start)), ..tokens] -> {
      named_type(name, None, tokens, start, start)
    }
    [#(t.DiscardName(name), P(i)), ..tokens] -> {
      let value = HoleType(Span(i, string_offset(i, name) + 1), name)
      Ok(#(value, tokens))
    }
    [#(t.Name(name), P(i)), ..tokens] -> {
      let value = VariableType(span_from_string(i, name), name)
      Ok(#(value, tokens))
    }
    [#(token, position), ..] -> {
      Error(UnexpectedToken(token, position))
    }
  }
}

fn named_type(
  name: String,
  module: Option(String),
  tokens: Tokens,
  start: Int,
  name_start: Int,
) -> Result(#(Type, Tokens), Error) {
  use #(parameters, end, tokens) <- result.try(case tokens {
    [#(t.LeftParen, _), ..tokens] ->
      comma_delimited([], tokens, type_, until: t.RightParen)

    _ -> {
      let end = name_start + string.byte_size(name)
      Ok(#([], end, tokens))
    }
  })
  let t = NamedType(Span(start, end), name, module, parameters)
  Ok(#(t, tokens))
}

fn fn_type(start: Int, tokens: Tokens) -> Result(#(Type, Tokens), Error) {
  let result = comma_delimited([], tokens, type_, until: t.RightParen)
  use #(parameters, _, tokens) <- result.try(result)
  use _, tokens <- expect(t.RightArrow, tokens)
  use #(return, tokens) <- result.try(type_(tokens))
  let span = Span(start, return.location.end)
  Ok(#(FunctionType(span, parameters, return), tokens))
}

fn tuple_type(start: Int, tokens: Tokens) -> Result(#(Type, Tokens), Error) {
  let result = comma_delimited([], tokens, type_, until: t.RightParen)
  use #(types, end, tokens) <- result.try(result)
  let span = Span(start, end)
  Ok(#(TupleType(span, types), tokens))
}

fn custom_type(
  module: Module,
  attributes: List(Attribute),
  name: String,
  parameters: List(String),
  publicity: Publicity,
  opaque_: Bool,
  tokens: Tokens,
  start: Int,
) -> Result(#(Module, Tokens), Error) {
  // <variant>.. }
  let ct = CustomType(Span(0, 0), name, publicity, opaque_, parameters, [])
  use #(ct, end, tokens) <- result.try(variants(ct, tokens))
  let ct = CustomType(..ct, location: Span(start, end))

  // Continue to the next statement
  let module = push_custom_type(module, attributes, ct)
  Ok(#(module, tokens))
}

fn name(tokens: Tokens) -> Result(#(String, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.Name(name), _), ..tokens] -> Ok(#(name, tokens))
    [#(token, position), ..] -> Error(UnexpectedToken(token, position))
  }
}

fn variants(
  ct: CustomType,
  tokens: Tokens,
) -> Result(#(CustomType, Int, Tokens), Error) {
  use ct, tokens <- until(t.RightBrace, ct, tokens)
  use #(attributes, tokens) <- result.try(attributes([], tokens))
  use name, _, tokens <- expect_upper_name(tokens)
  use #(fields, _, tokens) <- result.try(case tokens {
    [#(t.LeftParen, _), #(t.RightParen, P(i)), ..tokens] -> Ok(#([], i, tokens))
    [#(t.LeftParen, _), ..tokens] -> {
      comma_delimited([], tokens, variant_field, until: t.RightParen)
    }
    _ -> Ok(#([], 0, tokens))
  })
  let ct = push_variant(ct, Variant(name:, fields:, attributes:))
  Ok(#(ct, tokens))
}

fn attributes(
  accumulated_attributes: List(Attribute),
  tokens: Tokens,
) -> Result(#(List(Attribute), Tokens), Error) {
  case tokens {
    [#(t.At, _), ..tokens] -> {
      case attribute(tokens) {
        Error(error) -> Error(error)
        Ok(#(attribute, tokens)) ->
          attributes([attribute, ..accumulated_attributes], tokens)
      }
    }
    _ -> Ok(#(list.reverse(accumulated_attributes), tokens))
  }
}

fn variant_field(tokens: Tokens) -> Result(#(VariantField, Tokens), Error) {
  case tokens {
    [#(t.Name(name), _), #(t.Colon, _), ..tokens] -> {
      use #(type_, tokens) <- result.try(type_(tokens))
      Ok(#(LabelledVariantField(type_, name), tokens))
    }
    tokens -> {
      use #(type_, tokens) <- result.try(type_(tokens))
      Ok(#(UnlabelledVariantField(type_), tokens))
    }
  }
}

fn field(
  tokens: Tokens,
  of parser: fn(Tokens) -> Result(#(t, Tokens), Error),
) -> Result(#(Field(t), Tokens), Error) {
  case tokens {
    [#(t.Name(name), _), #(t.Colon, _), ..tokens] ->
      case tokens {
        // Field is using shorthand (`value:` instead of `value: value`)
        [#(t.Comma, _), ..] | [#(t.RightParen, _), ..] -> {
          Ok(#(ShorthandField(name), tokens))
        }
        // Field is not using shorthand
        _ -> {
          use #(t, tokens) <- result.try(parser(tokens))
          Ok(#(LabelledField(name, t), tokens))
        }
      }
    _ -> {
      use #(t, tokens) <- result.try(parser(tokens))
      Ok(#(UnlabelledField(t), tokens))
    }
  }
}
