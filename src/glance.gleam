import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result
import glexer.{type Position, Position}
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
    name: String,
    publicity: Publicity,
    parameters: List(FunctionParameter),
    return: Option(Type),
    body: List(Statement),
    location: Span,
  )
}

pub type Span {
  /// A span within a file, indicated by byte offsets.
  Span(start: Int, end: Int)
}

pub type Statement {
  Use(patterns: List(Pattern), function: Expression)
  Assignment(
    kind: AssignmentKind,
    pattern: Pattern,
    annotation: Option(Type),
    value: Expression,
  )
  Expression(Expression)
}

pub type AssignmentKind {
  Let
  Assert
}

pub type Pattern {
  PatternInt(value: String)
  PatternFloat(value: String)
  PatternString(value: String)
  PatternDiscard(name: String)
  PatternVariable(name: String)
  PatternTuple(elems: List(Pattern))
  PatternList(elements: List(Pattern), tail: Option(Pattern))
  PatternAssignment(pattern: Pattern, name: String)
  PatternConcatenate(left: String, right: AssignmentName)
  PatternBitString(
    segments: List(#(Pattern, List(BitStringSegmentOption(Pattern)))),
  )
  PatternConstructor(
    module: Option(String),
    constructor: String,
    arguments: List(Field(Pattern)),
    with_spread: Bool,
  )
}

pub type Expression {
  Int(String)
  Float(String)
  String(String)
  Variable(String)
  NegateInt(Expression)
  NegateBool(Expression)
  Block(List(Statement))
  Panic(Option(String))
  Todo(Option(String))
  Tuple(List(Expression))
  List(elements: List(Expression), rest: Option(Expression))
  Fn(
    arguments: List(FnParameter),
    return_annotation: Option(Type),
    body: List(Statement),
  )
  RecordUpdate(
    module: Option(String),
    constructor: String,
    record: Expression,
    fields: List(#(String, Expression)),
  )
  FieldAccess(container: Expression, label: String)
  Call(function: Expression, arguments: List(Field(Expression)))
  TupleIndex(tuple: Expression, index: Int)
  FnCapture(
    label: Option(String),
    function: Expression,
    arguments_before: List(Field(Expression)),
    arguments_after: List(Field(Expression)),
  )
  BitString(
    segments: List(#(Expression, List(BitStringSegmentOption(Expression)))),
  )

  Case(subjects: List(Expression), clauses: List(Clause))
  BinaryOperator(name: BinaryOperator, left: Expression, right: Expression)
}

pub type Clause {
  Clause(
    patterns: List(List(Pattern)),
    guard: Option(Expression),
    body: Expression,
  )
}

pub type BitStringSegmentOption(t) {
  BinaryOption
  IntOption
  FloatOption
  BitStringOption
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
    module: String,
    alias: Option(String),
    unqualified_types: List(UnqualifiedImport),
    unqualified_values: List(UnqualifiedImport),
  )
}

pub type Constant {
  Constant(
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
    name: String,
    publicity: Publicity,
    parameters: List(String),
    aliased: Type,
  )
}

pub type CustomType {
  CustomType(
    name: String,
    publicity: Publicity,
    opaque_: Bool,
    parameters: List(String),
    variants: List(Variant),
  )
}

pub type Variant {
  Variant(name: String, fields: List(Field(Type)))
}

pub type Field(t) {
  Field(label: Option(String), item: t)
}

pub type Type {
  NamedType(name: String, module: Option(String), parameters: List(Type))
  TupleType(elements: List(Type))
  FunctionType(parameters: List(Type), return: Type)
  VariableType(name: String)
}

pub type Error {
  UnexpectedEndOfInput
  UnexpectedToken(token: Token, position: Position)
}

pub fn module(src: String) -> Result(Module, Error) {
  glexer.new(src)
  |> glexer.lex
  |> list.filter(fn(pair) { !is_whitespace(pair.0) })
  |> slurp(Module([], [], [], [], []), [], _)
}

fn is_whitespace(token: Token) -> Bool {
  case token {
    t.EmptyLine | t.CommentNormal | t.CommentModule | t.CommentDoc(_) -> True
    _ -> False
  }
}

fn push_constant(
  module: Module,
  attributes: List(Attribute),
  constant: Constant,
) -> Module {
  Module(
    ..module,
    constants: [
      Definition(list.reverse(attributes), constant),
      ..module.constants
    ],
  )
}

fn push_function(
  module: Module,
  attributes: List(Attribute),
  function: Function,
) -> Module {
  Module(
    ..module,
    functions: [
      Definition(list.reverse(attributes), function),
      ..module.functions
    ],
  )
}

fn push_custom_type(
  module: Module,
  attributes: List(Attribute),
  custom_type: CustomType,
) -> Module {
  let custom_type =
    CustomType(..custom_type, variants: list.reverse(custom_type.variants))
  Module(
    ..module,
    custom_types: [
      Definition(list.reverse(attributes), custom_type),
      ..module.custom_types
    ],
  )
}

fn push_type_alias(
  module: Module,
  attributes: List(Attribute),
  type_alias: TypeAlias,
) -> Module {
  Module(
    ..module,
    type_aliases: [
      Definition(list.reverse(attributes), type_alias),
      ..module.type_aliases
    ],
  )
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
  next: fn(String, Tokens) -> Result(t, Error),
) -> Result(t, Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.UpperName(name), _), ..tokens] -> next(name, tokens)
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
) -> Result(#(acc, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(token, _), ..tokens] if token == limit -> Ok(#(acc, tokens))
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
    [#(t.External, _), ..tokens] -> Ok(#("external", tokens))
    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
    [] -> Error(UnexpectedEndOfInput)
  })
  use _, tokens <- expect(t.LeftParen, tokens)
  let result = comma_delimited([], tokens, expression, t.RightParen)
  use #(parameters, tokens) <- result.try(result)
  Ok(#(Attribute(name, parameters), tokens))
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

    [#(t.Import, _), ..tokens] -> {
      let result = import_statement(module, attributes, tokens)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }
    [#(t.Pub, _), #(t.Type, _), ..tokens] -> {
      let result = type_definition(module, attributes, Public, False, tokens)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }
    [#(t.Pub, _), #(t.Opaque, _), #(t.Type, _), ..tokens] -> {
      let result = type_definition(module, attributes, Public, True, tokens)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }
    [#(t.Type, _), ..tokens] -> {
      let result = type_definition(module, attributes, Private, False, tokens)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }
    [#(t.Pub, _), #(t.Const, _), ..tokens] -> {
      let result = const_definition(module, attributes, Public, tokens)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }
    [#(t.Const, _), ..tokens] -> {
      let result = const_definition(module, attributes, Private, tokens)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }
    [#(t.Pub, start), #(t.Fn, _), #(t.Name(name), _), ..tokens] -> {
      let Position(start) = start
      let result =
        function_definition(module, attributes, Public, name, start, tokens)
      use #(module, tokens) <- result.try(result)
      slurp(module, [], tokens)
    }
    [#(t.Fn, start), #(t.Name(name), _), ..tokens] -> {
      let Position(start) = start
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
) -> Result(#(Module, Tokens), Error) {
  use #(module_name, tokens) <- result.try(module_name("", tokens))
  use #(ts, vs, tokens) <- result.try(optional_unqualified_imports(tokens))
  let #(alias, tokens) = optional_module_alias(tokens)
  let import_ = Import(module_name, alias, ts, vs)
  let definition = Definition(list.reverse(attributes), import_)
  let module = Module(..module, imports: [definition, ..module.imports])
  Ok(#(module, tokens))
}

fn module_name(name: String, tokens: Tokens) -> Result(#(String, Tokens), Error) {
  case tokens {
    [#(t.Slash, _), #(t.Name(s), _), ..tokens] if name != "" -> {
      module_name(name <> "/" <> s, tokens)
    }
    [#(t.Name(s), _), ..tokens] if name == "" -> {
      module_name(s, tokens)
    }

    [] if name == "" -> Error(UnexpectedEndOfInput)
    [#(other, position), ..] if name == "" ->
      Error(UnexpectedToken(other, position))

    _ -> Ok(#(name, tokens))
  }
}

fn optional_module_alias(tokens: Tokens) -> #(Option(String), Tokens) {
  case tokens {
    [#(t.As, _), #(t.Name(alias), _), ..tokens] -> #(Some(alias), tokens)
    [#(t.As, _), #(t.DiscardName(alias), _), ..tokens] -> #(
      Some("_" <> alias),
      tokens,
    )
    _ -> #(None, tokens)
  }
}

fn optional_unqualified_imports(
  tokens: Tokens,
) -> Result(#(List(UnqualifiedImport), List(UnqualifiedImport), Tokens), Error) {
  case tokens {
    [#(t.Dot, _), #(t.LeftBrace, _), ..tokens] ->
      unqualified_imports([], [], tokens)
    _ -> Ok(#([], [], tokens))
  }
}

fn unqualified_imports(
  types: List(UnqualifiedImport),
  values: List(UnqualifiedImport),
  tokens: Tokens,
) -> Result(#(List(UnqualifiedImport), List(UnqualifiedImport), Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)

    [#(t.RightBrace, _), ..tokens] ->
      Ok(#(list.reverse(types), list.reverse(values), tokens))

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
      #(t.RightBrace, _),
      ..tokens
    ]
    | [
        #(t.Name(name), _),
        #(t.As, _),
        #(t.Name(alias), _),
        #(t.RightBrace, _),
        ..tokens
      ] -> {
      let import_ = UnqualifiedImport(name, Some(alias))
      Ok(#(list.reverse(types), list.reverse([import_, ..values]), tokens))
    }

    // Unaliased non-final value
    [#(t.UpperName(name), _), #(t.Comma, _), ..tokens]
    | [#(t.Name(name), _), #(t.Comma, _), ..tokens] -> {
      let import_ = UnqualifiedImport(name, None)
      unqualified_imports(types, [import_, ..values], tokens)
    }

    // Unaliased final value
    [#(t.UpperName(name), _), #(t.RightBrace, _), ..tokens]
    | [#(t.Name(name), _), #(t.RightBrace, _), ..tokens] -> {
      let import_ = UnqualifiedImport(name, None)
      Ok(#(list.reverse(types), list.reverse([import_, ..values]), tokens))
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
      #(t.RightBrace, _),
      ..tokens
    ] -> {
      let import_ = UnqualifiedImport(name, Some(alias))
      Ok(#(list.reverse([import_, ..types]), list.reverse(values), tokens))
    }

    // Unaliased non-final type
    [#(t.Type, _), #(t.UpperName(name), _), #(t.Comma, _), ..tokens] -> {
      let import_ = UnqualifiedImport(name, None)
      unqualified_imports([import_, ..types], values, tokens)
    }

    // Unaliased final type
    [#(t.Type, _), #(t.UpperName(name), _), #(t.RightBrace, _), ..tokens] -> {
      let import_ = UnqualifiedImport(name, None)
      Ok(#(list.reverse([import_, ..types]), list.reverse(values), tokens))
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
  use Position(end), tokens <- expect(t.LeftParen, tokens)
  let result = comma_delimited([], tokens, function_parameter, t.RightParen)
  use #(parameters, tokens) <- result.try(result)

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
    Function(name, publicity, parameters, return_type, body, location)
  let module = push_function(module, attributes, function)
  Ok(#(module, tokens))
}

fn optional_return_annotation(
  end: Int,
  tokens: Tokens,
) -> Result(#(Option(Type), Int, Tokens), Error) {
  case tokens {
    [#(t.RightArrow, Position(end)), ..tokens] -> {
      use #(return_type, tokens) <- result.try(type_(tokens))
      Ok(#(Some(return_type), end, tokens))
    }
    _ -> Ok(#(None, end, tokens))
  }
}

fn statements(
  acc: List(Statement),
  tokens: Tokens,
) -> Result(#(List(Statement), Int, Tokens), Error) {
  case tokens {
    [#(t.RightBrace, Position(end)), ..tokens] ->
      Ok(#(list.reverse(acc), end + 1, tokens))
    _ -> {
      use #(statement, tokens) <- result.try(statement(tokens))
      statements([statement, ..acc], tokens)
    }
  }
}

fn statement(tokens: Tokens) -> Result(#(Statement, Tokens), Error) {
  case tokens {
    [#(t.Let, _), #(t.Assert, _), ..tokens] -> assignment(Assert, tokens)
    [#(t.Let, _), ..tokens] -> assignment(Let, tokens)
    [#(t.Use, _), ..tokens] -> use_(tokens)
    tokens -> {
      use #(expression, tokens) <- result.try(expression(tokens))
      Ok(#(Expression(expression), tokens))
    }
  }
}

fn use_(tokens: Tokens) -> Result(#(Statement, Tokens), Error) {
  use #(patterns, tokens) <- result.try(case tokens {
    [#(t.LeftArrow, _), ..] -> Ok(#([], tokens))
    _ -> delimited([], tokens, pattern, t.Comma)
  })

  use _, tokens <- expect(t.LeftArrow, tokens)
  use #(function, tokens) <- result.try(expression(tokens))
  Ok(#(Use(patterns, function), tokens))
}

fn assignment(
  kind: AssignmentKind,
  tokens: Tokens,
) -> Result(#(Statement, Tokens), Error) {
  use #(pattern, tokens) <- result.try(pattern(tokens))
  use #(annotation, tokens) <- result.try(optional_type_annotation(tokens))
  use _, tokens <- expect(t.Equal, tokens)
  use #(expression, tokens) <- result.try(expression(tokens))
  let statement = Assignment(kind, pattern, annotation, expression)
  Ok(#(statement, tokens))
}

fn pattern_constructor(
  module: Option(String),
  constructor: String,
  tokens: Tokens,
) -> Result(#(Pattern, Tokens), Error) {
  case tokens {
    [#(t.LeftParen, _), ..tokens] -> {
      let result = pattern_constructor_arguments([], tokens)
      use #(patterns, spread, tokens) <- result.try(result)
      let arguments = list.reverse(patterns)
      let pattern = PatternConstructor(module, constructor, arguments, spread)
      Ok(#(pattern, tokens))
    }
    _ -> {
      let pattern = PatternConstructor(module, constructor, [], False)
      Ok(#(pattern, tokens))
    }
  }
}

fn pattern_constructor_arguments(
  arguments: List(Field(Pattern)),
  tokens: Tokens,
) -> Result(#(List(Field(Pattern)), Bool, Tokens), Error) {
  case tokens {
    [#(t.RightParen, _), ..tokens] -> Ok(#(arguments, False, tokens))

    [#(t.DotDot, _), #(t.Comma, _), #(t.RightParen, _), ..tokens]
    | [#(t.DotDot, _), #(t.RightParen, _), ..tokens] ->
      Ok(#(arguments, True, tokens))

    tokens -> {
      use #(pattern, tokens) <- result.try(field(tokens, pattern))
      let arguments = [pattern, ..arguments]

      case tokens {
        [#(t.RightParen, _), ..tokens] -> Ok(#(arguments, False, tokens))

        [#(t.Comma, _), #(t.DotDot, _), #(t.RightParen, _), ..tokens] ->
          Ok(#(arguments, True, tokens))

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
    [#(t.UpperName(name), _), ..tokens] ->
      pattern_constructor(None, name, tokens)
    [#(t.Name(module), _), #(t.Dot, _), #(t.UpperName(name), _), ..tokens] ->
      pattern_constructor(Some(module), name, tokens)

    [#(t.String(v), _), #(t.LessGreater, _), #(t.Name(n), _), ..tokens] ->
      Ok(#(PatternConcatenate(v, Named(n)), tokens))

    [#(t.String(v), _), #(t.LessGreater, _), #(t.DiscardName(n), _), ..tokens] ->
      Ok(#(PatternConcatenate(v, Discarded(n)), tokens))

    [#(t.Int(value), _), ..tokens] -> Ok(#(PatternInt(value), tokens))
    [#(t.Float(value), _), ..tokens] -> Ok(#(PatternFloat(value), tokens))
    [#(t.String(value), _), ..tokens] -> Ok(#(PatternString(value), tokens))
    [#(t.DiscardName(name), _), ..tokens] -> Ok(#(PatternDiscard(name), tokens))
    [#(t.Name(name), _), ..tokens] -> Ok(#(PatternVariable(name), tokens))

    [#(t.LeftSquare, _), ..tokens] -> {
      let result = list(pattern, Some(PatternDiscard("")), [], tokens)
      use #(elements, rest, tokens) <- result.map(result)
      #(PatternList(elements, rest), tokens)
    }

    [#(t.Hash, _), #(t.LeftParen, _), ..tokens] -> {
      let result = comma_delimited([], tokens, pattern, t.RightParen)
      use #(patterns, tokens) <- result.try(result)
      Ok(#(PatternTuple(patterns), tokens))
    }

    [#(t.LessLess, _), ..tokens] -> {
      let parser = bit_string_segment(pattern, _)
      let result = comma_delimited([], tokens, parser, t.GreaterGreater)
      use #(segments, tokens) <- result.try(result)
      Ok(#(PatternBitString(segments), tokens))
    }

    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
    [] -> Error(UnexpectedEndOfInput)
  })

  case tokens {
    [#(t.As, _), #(t.Name(name), _), ..tokens] -> {
      Ok(#(PatternAssignment(pattern, name), tokens))
    }
    _ -> Ok(#(pattern, tokens))
  }
}

fn expression(tokens: Tokens) -> Result(#(Expression, Tokens), Error) {
  expression_loop(tokens, [], [])
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
) -> Result(#(Expression, Tokens), Error) {
  use #(expression, tokens) <- result.try(expression_unit(tokens))

  case expression {
    None -> unexpected_error(tokens)
    Some(e) -> {
      let values = [e, ..values]
      case pop_binary_operator(tokens) {
        Ok(#(operator, tokens)) -> {
          case handle_operator(Some(operator), operators, values) {
            #(Some(expression), _, _) -> Ok(#(expression, tokens))
            #(None, operators, values) ->
              expression_loop(tokens, operators, values)
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
          let values = [BinaryOperator(previous, b, a), ..rest_values]
          handle_operator(Some(next), operators, values)
        }
        False -> {
          #(None, [next, previous, ..operators], values)
        }
      }
    }

    None, [operator, ..operators], [a, b, ..values] -> {
      let values = [BinaryOperator(operator, b, a), ..values]
      handle_operator(None, operators, values)
    }

    None, [], [expression] -> #(Some(expression), operators, values)
    None, [], [] -> #(None, operators, values)
    _, _, _ -> panic as "parser bug, expression not full reduced"
  }
}

fn expression_unit(
  tokens: Tokens,
) -> Result(#(Option(Expression), Tokens), Error) {
  use #(parsed, tokens) <- result.try(case tokens {
    [
      #(t.Name(module), _),
      #(t.Dot, _),
      #(t.UpperName(constructor), _),
      #(t.LeftParen, _),
      #(t.DotDot, _),
      ..tokens
    ] -> record_update(Some(module), constructor, tokens)
    [
      #(t.UpperName(constructor), _),
      #(t.LeftParen, _),
      #(t.DotDot, _),
      ..tokens
    ] -> record_update(None, constructor, tokens)

    [#(t.UpperName(name), _), ..tokens] -> Ok(#(Some(Variable(name)), tokens))

    [#(t.Panic, _), #(t.As, _), #(t.String(message), _), ..tokens] ->
      Ok(#(Some(Panic(Some(message))), tokens))
    [
      #(t.Panic, _),
      #(t.LeftParen, _),
      #(t.String(value), _),
      #(t.RightParen, _),
      ..tokens
    ] -> Ok(#(Some(Panic(Some(value))), tokens))
    [#(t.Panic, _), ..tokens] -> Ok(#(Some(Panic(None)), tokens))

    [#(t.Int(value), _), ..tokens] -> Ok(#(Some(Int(value)), tokens))
    [#(t.Float(value), _), ..tokens] -> Ok(#(Some(Float(value)), tokens))
    [#(t.String(value), _), ..tokens] -> Ok(#(Some(String(value)), tokens))
    [#(t.Name(name), _), ..tokens] -> Ok(#(Some(Variable(name)), tokens))

    [#(t.Fn, _), ..tokens] -> fn_(tokens)
    [#(t.Case, _), ..tokens] -> case_(tokens)

    [#(t.Todo, _), #(t.As, _), #(t.String(value), _), ..tokens] ->
      Ok(#(Some(Todo(Some(value))), tokens))

    [
      #(t.Todo, _),
      #(t.LeftParen, _),
      #(t.String(value), _),
      #(t.RightParen, _),
      ..tokens
    ] -> Ok(#(Some(Todo(Some(value))), tokens))

    [#(t.Todo, _), ..tokens] -> Ok(#(Some(Todo(None)), tokens))

    [#(t.LeftSquare, _), ..tokens] -> {
      let result = list(expression, None, [], tokens)
      use #(elements, rest, tokens) <- result.map(result)
      #(Some(List(elements, rest)), tokens)
    }

    [#(t.Hash, _), #(t.LeftParen, _), ..tokens] -> {
      let result = comma_delimited([], tokens, expression, t.RightParen)
      use #(expressions, tokens) <- result.map(result)
      #(Some(Tuple(expressions)), tokens)
    }

    [#(t.Bang, _), ..tokens] -> {
      use #(expression, tokens) <- result.map(expression(tokens))
      #(Some(NegateBool(expression)), tokens)
    }

    [#(t.Minus, _), ..tokens] -> {
      use #(expression, tokens) <- result.map(expression(tokens))
      #(Some(NegateInt(expression)), tokens)
    }

    [#(t.LeftBrace, _), ..tokens] -> {
      use #(statements, _, tokens) <- result.map(statements([], tokens))
      #(Some(Block(statements)), tokens)
    }

    [#(t.LessLess, _), ..tokens] -> {
      let parser = bit_string_segment(expression, _)
      let result = comma_delimited([], tokens, parser, t.GreaterGreater)
      use #(segments, tokens) <- result.map(result)
      #(Some(BitString(segments)), tokens)
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

    [#(t.Name("binary"), _), ..tokens] -> Ok(#(BinaryOption, tokens))
    [#(t.Name("int"), _), ..tokens] -> Ok(#(IntOption, tokens))
    [#(t.Name("float"), _), ..tokens] -> Ok(#(FloatOption, tokens))
    [#(t.Name("bit_string"), _), ..tokens] -> Ok(#(BitStringOption, tokens))
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

fn after_expression(
  parsed: Expression,
  tokens: Tokens,
) -> Result(#(Expression, Tokens), Error) {
  case tokens {
    // Record or module access
    [#(t.Dot, _), #(t.Name(label), _), ..tokens]
    | [#(t.Dot, _), #(t.UpperName(label), _), ..tokens] -> {
      after_expression(FieldAccess(parsed, label), tokens)
    }

    // Tuple index
    [#(t.Dot, _), #(t.Int(value) as token, position), ..tokens] -> {
      case int.parse(value) {
        Ok(i) -> after_expression(TupleIndex(parsed, i), tokens)
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

    [#(t.RightParen, _), ..tokens] -> {
      let call = Call(function, list.reverse(arguments))
      after_expression(call, tokens)
    }

    [
      #(t.Name(label), _),
      #(t.Colon, _),
      #(t.DiscardName(""), _),
      #(t.Comma, _),
      #(t.RightParen, _),
      ..tokens
    ]
    | [
        #(t.Name(label), _),
        #(t.Colon, _),
        #(t.DiscardName(""), _),
        #(t.RightParen, _),
        ..tokens
      ] -> {
      let capture =
        FnCapture(Some(label), function, list.reverse(arguments), [])
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

    [#(t.DiscardName(""), _), #(t.Comma, _), #(t.RightParen, _), ..tokens]
    | [#(t.DiscardName(""), _), #(t.RightParen, _), ..tokens] -> {
      let capture = FnCapture(None, function, list.reverse(arguments), [])
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
        [#(t.RightParen, _), ..tokens] -> {
          let call = Call(function, list.reverse(arguments))
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

    [#(t.RightParen, _), ..tokens] -> {
      let capture = FnCapture(label, function, before, list.reverse(after))
      after_expression(capture, tokens)
    }

    _ -> {
      use #(argument, tokens) <- result.try(field(tokens, expression))
      let after = [argument, ..after]
      case tokens {
        [#(t.Comma, _), ..tokens] -> {
          fn_capture(label, function, before, after, tokens)
        }
        [#(t.RightParen, _), ..tokens] -> {
          let call = FnCapture(label, function, before, list.reverse(after))
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
) -> Result(#(Option(Expression), Tokens), Error) {
  use #(record, tokens) <- result.try(expression(tokens))

  case tokens {
    [#(t.RightParen, _), ..tokens] -> {
      Ok(#(Some(RecordUpdate(module, constructor, record, [])), tokens))
    }
    [#(t.Comma, _), ..tokens] -> {
      let result =
        comma_delimited([], tokens, record_update_field, t.RightParen)
      use #(fields, tokens) <- result.try(result)
      Ok(#(Some(RecordUpdate(module, constructor, record, fields)), tokens))
    }
    _ -> Ok(#(None, tokens))
  }
}

fn record_update_field(
  tokens: Tokens,
) -> Result(#(#(String, Expression), Tokens), Error) {
  case tokens {
    [#(t.Name(name), _), #(t.Colon, _), ..tokens] -> {
      use #(expression, tokens) <- result.try(expression(tokens))
      Ok(#(#(name, expression), tokens))
    }
    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
    [] -> Error(UnexpectedEndOfInput)
  }
}

fn case_(tokens: Tokens) -> Result(#(Option(Expression), Tokens), Error) {
  use #(subjects, tokens) <- result.try(case_subjects([], tokens))
  use _, tokens <- expect(t.LeftBrace, tokens)
  use #(clauses, tokens) <- result.try(case_clauses([], tokens))
  Ok(#(Some(Case(subjects, clauses)), tokens))
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
) -> Result(#(List(Clause), Tokens), Error) {
  use #(clause, tokens) <- result.try(case_clause(tokens))
  let clauses = [clause, ..clauses]
  case tokens {
    [#(t.RightBrace, _), ..tokens] -> Ok(#(list.reverse(clauses), tokens))
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

fn fn_(tokens: Tokens) -> Result(#(Option(Expression), Tokens), Error) {
  // Parameters
  use _, tokens <- expect(t.LeftParen, tokens)
  let result = comma_delimited([], tokens, fn_parameter, t.RightParen)
  use #(parameters, tokens) <- result.try(result)

  // Return type
  use #(return, _, tokens) <- result.try(optional_return_annotation(0, tokens))

  // The function body
  use _, tokens <- expect(t.LeftBrace, tokens)
  use #(body, _, tokens) <- result.try(statements([], tokens))

  Ok(#(Some(Fn(parameters, return, body)), tokens))
}

fn list(
  parser: fn(Tokens) -> Result(#(t, Tokens), Error),
  discard: Option(t),
  acc: List(t),
  tokens: Tokens,
) -> Result(#(List(t), Option(t), Tokens), Error) {
  case tokens {
    [#(t.RightSquare, _), ..tokens] -> Ok(#(list.reverse(acc), None, tokens))

    [#(t.Comma, _), #(t.RightSquare, _), ..tokens] if acc != [] ->
      Ok(#(list.reverse(acc), None, tokens))
    [#(t.DotDot, _), #(t.RightSquare, _) as close, ..tokens] -> {
      case discard {
        None -> unexpected_error([close, ..tokens])
        Some(discard) -> Ok(#(list.reverse(acc), Some(discard), tokens))
      }
    }

    [#(t.DotDot, _), ..tokens] -> {
      use #(rest, tokens) <- result.try(parser(tokens))
      use _, tokens <- expect(t.RightSquare, tokens)
      Ok(#(list.reverse(acc), Some(rest), tokens))
    }
    _ -> {
      use #(element, tokens) <- result.try(parser(tokens))
      let acc = [element, ..acc]
      case tokens {
        [#(t.RightSquare, _), ..tokens]
        | [#(t.Comma, _), #(t.RightSquare, _), ..tokens] ->
          Ok(#(list.reverse(acc), None, tokens))

        [#(t.Comma, _), #(t.DotDot, _), #(t.RightSquare, _) as close, ..tokens] -> {
          case discard {
            None -> unexpected_error([close, ..tokens])
            Some(discard) -> Ok(#(list.reverse(acc), Some(discard), tokens))
          }
        }

        [#(t.Comma, _), #(t.DotDot, _), ..tokens] -> {
          use #(rest, tokens) <- result.try(parser(tokens))
          use _, tokens <- expect(t.RightSquare, tokens)
          Ok(#(list.reverse(acc), Some(rest), tokens))
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
) -> Result(#(Module, Tokens), Error) {
  // name
  use name, tokens <- expect_name(tokens)

  // Optional type annotation
  use #(annotation, tokens) <- result.try(optional_type_annotation(tokens))

  // = Expression
  use _, tokens <- expect(t.Equal, tokens)

  use #(expression, tokens) <- result.try(expression(tokens))

  let constant = Constant(name, publicity, annotation, expression)
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
) -> Result(#(List(t), Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)

    [#(token, _), ..tokens] if token == final -> {
      Ok(#(list.reverse(items), tokens))
    }

    _ -> {
      use #(element, tokens) <- result.try(parser(tokens))
      case tokens {
        [#(t.Comma, _), ..tokens] -> {
          comma_delimited([element, ..items], tokens, parser, final)
        }
        [#(token, _), ..tokens] if token == final -> {
          Ok(#(list.reverse([element, ..items]), tokens))
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
) -> Result(#(Module, Tokens), Error) {
  // Name(a, b, c)
  use name, tokens <- expect_upper_name(tokens)
  use #(parameters, tokens) <- result.try(optional_type_parameters(tokens))

  case tokens {
    [#(t.Equal, _), ..tokens] -> {
      type_alias(module, attributes, name, parameters, publicity, tokens)
    }
    [#(t.LeftBrace, _), ..tokens] -> {
      module
      |> custom_type(attributes, name, parameters, publicity, opaque_, tokens)
    }
    _ -> {
      let ct = CustomType(name, publicity, opaque_, parameters, [])
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
  tokens: Tokens,
) -> Result(#(Module, Tokens), Error) {
  use #(type_, tokens) <- result.try(type_(tokens))
  let alias = TypeAlias(name, publicity, parameters, type_)
  let module = push_type_alias(module, attributes, alias)
  Ok(#(module, tokens))
}

fn type_(tokens: Tokens) -> Result(#(Type, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.Fn, _), #(t.LeftParen, _), ..tokens] -> {
      fn_type(tokens)
    }
    [#(t.Hash, _), #(t.LeftParen, _), ..tokens] -> {
      tuple_type(tokens)
    }
    [#(t.Name(module), _), #(t.Dot, _), #(t.UpperName(name), _), ..tokens] -> {
      named_type(name, Some(module), tokens)
    }
    [#(t.UpperName(name), _), ..tokens] -> {
      named_type(name, None, tokens)
    }
    [#(t.Name(name), _), ..tokens] -> {
      Ok(#(VariableType(name), tokens))
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
) -> Result(#(Type, Tokens), Error) {
  use #(parameters, tokens) <- result.try(case tokens {
    [#(t.LeftParen, _), ..tokens] ->
      comma_delimited([], tokens, type_, until: t.RightParen)

    _ -> Ok(#([], tokens))
  })
  let t = NamedType(name, module, parameters)
  Ok(#(t, tokens))
}

fn fn_type(tokens: Tokens) -> Result(#(Type, Tokens), Error) {
  let result = comma_delimited([], tokens, type_, until: t.RightParen)
  use #(parameters, tokens) <- result.try(result)
  use _, tokens <- expect(t.RightArrow, tokens)
  use #(return, tokens) <- result.try(type_(tokens))
  Ok(#(FunctionType(parameters, return), tokens))
}

fn tuple_type(tokens: Tokens) -> Result(#(Type, Tokens), Error) {
  let result = comma_delimited([], tokens, type_, until: t.RightParen)
  use #(types, tokens) <- result.try(result)
  Ok(#(TupleType(types), tokens))
}

fn custom_type(
  module: Module,
  attributes: List(Attribute),
  name: String,
  parameters: List(String),
  publicity: Publicity,
  opaque_: Bool,
  tokens: Tokens,
) -> Result(#(Module, Tokens), Error) {
  // <variant>.. }
  let ct = CustomType(name, publicity, opaque_, parameters, [])
  use #(ct, tokens) <- result.try(variants(ct, tokens))

  // Continue to the next statement
  let module = push_custom_type(module, attributes, ct)
  Ok(#(module, tokens))
}

fn optional_type_parameters(
  tokens: Tokens,
) -> Result(#(List(String), Tokens), Error) {
  case tokens {
    [#(t.LeftParen, _), ..tokens] ->
      comma_delimited([], tokens, name, until: t.RightParen)
    _ -> Ok(#([], tokens))
  }
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
) -> Result(#(CustomType, Tokens), Error) {
  use ct, tokens <- until(t.RightBrace, ct, tokens)
  use name, tokens <- expect_upper_name(tokens)
  use #(parameters, tokens) <- result.try(optional_variant_fields(tokens))
  let ct = push_variant(ct, Variant(name, parameters))
  Ok(#(ct, tokens))
}

fn optional_variant_fields(
  tokens: Tokens,
) -> Result(#(List(Field(Type)), Tokens), Error) {
  case tokens {
    [#(t.LeftParen, _), #(t.RightParen, _), ..tokens] -> Ok(#([], tokens))
    [#(t.LeftParen, _), ..tokens] -> {
      comma_delimited([], tokens, field(_, of: type_), until: t.RightParen)
    }
    _ -> Ok(#([], tokens))
  }
}

fn field(
  tokens: Tokens,
  of parser: fn(Tokens) -> Result(#(t, Tokens), Error),
) -> Result(#(Field(t), Tokens), Error) {
  case tokens {
    [#(t.Name(name), _), #(t.Colon, _), ..tokens] -> {
      use #(t, tokens) <- result.try(parser(tokens))
      Ok(#(Field(Some(name), t), tokens))
    }
    _ -> {
      use #(t, tokens) <- result.try(parser(tokens))
      Ok(#(Field(None, t), tokens))
    }
  }
}
