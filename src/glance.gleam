import gleam/list
import gleam/option.{None, Option, Some}
import glexer/token.{Token} as t
import glexer.{Position}
import gleam/result

type Tokens =
  List(#(Token, Position))

pub type Module {
  Module(
    imports: List(Import),
    custom_types: List(CustomType),
    type_aliases: List(TypeAlias),
    constants: List(Constant),
    external_types: List(ExternalType),
    external_functions: List(ExternalFunction),
  )
}

pub type ExternalFunction {
  ExternalFunction(
    name: String,
    publicity: Publicity,
    parameters: List(Field(Type)),
    return: Type,
    module: String,
    function: String,
  )
}

pub type Import {
  Import(
    module: String,
    alias: Option(String),
    unqualified: List(UnqualifiedImport),
  )
}

pub type ConstantExpression {
  // TODO: define bitstring segments
  ConstantBitString
  ConstantInt(String)
  ConstantFloat(String)
  ConstantString(String)
  ConstantVariable(String)
  ConstantTuple(List(ConstantExpression))
  ConstantList(List(ConstantExpression))
  ConstantConstructor(
    name: String,
    module: Option(String),
    parameters: List(Field(ConstantExpression)),
  )
}

pub type Constant {
  Constant(
    name: String,
    publicity: Publicity,
    annotation: Option(Type),
    value: ConstantExpression,
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
    parameters: List(String),
    variants: List(Variant),
  )
}

pub type ExternalType {
  ExternalType(name: String, publicity: Publicity, parameters: List(String))
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
  FunctionType(paramters: List(Type), return: Type)
  VariableType(name: String)
}

pub type Error {
  UnexpectedEndOfInput
  UnexpectedToken(token: Token, position: Position)
}

// TODO: document
pub fn module(src: String) -> Result(Module, Error) {
  glexer.new(src)
  |> glexer.lex
  |> list.filter(fn(pair) { pair.0 != t.CommentNormal })
  |> slurp(Module([], [], [], [], [], []), _)
}

fn push_constant(module: Module, constant: Constant) -> Module {
  Module(..module, constants: [constant, ..module.constants])
}

fn push_external_function(
  module: Module,
  external_function: ExternalFunction,
) -> Module {
  Module(
    ..module,
    external_functions: [external_function, ..module.external_functions],
  )
}

fn push_external_type(module: Module, external_type: ExternalType) -> Module {
  Module(..module, external_types: [external_type, ..module.external_types])
}

fn push_custom_type(module: Module, custom_type: CustomType) -> Module {
  let custom_type =
    CustomType(..custom_type, variants: list.reverse(custom_type.variants))
  Module(..module, custom_types: [custom_type, ..module.custom_types])
}

fn push_type_alias(module: Module, type_alias: TypeAlias) -> Module {
  Module(..module, type_aliases: [type_alias, ..module.type_aliases])
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

fn expect_string(
  tokens: Tokens,
  next: fn(String, Tokens) -> Result(t, Error),
) -> Result(t, Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.String(s), _), ..tokens] -> next(s, tokens)
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

fn slurp(module: Module, tokens: Tokens) -> Result(Module, Error) {
  case tokens {
    [] -> Ok(module)
    [#(t.Import, _), ..tokens] -> {
      import_statement(module, tokens)
    }
    [#(t.Pub, _), #(t.Type, _), ..tokens] -> {
      type_definition(module, Public, tokens)
    }
    [#(t.Type, _), ..tokens] -> {
      type_definition(module, Private, tokens)
    }
    [#(t.Pub, _), #(t.Const, _), ..tokens] -> {
      const_definition(module, Public, tokens)
    }
    [#(t.Const, _), ..tokens] -> {
      const_definition(module, Private, tokens)
    }
    [#(t.Pub, _), #(t.External, _), #(t.Fn, _), ..tokens] -> {
      external_fn_definition(module, Public, tokens)
    }
    [#(t.External, _), #(t.Fn, _), ..tokens] -> {
      external_fn_definition(module, Private, tokens)
    }
    [#(t.Pub, _), #(t.External, _), #(t.Type, _), ..tokens] -> {
      external_type_definition(module, Public, tokens)
    }
    [#(t.External, _), #(t.Type, _), ..tokens] -> {
      external_type_definition(module, Private, tokens)
    }
    [_, ..tokens] -> slurp(module, tokens)
  }
}

fn import_statement(module: Module, tokens: Tokens) -> Result(Module, Error) {
  use #(module_name, tokens) <- result.try(module_name("", tokens))
  use #(unqualified, tokens) <- result.try(optional_unqualified_imports(tokens))
  let #(alias, tokens) = optional_module_alias(tokens)
  let import_ = Import(module_name, alias, unqualified)
  slurp(Module(..module, imports: [import_, ..module.imports]), tokens)
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
    _ -> #(None, tokens)
  }
}

fn optional_unqualified_imports(
  tokens: Tokens,
) -> Result(#(List(UnqualifiedImport), Tokens), Error) {
  case tokens {
    [#(t.Dot, _), #(t.LeftBrace, _), ..tokens] ->
      unqualified_imports([], tokens)
    _ -> Ok(#([], tokens))
  }
}

fn unqualified_imports(
  items: List(UnqualifiedImport),
  tokens: Tokens,
) -> Result(#(List(UnqualifiedImport), Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)

    [#(t.RightBrace, _), ..tokens] -> Ok(#(list.reverse(items), tokens))

    // Aliased non-final item
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
      unqualified_imports([import_, ..items], tokens)
    }

    // Aliased final item
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
      Ok(#(list.reverse([import_, ..items]), tokens))
    }

    // Unaliased non-final item
    [#(t.UpperName(name), _), #(t.Comma, _), ..tokens]
    | [#(t.Name(name), _), #(t.Comma, _), ..tokens] -> {
      let import_ = UnqualifiedImport(name, None)
      unqualified_imports([import_, ..items], tokens)
    }

    // Unaliased final item
    [#(t.UpperName(name), _), #(t.RightBrace, _), ..tokens]
    | [#(t.Name(name), _), #(t.RightBrace, _), ..tokens] -> {
      let import_ = UnqualifiedImport(name, None)
      Ok(#(list.reverse([import_, ..items]), tokens))
    }

    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
  }
}

fn external_fn_definition(
  module: Module,
  publicity: Publicity,
  tokens: Tokens,
) -> Result(Module, Error) {
  // Name
  use name, tokens <- expect_name(tokens)

  // Parameters
  use _, tokens <- expect(t.LeftParen, tokens)
  let parser = field(_, of: type_)
  let result = comma_delimited([], tokens, parser, t.RightParen)
  use #(parameters, tokens) <- result.try(result)

  // Return type
  use _, tokens <- expect(t.RightArrow, tokens)
  use #(return_type, tokens) <- result.try(type_(tokens))

  // Implementation location
  use _, tokens <- expect(t.Equal, tokens)
  use mod, tokens <- expect_string(tokens)
  use f, tokens <- expect_string(tokens)

  let extern =
    ExternalFunction(name, publicity, parameters, return_type, mod, f)
  module
  |> push_external_function(extern)
  |> slurp(tokens)
}

fn external_type_definition(
  module: Module,
  publicity: Publicity,
  tokens: Tokens,
) -> Result(Module, Error) {
  use name, tokens <- expect_upper_name(tokens)
  use #(parameters, tokens) <- result.try(optional_type_parameters(tokens))

  module
  |> push_external_type(ExternalType(name, publicity, parameters))
  |> slurp(tokens)
}

fn const_definition(
  module: Module,
  publicity: Publicity,
  tokens: Tokens,
) -> Result(Module, Error) {
  // name
  use name, tokens <- expect_name(tokens)

  // Optional type annotation
  use #(annotation, tokens) <- result.try(case tokens {
    [#(t.Colon, _), ..tokens] -> {
      use #(annotation, tokens) <- result.map(type_(tokens))
      #(Some(annotation), tokens)
    }
    _ -> Ok(#(None, tokens))
  })

  // = ConstantExpression
  use _, tokens <- expect(t.Equal, tokens)

  use #(expression, tokens) <- result.try(constant_expression(tokens))

  module
  |> push_constant(Constant(name, publicity, annotation, expression))
  |> slurp(tokens)
}

fn constant_expression(
  tokens: Tokens,
) -> Result(#(ConstantExpression, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.Name(module), _), #(t.Dot, _), #(t.UpperName(name), _), ..tokens] ->
      constant_constructor(name, Some(module), tokens)
    [#(t.UpperName(name), _), ..tokens] ->
      constant_constructor(name, None, tokens)
    [#(t.Int(i), _), ..tokens] -> Ok(#(ConstantInt(i), tokens))
    [#(t.Name(n), _), ..tokens] -> Ok(#(ConstantVariable(n), tokens))
    [#(t.Float(i), _), ..tokens] -> Ok(#(ConstantFloat(i), tokens))
    [#(t.String(i), _), ..tokens] -> Ok(#(ConstantString(i), tokens))
    [#(t.LeftSquare, _), ..tokens] -> constant_list(tokens)
    [#(t.Hash, _), #(t.LeftParen, _), ..tokens] -> constant_tuple(tokens)
    [#(t.LessLess, _), ..tokens] -> constant_bit_string(tokens)
    [#(token, position), ..] -> Error(UnexpectedToken(token, position))
  }
}

fn constant_bit_string(
  tokens: Tokens,
) -> Result(#(ConstantExpression, Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.GreaterGreater, _), ..tokens] -> Ok(#(ConstantBitString, tokens))
    [_, ..tokens] -> constant_bit_string(tokens)
  }
}

fn constant_constructor(
  name: String,
  module: Option(String),
  tokens: Tokens,
) -> Result(#(ConstantExpression, Tokens), Error) {
  case tokens {
    [#(t.LeftParen, _), ..tokens] -> {
      let parser = field(_, of: constant_expression)
      let result = comma_delimited([], tokens, parser, t.RightParen)
      use #(arguments, tokens) <- result.try(result)
      Ok(#(ConstantConstructor(name, module, arguments), tokens))
    }
    _ -> {
      Ok(#(ConstantConstructor(name, module, []), tokens))
    }
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
      }
    }
  }
}

fn constant_tuple(
  tokens: Tokens,
) -> Result(#(ConstantExpression, Tokens), Error) {
  let result =
    comma_delimited([], tokens, constant_expression, until: t.RightParen)
  use #(elements, tokens) <- result.try(result)
  Ok(#(ConstantTuple(elements), tokens))
}

fn constant_list(tokens: Tokens) -> Result(#(ConstantExpression, Tokens), Error) {
  let result =
    comma_delimited([], tokens, constant_expression, until: t.RightSquare)
  use #(elements, tokens) <- result.try(result)
  Ok(#(ConstantList(elements), tokens))
}

fn type_definition(
  module: Module,
  publicity: Publicity,
  tokens: Tokens,
) -> Result(Module, Error) {
  // Name(a, b, c)
  use name, tokens <- expect_upper_name(tokens)
  use #(parameters, tokens) <- result.try(optional_type_parameters(tokens))

  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.Equal, _), ..tokens] -> {
      type_alias(module, name, parameters, publicity, tokens)
    }
    [#(t.LeftBrace, _), ..tokens] -> {
      custom_type(module, name, parameters, publicity, tokens)
    }
  }
}

fn type_alias(
  module: Module,
  name: String,
  parameters: List(String),
  publicity: Publicity,
  tokens: Tokens,
) -> Result(Module, Error) {
  use #(type_, tokens) <- result.try(type_(tokens))
  module
  |> push_type_alias(TypeAlias(name, publicity, parameters, type_))
  |> slurp(tokens)
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
  name: String,
  parameters: List(String),
  publicity: Publicity,
  tokens: Tokens,
) -> Result(Module, Error) {
  // <variant>.. }
  let ct = CustomType(name, publicity, parameters, [])
  use #(ct, tokens) <- result.try(variants(ct, tokens))

  // Continue to the next statement
  let module = push_custom_type(module, ct)
  slurp(module, tokens)
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
