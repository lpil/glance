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
  )
}

pub type Import {
  Import(
    module: String,
    alias: Option(String),
    unqualified: List(UnqualifiedImport),
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

pub type Variant {
  Variant(name: String, fields: List(Field))
}

pub type Field {
  Field(label: Option(String), type_: Type)
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
  |> slurp(Module([], [], []), _)
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
    [#(t.LeftParen, _), ..tokens] -> types_then_paren([], tokens)
    _ -> Ok(#([], tokens))
  })
  let t = NamedType(name, module, parameters)
  Ok(#(t, tokens))
}

fn fn_type(tokens: Tokens) -> Result(#(Type, Tokens), Error) {
  use #(parameters, tokens) <- result.try(types_then_paren([], tokens))
  use _, tokens <- expect(t.RightArrow, tokens)
  use #(return, tokens) <- result.try(type_(tokens))
  Ok(#(FunctionType(parameters, return), tokens))
}

fn tuple_type(tokens: Tokens) -> Result(#(Type, Tokens), Error) {
  use #(types, tokens) <- result.try(types_then_paren([], tokens))
  Ok(#(TupleType(types), tokens))
}

fn types_then_paren(
  types: List(Type),
  tokens: Tokens,
) -> Result(#(List(Type), Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)

    [#(t.RightParen, _), ..tokens] -> {
      Ok(#(list.reverse(types), tokens))
    }

    _ -> {
      use #(type_, tokens) <- result.try(type_(tokens))
      case tokens {
        [] -> Error(UnexpectedEndOfInput)
        [#(t.RightParen, _), ..tokens] -> {
          Ok(#(list.reverse([type_, ..types]), tokens))
        }
        [#(t.Comma, _), ..tokens] -> {
          types_then_paren([type_, ..types], tokens)
        }
        [#(token, position), ..] -> {
          Error(UnexpectedToken(token, position))
        }
      }
    }
  }
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
    [#(t.LeftParen, _), ..tokens] -> type_parameters([], tokens)
    _ -> Ok(#([], tokens))
  }
}

fn type_parameters(
  parameters: List(String),
  tokens: Tokens,
) -> Result(#(List(String), Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)

    // Final parameter
    [#(t.Name(name), _), #(t.RightParen, _), ..tokens]
    | // Final parameter with trailing comma
    [#(t.Name(name), _), #(t.Comma, _), #(t.RightParen, _), ..tokens] ->
      Ok(#(list.reverse([name, ..parameters]), tokens))

    // Non-final parameter
    [#(t.Name(name), _), #(t.Comma, _), ..tokens] ->
      type_parameters([name, ..parameters], tokens)

    [#(t.RightParen, _), ..tokens] -> Ok(#(list.reverse(parameters), tokens))
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
) -> Result(#(List(Field), Tokens), Error) {
  case tokens {
    [#(t.LeftParen, _), #(t.RightParen, _), ..tokens] -> Ok(#([], tokens))
    [#(t.LeftParen, _), ..tokens] -> variant_fields([], tokens)
    _ -> Ok(#([], tokens))
  }
}

fn variant_fields(
  fields: List(Field),
  tokens: Tokens,
) -> Result(#(List(Field), Tokens), Error) {
  // An optional label
  //  my_field:
  let #(label, tokens) = case tokens {
    [#(t.Name(name), _), #(t.Colon, _), ..tokens] -> #(Some(name), tokens)
    _ -> #(None, tokens)
  }

  // The type of the field
  use #(type_, tokens) <- result.try(type_(tokens))

  case tokens {
    [] -> Error(UnexpectedEndOfInput)

    // The end of the fields
    [#(t.RightParen, _), ..tokens]
    | // A trailing comma 
    [#(t.Comma, _), #(t.RightParen, _), ..tokens] -> {
      Ok(#(list.reverse([Field(label, type_), ..fields]), tokens))
    }

    // A comma before another field
    [#(t.Comma, _), ..tokens] -> {
      variant_fields([Field(label, type_), ..fields], tokens)
    }

    [#(token, position), ..] -> {
      Error(UnexpectedToken(token, position))
    }
  }
}
