import gleam/list
import gleam/option.{None, Option, Some}
import glexer/token.{Token} as t
import glexer.{Position}
import gleam/result

type Tokens =
  List(#(Token, Position))

pub type Module {
  Module(custom_types: List(CustomType), type_aliases: List(TypeAlias))
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
  |> slurp(Module([], []), _)
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

// fn expect(
//   expected: Token,
//   tokens: Tokens,
//   next: fn(Position, Tokens) -> Result(t, Error),
// ) -> Result(t, Error) {
//   case tokens {
//     [] -> Error(UnexpectedEndOfInput)
//     [#(token, position), ..tokens] if token == expected ->
//       next(position, tokens)
//     [#(other, position), ..] -> Error(UnexpectedToken(other, position))
//   }
// }

// fn maybe(
//   expected: Token,
//   tokens: Tokens,
//   next: fn(Option(Position), Tokens) -> Result(t, Error),
// ) -> Result(t, Error) {
//   case tokens {
//     [] -> Error(UnexpectedEndOfInput)
//     [#(token, position), ..tokens] if token == expected ->
//       next(Some(position), tokens)
//     _ -> next(None, tokens)
//   }
// }

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
    [#(t.Pub, _), #(t.Type, _), ..tokens] -> {
      type_definition(module, Public, tokens)
    }
    [#(t.Type, _), ..tokens] -> {
      type_definition(module, Private, tokens)
    }
    [_, ..tokens] -> slurp(module, tokens)
  }
}

fn type_definition(
  module: Module,
  publicity: Publicity,
  tokens: Tokens,
) -> Result(Module, Error) {
  // Name(a, b, c)
  use name, tokens <- expect_upper_name(tokens)
  use #(parameters, tokens) <- result.try(slurp_optional_type_parameters(tokens))

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

fn slurp_optional_type_parameters(
  tokens: Tokens,
) -> Result(#(List(String), Tokens), Error) {
  case tokens {
    [#(t.LeftParen, _), ..tokens] -> slurp_type_parameters([], tokens)
    _ -> Ok(#([], tokens))
  }
}

fn slurp_type_parameters(
  parameters: List(String),
  tokens: Tokens,
) -> Result(#(List(String), Tokens), Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.Name(name), _), #(t.Comma, _), ..tokens] ->
      slurp_type_parameters([name, ..parameters], tokens)
    [#(t.Name(name), _), #(t.RightParen, _), ..tokens] ->
      Ok(#(list.reverse([name, ..parameters]), tokens))
    [#(t.RightParen, _), ..tokens] -> Ok(#(list.reverse(parameters), tokens))
  }
}

fn variants(
  ct: CustomType,
  tokens: Tokens,
) -> Result(#(CustomType, Tokens), Error) {
  use ct, tokens <- until(t.RightBrace, ct, tokens)
  use name, tokens <- expect_upper_name(tokens)
  let ct = push_variant(ct, Variant(name, []))
  Ok(#(ct, tokens))
}
