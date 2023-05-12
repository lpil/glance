import gleam/list
import gleam/option.{Option}
import glexer/token.{Token} as t
import glexer.{Position}
import gleam/result

type Tokens =
  List(#(Token, Position))

pub type Module {
  Module(custom_types: List(CustomType))
}

pub type Publicity {
  Public
  Private
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
  Dunno
}

pub type Error {
  UnexpectedEndOfInput
  UnexpectedToken(token: Token, expected: Token, position: Position)
}

// TODO: document
pub fn module(src: String) -> Result(Module, Error) {
  glexer.new(src)
  |> glexer.lex
  |> slurp(Module([]), _)
}

fn push_custom_type(module: Module, custom_type: CustomType) -> Module {
  let custom_type =
    CustomType(..custom_type, variants: list.reverse(custom_type.variants))
  Module(custom_types: [custom_type, ..module.custom_types])
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
    [#(other, position), ..] ->
      Error(UnexpectedToken(other, expected, position))
  }
}

fn expect_upper_name(
  tokens: Tokens,
  next: fn(String, Tokens) -> Result(t, Error),
) -> Result(t, Error) {
  case tokens {
    [] -> Error(UnexpectedEndOfInput)
    [#(t.UpperName(name), _), ..tokens] -> next(name, tokens)
    [#(other, position), ..] ->
      Error(UnexpectedToken(other, t.UpperName(""), position))
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
      slurp_custom_type(module, Public, tokens)
    }
    [#(t.Type, _), ..tokens] -> {
      slurp_custom_type(module, Private, tokens)
    }
    [_, ..tokens] -> slurp(module, tokens)
  }
}

fn slurp_custom_type(
  module: Module,
  publicity: Publicity,
  tokens: Tokens,
) -> Result(Module, Error) {
  // Name(a, b, c)
  use name, tokens <- expect_upper_name(tokens)
  use #(parameters, tokens) <- result.try(slurp_optional_type_parameters(tokens))

  // { <variant>.. }
  use _, tokens <- expect(t.LeftBrace, tokens)
  let ct = CustomType(name, publicity, parameters, [])
  use #(ct, tokens) <- result.try(slurp_variants(ct, tokens))

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

fn slurp_variants(
  ct: CustomType,
  tokens: Tokens,
) -> Result(#(CustomType, Tokens), Error) {
  use ct, tokens <- until(t.RightBrace, ct, tokens)
  use name, tokens <- expect_upper_name(tokens)
  let ct = push_variant(ct, Variant(name, []))
  Ok(#(ct, tokens))
}
