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
    functions: List(Function),
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

pub type Function {
  Function(
    name: String,
    publicity: Publicity,
    parameters: List(FunctionParameter),
    return: Option(Type),
    body: List(Statement),
  )
}

pub type Statement {
  // Use
  // Assignment
  Expression(Expression)
}

pub type Expression {
  // FnCapture(
  //     arguments_before: List(Arg<()>),
  //     arguments_after: List(Arg<()>),
  //     function: Expression,
  // )
  // Call(
  //     fun: Expression,
  //     arguments: List(CallArg<Expression>),
  // )
  // BinOp(
  //     name: BinOp,
  //     left: Expression,
  //     right: Expression,
  // )
  // PipeLine(
  //     expressions: List(Expression>,
  // )
  // Case(
  //     subjects: List(Expression),
  //     clauses: List(Clause),
  // )
  // TupleIndex(
  //     index: u64,
  //     tuple: Expression,
  // )
  // BitString(
  //     segments: List(UntypedExprBitStringSegment),
  // )
  Panic
  Int(String)
  Float(String)
  String(String)
  Variable(String)
  NegateInt(Expression)
  NegateBool(Expression)
  Block(List(Statement))
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
}

pub type FnParameter {
  FnParameter(name: ParameterName, type_: Option(Type))
}

pub type FunctionParameter {
  FunctionParameter(
    label: Option(String),
    name: ParameterName,
    type_: Option(Type),
  )
}

pub type ParameterName {
  NamedParameter(String)
  DiscardedParameter(String)
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
  |> slurp(Module([], [], [], [], [], [], []), _)
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

fn push_function(module: Module, function: Function) -> Module {
  Module(..module, functions: [function, ..module.functions])
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
    [#(t.Pub, _), #(t.Fn, _), #(t.Name(name), _), ..tokens] -> {
      function_definition(module, Public, name, tokens)
    }
    [#(t.Fn, _), #(t.Name(name), _), ..tokens] -> {
      function_definition(module, Private, name, tokens)
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

fn function_definition(
  module: Module,
  publicity: Publicity,
  name: String,
  tokens: Tokens,
) -> Result(Module, Error) {
  // Parameters
  use _, tokens <- expect(t.LeftParen, tokens)
  let result = comma_delimited([], tokens, function_parameter, t.RightParen)
  use #(parameters, tokens) <- result.try(result)

  // Return type
  use #(return_type, tokens) <- result.try(optional_return_annotation(tokens))

  // The function body
  use _, tokens <- expect(t.LeftBrace, tokens)
  use #(statements, tokens) <- result.try(statements([], tokens))

  let function = Function(name, publicity, parameters, return_type, statements)
  module
  |> push_function(function)
  |> slurp(tokens)
}

fn optional_return_annotation(
  tokens: Tokens,
) -> Result(#(Option(Type), Tokens), Error) {
  case tokens {
    [#(t.RightArrow, _), ..tokens] -> {
      use #(return_type, tokens) <- result.try(type_(tokens))
      Ok(#(Some(return_type), tokens))
    }
    _ -> Ok(#(None, tokens))
  }
}

fn statements(
  acc: List(Statement),
  tokens: Tokens,
) -> Result(#(List(Statement), Tokens), Error) {
  case tokens {
    [#(t.RightBrace, _), ..tokens] -> Ok(#(list.reverse(acc), tokens))
    _ -> {
      use #(statement, tokens) <- result.try(statement(tokens))
      statements([statement, ..acc], tokens)
    }
  }
}

fn statement(tokens: Tokens) -> Result(#(Statement, Tokens), Error) {
  case tokens {
    // [#(t.Let, _), ..tokens] -> {
    //   use #(statement, tokens) <- result.try(let_statement(tokens))
    //   Ok(#(statement, tokens))
    // }
    // [#(t.Return, _), ..tokens] -> {
    //   use #(statement, tokens) <- result.try(return_statement(tokens))
    //   Ok(#(statement, tokens))
    // }
    // [#(t.Name(name), _), ..tokens] -> {
    //   use #(statement, tokens) <- result.try(expression_statement(tokens))
    //   Ok(#(statement, tokens))
    // }
    tokens -> {
      use #(expression, tokens) <- result.try(expression(tokens))
      Ok(#(Expression(expression), tokens))
    }
  }
}

fn expression(tokens: Tokens) -> Result(#(Expression, Tokens), Error) {
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

    [#(t.Panic, _), ..tokens] -> Ok(#(Panic, tokens))
    [#(t.Int(value), _), ..tokens] -> Ok(#(Int(value), tokens))
    [#(t.Float(value), _), ..tokens] -> Ok(#(Float(value), tokens))
    [#(t.String(value), _), ..tokens] -> Ok(#(String(value), tokens))
    [#(t.Name(name), _), ..tokens] -> Ok(#(Variable(name), tokens))

    [#(t.LeftSquare, _), ..tokens] -> list([], tokens)
    [#(t.Fn, _), ..tokens] -> fn_(tokens)

    [
      #(t.Todo, _),
      #(t.LeftParen, _),
      #(t.String(value), _),
      #(t.RightParen, _),
      ..tokens
    ] -> Ok(#(Todo(Some(value)), tokens))
    [#(t.Todo, _), ..tokens] -> Ok(#(Todo(None), tokens))

    [#(t.Hash, _), #(t.LeftParen, _), ..tokens] -> {
      let result = comma_delimited([], tokens, expression, t.RightParen)
      use #(expressions, tokens) <- result.try(result)
      Ok(#(Tuple(expressions), tokens))
    }

    [#(t.Bang, _), ..tokens] -> {
      use #(expression, tokens) <- result.try(expression(tokens))
      Ok(#(NegateBool(expression), tokens))
    }

    [#(t.Minus, _), ..tokens] -> {
      use #(expression, tokens) <- result.try(expression(tokens))
      Ok(#(NegateInt(expression), tokens))
    }

    [#(t.LeftBrace, _), ..tokens] -> {
      use #(statements, tokens) <- result.try(statements([], tokens))
      Ok(#(Block(statements), tokens))
    }

    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
    [] -> Error(UnexpectedEndOfInput)
  })

  after_expression(parsed, tokens)
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

    _ -> Ok(#(parsed, tokens))
  }
}

fn record_update(
  module: Option(String),
  constructor: String,
  tokens: Tokens,
) -> Result(#(Expression, Tokens), Error) {
  use #(record, tokens) <- result.try(expression(tokens))

  case tokens {
    [#(t.RightParen, _), ..tokens] -> {
      Ok(#(RecordUpdate(module, constructor, record, []), tokens))
    }
    [#(t.Comma, _), ..tokens] -> {
      let result =
        comma_delimited([], tokens, record_update_field, t.RightParen)
      use #(fields, tokens) <- result.try(result)
      Ok(#(RecordUpdate(module, constructor, record, fields), tokens))
    }
    [#(other, position), ..] -> Error(UnexpectedToken(other, position))
    [] -> Error(UnexpectedEndOfInput)
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

fn fn_(tokens: Tokens) -> Result(#(Expression, Tokens), Error) {
  // Parameters
  use _, tokens <- expect(t.LeftParen, tokens)
  let result = comma_delimited([], tokens, fn_parameter, t.RightParen)
  use #(parameters, tokens) <- result.try(result)

  // Return type
  use #(return, tokens) <- result.try(optional_return_annotation(tokens))

  // The function body
  use _, tokens <- expect(t.LeftBrace, tokens)
  use #(body, tokens) <- result.try(statements([], tokens))

  Ok(#(Fn(parameters, return, body), tokens))
}

fn list(
  acc: List(Expression),
  tokens: Tokens,
) -> Result(#(Expression, Tokens), Error) {
  case tokens {
    [#(t.RightSquare, _), ..tokens] ->
      Ok(#(List(list.reverse(acc), None), tokens))

    [#(t.Comma, _), #(t.RightSquare, _), ..tokens] if acc != [] ->
      Ok(#(List(list.reverse(acc), None), tokens))

    _ -> {
      use #(element, tokens) <- result.try(expression(tokens))
      let acc = [element, ..acc]
      case tokens {
        [#(t.RightSquare, _), ..tokens]
        | [#(t.Comma, _), #(t.RightSquare, _), ..tokens] ->
          Ok(#(List(list.reverse(acc), None), tokens))

        [#(t.Comma, _), #(t.DotDot, _), ..tokens] -> {
          use #(rest, tokens) <- result.try(expression(tokens))
          use _, tokens <- expect(t.RightSquare, tokens)
          Ok(#(List(list.reverse(acc), Some(rest)), tokens))
        }

        [#(t.Comma, _), ..tokens] -> list(acc, tokens)

        [#(other, position), ..] -> Error(UnexpectedToken(other, position))
        [] -> Error(UnexpectedEndOfInput)
      }
    }
  }
}

fn fn_parameter(tokens: Tokens) -> Result(#(FnParameter, Tokens), Error) {
  use #(name, tokens) <- result.try(case tokens {
    [#(t.Name(name), _), ..tokens] -> {
      Ok(#(NamedParameter(name), tokens))
    }
    [#(t.DiscardName(name), _), ..tokens] -> {
      Ok(#(DiscardedParameter(name), tokens))
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
      Ok(#(Some(label), DiscardedParameter(name), tokens))
    }
    [#(t.DiscardName(name), _), ..tokens] -> {
      Ok(#(None, DiscardedParameter(name), tokens))
    }
    [#(t.Name(label), _), #(t.Name(name), _), ..tokens] -> {
      Ok(#(Some(label), NamedParameter(name), tokens))
    }
    [#(t.Name(name), _), ..tokens] -> {
      Ok(#(None, NamedParameter(name), tokens))
    }
    [#(token, position), ..] -> Error(UnexpectedToken(token, position))
  })

  // Annotation
  use #(type_, tokens) <- result.try(optional_type_annotation(tokens))

  Ok(#(FunctionParameter(label, parameter, type_), tokens))
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
  use #(annotation, tokens) <- result.try(optional_type_annotation(tokens))

  // = ConstantExpression
  use _, tokens <- expect(t.Equal, tokens)

  use #(expression, tokens) <- result.try(constant_expression(tokens))

  module
  |> push_constant(Constant(name, publicity, annotation, expression))
  |> slurp(tokens)
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
