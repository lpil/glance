# Changelog

## v4.0.0 - Unreleased.

- All definitions have a location span as their first field.
- All types have a location span as their first field.

## v3.0.0 - 2025-02-21

- The `"prefix" as prefix name <> rest ->` pattern syntax is now supported.

## v2.0.1 - 2025-02-12

- Fixed attributes with no parens failing to parse.

## v2.0.0 - 2024-12-09

- Added support for label shorthand syntax.
- `Field` now has two variants; `LabelledField` and `ShorthandField`.
- `Variant` now uses a new `VariantField` type instead of `Field(Type)`.
- `RecordUpdate` variant of `Expression` now uses a new `RecordUpdateField` 
  type instead of `#(String, Expression)`.

## v1.1.0 - 2024-12-04

- Updated to glexer v2.0.0

## v1.0.0 - 2024-09-30

- Added support for `bits` and `bytes` bit string options.
- `BinaryOption` has been renamed `BytesOption`.
- `BitStringOption` has been renamed `BitsOption`.

## v0.11.0 - 2024-04-25

- The `Panic` and `Todo` AST nodes have been updated to take expressions as the
  error message, to match current Gleam.
- Support for the old `panic("reason")` and `todo("reason")` syntax has been
  removed. These now parse as function calls.
- Added support for type holes.
- Fixed a bug where negative floats would parse as `NegateInt` of a float.

## v0.10.0 - 2024-04-20

- Added support for module alias discarded name. This is a breaking change as
  the import data structure has been changed to hold this information.

## v0.9.0 - 2024-04-10

- Remove `external fn` and `external type` syntax. Unsupported in Gleam since 0.31.0.
- Add support for list with spread operator and no fixed elements, i.e. `[..]`
- Fixed a typo so `FunctionType.paramters` is now called `FunctionType.parameters`.

## v0.8.2 - 2024-01-20

- Fixed a bug where record constructor patterns with a `..` and a trailing comma
  would fail to parse.

## v0.8.1 - 2023-12-05

- Fixed a bug where the parser could crash on some incomplete inputs.

## v0.8.0 - 2023-11-02

- Updated for Gleam v0.32.0.

## v0.7.1 - 2023-09-27

- Fixed a bug where use expressions with a zero arity callback would fail to
  parse.

## v0.7.0 - 2023-09-15

- Added support for the `pub type TypeName` syntax.
- Added support for the `@external(target, "module", "function")` syntax.

## v0.6.2 - 2023-09-07

- Fixed a bug where assignment type annotations would fail to parse.

## v0.6.1 - 2023-07-05

- Added support for parsing function captures that use a label with the `_`
  argument.

## v0.6.0 - 2023-06-29

- Added support for the `todo as "..."` and `panic as "..."` syntaxes.

## v0.5.2 - 2023-06-24

- Fixed a bug where `||` would fail to parse.

## v0.5.1 - 2023-06-21

- The `Definition` type gains the `definition` label.

## v0.5.0 - 2023-06-21

- Functions now include their location in the source code.

## v0.4.1 - 2023-06-20

- Fixed a bug where list patterns with a discarded remainder would fail to
  parse.

## v0.4.0 - 2023-06-20

- All Gleam code can now be parsed.

## 0.3.0 - 2023-05-23

- External types can now be parsed.
- External functions can now be parsed.

## 0.2.0 - 2023-05-19

- Constant defintions can now be parsed. Bit string segments are currently
  discarded.

## 0.1.0 - 2023-05-12

- Initial release, with support for parsing custom types, imports, and type
  aliases.
