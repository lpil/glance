# Changelog

- Remove `external fn` and `external type` syntax. Unsupported in Gleam since 0.31.0.
- Add support for list with spread operator and no fixed elements, i.e. `[..]`

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
