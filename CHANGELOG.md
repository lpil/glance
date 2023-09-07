# Changelog

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
