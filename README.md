# glance

[![Package Version](https://img.shields.io/hexpm/v/glance)](https://hex.pm/packages/glance)
[![Hex Docs](https://img.shields.io/badge/hex-docs-ffaff3)](https://hexdocs.pm/glance/)

A Gleam source code parser, in Gleam!

Currently this library will parse the following:

- Constants (discarding bit string segments).
- Custom types.
- External functions.
- External types.
- Functions (discarding their body).
- Imports.
- Type aliases.

All other Gleam syntax is skipped over.


## Usage

Add the package to your Gleam project:

```sh
gleam add glance
```

Then get parsing!

```gleam
import glance
import gleam/io

const code = "
  pub type Cardinal {
    North
    East
    South
    West
  }
"

pub fn main() {
  let assert Ok(parsed) = glance.module(code)
  io.debug(parsed.custom_types)
}
```

This program print this to the console:
  
```gleam
[
  CustomType(
    name: "Cardinal",
    publicity: Public,
    parameters: [],
    variants: [
      Variant("North", []),
      Variant("East", []),
      Variant("South", []),
      Variant("West", []),
    ],
  ),
]
```

API documentation can be found at <https://hexdocs.pm/glance>.
