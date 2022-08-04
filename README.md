# parthenon

[![Build Status](https://github.com/AntoineGagne/parthenon/actions/workflows/erlang.yml/badge.svg)](https://github.com/AntoineGagne/parthenon/actions)
[![Coverage](https://coveralls.io/repos/AntoineGagne/parthenon/badge.png?branch=master)](https://coveralls.io/r/AntoineGagne/parthenon?branch=master)
[![Hex Pm](http://img.shields.io/hexpm/v/parthenon.svg?style=flat)](https://hex.pm/packages/parthenon)
[![Docs](https://img.shields.io/badge/hex-docs-green.svg?style=flat)](https://hexdocs.pm/parthenon)

`parthenon` is an OTP application that parses AWS Athena structures into Erlang terms.

## Usage

Inside `rebar.config`:

```erl
%% ...
{deps, [
    %% ...
    parthenon
]}.
```

Inside ``.app.src``:

```erl
%% ...
{applications, [
    %% ...
    parthenon
]},
%% ...
```

Inside the code:

```erl
parthenon:add_schema(athena_structure, <<"struct<a: int, b: int>">>).
parthenon:decode(athena_structure, <<"{a=123, b=456}">>).

{ok, Binary} = file:read_file("/path/to/athena-structure-schema").
parthenon:add_schema(athena_structure, Binary).
{ok, RawStructure} = file:read_file("/path/to/raw-athena-structure").
parthenon:decode(athena_structure, RawStructure).
```

## Development

### Running all the tests and linters

You can run all the tests and linters with the ``rebar3`` alias:

```sh
rebar3 check
```
