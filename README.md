# parthenon

[![Build Status](https://github.com/AntoineGagne/parthenon/actions/workflows/erlang.yml/badge.svg)](https://github.com/AntoineGagne/parthenon/actions)
[![Coverage](https://coveralls.io/repos/AntoineGagne/parthenon/badge.png?branch=master)](https://coveralls.io/r/AntoineGagne/parthenon?branch=master)
[![Hex Pm](http://img.shields.io/hexpm/v/parthenon.svg?style=flat)](https://hex.pm/packages/parthenon)
[![Docs](https://img.shields.io/badge/hex-docs-green.svg?style=flat)](https://hexdocs.pm/parthenon)
[![Erlang Versions](https://img.shields.io/badge/Supported%20Erlang%2FOTP-21.0%20to%2026.1-blue)](http://www.erlang.org)

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
ok = parthenon:add_schema(athena_structure, <<"struct<a: int, b: int>">>).
{ok, #{a := 123, b := 456}} = parthenon:decode(athena_structure, <<"{a=123, b=456}">>, [
    {object_format, maps}, {key_format, existing_atom}
]).
```

To test it out from the shell, it is possible to simply do:

```erl
application:ensure_all_started(parthenon).
ok = parthenon:add_schema(athena_structure, <<"struct<a: int, b: int>">>).
{ok, #{a := 123, b := 456} = Response} = parthenon:decode(athena_structure, <<"{a=123, b=456}">>, [
    {object_format, maps}, {key_format, existing_atom}
]).

ok = parthenon:add_schema(nested_structures, <<"struct<a: int, b: array<struct<c: int, d: string>>>">>).
{ok, #{a := 123, b := [#{c := 456, d := <<"Some test, some test">>}]} = Response2} = parthenon:decode(
    nested_structures, <<"{a=123, b=[{c=456, d=Some test, some test}]}">>, [
        {object_format, maps}, {key_format, existing_atom}
    ]
).

%% If `jiffy' is present
<<"{\"b\":456,\"a\":123}">> = jiffy:encode(Response).
<<"{\"b\":[{\"d\":\"Some test, some test\",\"c\":456}],\"a\":123}">> = jiffy:encode(Response2).
```

## Development

### Running all the tests and linters

You can run all the tests and linters with the ``rebar3`` alias:

```sh
rebar3 check
```
