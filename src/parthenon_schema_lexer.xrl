Definitions.

Digit       = [0-9]
LeadIdChar  = [A-Za-z_]
IdChar      = ({LeadIdChar}|{Digit})
Identifier  = ({IdChar}*)
Space       = \x20
Whitespace  = [{Space}\t\n\r]
Punctuation = [:<>,]

Rules.

{Punctuation}         : {token, {list_to_atom(TokenChars), TokenLine}}.
{Identifier}          : word(TokenLine, TokenChars).
{Whitespace}+         : skip_token.

Erlang code.

-dialyzer({nowarn_function, yyrev/2}).
-export([word_type/1]).


word_type("struct") ->
    reserved;
word_type("array") ->
    reserved;
word_type("int") ->
    encoding;
word_type("string") ->
    encoding;
word_type("bigint") ->
    encoding;
word_type("double") ->
    encoding;
word_type("boolean") ->
    encoding;
word_type(_) ->
    unreserved.

word(TokenLine, TokenChars) ->
    case word_type(TokenChars) of
        reserved -> {token, {list_to_atom(TokenChars), TokenLine}};
        encoding -> {token, {encoding, TokenLine, list_to_atom(TokenChars)}};
        unreserved -> {token, {word, TokenLine, list_to_binary(TokenChars)}}
    end.
