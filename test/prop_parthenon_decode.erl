-module(prop_parthenon_decode).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_SCHEMA_NAME, random_schema).

%%%===================================================================
%%% Properties
%%%===================================================================

prop_can_decode_arrays() ->
    ?SETUP(
        fun setup/0,
        ?FORALL({Type, Value}, array(), begin
            {Schema, Athena} = from_ast({Type, Value}),
            io:format(user, "~nat=~s:~p:~p ~p~n", [?MODULE_STRING, ?FUNCTION_NAME, ?LINE, Schema]),
            ok = parthenon:add_schema(?A_SCHEMA_NAME, binary_to_list(iolist_to_binary(Schema))),
            io:format(user, "~nat=~s:~p:~p athena=~p~n", [
                ?MODULE_STRING, ?FUNCTION_NAME, ?LINE, iolist_to_binary(Athena)
            ]),
            equals({ok, Value}, parthenon:decode(?A_SCHEMA_NAME, iolist_to_binary(Athena)))
        end)
    ).

%%%===================================================================
%%% Generators
%%%===================================================================

array() ->
    ?LET(
        {Type, Value},
        frequency([
            {90, primitive()},
            {1, ?LAZY(array())}
        ]),
        ?LET(L, list(Value), {{array, Type}, L})
    ).

primitive() ->
    oneof([
        {tinyint, int()},
        {smallint, int()},
        {int, integer()},
        {integer, integer()},
        {bigint, largeint()},
        {float, float()},
        {double, float()},
        {boolean, boolean()},
        {string, string_()}
    ]).

struct_key() ->
    ?LET(K, non_empty(list(key_character())), list_to_binary(K)).

string_() ->
    ?LET(S, non_empty(list(value_character())), list_to_binary(S)).

key_character() ->
    ?SUCHTHAT(Character, byte(), not lists:member(Character, [$=])).

value_character() ->
    ?SUCHTHAT(Character, char(), not lists:member(Character, [$=, $\t, $\s, $\r, $\n, $,])).

%%%===================================================================
%%% Internal functions
%%%===================================================================

from_ast({{array, Child}, Values}) ->
    {Type, AsStrings} = lists:foldl(
        fun(V, {_, Acc}) ->
            {Type, Value} = from_ast({Child, V}),
            {Type, [Value | Acc]}
        end,
        {Child, []},
        Values
    ),
    WithCommas = lists:join($,, lists:reverse(AsStrings)),
    {["array<", ensure_string(Type), ">"], ["[", WithCommas, "]"]};
from_ast({tinyint, Value}) ->
    {"tinyint", integer_to_list(Value)};
from_ast({smallint, Value}) ->
    {"smallint", integer_to_list(Value)};
from_ast({int, Value}) ->
    {"int", integer_to_list(Value)};
from_ast({integer, Value}) ->
    {"integer", integer_to_list(Value)};
from_ast({bigint, Value}) ->
    {"bigint", integer_to_list(Value)};
from_ast({float, Value}) ->
    {"float", float_to_list(Value)};
from_ast({double, Value}) ->
    {"double", float_to_list(Value)};
from_ast({boolean, Value}) ->
    {"boolean", atom_to_list(Value)};
from_ast({string, Value}) ->
    {"string", binary_to_list(Value)}.

ensure_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
ensure_string(V) ->
    V.

setup() ->
    {ok, Applications} = application:ensure_all_started(parthenon),
    fun() -> lists:foreach(fun application:stop/1, Applications) end.
