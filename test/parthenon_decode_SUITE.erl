-module(parthenon_decode_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_SCHEMA_NAME, schema_1).
-define(A_SCHEMA,
    <<"struct<a: int, b: string, c: struct<d: int, f: array<string>>, e: array<bigint>>">>
).
-define(ANOTHER_SCHEMA_NAME, schema_2).
-define(ANOTHER_SCHEMA,
    <<"struct<a: int, b: string, c: string, d: int>">>
).

all() ->
    [
        {group, all}
    ].

init_per_suite(Config) ->
    {ok, Pid} = parthenon_schema_server:start_link(),
    unlink(Pid),
    ok = parthenon_schema_server:add_schema(?A_SCHEMA_NAME, ?A_SCHEMA),
    ok = parthenon_schema_server:add_schema(?ANOTHER_SCHEMA_NAME, ?ANOTHER_SCHEMA),
    [{schema_server_pid, Pid} | Config].

end_per_suite(Config) ->
    Pid = ?config(schema_server_pid, Config),
    link(Pid),
    Config.

groups() ->
    [
        {all, [parallel], [
            can_decode_empty_struct,
            can_decode_struct_with_primitive_elements,
            can_decode_struct_with_flat_elements,
            can_decode_struct_with_nested_elements,
            can_handle_unknown_keys,
            can_decode_object_to_proplists
        ]}
    ].

%%%===================================================================
%%% Test cases
%%%===================================================================

can_decode_empty_struct(_Config) ->
    ?assertEqual({ok, #{}}, parthenon_decode:try_decode(?A_SCHEMA_NAME, <<"{}">>)).

can_decode_struct_with_primitive_elements(_Config) ->
    ?assertEqual(
        {ok, #{a => 123, b => <<"foo bar">>, c => <<"Test, Test">>, d => 456}},
        parthenon_decode:try_decode(
            ?ANOTHER_SCHEMA_NAME, <<"{a=123,b=foo bar,c=Test, Test,d=456}">>
        )
    ).

can_decode_struct_with_flat_elements(_Config) ->
    ?assertEqual(
        {ok, #{a => 123, b => <<"foo bar">>, e => [456, 789]}},
        parthenon_decode:try_decode(?A_SCHEMA_NAME, <<"{a=123,b=foo bar,e=[456,789]}">>)
    ).

can_decode_struct_with_nested_elements(_Config) ->
    ?assertEqual(
        {ok, #{
            a => 123,
            b => <<"foo bar">>,
            c => #{d => 1011, f => [<<"foo bar">>, <<"baz bar">>]},
            e => [456, 789]
        }},
        parthenon_decode:try_decode(
            ?A_SCHEMA_NAME,
            <<"{a=123,b=foo bar,c={d=1011,f=[foo bar,baz bar]},e=[456,789]}">>
        )
    ).

can_handle_unknown_keys(_Config) ->
    ?assertEqual(
        {ok, #{
            a => 123,
            b => <<"foo bar">>,
            c => #{d => 1011, f => [<<"foo bar">>, <<"baz bar">>], <<"unknown_key">> => <<"baba">>},
            e => [456, 789],
            <<"unknown_key">> => [<<"ba">>, <<"ba">>],
            <<"unknown_object">> => #{
                <<"non_existing_key">> => <<"test">>,
                <<"non_existing_other">> => [<<"1">>, <<"2">>, <<"3">>]
            }
        }},
        parthenon_decode:try_decode(
            ?A_SCHEMA_NAME,
            <<"{a=123,b=foo bar,c={d=1011,f=[foo bar,baz bar],unknown_key=baba},e=[456,789],unknown_key=[ba,ba],unknown_object={non_existing_key=test,non_existing_other=[1,2,3]}}">>
        )
    ).

can_decode_object_to_proplists(_Config) ->
    ?assertEqual(
        {ok, [
            {e, [456, 789]},
            {c, [{f, [<<"foo bar">>, <<"baz bar">>]}, {d, 1011}]},
            {b, <<"foo bar">>},
            {a, 123}
        ]},
        parthenon_decode:try_decode(
            ?A_SCHEMA_NAME,
            <<"{a=123,b=foo bar,c={d=1011,f=[foo bar,baz bar]},e=[456,789]}">>,
            [{object_format, proplists}]
        )
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
