-module(parthenon_decode_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_SCHEMA_NAME, schema_1).
-define(A_SCHEMA,
    <<"struct<a: int, b: string, c: struct<d: int, f: array<string>>, e: array<bigint>>">>
).

all() ->
    [
        {group, all}
    ].

init_per_suite(Config) ->
    {ok, Pid} = parthenon_schema_server:start_link(),
    unlink(Pid),
    ok = parthenon_schema_server:add_schema(?A_SCHEMA_NAME, ?A_SCHEMA),
    [{schema_server_pid, Pid} | Config].

end_per_suite(Config) ->
    Pid = ?config(schema_server_pid, Config),
    link(Pid),
    Config.

groups() ->
    [
        {all, [parallel], [
            can_decode_empty_struct,
            can_decode_struct_with_flat_elements,
            can_decode_struct_with_nested_elements
        ]}
    ].

%%%===================================================================
%%% Test cases
%%%===================================================================

can_decode_empty_struct(_Config) ->
    ?assertEqual(#{}, parthenon_decode:decode(?A_SCHEMA_NAME, <<"\n\t\r {}">>)).

can_decode_struct_with_flat_elements(_Config) ->
    ?assertEqual(
        #{a => 123, b => <<"foo bar">>, e => [456, 789]},
        parthenon_decode:decode(?A_SCHEMA_NAME, <<"{a = 123, b = foo bar, e = [456, 789]}">>)
    ).

can_decode_struct_with_nested_elements(_Config) ->
    ?assertEqual(
        #{
            a => 123,
            b => <<"foo bar">>,
            c => #{d => 1011, f => [<<"foo bar">>, <<"baz bar">>]},
            e => [456, 789]
        },
        parthenon_decode:decode(
            ?A_SCHEMA_NAME,
            <<"{a = 123, b = foo bar, c = {d = 1011, f = [foo bar, baz bar]} e = [456, 789]}">>
        )
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
