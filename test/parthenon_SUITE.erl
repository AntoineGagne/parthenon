-module(parthenon_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_SCHEMA,
    <<"struct<a: int, b: string, c: struct<d: int, f: array<string>>, e: array<bigint>>">>
).
-define(AN_INVALID_SCHEMA, <<"struct<a:invalid>">>).

all() ->
    [
        {group, all}
    ].

groups() ->
    [
        {all, [parallel], [
            can_decode_with_valid_raw_schema,
            return_error_when_decoding_with_invalid_raw_schema
        ]}
    ].

init_per_group(_Group, Config) ->
    Config.

end_per_group(_Group, Config) ->
    Config.

%%%===================================================================
%%% Test cases
%%%===================================================================

can_decode_with_valid_raw_schema(_Config) ->
    ?assertEqual(
        {ok, #{
            a => 123,
            b => <<"foo bar">>,
            c => #{d => 1011, f => [<<"foo bar">>, <<"baz bar">>]},
            e => [456, 789]
        }},
        parthenon:decode_with_schema(
            ?A_SCHEMA,
            <<"{a=123,b=foo bar,c={d=1011,f=[foo bar,baz bar]},e=[456,789]}">>,
            []
        )
    ).

return_error_when_decoding_with_invalid_raw_schema(_Config) ->
    ?assertMatch({error, _}, parthenon:decode_with_schema(?AN_INVALID_SCHEMA, <<"{}">>, [])).

%%%===================================================================
%%% Internal functions
%%%===================================================================
