-module(parthenon_schema_server_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(A_SCHEMA_NAME, a_schema).
-define(A_NON_EXISTING_SCHEMA_NAME, a_non_existing_schema).
-define(A_VALID_SCHEMA, <<"struct<a: int>">>).
-define(AN_INVALID_SCHEMA, <<"struct<a:">>).

all() ->
    [
        can_add_schema,
        return_error_on_invalid_schema,
        return_not_found_on_non_existing_schema,
        return_not_started_when_getting_schema_with_unstarted_server
    ].

init_per_testcase(return_not_started_when_getting_schema_with_unstarted_server, Config) ->
    Config;
init_per_testcase(_Name, Config) ->
    {ok, _} = parthenon_schema_server:start_link(),
    Config.

end_per_testcase(_Name, Config) ->
    Config.

%%%===================================================================
%%% Test cases
%%%===================================================================

can_add_schema(_Config) ->
    ok = parthenon_schema_server:add_schema(?A_SCHEMA_NAME, ?A_VALID_SCHEMA),

    ?assertMatch({ok, _}, parthenon_schema_server:get_schema(?A_SCHEMA_NAME)).

return_error_on_invalid_schema(_Config) ->
    ?assertMatch(
        {error, _},
        parthenon_schema_server:add_schema(?A_SCHEMA_NAME, ?AN_INVALID_SCHEMA)
    ).

return_not_found_on_non_existing_schema(_Config) ->
    ?assertEqual(
        {error, not_found},
        parthenon_schema_server:get_schema(?A_NON_EXISTING_SCHEMA_NAME)
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
