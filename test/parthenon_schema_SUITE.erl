-module(parthenon_schema_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [{group, all}].

groups() ->
    [
        {all, [parallel], [
            can_parse_flat_struct,
            can_parse_nested_structs,
            return_error_on_invalid_encoding,
            contain_correct_encoder,
            can_parse_complex_schema,
            return_undefined_on_null_values
        ]}
    ].

init_per_suite(Config) ->
    DataDirectory = ?config(data_dir, Config),
    {ok, RawSchema} = file:read_file(filename:join(DataDirectory, "schema-001")),
    [{raw_schema, RawSchema} | Config].

end_per_suite(Config) ->
    Config.

%%%===================================================================
%%% Test cases
%%%===================================================================

can_parse_flat_struct(_Config) ->
    ?assertMatch(
        {ok, #{<<"test_1">> := _}},
        parthenon_schema:create("struct<test_1:int>")
    ),
    ?assertMatch(
        {ok, #{<<"test_1">> := _, <<"test_2">> := _}},
        parthenon_schema:create("struct<test_1:int,test_2:double>")
    ).

can_parse_nested_structs(_Config) ->
    ?assertMatch(
        {ok, #{<<"test_1">> := #{<<"a">> := _}}},
        parthenon_schema:create("struct<test_1:struct<a: boolean>>")
    ),
    ?assertMatch(
        {ok, #{<<"test_1">> := {map_array, #{<<"a">> := _}}}},
        parthenon_schema:create("struct<test_1:array<struct<a: boolean>>>")
    ),
    ?assertMatch(
        {ok, #{<<"test_1">> := #{<<"a">> := {map_array, #{<<"b">> := _}}}}},
        parthenon_schema:create("struct<test_1:struct<a: array<struct<b: boolean>>>>")
    ),
    ?assertMatch(
        {ok, #{<<"test_1">> := {map_array, #{<<"a">> := {map_array, #{<<"b">> := _}}}}}},
        parthenon_schema:create("struct<test_1:array<struct<a: array<struct<b: boolean>>>>>")
    ).

return_error_on_invalid_encoding(_Config) ->
    ?assertMatch(
        {error, _},
        parthenon_schema:create(<<"struct<a:invalid>">>)
    ).

contain_correct_encoder(_Config) ->
    {ok, Schema} = parthenon_schema:create(
        <<
            "struct<boolean_: boolean, integer: int, double_: double, big_integer: bigint, string_: string,"
            "integer_list: array<int>, boolean_list: array<boolean>, double_list: array<double>, big_integer_list: array<bigint>,"
            "string_list: array<string>>"
        >>
    ),

    ?assertEqual(1, apply_encoder(<<"integer">>, <<"1">>, Schema)),
    ?assertEqual(true, apply_encoder(<<"boolean_">>, <<"true">>, Schema)),
    ?assertEqual(1.0, apply_encoder(<<"double_">>, <<"1.0">>, Schema)),
    ?assertEqual(100, apply_encoder(<<"big_integer">>, <<"100">>, Schema)),
    ?assertEqual(<<"test">>, apply_encoder(<<"string_">>, <<"test">>, Schema)),
    ?assertEqual([2], apply_encoder(<<"integer_list">>, [<<"2">>], Schema)),
    ?assertEqual(
        [true, false], apply_encoder(<<"boolean_list">>, [<<"true">>, <<"false">>], Schema)
    ),
    ?assertEqual([2.0], apply_encoder(<<"double_list">>, [<<"2.0">>], Schema)),
    ?assertEqual([101], apply_encoder(<<"big_integer_list">>, [<<"101">>], Schema)),
    ?assertEqual([<<"test_2">>], apply_encoder(<<"string_list">>, [<<"test_2">>], Schema)).

can_parse_complex_schema(Config) ->
    ?assertMatch({ok, _}, parthenon_schema:create(?config(raw_schema, Config))).

return_undefined_on_null_values(_Config) ->
    {ok, Schema} = parthenon_schema:create(
        <<
            "struct<boolean_: boolean, integer: int, double_: double, big_integer: bigint, string_: string,"
            "integer_list: array<int>, boolean_list: array<boolean>, double_list: array<double>, big_integer_list: array<bigint>,"
            "string_list: array<string>>"
        >>
    ),

    ?assertEqual(undefined, apply_encoder(<<"integer">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"boolean_">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"double_">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"big_integer">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"string_">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"integer_list">>, <<"null">>, Schema)),
    ?assertEqual([undefined], apply_encoder(<<"integer_list">>, [<<"null">>], Schema)),
    ?assertEqual(
        [undefined, undefined],
        apply_encoder(<<"boolean_list">>, [<<"null">>, <<"null">>], Schema)
    ),
    ?assertEqual([undefined], apply_encoder(<<"double_list">>, [<<"null">>], Schema)),
    ?assertEqual([undefined], apply_encoder(<<"big_integer_list">>, [<<"null">>], Schema)),
    ?assertEqual([undefined], apply_encoder(<<"string_list">>, [<<"null">>], Schema)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

apply_encoder(Field, Value, Schema) ->
    Encoder = maps:get(Field, Schema),
    Encoder(Value).
