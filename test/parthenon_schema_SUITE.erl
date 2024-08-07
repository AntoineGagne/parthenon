-module(parthenon_schema_SUITE).

-include("parthenon.hrl").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

-define(SOME_OPTIONS, #schema_options{}).

all() ->
    [{group, all}].

groups() ->
    [
        {all, [parallel], [
            can_parse_flat_struct,
            can_parse_nested_structs,
            can_parse_top_level_array,
            can_parse_top_level_struct_array,
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

can_parse_top_level_array(_Config) ->
    ?assertMatch(
        {ok, _},
        parthenon_schema:create("array<boolean>")
    ).

can_parse_top_level_struct_array(_Config) ->
    ?assertMatch(
        {ok, {map_array, #{<<"test_1">> := _}}},
        parthenon_schema:create("array<struct<test_1: int>>")
    ).

return_error_on_invalid_encoding(_Config) ->
    ?assertMatch(
        {error, _},
        parthenon_schema:create(<<"struct<a:invalid>">>)
    ).

contain_correct_encoder(_Config) ->
    {ok, Schema} = parthenon_schema:create(
        <<
            "struct<boolean_: boolean, int_: int, integer_: integer, double_: double, big_integer: bigint, "
            "string_: string, tiny_integer: tinyint, small_integer: smallint, integer_: integer, float_: float,"
            "int_list: array<int>, integer_list: array<integer>, boolean_list: array<boolean>, "
            "double_list: array<double>, big_integer_list: array<bigint>,string_list: array<string>, "
            "tiny_integer_list: array<tinyint>, small_integer_list: array<smallint>, float_list: array<float>, "
            "int_map: map<int, int>, integer_map: map<integer, integer>, boolean_map: map<boolean, boolean>, "
            "double_map: map<double, double>, big_integer_map: map<bigint, bigint>,string_map: map<string, string>, "
            "tiny_integer_map: map<tinyint, tinyint>, small_integer_map: map<smallint, smallint>, "
            "float_map: map<float, float>, integer_list_map: map<boolean, array<int>>, "
            "struct_map: map<integer, struct<foo: int>>>"
        >>
    ),

    ?assertEqual(1, apply_encoder(<<"int_">>, <<"1">>, Schema)),
    ?assertEqual(1, apply_encoder(<<"integer_">>, <<"1">>, Schema)),
    ?assertEqual(1, apply_encoder(<<"tiny_integer">>, <<"1">>, Schema)),
    ?assertEqual(1, apply_encoder(<<"small_integer">>, <<"1">>, Schema)),
    ?assertEqual(true, apply_encoder(<<"boolean_">>, <<"true">>, Schema)),
    ?assertEqual(1.0, apply_encoder(<<"double_">>, <<"1.0">>, Schema)),
    ?assertEqual(1.0, apply_encoder(<<"float_">>, <<"1.0">>, Schema)),
    ?assertEqual(100, apply_encoder(<<"big_integer">>, <<"100">>, Schema)),
    ?assertEqual(<<"test">>, apply_encoder(<<"string_">>, <<"test">>, Schema)),

    ?assertEqual([2], apply_list_encoder(<<"int_list">>, [<<"2">>], Schema)),
    ?assertEqual([2], apply_list_encoder(<<"integer_list">>, [<<"2">>], Schema)),
    ?assertEqual([2], apply_list_encoder(<<"tiny_integer_list">>, [<<"2">>], Schema)),
    ?assertEqual([2], apply_list_encoder(<<"small_integer_list">>, [<<"2">>], Schema)),
    ?assertEqual(
        [true, false], apply_list_encoder(<<"boolean_list">>, [<<"true">>, <<"false">>], Schema)
    ),
    ?assertEqual([2.0], apply_list_encoder(<<"double_list">>, [<<"2.0">>], Schema)),
    ?assertEqual([2.0], apply_list_encoder(<<"float_list">>, [<<"2.0">>], Schema)),
    ?assertEqual([101], apply_list_encoder(<<"big_integer_list">>, [<<"101">>], Schema)),
    ?assertEqual([<<"test_2">>], apply_list_encoder(<<"string_list">>, [<<"test_2">>], Schema)),

    ?assertEqual({1, 1}, apply_encoder(<<"int_map">>, {<<"1">>, <<"1">>}, Schema)),
    ?assertEqual({1, 1}, apply_encoder(<<"integer_map">>, {<<"1">>, <<"1">>}, Schema)),
    ?assertEqual({1, 1}, apply_encoder(<<"tiny_integer_map">>, {<<"1">>, <<"1">>}, Schema)),
    ?assertEqual({1, 1}, apply_encoder(<<"small_integer_map">>, {<<"1">>, <<"1">>}, Schema)),
    ?assertEqual(
        {true, false}, apply_encoder(<<"boolean_map">>, {<<"true">>, <<"false">>}, Schema)
    ),
    ?assertEqual(
        {2.0, 2.0}, apply_encoder(<<"double_map">>, {<<"2.0">>, <<"2.0">>}, Schema)
    ),
    ?assertEqual(
        {101, 101}, apply_encoder(<<"big_integer_map">>, {<<"101">>, <<"101">>}, Schema)
    ),
    ?assertEqual(
        {<<"test_key">>, <<"test_value">>},
        apply_encoder(<<"string_map">>, {<<"test_key">>, <<"test_value">>}, Schema)
    ),
    ?assertEqual(
        {2.0, 2.0},
        apply_encoder(<<"float_map">>, {<<"2.0">>, <<"2.0">>}, Schema)
    ),

    {map, BooleanKeyEncoder, IntEncoder} = maps:get(<<"integer_list_map">>, Schema),
    ?assertEqual(
        {true, [2]},
        {BooleanKeyEncoder(<<"true">>, ?SOME_OPTIONS), [
            IntEncoder(V, ?SOME_OPTIONS)
         || V <- [<<"2">>]
        ]}
    ),
    {map, IntegerEncoder, StructEncoder} = maps:get(<<"struct_map">>, Schema),
    ?assertEqual(
        {1, 4},
        {IntegerEncoder(<<"1">>, ?SOME_OPTIONS), apply_encoder(<<"foo">>, <<"4">>, StructEncoder)}
    ).

can_parse_complex_schema(Config) ->
    ?assertMatch({ok, _}, parthenon_schema:create(?config(raw_schema, Config))).

return_undefined_on_null_values(_Config) ->
    {ok, Schema} = parthenon_schema:create(
        <<
            "struct<boolean_: boolean, int_: int, integer_: integer, double_: double, big_integer: bigint, "
            "tiny_integer: tinyint, small_integer: smallint, float_: float, integer_list: array<integer>, "
            "tiny_integer_list: array<tinyint>, small_integer_list: array<smallint>, string_: string, "
            "int_list: array<int>, boolean_list: array<boolean>, double_list: array<double>, "
            "big_integer_list: array<bigint>, string_list: array<string>, float_list: array<float>>"
        >>
    ),

    ?assertEqual(undefined, apply_encoder(<<"int_">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"integer_">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"boolean_">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"float_">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"double_">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"small_integer">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"tiny_integer">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"big_integer">>, <<"null">>, Schema)),
    ?assertEqual(undefined, apply_encoder(<<"string_">>, <<"null">>, Schema)),
    ?assertEqual([undefined], apply_list_encoder(<<"int_list">>, [<<"null">>], Schema)),
    ?assertEqual([undefined], apply_list_encoder(<<"integer_list">>, [<<"null">>], Schema)),
    ?assertEqual([undefined], apply_list_encoder(<<"tiny_integer_list">>, [<<"null">>], Schema)),
    ?assertEqual([undefined], apply_list_encoder(<<"small_integer_list">>, [<<"null">>], Schema)),
    ?assertEqual(
        [undefined, undefined],
        apply_list_encoder(<<"boolean_list">>, [<<"null">>, <<"null">>], Schema)
    ),
    ?assertEqual([undefined], apply_list_encoder(<<"float_list">>, [<<"null">>], Schema)),
    ?assertEqual([undefined], apply_list_encoder(<<"double_list">>, [<<"null">>], Schema)),
    ?assertEqual([undefined], apply_list_encoder(<<"big_integer_list">>, [<<"null">>], Schema)),
    ?assertEqual([undefined], apply_list_encoder(<<"string_list">>, [<<"null">>], Schema)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

apply_encoder(Field, Value, Schema) ->
    case maps:get(Field, Schema) of
        {map, KeyEncoder, ValueEncoder} ->
            {K, V} = Value,
            {KeyEncoder(K, ?SOME_OPTIONS), ValueEncoder(V, ?SOME_OPTIONS)};
        Encoder ->
            Encoder(Value, ?SOME_OPTIONS)
    end.

apply_list_encoder(Field, Value, Schema) ->
    Encoder = maps:get(Field, Schema),
    [Encoder(V, ?SOME_OPTIONS) || V <- Value].
