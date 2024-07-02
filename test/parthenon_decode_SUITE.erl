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
-define(A_THIRD_SCHEMA_NAME, schema_3).
-define(A_FOURTH_SCHEMA_NAME, schema_4).
-define(A_FIFTH_SCHEMA_NAME, schema_5).
-define(A_SIXTH_SCHEMA_NAME, schema_6).
-define(A_SEVENTH_SCHEMA_NAME, schema_7).
-define(AN_EIGHT_SCHEMA_NAME, schema_8).
-define(A_NINTH_SCHEMA_NAME, schema_9).
-define(ANOTHER_SCHEMA,
    <<"struct<a: int, b: string, c: string, d: int>">>
).
-define(A_THIRD_SCHEMA, <<"array<int>">>).
-define(A_FOURTH_SCHEMA, <<"array<struct<a: int, b: string>>">>).
-define(A_FIFTH_SCHEMA, <<"struct<a: int, b: array<struct<c: int, d: string>>>">>).
-define(A_SIXTH_SCHEMA, <<"struct<a: map<boolean, struct<b: array<int>>>>">>).
-define(A_SEVENTH_SCHEMA, <<"struct<a: map<int, boolean>, b: map<string, string>>">>).
-define(AN_EIGHT_SCHEMA, <<"struct<a: array<map<int, boolean>>>">>).
-define(A_NINTH_SCHEMA, <<"array<array<int>>">>).

all() ->
    [
        {group, all}
    ].

init_per_suite(Config) ->
    {ok, Pid} = parthenon_schema_server:start_link(),
    unlink(Pid),
    ok = parthenon_schema_server:add_schema(?A_SCHEMA_NAME, ?A_SCHEMA),
    ok = parthenon_schema_server:add_schema(?ANOTHER_SCHEMA_NAME, ?ANOTHER_SCHEMA),
    ok = parthenon_schema_server:add_schema(?A_THIRD_SCHEMA_NAME, ?A_THIRD_SCHEMA),
    ok = parthenon_schema_server:add_schema(?A_FOURTH_SCHEMA_NAME, ?A_FOURTH_SCHEMA),
    ok = parthenon_schema_server:add_schema(?A_FIFTH_SCHEMA_NAME, ?A_FIFTH_SCHEMA),
    ok = parthenon_schema_server:add_schema(?A_FIFTH_SCHEMA_NAME, ?A_FIFTH_SCHEMA),
    ok = parthenon_schema_server:add_schema(?A_SIXTH_SCHEMA_NAME, ?A_SIXTH_SCHEMA),
    ok = parthenon_schema_server:add_schema(?A_SEVENTH_SCHEMA_NAME, ?A_SEVENTH_SCHEMA),
    ok = parthenon_schema_server:add_schema(?AN_EIGHT_SCHEMA_NAME, ?AN_EIGHT_SCHEMA),
    ok = parthenon_schema_server:add_schema(?A_NINTH_SCHEMA_NAME, ?A_NINTH_SCHEMA),
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
            can_decode_object_to_proplists,
            can_decode_object_to_tuple,
            can_handle_spaces_in_values,
            can_convert_keys_to_atom,
            can_convert_keys_to_binary,
            can_decode_top_level_list,
            can_decode_empty_list,
            can_decode_top_level_struct_list,
            can_handle_struct_ending_with_array_with_nested_struct,
            can_specify_different_null_values,
            can_decode_map,
            can_decode_struct_with_map_from_boolean_to_struct_with_list,
            can_decode_list_containing_maps,
            can_decode_nested_lists,
            can_decode_nested_empty_lists,
            can_decode_nested_lists_with_empty_lists,
            can_decode_nested_lists_with_nulls,
            can_decode_nested_lists_with_only_nulls
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

can_decode_object_to_tuple(_Config) ->
    ?assertEqual(
        {ok,
            {[
                {e, [456, 789]},
                {c, {[{f, [<<"foo bar">>, <<"baz bar">>]}, {d, 1011}]}},
                {b, <<"foo bar">>},
                {a, 123}
            ]}},
        parthenon_decode:try_decode(
            ?A_SCHEMA_NAME,
            <<"{a=123,b=foo bar,c={d=1011,f=[foo bar,baz bar]},e=[456,789]}">>,
            [{object_format, tuple}]
        )
    ).

can_handle_spaces_in_values(_Config) ->
    ?assertEqual(
        {ok, [
            {e, [456, 789]},
            {c, [{f, [<<"foo bar">>, <<"baz bar">>]}, {d, 1011}]},
            {b, <<"foo bar">>},
            {a, 123}
        ]},
        parthenon_decode:try_decode(
            ?A_SCHEMA_NAME,
            <<"{\n\ta = 123,\n\tb = foo bar ,\n\t c = {\n\t\td = 1011 ,\n\t\tf = [ foo bar,\n baz bar ] } ,\n\t e = [ 456, 789 ] } ">>,
            [{object_format, proplists}]
        )
    ).

can_convert_keys_to_atom(_Config) ->
    ?assertEqual(
        {ok, #{
            a => 123,
            b => <<"foo bar">>,
            c => #{d => 1011, f => [<<"foo bar">>, <<"baz bar">>], unknown_key_1 => <<"baba">>},
            e => [456, 789],
            unknown_key_1 => [<<"ba">>, <<"ba">>],
            unknown_object_1 => #{
                non_existing_key_1 => <<"test">>,
                non_existing_other_1 => [<<"1">>, <<"2">>, <<"3">>]
            }
        }},
        parthenon_decode:try_decode(
            ?A_SCHEMA_NAME,
            <<"{a=123,b=foo bar,c={d=1011,f=[foo bar,baz bar],unknown_key_1=baba},e=[456,789],unknown_key_1=[ba,ba],unknown_object_1={non_existing_key_1=test,non_existing_other_1=[1,2,3]}}">>,
            [{key_format, atom}]
        )
    ).

can_convert_keys_to_binary(_Config) ->
    ?assertEqual(
        {ok, #{
            <<"a">> => 123,
            <<"b">> => <<"foo bar">>,
            <<"c">> => #{
                <<"d">> => 1011,
                <<"f">> => [<<"foo bar">>, <<"baz bar">>],
                <<"unknown_key">> => <<"baba">>
            },
            <<"e">> => [456, 789],
            <<"unknown_key">> => [<<"ba">>, <<"ba">>],
            <<"unknown_object">> => #{
                <<"non_existing_key">> => <<"test">>,
                <<"non_existing_other">> => [<<"1">>, <<"2">>, <<"3">>]
            }
        }},
        parthenon_decode:try_decode(
            ?A_SCHEMA_NAME,
            <<"{a=123,b=foo bar,c={d=1011,f=[foo bar,baz bar],unknown_key=baba},e=[456,789],unknown_key=[ba,ba],unknown_object={non_existing_key=test,non_existing_other=[1,2,3]}}">>,
            [{key_format, binary}]
        )
    ).

can_decode_top_level_list(_Config) ->
    ?assertEqual(
        {ok, [1, 2, 3, 4]}, parthenon_decode:try_decode(?A_THIRD_SCHEMA_NAME, <<"[1, 2, 3, 4]">>)
    ).

can_decode_empty_list(_Config) ->
    ?assertEqual(
        {ok, []}, parthenon_decode:try_decode(?A_THIRD_SCHEMA_NAME, <<"[]">>)
    ).

can_decode_top_level_struct_list(_Config) ->
    ?assertEqual(
        {ok, [#{a => 1, b => <<"foo">>}, #{a => 4, b => <<"bar">>}]},
        parthenon_decode:try_decode(?A_FOURTH_SCHEMA_NAME, <<"[{a=1, b=foo}, {a=4, b=bar}]">>)
    ).

can_handle_struct_ending_with_array_with_nested_struct(_Config) ->
    ?assertEqual(
        {ok, #{a => 123, b => [#{c => 456, d => <<"Some test, some test">>}]}},
        parthenon_decode:try_decode(
            ?A_FIFTH_SCHEMA_NAME, <<"{a=123, b=[{c=456, d=Some test, some test}]}">>
        )
    ).

can_specify_different_null_values(_Config) ->
    ?assertEqual(
        {ok, #{a => null, b => [#{c => null, d => null}]}},
        parthenon_decode:try_decode(
            ?A_FIFTH_SCHEMA_NAME, <<"{a=null, b=[{c=null, d=null}]}">>, [
                {schema_options, [{null_as, null}]}
            ]
        )
    ).

can_decode_map(_Config) ->
    ?assertEqual(
        {ok, #{
            a => #{1 => true, 2 => false, 3 => null},
            b => #{
                <<"test_42">> => <<"bar">>,
                <<"application/json">> => <<"test=1">>,
                <<"test_43">> => null
            }
        }},
        parthenon_decode:try_decode(
            ?A_SEVENTH_SCHEMA_NAME,
            <<"{a={1=true, 2=false, 3=null}, b={test_42=bar, application/json=test=1, test_43=null}}">>,
            [
                {schema_options, [{null_as, null}]}
            ]
        )
    ).

can_decode_struct_with_map_from_boolean_to_struct_with_list(_Config) ->
    ?assertEqual(
        {ok, #{a => #{true => #{b => [1, null, 2]}, false => #{b => [3, 4, 5]}}}},
        parthenon_decode:try_decode(
            ?A_SIXTH_SCHEMA_NAME, <<"{a={true={b=[1, null, 2]}, false={b=[3, 4, 5]}}}">>, [
                {schema_options, [{null_as, null}]}
            ]
        )
    ).

can_decode_list_containing_maps(_Config) ->
    ?assertEqual(
        {ok, #{a => [#{1 => true, 2 => false}, #{3 => null, 4 => false}]}},
        parthenon_decode:try_decode(
            ?AN_EIGHT_SCHEMA_NAME, <<"{a=[{1=true, 2=false}, {3=null, 4=false}]}">>, [
                {schema_options, [{null_as, null}]}
            ]
        )
    ).

can_decode_nested_lists(_Config) ->
    ?assertEqual(
        {ok, [[1, 2, 3], [4, 5, 6]]},
        parthenon_decode:try_decode(
            ?A_NINTH_SCHEMA_NAME, <<"[[1, 2, 3], [4, 5, 6]]">>, [
                {schema_options, [{null_as, null}]}
            ]
        )
    ).

can_decode_nested_empty_lists(_Config) ->
    ?assertEqual(
        {ok, [[], []]},
        parthenon_decode:try_decode(
            ?A_NINTH_SCHEMA_NAME, <<"[[], []]">>, [
                {schema_options, [{null_as, null}]}
            ]
        )
    ).

can_decode_nested_lists_with_empty_lists(_Config) ->
    ?assertEqual(
        {ok, [[1, 2, 3], [], [4, 5, 6]]},
        parthenon_decode:try_decode(
            ?A_NINTH_SCHEMA_NAME, <<"[[1, 2, 3], [], [4, 5, 6]]">>, [
                {schema_options, [{null_as, null}]}
            ]
        )
    ).

can_decode_nested_lists_with_nulls(_Config) ->
    ?assertEqual(
        {ok, [[1, 2, 3], null, [4, 5, 6]]},
        parthenon_decode:try_decode(
            ?A_NINTH_SCHEMA_NAME, <<"[[1, 2, 3], null, [4, 5, 6]]">>, [
                {schema_options, [{null_as, null}]}
            ]
        )
    ).

can_decode_nested_lists_with_only_nulls(_Config) ->
    ?assertEqual(
        {ok, [null, null, null]},
        parthenon_decode:try_decode(
            ?A_NINTH_SCHEMA_NAME, <<"[null, null, null]">>, [
                {schema_options, [{null_as, null}]}
            ]
        )
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
