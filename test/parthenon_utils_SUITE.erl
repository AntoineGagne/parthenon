-module(parthenon_utils_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(nowarn_export_all).
-compile(export_all).

all() ->
    [
        {group, all}
    ].

groups() ->
    [
        {all, [parallel], [
            can_lightweight_trim_empty_binary,
            can_lightweight_trim_single_element_binary,
            can_lightweight_trim_left_spaces,
            can_lightweight_trim_right_spaces,
            can_lightweight_trim_both_side,
            can_trim_empty_binary,
            can_trim_single_element_binary,
            can_trim_left_spaces,
            can_trim_right_spaces,
            can_trim_both_side,
            keep_spaces_in_string_intact
        ]}
    ].

%%%===================================================================
%%% Test cases
%%%===================================================================

can_lightweight_trim_empty_binary(_Config) ->
    ?assertEqual(<<>>, parthenon_utils:lightweight_trim(<<>>)).

can_lightweight_trim_single_element_binary(_Config) ->
    ?assertEqual(<<"1">>, parthenon_utils:lightweight_trim(<<"1">>)).

can_lightweight_trim_left_spaces(_Config) ->
    ?assertEqual(<<"123">>, parthenon_utils:lightweight_trim(<<"\n \t\r123">>)).

can_lightweight_trim_right_spaces(_Config) ->
    ?assertEqual(<<"123">>, parthenon_utils:lightweight_trim(<<"123\n \t\r">>)).

can_lightweight_trim_both_side(_Config) ->
    ?assertEqual(<<"123">>, parthenon_utils:lightweight_trim(<<"\n \t\r123\n \t\r">>)).

can_trim_empty_binary(_Config) ->
    ?assertEqual(<<>>, parthenon_utils:trim(<<>>)).

can_trim_single_element_binary(_Config) ->
    ?assertEqual(<<"1">>, parthenon_utils:trim(<<"1">>)).

can_trim_left_spaces(_Config) ->
    ?assertEqual(<<"123">>, parthenon_utils:trim(<<"\n \t\r123">>)).

can_trim_right_spaces(_Config) ->
    ?assertEqual(<<"123">>, parthenon_utils:trim(<<"123\n \t\r">>)).

can_trim_both_side(_Config) ->
    ?assertEqual(<<"123">>, parthenon_utils:trim(<<"\n \t\r123\n \t\r">>)).

keep_spaces_in_string_intact(_Config) ->
    ?assertEqual(
        <<"foo \t\nbar\r potato">>, parthenon_utils:trim(<<"\n \t\rfoo \t\nbar\r potato\n \t\r">>)
    ).

%%%===================================================================
%%% Internal functions
%%%===================================================================
