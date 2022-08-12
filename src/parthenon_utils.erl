-module(parthenon_utils).

%% API
-export([trim/1]).

%%%===================================================================
%%% API
%%%===================================================================

trim(Binary) ->
    TrimmedLeft = trim_left(Binary),
    ReversedLeft = reverse(TrimmedLeft),
    TrimmedRight = trim_left(ReversedLeft),
    reverse(TrimmedRight).

%%%===================================================================
%%% Internal functions
%%%===================================================================

trim_left(<<$\n, Rest/binary>>) ->
    trim_left(Rest);
trim_left(<<$\t, Rest/binary>>) ->
    trim_left(Rest);
trim_left(<<$\s, Rest/binary>>) ->
    trim_left(Rest);
trim_left(<<$\r, Rest/binary>>) ->
    trim_left(Rest);
trim_left(Rest) ->
    Rest.

%% Taken from: https://stackoverflow.com/a/43310493
-spec reverse(binary()) -> binary().
reverse(Binary) ->
    Size = bit_size(Binary),
    <<X:Size/integer-little>> = Binary,
    <<X:Size/integer-big>>.
