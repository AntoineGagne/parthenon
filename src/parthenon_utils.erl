-module(parthenon_utils).

%% API
-export([
    trim/1,
    lightweight_trim/1
]).

-compile(
    {inline, [
        lightweight_trim/1,
        trim/1
    ]}
).

%%%===================================================================
%%% API
%%%===================================================================

trim(Binary) ->
    TrimmedLeft = trim_left(Binary),
    ReversedLeft = reverse(TrimmedLeft),
    TrimmedRight = trim_left(ReversedLeft),
    reverse(TrimmedRight).

lightweight_trim(Binary) ->
    do_lightweight_trim(Binary, <<>>).

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

do_lightweight_trim(<<$\n, Rest/binary>>, Buffer) ->
    do_lightweight_trim(Rest, Buffer);
do_lightweight_trim(<<$\t, Rest/binary>>, Buffer) ->
    do_lightweight_trim(Rest, Buffer);
do_lightweight_trim(<<$\s, Rest/binary>>, Buffer) ->
    do_lightweight_trim(Rest, Buffer);
do_lightweight_trim(<<$\r, Rest/binary>>, Buffer) ->
    do_lightweight_trim(Rest, Buffer);
do_lightweight_trim(<<Character, Rest/binary>>, Buffer) ->
    do_lightweight_trim(Rest, <<Buffer/binary, Character>>);
do_lightweight_trim(<<>>, Buffer) ->
    Buffer.
