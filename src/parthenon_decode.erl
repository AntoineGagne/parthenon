-module(parthenon_decode).

%% API
-export([
    try_decode/2
]).

%%%===================================================================
%%% API
%%%===================================================================

try_decode(SchemaName, Binary) ->
    try
        {Object, _Rest} = decode(SchemaName, Binary),
        {ok, Object}
    catch
        _:E:S ->
            {error, {E, S}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

decode(SchemaName, Binary) ->
    case parthenon_schema_server:get_schema(SchemaName) of
        {ok, Schema} ->
            do_decode(Binary, Schema);
        {error, _} = Error ->
            Error
    end.

do_decode(<<${, Rest/binary>>, Schema) ->
    object(Rest, key, <<>>, #{}, Schema);
do_decode(<<Invalid, _Rest/binary>>, _Schema) ->
    throw({invalid_character, Invalid, 1}).

object(<<$=, Rest/binary>>, key, Buffer, Object, Schema) ->
    Key = to_key(Buffer),
    object(Rest, {value, Key, undefined}, <<>>, Object, Schema);
object(<<$=, Rest/binary>>, {value, Key, undefined}, Buffer, Object, Schema) ->
    object(Rest, {value, Key, undefined}, <<Buffer/binary, $=>>, Object, Schema);
object(<<$=, Rest/binary>>, {value, Key, LastComma}, Buffer, Object, Schema) ->
    Encoder = wrap_encoder(maps:get(Key, Schema, fun identity/1)),
    Value = binary:part(Buffer, 0, LastComma - 1),
    NewKey = to_key(
        binary:part(Buffer, LastComma, byte_size(Buffer) - LastComma)
    ),
    NewObject = Object#{Key => Encoder(Value)},
    object(Rest, {value, NewKey, undefined}, <<>>, NewObject, Schema);
object(<<$[, Rest/binary>>, {value, Key, _}, <<>>, Object, Schema) ->
    Encoder = maps:get(Key, Schema, fun identity/1),
    {List, Rest2} = list(Rest, Encoder),
    NewObject = Object#{Key => List},
    Rest3 = maybe_consume_character(Rest2, $,),
    object(Rest3, key, <<>>, NewObject, Schema);
object(<<${, Rest/binary>>, {value, Key, _}, <<>>, Object, Schema) ->
    Encoder = maps:get(Key, Schema, #{}),
    {SubObject, Rest2} = object(Rest, key, <<>>, #{}, Encoder),
    NewObject = Object#{Key => SubObject},
    Rest3 = maybe_consume_character(Rest2, $,),
    object(Rest3, key, <<>>, NewObject, Schema);
object(<<$,, Rest/binary>>, {value, Key, _}, Buffer, Object, Schema) ->
    object(Rest, {value, Key, byte_size(Buffer) + 1}, <<Buffer/binary, $,>>, Object, Schema);
object(<<$}, Rest/binary>>, {value, Key, _}, Buffer, Object, Schema) ->
    Encoder = wrap_encoder(maps:get(Key, Schema, fun identity/1)),
    {Object#{Key => Encoder(Buffer)}, Rest};
object(<<$}, Rest/binary>>, key, _Buffer, Object, _Schema) ->
    {Object, Rest};
object(<<Character, Rest/binary>>, key, Buffer, Object, Schema) ->
    object(Rest, key, <<Buffer/binary, Character>>, Object, Schema);
object(<<Character, Rest/binary>>, {value, Key, LastComma}, Buffer, Object, Schema) ->
    object(Rest, {value, Key, LastComma}, <<Buffer/binary, Character>>, Object, Schema).

list(Binary, Encoder) ->
    do_list(Binary, [], <<>>, Encoder).

do_list(<<$], Rest/binary>>, List, _Buffer, {map_array, _}) ->
    {lists:reverse(List), Rest};
do_list(<<$], Rest/binary>>, List, Buffer, Encoder) ->
    NewList = lists:reverse([Buffer | List]),
    {Encoder(NewList), Rest};
do_list(<<${, Rest/binary>>, List, _Buffer, {map_array, Encoder}) ->
    {Object, Rest2} = object(Rest, key, <<>>, #{}, Encoder),
    do_list(Rest2, [Object | List], <<>>, {map_array, Encoder});
do_list(<<$,, Rest/binary>>, List, Buffer, Encoder = {map_array, _}) ->
    do_list(Rest, List, Buffer, Encoder);
do_list(<<$,, Rest/binary>>, List, Buffer, Encoder) ->
    do_list(Rest, [Buffer | List], <<>>, Encoder);
do_list(<<Character, Rest/binary>>, List, Buffer, Encoder) ->
    do_list(Rest, List, <<Buffer/binary, Character>>, Encoder).

maybe_consume_character(<<Character, Rest/binary>>, Character) ->
    Rest;
maybe_consume_character(Binary, _) ->
    Binary.

wrap_encoder(Fun) ->
    fun
        (<<"null">>) ->
            undefined;
        (Value) ->
            Fun(Value)
    end.

to_key(Raw) ->
    Trimmed = string:trim(Raw, both),
    try
        binary_to_existing_atom(Trimmed)
    catch
        _:_:_ ->
            Trimmed
    end.

identity(X) ->
    X.
