-module(parthenon_decode).

%% API
-export([
    try_decode/2,
    try_decode/3
]).

-record(decode_options, {
    key_format = existing_atom :: key_format(),
    object_format = maps :: object_format()
}).

-type decode_options() :: #decode_options{}.
-type option() ::
    {object_format, object_format()}
    | {key_format, key_format()}.
-type object_format() :: maps | proplists | tuple.
-type key_format() :: existing_atom | atom | binary.

-export_type([option/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec try_decode(SchemaName :: atom(), Binary :: binary()) -> {ok, term()} | {error, term()}.
try_decode(SchemaName, Binary) ->
    try_decode(SchemaName, Binary, []).

-spec try_decode(SchemaName :: atom(), Binary :: binary(), Options :: [option()]) ->
    {ok, term()} | {error, term()}.
try_decode(SchemaName, Binary, RawOptions) ->
    try
        Options = options(RawOptions),
        {Object, _Rest} = decode(SchemaName, Binary, Options),
        {ok, Object}
    catch
        _:E:S ->
            {error, {E, S}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

decode(SchemaName, Binary, Options) ->
    case parthenon_schema_server:get_schema(SchemaName) of
        {ok, Schema} ->
            do_decode(Binary, Schema, Options);
        {error, _} = Error ->
            Error
    end.

do_decode(<<${, Rest/binary>>, Schema, Options) ->
    object(Rest, key, <<>>, make_object(Options), Schema, Options);
do_decode(<<Invalid, _Rest/binary>>, _Schema, _Options) ->
    throw({invalid_character, Invalid, 1}).

object(<<$=, Rest/binary>>, key, Buffer, Object, Schema, Options) ->
    Key = to_key(Buffer),
    object(Rest, {value, Key, undefined}, <<>>, Object, Schema, Options);
object(<<$=, Rest/binary>>, {value, Key, undefined}, Buffer, Object, Schema, Options) ->
    object(Rest, {value, Key, undefined}, <<Buffer/binary, $=>>, Object, Schema, Options);
object(<<$=, Rest/binary>>, {value, Key, LastComma}, Buffer, Object, Schema, Options) ->
    Encoder = wrap_encoder(maps:get(Key, Schema, fun identity/1)),
    Value = binary:part(Buffer, 0, LastComma - 1),
    NewKey = to_key(
        binary:part(Buffer, LastComma, byte_size(Buffer) - LastComma)
    ),
    NewObject = update_object(Key, Encoder(Value), Object, Options),
    object(Rest, {value, NewKey, undefined}, <<>>, NewObject, Schema, Options);
object(<<$[, Rest/binary>>, {value, Key, _}, <<>>, Object, Schema, Options) ->
    Encoder = maps:get(Key, Schema, fun identity/1),
    {List, Rest2} = list(Rest, Encoder, Options),
    NewObject = update_object(Key, List, Object, Options),
    Rest3 = maybe_consume_character(Rest2, $,),
    object(Rest3, key, <<>>, NewObject, Schema, Options);
object(<<${, Rest/binary>>, {value, Key, _}, <<>>, Object, Schema, Options) ->
    Encoder = maps:get(Key, Schema, #{}),
    {SubObject, Rest2} = object(Rest, key, <<>>, make_object(Options), Encoder, Options),
    NewObject = update_object(Key, SubObject, Object, Options),
    Rest3 = maybe_consume_character(Rest2, $,),
    object(Rest3, key, <<>>, NewObject, Schema, Options);
object(<<$,, Rest/binary>>, {value, Key, _}, Buffer, Object, Schema, Options) ->
    NewState = {value, Key, byte_size(Buffer) + 1},
    object(Rest, NewState, <<Buffer/binary, $,>>, Object, Schema, Options);
object(<<$}, Rest/binary>>, {value, Key, _}, Buffer, Object, Schema, Options) ->
    Encoder = wrap_encoder(maps:get(Key, Schema, fun identity/1)),
    {update_object(Key, Encoder(Buffer), Object, Options), Rest};
object(<<$}, Rest/binary>>, key, _Buffer, Object, _Schema, _Options) ->
    {Object, Rest};
object(<<Character, Rest/binary>>, key, Buffer, Object, Schema, Options) ->
    object(Rest, key, <<Buffer/binary, Character>>, Object, Schema, Options);
object(<<Character, Rest/binary>>, {value, Key, LastComma}, Buffer, Object, Schema, Options) ->
    object(Rest, {value, Key, LastComma}, <<Buffer/binary, Character>>, Object, Schema, Options).

list(Binary, Encoder, Options) ->
    do_list(Binary, [], <<>>, Encoder, Options).

do_list(<<$], Rest/binary>>, List, _Buffer, {map_array, _}, _Options) ->
    {lists:reverse(List), Rest};
do_list(<<$], Rest/binary>>, List, Buffer, Encoder, _Options) ->
    NewList = lists:reverse([Buffer | List]),
    {Encoder(NewList), Rest};
do_list(<<${, Rest/binary>>, List, _Buffer, {map_array, Encoder}, Options) ->
    {Object, Rest2} = object(Rest, key, <<>>, make_object(Options), Encoder, Options),
    do_list(Rest2, [Object | List], <<>>, {map_array, Encoder}, Options);
do_list(<<$,, Rest/binary>>, List, Buffer, Encoder = {map_array, _}, Options) ->
    do_list(Rest, List, Buffer, Encoder, Options);
do_list(<<$,, Rest/binary>>, List, Buffer, Encoder, Options) ->
    do_list(Rest, [Buffer | List], <<>>, Encoder, Options);
do_list(<<Character, Rest/binary>>, List, Buffer, Encoder, Options) ->
    do_list(Rest, List, <<Buffer/binary, Character>>, Encoder, Options).

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
    case unicode:characters_to_binary(Trimmed) of
        {error, _, _} ->
            Raw;
        {incomplete, _, _} ->
            Raw;
        Binary ->
            try_binary_to_existing_atom(Binary)
    end.

-spec try_binary_to_existing_atom(Binary :: binary()) -> atom() | binary().
try_binary_to_existing_atom(Binary) ->
    try
        binary_to_existing_atom(Binary, utf8)
    catch
        _:_:_ ->
            Binary
    end.

make_object(#decode_options{object_format = maps}) ->
    #{};
make_object(#decode_options{object_format = proplists}) ->
    [];
make_object(#decode_options{object_format = tuple}) ->
    {[]}.

update_object(Key, Value, Object, Options = #decode_options{object_format = maps}) ->
    Object#{to_key(Key, Options) => Value};
update_object(Key, Value, Object, Options = #decode_options{object_format = proplists}) ->
    [{to_key(Key, Options), Value} | Object];
update_object(Key, Value, {Object}, Options = #decode_options{object_format = tuple}) ->
    {[{to_key(Key, Options), Value} | Object]}.

to_key(Key, #decode_options{key_format = atom}) when is_binary(Key) ->
    binary_to_atom(Key, utf8);
to_key(Key, #decode_options{key_format = binary}) when is_atom(Key) ->
    atom_to_binary(Key);
to_key(Key, _) ->
    Key.

-spec identity(X) -> X.
identity(X) ->
    X.

-spec options([option()]) -> decode_options().
options(RawOptions) ->
    Defaults = #decode_options{},
    lists:foldl(fun do_options/2, Defaults, RawOptions).

-spec do_options(option(), decode_options()) -> decode_options().
do_options({object_format, Format}, Options) ->
    Options#decode_options{object_format = object_format(Format)};
do_options({key_format, Format}, Options) ->
    Options#decode_options{key_format = key_format(Format)}.

-spec object_format(object_format() | term()) -> object_format().
object_format(Format = maps) ->
    Format;
object_format(Format = proplists) ->
    Format;
object_format(Format = tuple) ->
    Format;
object_format(InvalidFormat) ->
    throw({invalid_option_value, {object_format, InvalidFormat}}).

-spec key_format(key_format() | term()) -> key_format().
key_format(Format = existing_atom) ->
    Format;
key_format(Format = atom) ->
    Format;
key_format(Format = binary) ->
    Format;
key_format(InvalidFormat) ->
    throw({invalid_option_value, {key_format, InvalidFormat}}).
