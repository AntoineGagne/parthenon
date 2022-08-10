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
        case decode(SchemaName, Binary, Options) of
            {ok, Object} ->
                {ok, Object};
            {error, _} = Error ->
                Error
        end
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
            {ok, do_decode(Binary, Schema, Options)};
        {error, _} = Error ->
            Error
    end.

do_decode(<<${, Rest/binary>>, Schema, Options) ->
    Next = {object_key, make_object(Options), Schema, Options},
    whitespace(Rest, Next, []);
do_decode(<<$[, Rest/binary>>, Schema, Options) ->
    Next = {list, [], Schema, Options},
    whitespace(Rest, Next, []);
do_decode(<<Invalid, _Rest/binary>>, _Schema, _Options) ->
    throw({invalid_character, Invalid, 1}).

object(Binary, Object, Nexts, Schema, Options) ->
    Next = {object_key, Object, Schema, Options},
    whitespace(Binary, Next, Nexts).

object_key(<<$=, Rest/binary>>, Key, Object, Nexts, Schema, Options) ->
    Next = {object_value, trim(Key), Object, Schema, Options},
    whitespace(Rest, Next, Nexts);
object_key(<<$,, Rest/binary>>, _Key, Object, Nexts, Schema, Options) ->
    object_key(Rest, <<>>, Object, Nexts, Schema, Options);
object_key(<<$}, Rest/binary>>, _Buffer, Object, Nexts, _Schema, _Options) ->
    next(Rest, Object, Nexts);
object_key(<<Character, Rest/binary>>, Buffer, Object, Nexts, Schema, Options) ->
    object_key(Rest, <<Buffer/binary, Character>>, Object, Nexts, Schema, Options).

object_value(<<$=, Rest/binary>>, Key, undefined, Buffer, Object, Nexts, Schema, Options) ->
    object_value(Rest, Key, undefined, <<Buffer/binary, $=>>, Object, Nexts, Schema, Options);
object_value(<<$=, Rest/binary>>, Key, LastComma, Buffer, Object, Nexts, Schema, Options) ->
    Encoder = wrap_encoder(maps:get(Key, Schema, fun identity/1)),
    Value = binary:part(Buffer, 0, LastComma - 1),
    NewKey = trim(binary:part(Buffer, LastComma, byte_size(Buffer) - LastComma)),
    NewObject = update_object(Key, Encoder(Value), Object, Options),
    whitespace(Rest, {object_value, NewKey, NewObject, Schema, Options}, Nexts);
object_value(<<$[, Rest/binary>>, Key, _, <<>>, Object, Nexts, Schema, Options) ->
    Encoder = maps:get(Key, Schema, fun identity/1),
    Current = {list, [], Encoder, Options},
    Next = {object, Key, Object, Schema, Options},
    whitespace(Rest, Current, [Next | Nexts]);
object_value(<<${, Rest/binary>>, Key, _, <<>>, Object, Nexts, Schema, Options) ->
    Encoder = maps:get(Key, Schema, #{}),
    Current = {object_key, make_object(Options), Encoder, Options},
    Next = {object, Key, Object, Schema, Options},
    whitespace(Rest, Current, [Next | Nexts]);
object_value(<<$}, Rest/binary>>, Key, undefined, Buffer, Object, Nexts, Schema, Options) ->
    Encoder = wrap_encoder(maps:get(Key, Schema, fun identity/1)),
    NewObject = update_object(Key, Encoder(Buffer), Object, Options),
    next(Rest, NewObject, Nexts);
object_value(<<$,, Rest/binary>>, Key, _, Buffer, Object, Nexts, Schema, Options) ->
    CurrentPosition = byte_size(Buffer) + 1,
    NewBuffer = <<Buffer/binary, $,>>,
    object_value(Rest, Key, CurrentPosition, NewBuffer, Object, Nexts, Schema, Options);
object_value(<<Character, Rest/binary>>, Key, LastComma, Buffer, Object, Nexts, Schema, Options) ->
    NewBuffer = <<Buffer/binary, Character>>,
    object_value(Rest, Key, LastComma, NewBuffer, Object, Nexts, Schema, Options).

list(<<$], Rest/binary>>, List, _Buffer, Nexts, {map_array, _}, _Options) ->
    next(Rest, lists:reverse(List), Nexts);
list(<<$], Rest/binary>>, List, Buffer, Nexts, Encoder, _Options) ->
    NewList = lists:reverse([Buffer | List]),
    next(Rest, Encoder(NewList), Nexts);
list(<<${, Rest/binary>>, List, _Buffer, Nexts, {map_array, Encoder}, Options) ->
    Next = {list, List, {map_array, Encoder}, Options},
    object(Rest, make_object(Options), [Next | Nexts], Encoder, Options);
list(<<$,, Rest/binary>>, List, _Buffer, Nexts, Encoder = {map_array, _}, Options) ->
    Next = {list, List, Encoder, Options},
    whitespace(Rest, Next, Nexts);
list(<<$,, Rest/binary>>, List, Buffer, Nexts, Encoder, Options) ->
    Next = {list, [Buffer | List], Encoder, Options},
    whitespace(Rest, Next, Nexts);
list(<<Character, Rest/binary>>, List, Buffer, Nexts, Encoder, Options) ->
    list(Rest, List, <<Buffer/binary, Character>>, Nexts, Encoder, Options).

next(<<_/binary>>, Value, []) ->
    Value;
next(<<Rest/binary>>, Value, [Next | Nexts]) ->
    case Next of
        {object, Key, Object, Schema, Options} ->
            WithValue = update_object(Key, Value, Object, Options),
            object(Rest, WithValue, Nexts, Schema, Options);
        {list, List, Encoder, Options} ->
            WithValue = [Value | List],
            list(Rest, WithValue, <<>>, Nexts, Encoder, Options)
    end.

whitespace(<<$\n, Rest/binary>>, Next, Nexts) ->
    whitespace(Rest, Next, Nexts);
whitespace(<<$\t, Rest/binary>>, Next, Nexts) ->
    whitespace(Rest, Next, Nexts);
whitespace(<<$\s, Rest/binary>>, Next, Nexts) ->
    whitespace(Rest, Next, Nexts);
whitespace(<<Binary/binary>>, Next, Nexts) ->
    case Next of
        {object_key, Object, Schema, Options} ->
            object_key(Binary, <<>>, Object, Nexts, Schema, Options);
        {object_value, Key, Object, Schema, Options} ->
            object_value(Binary, Key, undefined, <<>>, Object, Nexts, Schema, Options);
        {list, List, Encoder, Options} ->
            list(Binary, List, <<>>, Nexts, Encoder, Options)
    end.

wrap_encoder(Fun) ->
    fun
        (<<"null">>) ->
            undefined;
        (Value) ->
            Fun(Value)
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
to_key(Key, #decode_options{key_format = existing_atom}) when is_binary(Key) ->
    to_existing_atom(Key);
to_key(Key, _) ->
    trim(Key).

to_existing_atom(Raw) ->
    Binary = trim(Raw),
    try_binary_to_existing_atom(Binary).

trim(Raw) ->
    Trimmed = string:trim(Raw, both),
    case unicode:characters_to_binary(Trimmed) of
        {error, R1, R2} ->
            throw({error, {trim, Raw, R1, R2}});
        Error = {incomplete, _, _} ->
            throw({error, {trim, Raw, Error}});
        Binary ->
            Binary
    end.

-spec try_binary_to_existing_atom(Binary :: binary()) -> atom() | binary().
try_binary_to_existing_atom(Binary) ->
    try
        binary_to_existing_atom(Binary, utf8)
    catch
        _:_:_ ->
            Binary
    end.

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
