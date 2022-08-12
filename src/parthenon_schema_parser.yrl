Nonterminals schema struct_root struct_list struct_list_elements struct_element list encoder.

Terminals '<' '>' ',' ':' 'struct' 'array' word encoding.

Rootsymbol schema.

schema -> struct_root : '$1'.
schema -> list : '$1'.

struct_root -> 'struct' struct_list : maps:from_list('$2').

struct_list -> '<' struct_list_elements '>' : '$2'.

struct_list_elements ->  struct_element : [ '$1' ].

struct_list_elements -> struct_element ',' struct_list_elements : [ '$1' | '$3' ].

struct_element -> word ':' encoder : mapping('$1', '$3').

encoder -> encoding : create_encoder('$1').

encoder -> list : '$1'.

encoder -> struct_root : '$1'.

list -> 'array' '<' encoder '>' : create_encoder({list, '$3'}).

Erlang code.

-compile(
    {inline, [
        lightweight_trim/1,
        do_lightweight_trim/2
    ]}
).

mapping(Identifier, Encoder) ->
    {extract_identifier(Identifier), Encoder}.

extract_identifier({word, _Line, Name}) ->
    Name.

create_encoder({encoding, _Line, string}) ->
    Encoder = to_encoder(string),
    with_null_as_undefined(with_trim(Encoder));
create_encoder({encoding, _Line, Encoding}) ->
    Encoder = to_encoder(Encoding),
    with_null_as_undefined(with_lightweight_trim(Encoder));
create_encoder({list, Encoder}) when is_map(Encoder) ->
    {map_array, Encoder};
create_encoder({list, Encoder}) ->
    with_null_as_undefined(Encoder);
create_encoder(Unknown) ->
    throw({unknown_encoding, Unknown}).

with_null_as_undefined(F) ->
    fun
        (<<"null">>) -> undefined;
        (Other) -> F(Other)
    end.

with_trim(F) ->
    fun(Binary) ->
        F(parthenon_utils:trim(Binary))
    end.

with_lightweight_trim(F) ->
    fun(Binary) ->
        F(lightweight_trim(Binary))
    end.

lightweight_trim(Binary) ->
    do_lightweight_trim(Binary, <<>>).

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

to_encoder(int) ->
    fun binary_to_integer/1;
to_encoder(bigint) ->
    fun binary_to_integer/1;
to_encoder(double) ->
    fun binary_to_float/1;
to_encoder(boolean) ->
    fun binary_to_boolean/1;
to_encoder(string) ->
    fun identity/1;
to_encoder(Unknown) ->
    throw({unknown_encoding, Unknown}).

binary_to_boolean(<<"true">>) ->
    true;
binary_to_boolean(<<"false">>) ->
    false;
binary_to_boolean(Invalid) ->
    throw({conversion, {boolean, Invalid}}).

identity(X) ->
    X.
