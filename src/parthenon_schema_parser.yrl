Nonterminals schema struct_root struct_list struct_list_elements struct_element encoder.

Terminals '<' '>' ',' ':' 'struct' 'array' word encoding.

Rootsymbol schema.

schema -> struct_root : '$1'.

struct_root -> 'struct' struct_list : maps:from_list('$2').

struct_list -> '<' struct_list_elements '>' : '$2'.

struct_list_elements ->  struct_element : [ '$1' ].

struct_list_elements -> struct_element ',' struct_list_elements : [ '$1' | '$3' ].

struct_element -> word ':' encoder : mapping('$1', '$3').

encoder -> encoding : create_encoder('$1').

encoder -> 'array' '<' encoder '>' : create_encoder({list, '$3'}).

encoder -> struct_root : '$1'.

Erlang code.

mapping(Identifier, Encoder) ->
    {extract_identifier(Identifier), Encoder}.

extract_identifier({word, _Line, Name}) ->
    Name.

create_encoder({encoding, _Line, Encoding}) ->
    Encoder = to_encoder(Encoding),
    with_null_as_undefined(Encoder);
create_encoder({list, Encoder}) when is_map(Encoder) ->
    {map_array, Encoder};
create_encoder({list, Encoder}) ->
    ListEncoder = fun(List) -> lists:map(Encoder, List) end,
    with_null_as_undefined(ListEncoder);
create_encoder(Unknown) ->
    throw({unknown_encoding, Unknown}).

with_null_as_undefined(F) ->
    fun
        (<<"null">>) -> undefined;
        (Other) -> F(Other)
    end.

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
