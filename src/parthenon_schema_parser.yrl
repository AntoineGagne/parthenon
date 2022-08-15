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

-include("parthenon.hrl").

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
        (<<"null">>, #schema_options{null_as = NullAlias}) -> NullAlias;
        (Other, Options) -> F(Other, Options)
    end.

with_trim(F) ->
    fun(Binary, Options) ->
        F(parthenon_utils:trim(Binary), Options)
    end.

with_lightweight_trim(F) ->
    fun(Binary, Options) ->
        F(parthenon_utils:lightweight_trim(Binary), Options)
    end.

to_encoder(int) ->
    fun(Value, _Options) -> binary_to_integer(Value) end;
to_encoder(bigint) ->
    fun(Value, _Options) -> binary_to_integer(Value) end;
to_encoder(double) ->
    fun(Value, _Options) -> binary_to_float(Value) end;
to_encoder(boolean) ->
    fun binary_to_boolean/2;
to_encoder(string) ->
    fun(Value, _Options) -> identity(Value) end;
to_encoder(Unknown) ->
    throw({unknown_encoding, Unknown}).

binary_to_boolean(<<"true">>, _Options) ->
    true;
binary_to_boolean(<<"false">>, _Options) ->
    false;
binary_to_boolean(Invalid, _Options) ->
    throw({conversion, {boolean, Invalid}}).

identity(X) ->
    X.
