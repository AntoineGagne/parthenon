-module(parthenon_schema).

%% API
-export([create/1]).

-type schema() :: encoder() | schema_object().
-type schema_object() :: #{schema_key() := schema_value()}.
-type schema_key() :: atom().
-type schema_value() :: encoder() | schema_object() | {map_array, schema()}.
-type encoder() :: fun((binary()) -> supported_types()).
-type supported_types() ::
    undefined | integer() | float() | binary() | boolean() | [supported_types()].

-export_type([
    schema/0,
    schema_key/0,
    schema_value/0,
    encoder/0,
    supported_types/0
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec create(Schema :: binary() | string()) -> {ok, schema()} | {error, term()}.
create(RawSchema) ->
    Schema = ensure_string(RawSchema),
    try parthenon_schema_lexer:string(Schema) of
        {ok, Tokens, _} ->
            parthenon_schema_parser:parse(Tokens);
        {error, Reason, Line} ->
            {error, {Reason, Line}}
    catch
        throw:Error ->
            {error, Error}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-spec ensure_string(binary() | string()) -> string().
ensure_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary);
ensure_string(String) ->
    String.
