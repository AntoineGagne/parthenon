-module(parthenon_schema).

-include("parthenon.hrl").

%% API
-export([create/1]).

-type schema() :: schema_value().
-type schema_object() :: #{schema_key() := schema_value()}.
-type schema_key() :: binary().
-type schema_map() :: {map, encoder(), schema()}.
-type schema_value() ::
    encoder() | schema_object() | {map_array, schema()} | schema_map().
-type encoder() :: fun((binary(), schema_options()) -> supported_types()).
-type supported_types() ::
    undefined | primitive_types() | [supported_types()].
-type primitive_types() :: integer() | float() | binary() | boolean().

-export_type([
    schema/0,
    schema_object/0,
    schema_map/0,
    schema_key/0,
    schema_value/0,
    encoder/0,
    supported_types/0,
    primitive_types/0
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
ensure_string(String) when is_list(String) ->
    String.
