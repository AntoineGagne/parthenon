-module(parthenon).

-export([
    add_schema/2,
    decode/2
]).

%%%===================================================================
%%% API
%%%===================================================================

-spec add_schema(SchemaName :: atom(), RawSchema :: binary() | string()) -> ok | {error, term()}.
add_schema(SchemaName, RawSchema) ->
    parthenon_schema_server:add_schema(SchemaName, RawSchema).

-spec decode(SchemaName :: atom(), Binary :: binary()) -> {ok, term()} | {error, term()}.
decode(SchemaName, Binary) ->
    parthenon_decode:try_decode(SchemaName, Binary).

%%%===================================================================
%%% Internal functions
%%%===================================================================
