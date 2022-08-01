-module(parthenon).

-export([
    add_schema/2,
    decode/2,
    decode/3
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

-spec decode(SchemaName :: atom(), Binary :: binary(), Options :: [parthenon_decode:option()]) ->
    {ok, term()} | {error, term()}.
decode(SchemaName, Binary, Options) ->
    parthenon_decode:try_decode(SchemaName, Binary, Options).

%%%===================================================================
%%% Internal functions
%%%===================================================================
