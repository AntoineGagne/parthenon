-module(parthenon).

-export([add_schema/2]).

%%%===================================================================
%%% API
%%%===================================================================

-spec add_schema(SchemaName :: atom(), RawSchema :: binary() | string()) -> ok | {error, term()}.
add_schema(SchemaName, RawSchema) ->
    parthenon_schema_server:add_schema(SchemaName, RawSchema).

%%%===================================================================
%%% Internal functions
%%%===================================================================
