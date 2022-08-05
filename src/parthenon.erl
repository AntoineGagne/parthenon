%% @author Antoine Gagné <gagnantoine@gmail.com>
%% @copyright 2022 Antoine Gagné
%% @doc Parse Athena structures and schemas.
-module(parthenon).

-export([
    add_schema/2,
    decode/2,
    decode/3,
    decode_with_schema/3
]).

-type decode_options() :: [parthenon_decode:option()].

%%%===================================================================
%%% API
%%%===================================================================

-spec add_schema(SchemaName :: atom(), RawSchema :: binary() | string()) -> ok | {error, term()}.
%% @doc
%% Parse the provided schema and register it in the list of available schemas
%% as the specified name.
%% @end
add_schema(SchemaName, RawSchema) ->
    parthenon_schema_server:add_schema(SchemaName, RawSchema).

-spec decode(SchemaName :: atom(), Binary :: binary()) -> {ok, term()} | {error, term()}.
%% @doc
%% Parse the raw Athena structure with the specified schema.
%% @end
decode(SchemaName, Binary) ->
    parthenon_decode:try_decode(SchemaName, Binary, []).

-spec decode(SchemaName :: atom(), Binary :: binary(), Options :: decode_options()) ->
    {ok, term()} | {error, term()}.
%% @doc
%% Parse the raw Athena structure with the specified schema and apply the
%% specified options.
%%
%% The supported options are:
%%
%% <ul>
%%   <li>`object_format': Specify the format of the objects.
%%      <ul>
%%        <li>`maps' (default): return objects as maps</li>
%%        <li>`proplists': return objects as proplists</li>
%%        <li>`tuple' will return objects in the `{[{<key>, <value>}]}' format</li>
%%      </ul>
%%   </li>
%%   <li>`key_format': Specify the format of the keys.
%%     <ul>
%%       <li>`atom': return the keys as atoms</li>
%%       <li>`binary' will return the keys as binary.</li>
%%       <li>`existing_atom' (default): return the keys as atoms if they
%%       already exists or return them into binary if they do not</li>
%%     </ul>
%%  </li>
%% </ul>
%%
%% Example usage:
%%
%% ```
%% %% Register the `point' schema into the registry
%% ok = parthenon:add_schema(point, <<"struct<x: int, y: int, z: int>">>).
%%
%% %% Decode the `point' structure into a map with binary keys
%% {ok, #{<<"x">> := 3, <<"y">> := 2, <<"z">> := 4}} = parthenon:decode(
%%     point, <<"{x=3, y=2, z=4}">>, [{key_format, binary}, {object_format, maps}]
%% ).
%% '''
%% @end
decode(SchemaName, Binary, Options) ->
    parthenon_decode:try_decode(SchemaName, Binary, Options).

-spec decode_with_schema(Schema :: binary(), Binary :: binary(), Options :: decode_options()) ->
    {ok, term()} | {error, term()}.
%% @doc
%% Parse the raw Athena structure with the provided raw schema.
%%
%% See {@link parthenon:decode/3} for more information about the supported options.
%%
%% Example usage:
%%
%% ```
%% %% Decode the `point' structure into a map with binary keys
%% Schema = <<"struct<x: int, y: int, z: int>">>.
%% {ok, #{<<"x">> := 3, <<"y">> := 2, <<"z">> := 4}} = parthenon:decode_with_schema(
%%     Schema, <<"{x=3, y=2, z=4}">>, [{key_format, binary}, {object_format, maps}]
%% ).
%% '''
%% @end
decode_with_schema(RawSchema, Binary, Options) ->
    case parthenon_schema:create(RawSchema) of
        {ok, Schema} ->
            parthenon_decode:try_decode_with_schema(Schema, Binary, Options);
        {error, _} = Error ->
            Error
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================
