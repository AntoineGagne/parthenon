-module(parthenon_schema_server).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    add_schema/2,
    get_schema/1
]).

%% gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2
]).

-record(state, {}).

-define(TABLE_NAME, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec add_schema(SchemaName :: atom(), RawSchema :: binary() | string()) -> ok | {error, term()}.
add_schema(SchemaName, RawSchema) ->
    gen_server:call(?MODULE, {add_schema, SchemaName, RawSchema}).

-spec get_schema(SchemaName :: atom()) -> {ok, parthenon_schema:schema()} | {error, not_found}.
get_schema(SchemaName) ->
    case ets:lookup(?TABLE_NAME, SchemaName) of
        [{_, Schema}] ->
            {ok, Schema};
        [] ->
            {error, not_found}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    ets:new(?TABLE_NAME, [set, protected, {read_concurrency, true}, named_table]),
    {ok, #state{}}.

handle_call({add_schema, SchemaName, RawSchema}, _From, State) ->
    Result =
        case parthenon_schema:create(RawSchema) of
            {ok, Schema} ->
                ets:insert(?TABLE_NAME, {SchemaName, Schema}),
                ok;
            Error = {error, _} ->
                Error
        end,
    {reply, Result, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
