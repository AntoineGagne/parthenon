-module(parthenon_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

init([]) ->
    {ok,
        {
            #{
                strategy => one_for_one,
                intensity => 5,
                period => 10
            },
            [
                #{
                    id => parthenon_schema_server,
                    start => {parthenon_schema_server, start_link, []},
                    restart => permanent,
                    shutdown => 5000,
                    type => worker,
                    modules => [parthenon_schema_server]
                }
            ]
        }}.
