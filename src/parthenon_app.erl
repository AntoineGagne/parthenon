-module(parthenon_app).

-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    parthenon_sup:start_link().

-spec stop(State :: term()) -> ok.
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
