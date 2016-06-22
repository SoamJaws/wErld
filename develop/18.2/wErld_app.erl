-module(wErld_app).
-ifndef(EUNIT).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    wErld_sup:start_link().

stop(_State) ->
    ok.

-endif.
