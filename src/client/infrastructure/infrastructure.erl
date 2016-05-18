-module(infrastructure).
-include("infrastructure_state.hrl").

-behaviour(gen_server).
-export([ start_link/1
        , stop/1
        , state/1
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

%% TODO
%% Design instructions data model
%% Add api for citizens to use, i.e. {get_route, From, To}
%% Add api for vehicles to use, i.e. {get_line, StartStop}
%% Implement pathfinding algorithm. Includes quering lines
%% for stop existence and getting the duration of routes.

%% Public API

start_link(Lines) ->
  gen_server:start_link(?MODULE, [Lines], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

init([Lines]) ->
  gen_server:call({subscribe, time}),
  {ok, #infrastructure_state{lines=Lines}}.


handle_call(stop, _From, Subscriptions) ->
  {stop, normal, stopped, Subscriptions}.

handle_cast(_, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
