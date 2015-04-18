-module(clock).
-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link(Delta) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [Delta], []).

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state(?MODULE).

init([Delta]) ->
  gen_server:cast(?MODULE, tick),
  {ok, {Delta, 62167219200}}. % Epoch in gregorian seconds


handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast(tick, {Delta, GregorianSeconds}) ->
  timer:sleep(5000),
  {noreply, {Delta, GregorianSeconds+Delta}}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
