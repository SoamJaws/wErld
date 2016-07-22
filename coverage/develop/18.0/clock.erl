-module(clock).
-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link(Delta, Frequency) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [Delta, Frequency], []).

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state(?MODULE).

init([Delta, Frequency]) ->
  gen_server:cast(self(), tick),
  {ok, {Delta, Frequency, 0}}. % Epoch in gregorian seconds


handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast(tick, {Delta, Frequency, GregorianSeconds}) ->
  timer:sleep(500),
  gen_server:cast(self(), tick),
  {noreply, {Delta, Frequency, GregorianSeconds+Delta}}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
