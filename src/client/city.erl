-module(blackboard).
-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link(Name) ->
  gen_server:start_link(?MODULE, [Name], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

state(Pid) ->
  state(Pid).

init([Name]) ->
  gen_server:call({subscribe, time}),
  {ok, [Name]}.


handle_call(stop, _From, Subscriptions) ->
  {stop, normal, stopped, Subscriptions};

handle_cast({time, Time}, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
