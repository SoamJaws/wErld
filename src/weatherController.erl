-module(blackboard).
-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(weatherState, {city,type,temp,intensity,lastChange}).
-weatherTypes([sunny,rainy,snowy]).

%% Public API

start_link(City, Intensity) ->
  gen_server:start_link(?MODULE, [City, Intensity], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

state(Pid) ->
  state(Pid).

init([City, Intensity]) ->
  gen_server:call(blackboard,{subscribe, time}),
  {reply, TimePid} = gen_server:call(blackboard,{request, timePid})
  {reply, Time} = gen_server:call(TimePid,{request,currentTime})
  {ok, #weatherState{city=City,type=sunny,temp=20,intensity=Intensity,lastChange=Time}}.


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
