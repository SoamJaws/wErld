-module(weatherController).

-behaviour(gen_server).
-export([start/2, stop/1, state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(weatherState, {city,type,temp,updateInterval,lastChange}).
-weatherTypes([sunny,rainy,snowy]).

%% Public API

start(City, UpdateInterval) ->
  gen_server:start(?MODULE, [City, UpdateInterval], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

%% Gen server callbacks

init([City, UpdateInterval]) ->
  gen_server:call(blackboard,{subscribe, time}),
  TimePid = gen_server:call(blackboard,{request, timePid}),
  Time = gen_server:call(TimePid,{request,currentTime}),
  { ok, #weatherState{ city=City
                     , type=sunny
                     , temp=20
                     , updateInterval=UpdateInterval
                     , lastChange=Time
                     } }.


handle_call(stop, _From, Subscriptions) ->
  {stop, normal, stopped, Subscriptions}.


handle_cast({time, Time}, State) ->
  if
    Time - State#weatherState.lastChange >= State#weatherState.updateInterval ->
      %% Change weather if should be done
      %% Change temp according to statistical temp and time of day
      UpdatedState = State#weatherState{lastChange=Time},
      {noreply, UpdatedState};
    true ->
      {noreply, State}
  end.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Internal functions
