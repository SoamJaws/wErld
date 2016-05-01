-module(stop).
-compile(export_all).
-include("infrastructure_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link(Id) ->
  gen_server:start_link(?MODULE, #stop_state{id=Id}, []).

start_link(Id, Capacity) ->
  gen_server:start_link(?MODULE, #stop_state{id=Id, capacity=Capacity}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

init(State) ->
  {ok, State}.

check_in(Pid, Passenger) ->
  gen_server:call(Pid, {checkin, Passenger}).

handle_call({checkin, Passenger}, _From, State) ->
  Passengers = State#stop_state.passengers,
  Capacity = State#stop_state.capacity,
  NoPassenger = length(Passengers),
  if
    NoPassengers < Capacity ->
      {ok, State#stop_state{passengers=Passengers++[Passenger]}};
    true ->
      {nok, State}
  end;

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {{ok, State}, State}.

handle_cast({time, _Time}, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
