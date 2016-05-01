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

passenger_check_in(Pid, Passenger) ->
  gen_server:call(Pid, {passenger_check_in, Passenger}).

passenger_check_out(Pid, Passenger) ->
  gen_server:cast(Pid, {passenger_check_out, Passenger}).

vehicle_check_in(Pid, Vehicle) ->
  gen_server:call(Pid, {vehicle_check_in, Vehicle}).

vehicle_check_out(Pid, Vehicle) ->
  gen_server:cast(Pid, {vehicle_check_out, Vehicle}).

handle_call({passenger_check_in, Passenger}, _From, State) ->
  Passengers = State#stop_state.passengers,
  Capacity = State#stop_state.capacity,
  NoPassengers = length(Passengers),
  if
    NoPassengers < Capacity ->
      {reply, ok, State#stop_state{passengers=Passengers++[Passenger]}};
    true ->
      {reply, nok, State}
  end;

handle_call({vehicle_check_in, Vehicle}, _From, State) ->
  case State#stop_state.currentVehicle of
    none ->
      {reply, ok, State#stop_state{currentVehicle=Vehicle}};
    _    ->
      {reply, nok, State}
  end;

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State}.


handle_cast({passenger_check_out, Passenger}, State) ->
  Passengers = lists:delete(Passenger, State#stop_state.passengers),
  {noreply, State#stop_state{passengers=Passengers}};

handle_cast({vehicle_checkout_out, Vehicle}, State) ->
  if
    State#stop_state.currentVehicle == Vehicle ->
      {noreply, State#stop_state{currentVehicle=none}};
    true ->
      {noreply, State}
  end.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
