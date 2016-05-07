-module(vehicle).
-compile(export_all).
-include("infrastructure_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% TODO
%% * Subscribe to time, needed for evaluating stop arrivals

%% Public API

start_link(Type, Capacity) ->
  gen_server:start_link(?MODULE, #vehicle_state{type=Type, capacity=Capacity}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

init(State) ->
  {ok, State}.

passenger_board(Pid, Passenger) ->
  gen_server:call(Pid, {passenger_board, Passenger}).

passenger_leave(Pid, Passenger) ->
  gen_server:cast(Pid, {passenger_leave, Passenger}).

handle_call({passenger_board, Passenger}, _From, State) ->
  Passengers = State#stop_state.passengers,
  Capacity = State#stop_state.capacity,
  NoPassengers = length(Passengers),
  if
    NoPassengers < Capacity ->
      if Capacity - NoPassengers == 1
        {boarding, Stop} = State#stop_state.action,
        gen_server:cast(Stop, boarding_vehicle_full)
      end,
      {reply, ok, State#stop_state{passengers=Passengers++[Passenger]}};
    true ->
      {reply, nok, State}
  end;

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State}.


handle_cast({passenger_leave, Passenger}, State) ->
  Passengers = lists:delete(Passenger, State#stop_state.passengers),
  {noreply, State#stop_state{passengers=Passengers}};

handle_cast(boarding_complete, State) ->
  %% TODO GOGOGO!
  undefined.

handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
