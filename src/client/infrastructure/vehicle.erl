-module(vehicle).
-compile(export_all).
-include("infrastructure_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% TODO
%% * Procedure for passenger leave before boarding

%% Public API

start_link(Type, Capacity) ->
  gen_server:start_link(?MODULE, #vehicle_state{type=Type, capacity=Capacity}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

init(State) ->
  gen_server:call(blackboard, {subscribe, time}),
  {ok, State}.

passenger_board(Pid, Passenger) ->
  gen_server:call(Pid, {passenger_board, Passenger}).

passenger_leave(Pid, Passenger) ->
  gen_server:call(Pid, {passenger_leave, Passenger}).

boarding_complete(Pid) ->
  gen_server:cast(Pid, boarding_complete).

checkin_ok(Pid, Stop) ->
  gen_server:cast(Pid, {checkin_ok, Stop})

handle_call({passenger_board, Passenger}, _From, State) ->
  Passengers = State#vehicle_state.passengers,
  Capacity = State#vehicle_state.capacity,
  NoPassengers = length(Passengers),
  if
    NoPassengers < Capacity ->
      if Capacity - NoPassengers == 1 ->
        boarding_complete(self())
      end,
      {reply, ok, State#vehicle_state{passengers=Passengers++[Passenger]}};
    true ->
      {reply, nok, State}
  end;

handle_call({passenger_leave, Passenger}, State) ->
  case State#vehicle_state.action of
    {driving, _} ->
      {reply, nok, State};
    _ ->
      Passengers = lists:delete(Passenger, State#vehicle_state.passengers),
      {reply, ok, State#vehicle_state{passengers=Passengers}}
  end;

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State}.


handle_cast({checkin_ok, Stop}, State) ->
  {noreply, State#vehicle_state{action={boarding, Stop}}};

handle_cast(boarding_complete, State) ->
  %% TODO GOGOGO!
  undefined;

handle_cast({time, Time}, State) ->
  case State#vehicle_state.action of
    {driving, Stop, Duration} ->
      if
        Time - State#vehicle_state.lastDeparture >= Duration ->
          stop:vehicle_check_in(Stop, self());
        true ->
          {noreply, State}
      end;
    _ ->
      {noreply, State}
  end.

handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
