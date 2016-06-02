-module(vehicle).
-include("infrastructure_state.hrl").
-include_lib("eunit/include/eunit.hrl").
-behaviour(gen_server).

%% Public API
-export([ start_link/2
        , stop/1
        , state/1
        , passenger_board/2
        , boarding_complete/1
        , checkin_ok/2]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

start_link(Type, Capacity) ->
  gen_server:start_link(?MODULE, #vehicle_state{type=Type, capacity=Capacity}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

passenger_board(Pid, Passenger) ->
  gen_server:call(Pid, {passenger_board, Passenger}).

boarding_complete(Pid) ->
  gen_server:cast(Pid, boarding_complete).

checkin_ok(Pid, Stop) ->
  gen_server:cast(Pid, {checkin_ok, Stop}).


%% gen_server

init(State) ->
  gen_server:call(blackboard, {subscribe, time}),
  {ok, State}.


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

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State}.


handle_cast({checkin_ok, Stop}, State) ->
  {noreply, State#vehicle_state{action={boarding, Stop}}};

handle_cast(boarding_complete, State) ->
  {_, Line} = State#vehicle_state.line,
  {boarding, Stop} = State#vehicle_state.action,
  {NextStop, Dur} = line:get_next_stop(Line, State#vehicle_state.target, Stop),
  TimePid = gen_server:call(blackboard,{request, timePid}),
  Time = gen_server:call(TimePid,{request,currentTime}),
  {noreply, State#vehicle_state{action={driving, NextStop, Dur}, lastDeparture=Time}};

handle_cast({time, Time}, State) ->
  case State#vehicle_state.action of
    {driving, Stop, Duration} ->
      if
        Time - State#vehicle_state.lastDeparture >= Duration ->
          UpdatedState = if
                           Stop == State#vehicle_state.target ->
                           Target = line:get_other_end(State#vehicle_state.line, State#vehicle_state.target),
                           State#vehicle_state{target=Target};
                         true ->
                           State
                         end,
          StayingPassengers = notify_passengers_checkin(State#vehicle_state.passengers),
          stop:vehicle_check_in(Stop, self()),
          {noreply, UpdatedState#vehicle_state{passengers=StayingPassengers}};
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


%% Backend

notify_passengers_checkin([]) -> [];
notify_passengers_checkin([Passenger|Passengers]) ->
  Reply = citizen:vehicle_notify_checkin(Passenger),
  case Reply of
    leave ->
      notify_passengers_checkin(Passengers);
    stay ->
      [Passenger|notify_passengers_checkin(Passengers)]
  end.
