-module(vehicle).
-include("infrastructure.hrl").
-behaviour(gen_server).

%% Public API
-export([ start_link/4
        , stop/1
        , state/1
        , ?PASSENGER_BOARD/2
        , ?INCREMENT_BOARDING_PASSENGER/2
        , ?CHECKIN_OK/4]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

start_link(Capacity, Line, Target, Type) ->
  gen_server:start_link(?MODULE, #vehicle_state{capacity=Capacity, line=Line, target=Target, type=Type}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

?PASSENGER_BOARD(Pid, Passenger) ->
  gen_server:call(Pid, {?PASSENGER_BOARD, Passenger}).

?INCREMENT_BOARDING_PASSENGER(Pid, BlockCaller) ->
  gen_server:cast(Pid, {?INCREMENT_BOARDING_PASSENGER, BlockCaller, self()}),
  gen_server_utils:block_caller(BlockCaller).

?CHECKIN_OK(Pid, Stop, BoardingPassengers, BlockCaller) ->
  gen_server:cast(Pid, {?CHECKIN_OK, Stop, BoardingPassengers, BlockCaller, self()}),
  gen_server_utils:block_caller(BlockCaller).


%% gen_server

init(State) ->
  gen_server:cast({global, blackboard}, {subscribe, time}),
  Line = State#vehicle_state.line,
  LineNumber = line:?GET_NUMBER(Line),
  Stop = line:?GET_OTHER_END(Line, State#vehicle_state.target),
  stop:?VEHICLE_CHECK_IN(Stop, self(), false),
  {ok, State#vehicle_state{line={LineNumber, Line}}}.


handle_call({?PASSENGER_BOARD, Passenger}, _From, State) ->
  Passengers = State#vehicle_state.passengers,
  Capacity = State#vehicle_state.capacity,
  NoPassengers = length(Passengers),
  BoardingPassengers = State#vehicle_state.boardingPassengers,
  if
    NoPassengers < Capacity ->
      NewState = if (Capacity - NoPassengers == 1) or (BoardingPassengers == 1) ->
                   boarding_complete(State);
                 true ->
                   State
                 end,
      {reply, ok, NewState#vehicle_state{passengers=Passengers++[Passenger], boardingPassengers=BoardingPassengers-1}};
    true ->
      {reply, nok, State}
  end;

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State}.


handle_cast({?INCREMENT_BOARDING_PASSENGER, NotifyCaller, Caller}, State) ->
  BoardingPassengers = State#vehicle_state.boardingPassengers,
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, State#vehicle_state{boardingPassengers=BoardingPassengers+1}};

handle_cast({?CHECKIN_OK, Stop, BoardingPassengers, NotifyCaller, Caller}, State) ->
  NewState = if
               BoardingPassengers == 0 ->
                 boarding_complete(State#vehicle_state{action={boarding, Stop}});
               true ->
                 State#vehicle_state{action={boarding, Stop}, boardingPassengers=BoardingPassengers}
             end,
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, NewState};

handle_cast({time, Time}, State) ->
  case State#vehicle_state.action of
    {driving, Stop, Duration} ->
      if
        Time - State#vehicle_state.lastDeparture >= Duration ->
          UpdatedState = if
                           Stop == State#vehicle_state.target ->
                             {_, Line} = State#vehicle_state.line,
                             Target = line:?GET_OTHER_END(Line, State#vehicle_state.target),
                             State#vehicle_state{target=Target};
                         true ->
                           State
                         end,
          StayingPassengers = notify_passengers_checkin(State#vehicle_state.passengers),
          stop:?VEHICLE_CHECK_IN(Stop, self(), false),
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

boarding_complete(State) ->
  Passengers = State#vehicle_state.passengers,
  Capacity = State#vehicle_state.capacity,
  NoPassengers = length(Passengers),
  BoardingPassengers = State#vehicle_state.boardingPassengers,
  if
    (BoardingPassengers == 0) or (NoPassengers == Capacity) ->
      {_, Line} = State#vehicle_state.line,
      {boarding, Stop} = State#vehicle_state.action,
      {NextStop, Dur} = line:?GET_NEXT_STOP(Line, State#vehicle_state.target, Stop),
      TimePid = gen_server:call({global, blackboard}, {request, timePid}),
      Time = gen_server:call(TimePid, {request, currentTime}),
      stop:?VEHICLE_CHECK_OUT(Stop, self(), false),
      State#vehicle_state{action={driving, NextStop, Dur}, lastDeparture=Time, boardingPassengers=0};
    true ->
      State
  end.

notify_passengers_checkin([]) -> [];
notify_passengers_checkin([Passenger|Passengers]) ->
  Reply = citizen:vehicle_checked_in(Passenger, self()),
  case Reply of
    leave ->
      notify_passengers_checkin(Passengers);
    stay ->
      [Passenger|notify_passengers_checkin(Passengers)]
  end.
