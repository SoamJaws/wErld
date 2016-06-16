-module(vehicle).
-include("infrastructure.hrl").
-behaviour(gen_server).

%% Public API
-export([ start_link/2
        , stop/1
        , state/1
        , ?PASSENGER_BOARD/2
        , ?BOARDING_COMPLETE/2
        , ?CHECKIN_OK/3]).

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

?PASSENGER_BOARD(Pid, Passenger) ->
  gen_server:call(Pid, {?PASSENGER_BOARD, Passenger}).

?BOARDING_COMPLETE(Pid, BlockCaller) ->
  gen_server:cast(Pid, {?BOARDING_COMPLETE, BlockCaller, self()}),
  gen_server_utils:block_caller(BlockCaller).

?CHECKIN_OK(Pid, Stop, BlockCaller) ->
  gen_server:cast(Pid, {?CHECKIN_OK, Stop, BlockCaller, self()}),
  gen_server_utils:block_caller(BlockCaller).


%% gen_server

init(State) ->
  gen_server:call(blackboard, {subscribe, time}),
  {ok, State}.


handle_call({?PASSENGER_BOARD, Passenger}, _From, State) ->
  Passengers = State#vehicle_state.passengers,
  Capacity = State#vehicle_state.capacity,
  NoPassengers = length(Passengers),
  if
    NoPassengers < Capacity ->
      if Capacity - NoPassengers == 1 ->
        ?BOARDING_COMPLETE(self(), false)
      end,
      {reply, ok, State#vehicle_state{passengers=Passengers++[Passenger]}};
    true ->
      {reply, nok, State}
  end;

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State}.


handle_cast({?CHECKIN_OK, Stop, NotifyCaller, Caller}, State) ->
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, State#vehicle_state{action={boarding, Stop}}};

handle_cast({?BOARDING_COMPLETE, NotifyCaller, Caller}, State) ->
  {_, Line} = State#vehicle_state.line,
  {boarding, Stop} = State#vehicle_state.action,
  {NextStop, Dur} = line:?GET_NEXT_STOP(Line, State#vehicle_state.target, Stop),
  TimePid = gen_server:call(blackboard,{request, timePid}),
  Time = gen_server:call(TimePid,{request,currentTime}),
  stop:?VEHICLE_CHECK_OUT(Stop, self(), false),
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, State#vehicle_state{action={driving, NextStop, Dur}, lastDeparture=Time}};

handle_cast({time, Time}, State) ->
  case State#vehicle_state.action of
    {driving, Stop, Duration} ->
      if
        Time - State#vehicle_state.lastDeparture >= Duration ->
          UpdatedState = if
                           Stop == State#vehicle_state.target ->
                           Target = line:?GET_OTHER_END(State#vehicle_state.line, State#vehicle_state.target),
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

notify_passengers_checkin([]) -> [];
notify_passengers_checkin([Passenger|Passengers]) ->
  Reply = citizen:vehicle_checked_in(Passenger, self()),
  case Reply of
    leave ->
      notify_passengers_checkin(Passengers);
    stay ->
      [Passenger|notify_passengers_checkin(Passengers)]
  end.
