-module(vehicle).
-include("public_transport.hrl").
-include("time.hrl").
-behaviour(gen_server).
-behaviour(time_subscriber).

%% Public API
-export([ ?PASSENGER_BOARD/2
        , ?INCREMENT_BOARDING_PASSENGER/2
        , ?CHECKIN_OK/4]).

%% Time subscriber
-export([ ?NEW_TIME/2
        , ?NEW_TIME/3]).

%% gen_server
-export([ start_link/6
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

-spec ?PASSENGER_BOARD(vehicle(), citizen()) -> ok | nok.
?PASSENGER_BOARD(?RECIPENT, Passenger) ->
  gen_server:call(Pid, {?PASSENGER_BOARD, Passenger}).

-spec ?INCREMENT_BOARDING_PASSENGER(vehicle(), boolean()) -> ok.
?INCREMENT_BOARDING_PASSENGER(?RECIPENT, BlockCaller) ->
  gen_server_utils:cast(Pid, {?INCREMENT_BOARDING_PASSENGER}, BlockCaller).

-spec ?CHECKIN_OK(vehicle(), stop(), non_neg_integer(), boolean()) -> ok.
?CHECKIN_OK(?RECIPENT, Stop, BoardingPassengers, BlockCaller) ->
  gen_server_utils:cast(Pid, {?CHECKIN_OK, Stop, BoardingPassengers}, BlockCaller).


%% Time subscriber

-spec ?NEW_TIME(vehicle(), time()) -> ok.
?NEW_TIME(?RECIPENT, Time) ->
  ?NEW_TIME(?RECIPENT, Time, false).

-spec ?NEW_TIME(vehicle(), time(), boolean()) -> ok.
?NEW_TIME(?RECIPENT, Time, BlockCaller) ->
  gen_server_utils:cast(Pid, {?NEW_TIME, Time}, BlockCaller).


%% gen_server

-spec start_link(pos_integer(), atom(), line(), pos_integer(), stop(), vehicle_type()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(Capacity, Id, Line, LineNumber, Target, Type) ->
  gen_server:start_link(?MODULE, {Capacity, Id, Line, LineNumber, Target, Type}, []).


-spec init({pos_integer(), atom(), line(), pos_integer(), stop(), vehicle_type()}) -> {ok, vehicle_state()}.
init({Capacity, Id, Line, LineNumber, Target, Type}) ->
  Pid = self(),
  time:?SUBSCRIBE(?RECIPENT),
  Stop = line:?GET_OTHER_END(Line, Target),
  Pid = self(),
  stop:?VEHICLE_CHECK_IN(Stop, ?RECIPENT, false),
  {ok, #vehicle_state{capacity=Capacity, id=Id, line={LineNumber, Line}, target=Target, type=Type}}.


-spec handle_call({?PASSENGER_BOARD, citizen()}, {pid(), any()}, vehicle_state()) -> {reply, ok | nok, vehicle_state()}.
handle_call({?PASSENGER_BOARD, Passenger}, _From, State) ->
  Passengers = State#vehicle_state.passengers,
  Capacity = State#vehicle_state.capacity,
  NoPassengers = length(Passengers),
  BoardingPassengers = State#vehicle_state.boardingPassengers,
  if
    NoPassengers < Capacity ->
      UpdatedState = State#vehicle_state{passengers=Passengers++[Passenger], boardingPassengers=BoardingPassengers-1},
      NewState = if (Capacity - NoPassengers == 1) or (BoardingPassengers == 1) ->
                   boarding_complete(UpdatedState);
                 true ->
                   UpdatedState
                 end,
      {reply, ok, NewState};
    true ->
      {reply, nok, State}
  end.


-spec handle_cast({?NEW_TIME, time(), boolean(), pid()}, vehicle_state()) -> {noreply, vehicle_state()}
      ;          ({?INCREMENT_BOARDING_PASSENGER, boolean(), pid()}, vehicle_state()) -> {noreply, vehicle_state()}
      ;          ({?CHECKIN_OK, stop(), non_neg_integer(), boolean(), pid()}, vehicle_state()) -> {noreply, vehicle_state()}.
handle_cast({?NEW_TIME, Time, NotifyCaller, Caller}, State) ->
  NewState = case State#vehicle_state.action of
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
                     Id = State#vehicle_state.id,
                     Pid = self(),
                     stop:?VEHICLE_CHECK_IN(Stop, ?RECIPENT, false),
                     UpdatedState#vehicle_state{passengers=StayingPassengers};
                   true ->
                     State
                 end;
               _ ->
                 State
             end,
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, NewState};

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
  {noreply, NewState}.


-spec handle_info(timeout | any(), vehicle_state()) -> {noreply, vehicle_state()}.
handle_info(_Info, State) ->
  {noreply, State}.


-spec terminate(normal | shutdown | {shutdown, any()} | any(), vehicle_state()) -> ok.
terminate(_Reason, _State) ->
  ok.


-spec code_change(term() | {down, term()}, stop_state(), term()) -> {ok, vehicle_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Backend

-spec boarding_complete(vehicle_state()) -> vehicle_state().
boarding_complete(State) ->
  {_, Line} = State#vehicle_state.line,
  {boarding, Stop} = State#vehicle_state.action,
  {NextStop, Dur} = line:?GET_NEXT_STOP(Line, State#vehicle_state.target, Stop),
  Id = State#vehicle_state.id,
  Pid = self(),
  stop:?VEHICLE_CHECK_OUT(Stop, ?RECIPENT, false),
  Time = time:?GET_CURRENT_TIME(),
  State#vehicle_state{action={driving, NextStop, Dur}, lastDeparture=Time, boardingPassengers=0}.

-spec notify_passengers_checkin([citizen()]) -> [citizen()].
notify_passengers_checkin([]) -> [];
notify_passengers_checkin([Passenger|Passengers]) ->
  Reply = citizen:vehicle_checked_in(Passenger, self()),
  case Reply of
    leave ->
      notify_passengers_checkin(Passengers);
    stay ->
      [Passenger|notify_passengers_checkin(Passengers)]
  end.
