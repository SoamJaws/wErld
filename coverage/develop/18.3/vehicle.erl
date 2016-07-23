-module(vehicle).
-include("public_transport.hrl").
-include("time.hrl").
-include("logger.hrl").
-behaviour(gen_server).
-behaviour(time_subscriber).

%% Public API
-export([ ?PASSENGER_BOARD/2
        , ?INCREMENT_BOARDING_PASSENGER/1
        , ?INCREMENT_BOARDING_PASSENGER/2
        , ?CHECKIN_OK/3
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
  ?LOG_SEND(io_lib:format("PASSENGER_BOARD Vehicle=~p Passenger=~p", [?RECIPENT, Passenger])),
  Reply = gen_server:call(Pid, {?PASSENGER_BOARD, Passenger}),
  ?LOG_RECEIVE(io_lib:format("REPLY PASSENGER_BOARD ~p", [Reply])),
  Reply.

-spec ?INCREMENT_BOARDING_PASSENGER(vehicle()) -> ok.
?INCREMENT_BOARDING_PASSENGER(?RECIPENT) ->
  ?INCREMENT_BOARDING_PASSENGER(?RECIPENT, false).

-spec ?INCREMENT_BOARDING_PASSENGER(vehicle(), boolean()) -> ok.
?INCREMENT_BOARDING_PASSENGER(?RECIPENT, BlockCaller) ->
  ?LOG_SEND(io_lib:format("INCREMENT_BOARDING_PASSENGER Vehicle=~p", [?RECIPENT])),
  gen_server_utils:cast(Pid, {?INCREMENT_BOARDING_PASSENGER}, BlockCaller).

-spec ?CHECKIN_OK(vehicle(), stop(), non_neg_integer()) -> ok.
?CHECKIN_OK(?RECIPENT, Stop, BoardingPassengers) ->
  ?CHECKIN_OK(?RECIPENT, Stop, BoardingPassengers, false).

-spec ?CHECKIN_OK(vehicle(), stop(), non_neg_integer(), boolean()) -> ok.
?CHECKIN_OK(?RECIPENT, Stop, BoardingPassengers, BlockCaller) ->
  ?LOG_SEND(io_lib:format("CHECKIN_OK Vehicle=~p Stop=~p BoardingPassengers~p", [?RECIPENT, Stop, BoardingPassengers])),
  gen_server_utils:cast(Pid, {?CHECKIN_OK, Stop, BoardingPassengers}, BlockCaller).


%% Time subscriber

-spec ?NEW_TIME(vehicle(), time()) -> ok.
?NEW_TIME(?RECIPENT, Time) ->
  ?NEW_TIME(?RECIPENT, Time, false).

-spec ?NEW_TIME(vehicle(), time(), boolean()) -> ok.
?NEW_TIME(?RECIPENT, Time, BlockCaller) ->
  ?LOG_SEND(io_lib:format("NEW_TIME Vehicle=~p Time=~p", [?RECIPENT, Time])),
  gen_server_utils:cast(Pid, {?NEW_TIME, Time}, BlockCaller).


%% gen_server

-spec start_link(pos_integer(), atom(), line(), pos_integer(), stop(), vehicle_type()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(Capacity, Id, Line, LineNumber, Target, Type) ->
  gen_server:start_link(?MODULE, {Capacity, Id, Line, LineNumber, Target, Type}, []).


-spec init({pos_integer(), atom(), line(), pos_integer(), stop(), vehicle_type()}) -> {ok, vehicle_state()}.
init({Capacity, Id, Line, LineNumber, Target, Type}) ->
  put(id, Id),
  put(module, ?MODULE_STRING),
  Pid = self(),
  time:?SUBSCRIBE(?RECIPENT),
  Stop = line:?GET_OTHER_END(Line, Target),
  stop:?VEHICLE_CHECK_IN(Stop, ?RECIPENT),
  ?LOG_INFO("Vehicle started"),
  {ok, #vehicle_state{capacity=Capacity, id=Id, line={LineNumber, Line}, target=Target, type=Type}}.


-spec handle_call({?PASSENGER_BOARD, citizen()}, {pid(), any()}, vehicle_state()) -> {reply, ok | nok, vehicle_state()}.
handle_call({?PASSENGER_BOARD, Passenger}, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("PASSENGER_BOARD Passenger=~p", [Passenger])),
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
  ?LOG_RECEIVE(io_lib:format("NEW_TIME Time=~p", [Time])),
  NewState = case State#vehicle_state.action of
               {driving, Stop, Duration} ->
                 if
                   Time - State#vehicle_state.lastDeparture >= Duration ->
                     UpdatedState = if
                                      Stop == State#vehicle_state.target ->
                                        {_, Line} = State#vehicle_state.line,
                                        CurrentTarget = State#vehicle_state.target,
                                        NewTarget = line:?GET_OTHER_END(Line, State#vehicle_state.target),
                                        State#vehicle_state{target=NewTarget};
                                    true ->
                                      State
                                    end,
                     Id = State#vehicle_state.id,
                     Pid = self(),
                     StayingPassengers = notify_passengers_checkin(State#vehicle_state.passengers, Id),
                     stop:?VEHICLE_CHECK_IN(Stop, ?RECIPENT),
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
  ?LOG_RECEIVE(io_lib:format("INCREMENT_BOARDING_PASSENGER", [])),
  BoardingPassengers = State#vehicle_state.boardingPassengers,
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, State#vehicle_state{boardingPassengers=BoardingPassengers+1}};

handle_cast({?CHECKIN_OK, Stop, BoardingPassengers, NotifyCaller, Caller}, State) ->
  ?LOG_RECEIVE(io_lib:format("CHECKIN_OK Stop=~p BoardingPassengers=~p", [Stop, BoardingPassengers])),
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
  ?LOG_INFO(io_lib:format("boarding_complete", [])),
  {_, Line} = State#vehicle_state.line,
  {boarding, CurrentStop} = State#vehicle_state.action,
  TargetStop = State#vehicle_state.target,
  {NextStop, Dur} = line:?GET_NEXT_STOP(Line, TargetStop, CurrentStop),
  Id = State#vehicle_state.id,
  Pid = self(),
  stop:?VEHICLE_CHECK_OUT(CurrentStop, ?RECIPENT),
  Time = time:?GET_CURRENT_TIME(),
  State#vehicle_state{action={driving, NextStop, Dur}, lastDeparture=Time, boardingPassengers=0}.

-spec notify_passengers_checkin([citizen()], atom()) -> [citizen()].
notify_passengers_checkin([], Id) -> [];
notify_passengers_checkin([Passenger|Passengers], Id) ->
  ?LOG_INFO(io_lib:format("notify_passengers_checkin Passenger=~p Passengers=~p Id=~p", [Passenger, Passengers, Id])),
  Pid = self(),
  Reply = citizen:vehicle_checked_in(Passenger, ?RECIPENT),
  case Reply of
    leave ->
      notify_passengers_checkin(Passengers, Id);
    stay ->
      [Passenger|notify_passengers_checkin(Passengers, Id)]
  end.