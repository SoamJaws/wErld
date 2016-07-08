-module(stop).
-include("public_transport.hrl").
-include("logger.hrl").
-behaviour(gen_server).

%% Public API
-export([ ?PASSENGER_CHECK_IN/2
        , ?PASSENGER_CHECK_OUT/3
        , ?VEHICLE_CHECK_IN/3
        , ?VEHICLE_CHECK_OUT/3]).

%% gen_server
-export([ start_link/1
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

-spec ?PASSENGER_CHECK_IN(stop(), pid()) -> ok | {nok, nonempty_string()}.
?PASSENGER_CHECK_IN(?RECIPENT, Passenger) ->
  gen_server:call(Pid, {?PASSENGER_CHECK_IN, Passenger}).

-spec ?PASSENGER_CHECK_OUT(stop(), pid(), boolean())-> ok.
?PASSENGER_CHECK_OUT(?RECIPENT, Passenger, BlockCaller) ->
  gen_server_utils:cast(Pid, {?PASSENGER_CHECK_OUT, Passenger}, BlockCaller).

-spec ?VEHICLE_CHECK_IN(stop(), vehicle(), boolean()) -> ok.
?VEHICLE_CHECK_IN(?RECIPENT, Vehicle, BlockCaller) ->
  gen_server_utils:cast(Pid, {?VEHICLE_CHECK_IN, Vehicle}, BlockCaller).

-spec ?VEHICLE_CHECK_OUT(stop(), vehicle(), boolean()) -> ok.
?VEHICLE_CHECK_OUT(?RECIPENT, Vehicle, BlockCaller) ->
  gen_server_utils:cast(Pid, {?VEHICLE_CHECK_OUT, Vehicle}, BlockCaller).


%% gen_server

-spec start_link(atom()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(Id) ->
  gen_server:start_link(?MODULE, Id, []).


-spec init(atom()) -> {ok, stop_state()}.
init(Id) ->
  put(id, Id),
  ?LOG_INFO("Stop started"),
  {ok, #stop_state{id=Id}}.


-spec handle_call({?PASSENGER_CHECK_IN, pid()}, {pid(), any()}, stop_state()) -> {reply, ok | {nok, string()}, stop_state()}.
handle_call({?PASSENGER_CHECK_IN, Passenger}, _From, State) ->
  Passengers = State#stop_state.passengers,
  AlreadyCheckedIn = lists:member(Passenger, Passengers),
  if
    AlreadyCheckedIn ->
      {reply, {nok, "Already checked in"}, State};
    true ->
      CurrentVehicle = State#stop_state.currentVehicle,
      case CurrentVehicle of
        none ->
          ok;
        Vehicle ->
          WillBoard = citizen:vehicle_checked_in(Passenger, Vehicle),
          if
            WillBoard ->
              vehicle:?INCREMENT_BOARDING_PASSENGER(Vehicle, false);
            true ->
              ok
          end
      end,
      {reply, ok, State#stop_state{passengers=Passengers++[Passenger]}}
  end.


-spec handle_cast({?PASSENGER_CHECK_OUT, pid(), boolean(), pid()}, stop_state()) -> {noreply, stop_state()}
      ;          ({?VEHICLE_CHECK_IN, vehicle(), boolean(), pid()}, stop_state()) -> {noreply, stop_state()}
      ;          ({?VEHICLE_CHECK_OUT, vehicle(), boolean(), pid()}, stop_state()) -> {noreply, stop_state()}.
handle_cast({?PASSENGER_CHECK_OUT, Passenger, NotifyCaller, Caller}, State) ->
  Passengers = lists:delete(Passenger, State#stop_state.passengers),
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, State#stop_state{passengers=Passengers}};

handle_cast({?VEHICLE_CHECK_IN, Vehicle, NotifyCaller, Caller}, State) ->
  NewState = case State#stop_state.currentVehicle of
               none ->
                 BoardingPassengers = notify_vehicle_checked_in(State#stop_state.passengers, Vehicle),
                 vehicle:?CHECKIN_OK(Vehicle, self(), BoardingPassengers, false),
                 State#stop_state{currentVehicle=Vehicle};
               _    ->
                 VehicleQueue = State#stop_state.vehicleQueue,
                 State#stop_state{vehicleQueue=VehicleQueue++[Vehicle]}
  end,
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, NewState};

handle_cast({?VEHICLE_CHECK_OUT, Vehicle, NotifyCaller, Caller}, State) ->
  NewState = if
               State#stop_state.currentVehicle == Vehicle ->
                 case State#stop_state.vehicleQueue of
                   [NextVehicle|VehicleQueue] ->
                     BoardingPassengers = notify_vehicle_checked_in(State#stop_state.passengers, NextVehicle),
                     vehicle:?CHECKIN_OK(NextVehicle, self(), BoardingPassengers, false),
                     State#stop_state{currentVehicle=NextVehicle, vehicleQueue=VehicleQueue};
                   [] ->
                     State#stop_state{currentVehicle=none}
                 end;
               true ->
                 State
  end,
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, NewState}.


-spec handle_info(timeout | any(), stop_state()) -> {noreply, stop_state()}.
handle_info(_Info, State) ->
  {noreply, State}.


-spec terminate(normal | shutdown | {shutdown, any()} | any(), stop_state()) -> ok.
terminate(_Reason, _State) ->
  ok.


-spec code_change(term() | {down, term()}, stop_state(), term()) -> {ok, stop_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Backend

-spec notify_vehicle_checked_in([pid()], vehicle()) -> non_neg_integer().
notify_vehicle_checked_in(Passengers, Vehicle) ->
  notify_vehicle_checked_in(Passengers, Vehicle, 0).

-spec notify_vehicle_checked_in([pid()], vehicle(), non_neg_integer()) -> non_neg_integer().
notify_vehicle_checked_in([], _Vehicle, BoardingPassengers) -> BoardingPassengers;
notify_vehicle_checked_in([Passenger|Passengers], Vehicle, BoardingPassengers) ->
  WillBoard = citizen:vehicle_checked_in(Passenger, Vehicle),
  if
    WillBoard ->
      notify_vehicle_checked_in(Passengers, Vehicle, BoardingPassengers + 1);
    true ->
      notify_vehicle_checked_in(Passengers, Vehicle, BoardingPassengers)
  end.
