-module(stop).
-include("public_transport.hrl").
-behaviour(gen_server).

%% Public API
-export([ start_link/1
        , stop/1
        , state/1
        , ?PASSENGER_CHECK_IN/2
        , ?PASSENGER_CHECK_OUT/3
        , ?VEHICLE_CHECK_IN/3
        , ?VEHICLE_CHECK_OUT/3]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

-spec start_link(atom()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(Id) ->
  gen_server:start_link(?MODULE, #stop_state{id=Id}, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
  gen_server:call(Pid, stop).

-spec state(pid()) -> stop_state().
state(Pid) ->
  gen_server:call(Pid, state).

-spec ?PASSENGER_CHECK_IN(pid(), pid()) -> ok | {nok, nonempty_string()}.
?PASSENGER_CHECK_IN(Pid, Passenger) ->
  gen_server:call(Pid, {?PASSENGER_CHECK_IN, Passenger}).

-spec ?PASSENGER_CHECK_OUT(pid(), pid(), boolean())-> ok.
?PASSENGER_CHECK_OUT(Pid, Passenger, BlockCaller) ->
  gen_server:cast(Pid, {?PASSENGER_CHECK_OUT, Passenger, BlockCaller, self()}),
  gen_server_utils:block_caller(BlockCaller).

-spec ?VEHICLE_CHECK_IN(pid(), pid(), boolean()) -> ok.
?VEHICLE_CHECK_IN(Pid, Vehicle, BlockCaller) ->
  gen_server:cast(Pid, {?VEHICLE_CHECK_IN, Vehicle, BlockCaller, self()}),
  gen_server_utils:block_caller(BlockCaller).

-spec ?VEHICLE_CHECK_OUT(pid(), pid(), boolean()) -> ok.
?VEHICLE_CHECK_OUT(Pid, Vehicle, BlockCaller) ->
  gen_server:cast(Pid, {?VEHICLE_CHECK_OUT, Vehicle, BlockCaller, self()}),
  gen_server_utils:block_caller(BlockCaller).


%% gen_server

-spec init(stop_state()) -> {ok, stop_state()}.
init(State) ->
  {ok, State}.


-spec handle_call({?PASSENGER_CHECK_IN, pid()}, pid(), stop_state()) -> {reply, {nok, string()}, stop_state()} | {reply, ok, stop_state()}
      ;          (stop, pid(), stop_state()) -> {stop, normal, stopped, stop_state()}
      ;          (state, pid(), stop_state()) -> {reply, stop_state(), stop_state()}.
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
  end;

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, State, State}.


-spec handle_cast({?PASSENGER_CHECK_OUT, pid(), boolean(), pid()}, stop_state()) -> {noreply, stop_state()}
      ;          ({?VEHICLE_CHECK_IN, pid(), boolean(), pid()}, stop_state()) -> {noreply, stop_state()}
      ;          ({?VEHICLE_CHECK_OUT, pid(), boolean(), pid()}, stop_state()) -> {noreply, stop_state()}.
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

-spec notify_vehicle_checked_in([pid()], pid()) -> non_neg_integer().
notify_vehicle_checked_in(Passengers, Vehicle) ->
  notify_vehicle_checked_in(Passengers, Vehicle, 0).

-spec notify_vehicle_checked_in([pid()], pid(), non_neg_integer()) -> non_neg_integer().
notify_vehicle_checked_in([], _Vehicle, BoardingPassengers) -> BoardingPassengers;
notify_vehicle_checked_in([Passenger|Passengers], Vehicle, BoardingPassengers) ->
  WillBoard = citizen:vehicle_checked_in(Passenger, Vehicle),
  if
    WillBoard ->
      notify_vehicle_checked_in(Passengers, Vehicle, BoardingPassengers + 1);
    true ->
      notify_vehicle_checked_in(Passengers, Vehicle, BoardingPassengers)
  end.
