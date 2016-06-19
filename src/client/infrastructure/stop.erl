-module(stop).
-include("infrastructure.hrl").
-behaviour(gen_server).

%% Public API
-export([ start_link/1
        , start_link/2
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

start_link(Id) ->
  gen_server:start_link(?MODULE, #stop_state{id=Id}, []).

start_link(Id, Capacity) ->
  gen_server:start_link(?MODULE, #stop_state{id=Id, capacity=Capacity}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

?PASSENGER_CHECK_IN(Pid, Passenger) ->
  gen_server:call(Pid, {?PASSENGER_CHECK_IN, Passenger}).

?PASSENGER_CHECK_OUT(Pid, Passenger, BlockCaller) ->
  gen_server:cast(Pid, {?PASSENGER_CHECK_OUT, Passenger, BlockCaller, self()}),
  gen_server_utils:block_caller(BlockCaller).

?VEHICLE_CHECK_IN(Pid, Vehicle, BlockCaller) ->
  gen_server:cast(Pid, {?VEHICLE_CHECK_IN, Vehicle, BlockCaller, self()}),
  gen_server_utils:block_caller(BlockCaller).

?VEHICLE_CHECK_OUT(Pid, Vehicle, BlockCaller) ->
  gen_server:cast(Pid, {?VEHICLE_CHECK_OUT, Vehicle, BlockCaller, self()}),
  gen_server_utils:block_caller(BlockCaller).


%% gen_server

init(State) ->
  {ok, State}.


handle_call({?PASSENGER_CHECK_IN, Passenger}, _From, State) ->
  Passengers = State#stop_state.passengers,
  Capacity = State#stop_state.capacity,
  NoPassengers = length(Passengers),
  AlreadyCheckedIn = lists:member(Passenger, Passengers),
  if
    AlreadyCheckedIn ->
      {reply, {nok, "Already checked in"}, State};
    NoPassengers < Capacity ->
      {reply, ok, State#stop_state{passengers=Passengers++[Passenger]}};
    true ->
      {reply, {nok, "Capacity reached"}, State}
  end;

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, State, State}.


handle_cast({?PASSENGER_CHECK_OUT, Passenger, NotifyCaller, Caller}, State) ->
  Passengers = lists:delete(Passenger, State#stop_state.passengers),
  case Passengers of
    [] -> %%TODO Maybe not all passengers are waiting for the currentVehicle 
      case State#stop_state.currentVehicle of 
        none ->
          ok; %%Ok, passengers are permitted to leave the stop without boarding
        _ ->
          vehicle:?BOARDING_COMPLETE(State#stop_state.currentVehicle, false)
      end;
    _ ->
      ok
  end,
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, State#stop_state{passengers=Passengers}};

handle_cast({?VEHICLE_CHECK_IN, Vehicle, NotifyCaller, Caller}, State) ->
  NewState = case State#stop_state.currentVehicle of
               none ->
                 vehicle:?CHECKIN_OK(Vehicle, self(), false),
                 notify_vehicle_checked_in(State#stop_state.passengers, Vehicle),
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
                     vehicle:?CHECKIN_OK(NextVehicle, self(), false),
                     notify_vehicle_checked_in(State#stop_state.passengers, NextVehicle),
                     State#stop_state{currentVehicle=NextVehicle, vehicleQueue=VehicleQueue};
                   [] ->
                     State#stop_state{currentVehicle=none}
                 end;
               true ->
                 State
  end,
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, NewState}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Backend

notify_vehicle_checked_in([], _Vehicle) -> [];
notify_vehicle_checked_in([Passenger|Passengers], Vehicle) ->
  citizen:vehicle_checked_in(Passenger, Vehicle), %%Must block
  notify_vehicle_checked_in(Passengers, Vehicle).
