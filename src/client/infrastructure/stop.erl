-module(stop).
-include("infrastructure_state.hrl").
-behaviour(gen_server).

%% Public API
-export([ start_link/1
        , start_link/2
        , stop/1
        , state/1
        , passenger_check_in/2
        , passenger_check_out/3
        , vehicle_check_in/3
        , vehicle_check_out/3]).

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

passenger_check_in(Pid, Passenger) ->
  gen_server:call(Pid, {passenger_check_in, Passenger}).

passenger_check_out(Pid, Passenger, BlockCaller) ->
  gen_server:cast(Pid, {passenger_check_out, Passenger, BlockCaller, self()}),
  block_caller(BlockCaller).

vehicle_check_in(Pid, Vehicle, BlockCaller) ->
  gen_server:cast(Pid, {vehicle_check_in, Vehicle, BlockCaller, self()}),
  block_caller(BlockCaller).

vehicle_check_out(Pid, Vehicle, BlockCaller) ->
  gen_server:cast(Pid, {vehicle_check_out, Vehicle, BlockCaller, self()}),
  block_caller(BlockCaller).


%% gen_server

init(State) ->
  {ok, State}.


handle_call({passenger_check_in, Passenger}, _From, State) ->
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
  {reply, {ok, State}, State}.


handle_cast({passenger_check_out, Passenger, BlockCaller, Caller}, State) ->
  Passengers = lists:delete(Passenger, State#stop_state.passengers),
  case Passengers of
    [] -> %%TODO Maybe not all passengers are waiting for the currentVehicle 
      case State#stop_state.currentVehicle of 
        none ->
          ok; %%Ok, passengers are permitted to leave the stop without boarding
        _ ->
          vehicle:boarding_complete(State#stop_state.currentVehicle, false)
      end;
    _ ->
      ok
  end,
  notify_caller(BlockCaller, Caller),
  {noreply, State#stop_state{passengers=Passengers}};

handle_cast({vehicle_check_in, Vehicle, BlockCaller, Caller}, State) ->
  NewState = case State#stop_state.currentVehicle of
               none ->
                 vehicle:checkin_ok(Vehicle, self(), false),
                 notify_vehicle_checked_in(State#stop_state.passengers, Vehicle),
                 State#stop_state{currentVehicle=Vehicle};
               _    ->
                 VehicleQueue = State#stop_state.vehicleQueue,
                 State#stop_state{vehicleQueue=VehicleQueue++[Vehicle]}
  end,
  notify_caller(BlockCaller, Caller),
  {noreply, NewState};

handle_cast({vehicle_check_out, Vehicle, BlockCaller, Caller}, State) ->
  NewState = if
               State#stop_state.currentVehicle == Vehicle ->
                 case State#stop_state.vehicleQueue of
                   [NextVehicle|VehicleQueue] ->
                     vehicle:checkin_ok(NextVehicle, self(), false),
                     notify_vehicle_checked_in(State#stop_state.passengers, NextVehicle),
                     State#stop_state{currentVehicle=NextVehicle, vehicleQueue=VehicleQueue};
                   [] ->
                     State#stop_state{currentVehicle=none}
                 end;
               true ->
                 State
  end,
  notify_caller(BlockCaller, Caller),
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

block_caller(BlockCaller) ->
  if
    BlockCaller ->
      receive
        done -> ok
      end;
    true ->
      ok
  end.

notify_caller(BlockCaller, Caller) ->
  if
    BlockCaller ->
      Caller ! done;
    true ->
      ok
  end.
