-define(INFRASTRUCTURE_DATA_PATH, "$WERLDROOT/appdata/infrastructure").

%%-----------------------------------------------------------
%% Data Type: infrastructure stop
%% where:
%%
%%    id:             A string (default is undefined).
%%                    Should be unique for each client.
%%
%%    currentVehicle: A Pid (default is none).
%%                    The pid of the Vehicle process boarding
%%                    passangers at this stop.
%%
%%    passengers:     A list of Pids (default is []).
%%                    A FIFO queue of waiting Persons
%%
%%    vehicleQueue:   A list of Pids (default is []).
%%                    A FIFO queue of waiting vehicles.
%%
%%------------------------------------------------------------
-record(stop_state, {id, capacity = 50, currentVehicle = none, passengers = [], vehicleQueue = []}).

-define(PASSENGER_CHECK_IN,  passenger_check_in).
-define(PASSENGER_CHECK_OUT, passenger_check_out).
-define(VEHICLE_CHECK_IN,    vehicle_check_in).
-define(VEHICLE_CHECK_OUT,   vehicle_check_out).

%%-----------------------------------------------------------
%% Data Type: infrastructure vehicle
%% where:
%%
%%    action:        A tuple {waiting, Pid}, {boardin, Pid}
%%                   or {driving, Pid, Duration}
%%
%%                   If waiting, Pid is the stop
%%                   waiting at, or none of not waiting at a
%%                   stop (assumed to be waiting in a depot).
%%                   If boarding, Pid is the stop boarding at.
%%                   If driving, Pid is the next stop and
%%                   Duration is the time it takes to get to
%%                   that stop.
%%
%%    capacity:      An int. The max number of passengers
%%                   for this Vehicle.
%%
%%    lastDeparture: A timestamp. The time at which the
%%                   vehicle left the previous stop.
%%
%%    line:          A tuple {int, Pid}.
%%                   The int is the line number. The Pid is
%%                   the Pid of the line process.
%%
%%    passengers:    A list of Pids (default is []).
%%                   All Persons currently riding this Vehicle
%%
%%    target:        A Pid.
%%                   The Pid of the Stop that is the end station
%%
%%    type:          An atom.
%%                   The type of Vehicle, i.e. bus, train, tram
%%
%%------------------------------------------------------------
-record(vehicle_state, {action = {waiting, none}, capacity, lastDeparture, line, passengers = [], target, type}).

-define(PASSENGER_BOARD, passenger_board).
-define(BOARDING_COMPLETE, boarding_complete).
-define(CHECKIN_OK, checkin_ok).

%%-----------------------------------------------------------
%% Data Type: infrastructure vehicle
%% where:
%%
%%    number:     An int.
%%
%%    stops:      A list of Pids with durations (ints) in
%%                between. Head and last shall be Pids of the
%%                stops, and duration in between is the
%%                travel duration between the stops.
%%
%%    type:       An atom.
%%                The type of Vehicle, i.e. bus, train, tram.
%%
%%------------------------------------------------------------
-record(line_state, {number, stops, type}).

-define(GET_NEXT_STOP,    get_next_stop).
-define(GET_NEIGHBORS,    get_neighbors).
-define(GET_OTHER_END,    get_other_end).
-define(CONTAINS_STOP,    contains_stop).
-define(GET_DURATION,     get_duration).
-define(IS_END_STOP,      is_end_stop).
-define(GET_INTERSECTION, get_intersection).

%%-----------------------------------------------------------
%% Data Type: infrastructure
%% where:
%%
%%    lines:     A list of Pids of all lines in the
%%               infrastructure
%%
%%------------------------------------------------------------
-record(infrastructure_state, {lines}).

-define(GET_ROUTE, get_route).