-ifndef(__PUBLIC_TRANSPORT_HRL).
-define(__PUBLIC_TRANSPORT_HRL, true).

-include("gen_server_utils.hrl").
-include("citizen.hrl").

-ifndef(TEST).
-define(PUBLIC_TRANSPORT_DATA_PATH, code:priv_dir(wErld) ++ "/public_transport").
-else.
-define(PUBLIC_TRANSPORT_DATA_PATH, code:priv_dir(wErld) ++ "/public_transport_test").
-endif.

%%-----------------------------------------------------------
%% Data Type: public_transport stop
%% where:
%%
%%    id:                   A string (default is undefined).
%%                          Should be unique for each client.
%%
%%    currentVehicle:       A Pid (default is none).
%%                          The pid of the Vehicle process boarding
%%                          passangers at this stop.
%%
%%    passengers:           A list of Pids (default is []).
%%                          A FIFO queue of waiting Persons
%%
%%    vehicleQueue:         A list of Pids (default is []).
%%                          A FIFO queue of waiting vehicles.
%%
%%------------------------------------------------------------
-record(stop_state, { id                    :: atom()
                    , currentVehicle = none :: none | vehicle()
                    , passengers = []       :: [citizen()]
                    , vehicleQueue = []     :: [vehicle()]
                    }).
-type stop_id() :: id(stop).
-type stop() :: address(stop).
-type stop_state() :: #stop_state{}.

-define(PASSENGER_CHECK_IN,  passenger_check_in).
-define(PASSENGER_CHECK_OUT, passenger_check_out).
-define(VEHICLE_CHECK_IN,    vehicle_check_in).
-define(VEHICLE_CHECK_OUT,   vehicle_check_out).

%%-----------------------------------------------------------
%% Data Type: public_transport vehicle
%% where:
%%
%%    action:             A tuple {waiting, Pid}, {boardin, Pid}
%%                        or {driving, Pid, Duration}
%%
%%                        If waiting, Pid is the stop
%%                        waiting at, or none of not waiting at a
%%                        stop (assumed to be waiting in a depot).
%%                        If boarding, Pid is the stop boarding at.
%%                        If driving, Pid is the next stop and
%%                        Duration is the time it takes to get to
%%                        that stop.
%%
%%    capacity:           An int. The max number of passengers
%%                        for this Vehicle.
%%
%%    lastDeparture:      A timestamp. The time at which the
%%                        vehicle left the previous stop.
%%
%%    line:               A tuple {int, Pid}.
%%                        The int is the line number. The Pid is
%%                        the Pid of the line process.
%%
%%    passengers:         A list of Pids (default is []).
%%                        All Persons currently riding this Vehicle
%%
%%    boardingPassengers: An int
%%                        The number of persons that will board
%%                        during boarding
%%
%%    target:             A Pid.
%%                        The Pid of the Stop that is the end station
%%
%%    type:               An atom.
%%                        The type of Vehicle, i.e. bus, train, tram
%%
%%------------------------------------------------------------
-record(vehicle_state, { action = {waiting, none} :: {waiting, stop() | none} | {boarding, stop()} | {driving, stop(), pos_integer()}
                       , capacity                 :: pos_integer()
                       , id                       :: atom()
                       , lastDeparture            :: non_neg_integer()
                       , line                     :: {pos_integer(), line()}
                       , passengers = []          :: [citizen()]
                       , boardingPassengers = 0   :: non_neg_integer()
                       , target                   :: stop()
                       , type                     :: vehicle_type()
                       }).
-type vehicle_id() :: id(vehicle).
-type vehicle() :: address(vehicle).
-type vehicle_type() :: bus | train | tram.
-type vehicle_state() :: #vehicle_state{}.

-define(PASSENGER_BOARD, passenger_board).
-define(INCREMENT_BOARDING_PASSENGER, increment_boarding_passenger).
-define(CHECKIN_OK, checkin_ok).

%%-----------------------------------------------------------
%% Data Type: public_transport vehicle
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
-record(line_state, { number :: pos_integer()
                    , stops  :: [stop() | pos_integer()]
                    , type   :: vehicle_type()
                    }).
-type line_id() :: id(line).
-type line() :: address(line).
-type line_state() :: #line_state{}.

-define(GET_NEXT_STOP,    get_next_stop).
-define(GET_NEIGHBORS,    get_neighbors).
-define(GET_OTHER_END,    get_other_end).
-define(CONTAINS_STOP,    contains_stop).
-define(GET_DURATION,     get_duration).
-define(IS_END_STOP,      is_end_stop).
-define(GET_INTERSECTION, get_intersection).
-define(GET_NUMBER, get_number).
-define(GET_TARGET, get_target).

%%-----------------------------------------------------------
%% Data Type: public_transport
%% where:
%%
%%    lines: A list of Pids of all lines in the
%%           public_transport
%%
%%    stops: A dict of stop ids and pids
%%
%%------------------------------------------------------------
-record(public_transport_state, { lines :: [line()]
                                , stops :: dict:dict(stop_id(), pid())
                              }).
-type public_transport_state() :: #public_transport_state{}.
-type route_step() :: {line(), stop(), stop()}.
-type route() :: {[route_step()], pos_integer()}.

-define(GET_ROUTE, get_route).

-endif.
