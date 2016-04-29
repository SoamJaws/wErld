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
%%------------------------------------------------------------
-record(stop_state, {id, capacity = 50, currentVehicle = none, passengers = []}).

%%-----------------------------------------------------------
%% Data Type: infrastructure vehicle
%% where:
%%
%%    action:     A tuple {atom, Pid}.
%%                The atom should be waiting, boarding or
%%                driving.
%%
%%                IF waiting, Pid is the stop
%%                waiting at, or none of not waiting at a
%%                stop (assumed to be waiting in a depot).
%%                If boarding, Pid is the stop boarding at.
%%                If driving, Pid is the next stop.
%%
%%    line:       A tuple {int, Pid}.
%%                The int is the line number. The Pid is
%%                the Pid of the line process.
%%
%%    passengers: A list of Pids (default is []).
%%                All Persons currently riding this Vehicle
%%
%%    target:     A Pid.
%%                The Pid of the Stop that is the end station
%%
%%    type:       An atom.
%%                The type of Vehicle, i.e. bus, train, tram
%%
%%------------------------------------------------------------
-record(vehicle_state, {action, line, passengers = [], target, type}).
