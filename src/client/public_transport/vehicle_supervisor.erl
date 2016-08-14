-module(vehicle_supervisor).
-include("public_transport.hrl").
-behaviour(supervisor).

-export([ start_link/0
        , start_vehicle/4
        , stop_vehicle/1]).

-export([init/1]).

start_link() ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, []).

start_vehicle(Capacity, LineNumber, Target, Type) ->
  Id = list_to_atom(atom_to_list(Type) ++ "_" ++ integer_to_list(LineNumber)),
  Result = supervisor:start_child({global, ?MODULE}, [Capacity, Id, LineNumber, Target, Type]),
  case Result of
    {ok, Pid} ->
      ?ADDRESS(vehicle);
    Result ->
      Result
  end.

stop_vehicle(?ADDRESS_NO_ID(vehicle)) ->
  supervisor:terminate_child({global, ?MODULE}, Pid).
  

init(_Args) ->
  SupFlags = { simple_one_for_one
             , 0
             , 1
             },
  ChildSpecs = [ { vehicle
                 , {vehicle, start_link, []}
                 , transient
                 , 1000
                 , worker
                 , [vehicle]
                 }
               ],
  {ok, {SupFlags, ChildSpecs}}.
