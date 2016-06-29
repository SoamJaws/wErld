-module(vehicle_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(vehicle_supervisor, []).

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
