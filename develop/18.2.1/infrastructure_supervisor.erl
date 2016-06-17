-module(infrastructure_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(infrastructure_supervisor, []).

init(_Args) ->
    SupFlags = {one_for_one, 1, 5},
    ChildSpecs = [ { infrastructure
                   , {infrastructure, start_link, []}
                   , permanent
                   , 1000
                   , worker
                   , [infrastructure]
                   }
                 , { line_supervisor
                   , {line_supervisor, start_link, []}
                   , permanent
                   , 1000
                   , supervisor
                   , [line]
                   }
                 , { stop_supervisor
                   , {stop_supervisor, start_link, []}
                   , permanent
                   , 1000
                   , supervisor
                   , [stop]
                   }
                 , { vehicle_supervisor
                   , {vehicle_supervisor, start_link, []}
                   , permanent
                   , 1000
                   , supervisor
                   , [vehicle]
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.
