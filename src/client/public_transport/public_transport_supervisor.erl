-module(public_transport_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(public_transport_supervisor, []).

init(_Args) ->
    SupFlags = {one_for_one, 1, 5},
    ChildSpecs = [ { line_supervisor
                   , {line_supervisor, start_link, []}
                   , permanent
                   , 1000
                   , supervisor
                   , [line_supervisor]
                   }
                 , { stop_supervisor
                   , {stop_supervisor, start_link, []}
                   , permanent
                   , 1000
                   , supervisor
                   , [stop_supervisor]
                   }
                 , { vehicle_supervisor
                   , {vehicle_supervisor, start_link, []}
                   , permanent
                   , 1000
                   , supervisor
                   , [vehicle_supervisor]
                   }
                 , { public_transport
                   , {public_transport, start_link, []}
                   , permanent
                   , 1000
                   , worker
                   , [public_transport]
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.
