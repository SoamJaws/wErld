-module(public_transport_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(public_transport_supervisor, []).

init(_Args) ->
  SupFlags = {one_for_one, 0, 1},
  ChildSpecs = [ { line_supervisor
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
               , { public_transport
                 , {public_transport, start_link, []}
                 , permanent
                 , 1000
                 , worker
                 , [public_transport]
                 }
               ],
  {ok, {SupFlags, ChildSpecs}}.
