-module(public_transport_supervisor).
-behaviour(supervisor).

-export([start_link/2]).
-export([init/1]).

start_link(StopIds, LineSpecs) ->
    supervisor:start_link(public_transport_supervisor, {StopIds, LineSpecs}).

init({StopIds, LineSpecs}) ->
  SupFlags = {one_for_one, 0, 1},
  ChildSpecs = [ { stop_supervisor
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
                 , {public_transport, start_link, [StopIds, LineSpecs]}
                 , permanent
                 , 1000
                 , worker
                 , [public_transport]
                 }
               ],
  {ok, {SupFlags, ChildSpecs}}.
