-module(client_supervisor).
-behaviour(supervisor).

-export([start_link/3]).
-export([init/1]).

start_link(City, Climate, UpdateInterval) ->
    supervisor:start_link(?MODULE, {City, Climate, UpdateInterval}).

init({City, Climate, UpdateInterval}) ->
  SupFlags = {one_for_one, 0, 1},
  ChildSpecs = [ { weather_controller
                 , {weather_controller, start_link, [City, Climate, UpdateInterval]}
                 , permanent
                 , 1000
                 , worker
                 , [weather_controller]
                 }
               ],
  {ok, {SupFlags, ChildSpecs}}.
