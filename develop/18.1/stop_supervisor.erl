-module(stop_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(stop_supervisor, []).

init(_Args) ->
    SupFlags = { simple_one_for_one
               , 0
               , 1
               },
    ChildSpecs = [ { stop
                   , {stop, start_link, []}
                   , transient
                   , 1000
                   , worker
                   , [stop]
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.
