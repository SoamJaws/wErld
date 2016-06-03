-module(line_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(line_supervisor, []).

init(_Args) ->
    SupFlags = { simple_one_for_one
               , 0
               , 1
               },
    ChildSpecs = [ { line
                   , {line, start_link, []}
                   , transient
                   , 1000
                   , worker
                   , [line]
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.
