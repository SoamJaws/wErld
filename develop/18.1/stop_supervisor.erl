-module(stop_supervisor).
-include("public_transport.hrl").
-behaviour(supervisor).

-export([ start_link/0
        , start_stop/1
        , stop_stop/1]).

-export([init/1]).

start_link() ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, []).

start_stop(Id) ->
  Result = supervisor:start_child({global, ?MODULE}, [Id]),
  case Result of
    {ok, Pid} ->
      ?ADDRESS(stop);
    Result ->
      Result
  end.

stop_stop(?ADDRESS(stop)) ->
  supervisor:terminate_child({global, ?MODULE}, Pid).
  

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