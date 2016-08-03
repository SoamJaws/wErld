-module(line_supervisor).
-include("public_transport.hrl").
-behaviour(supervisor).

-export([ start_link/0
        , start_line/3
        , stop_line/1]).

-export([init/1]).

start_link() ->
  supervisor:start_link({global, ?MODULE}, ?MODULE, []).

start_line(Number, Stops, Type) ->
  Result = supervisor:start_child({global, ?MODULE}, [Number, Stops, Type]),
  case Result of
    {ok, Pid} ->
      Id = list_to_atom(atom_to_list(Type) ++ [$_|integer_to_list(Number)]),
      ?ADDRESS(line);
    Result ->
      Result
  end.

stop_line(?ADDRESS_NO_ID(line)) ->
  supervisor:terminate_child({global, ?MODULE}, Pid).
  

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
