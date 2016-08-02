-module(debug_pt).
-export([run/0]).

run() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(public_transport, '_', '_', []),
  dbg:p(all, c),
  public_transport_supervisor:start_link(),
  Route = public_transport:get_route(a, o),
  io:fwrite("~w~n", [Route]).
