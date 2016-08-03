-module(debug_pt).
-export([run/0]).

run() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(public_transport, '_', '_', [{'_', [], [{return_trace}]}]),
  dbg:p(all, c),
  public_transport_supervisor:start_link(),
  Route = public_transport:get_route(a, o),
  io:fwrite("~n~n~w~n~n", [Route]).
