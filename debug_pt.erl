-module(debug_pt).
-export([run/0]).

run() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(public_transport, '_', '_', [{'_', [], [{return_trace}]}]),
  dbg:p(all, c),
  StopIds = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p],
  LineSpecs = [ {1, [a, 1, b, 2, c, 1, d, 4, e, 3, f, 2, g], bus}
              , {2, [i, 3, h, 2, b, 1, j, 2, k], tram}
              , {3, [l, 3, e, 2, m, 1, n], tram}
              , {4, [m, 2, o, 3, p], bus}
  public_transport_supervisor:start_link(StopIds, LineSpecs),
  Route = public_transport:get_route(a, o),
  io:fwrite("~n~n~w~n~n", [Route]).
