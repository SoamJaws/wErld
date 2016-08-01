-module(debug_pt).
-export([run/0]).

run() ->
  dbg:start(),
  dbg:tracer(),
  dbg:tpl(public_transport, get_route_concurrent, 7, []),
  dbg:p(all, c),
  public_transport_supervisor:start_link(),
  public_transport:get_route(a, o).
