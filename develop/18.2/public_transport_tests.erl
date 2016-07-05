-module(public_transport_tests).
-include("public_transport.hrl").
-include_lib("eunit/include/eunit.hrl").

public_transport_test() ->
  {ok, _PublicTransportSupervisor} = public_transport_supervisor:start_link(),
  {ok, PublicTransport} = public_transport:start_link(),
  Route = public_transport:?GET_ROUTE(PublicTransport, a, g),
  ?debugFmt("~p~n", [Route]).
