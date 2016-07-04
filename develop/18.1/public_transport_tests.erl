-module(public_transport_tests).
-include_lib("eunit/include/eunit.hrl").

public_transport_test() ->
  {ok, _PublicTransportSupervisor} = public_transport_supervisor:start_link(),
  {ok, _PublicTransport} = public_transport:start_link(),
  ?debugFmt("~p~n", [supervisor:which_children({global, stop_supervisor})]).
