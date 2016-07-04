-module(public_transport_tests).
-include_lib("eunit/include/eunit.hrl").

public_transport_test() ->
  {ok, _PublicTransport} = public_transport:start_link().
