-module(public_transport_tests).
-include("public_transport.hrl").
-include_lib("eunit/include/eunit.hrl").

public_transport_test() ->
  ok.
  %{ok, _PublicTransportSupervisor} = public_transport_supervisor:start_link(),
  %{ok, PublicTransport} = public_transport:start_link(),
  %{Route, Dur} = public_transport:?GET_ROUTE(PublicTransport, a, o),
  %Ids = lists:map(fun({L1,S2,S3}) ->
  %                  LS1 = stop:state(L1),
  %                  SS2 = stop:state(S2),
  %                  SS3 = stop:state(S3),
  %                  {LS1#line_state.number, SS2#stop_state.id, SS3#stop_state.id}
  %                end, Route),
  %?debugFmt("~p~n", [Ids]).