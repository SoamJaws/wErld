-module(public_transport_SUITE).
-include("public_transport.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, test1]).

all() -> [test1].

test1(_Config) ->
  {ok, _PublicTransportSupervisor} = public_transport_supervisor:start_link(),
  {Route, Dur} = public_transport:?GET_ROUTE(a, o),
  Ids = lists:map(fun({L1,S2,S3}) ->
                    LS1 = stop:state(L1),
                    SS2 = stop:state(S2),
                    SS3 = stop:state(S3),
                    {LS1#line_state.number, SS2#stop_state.id, SS3#stop_state.id}
                  end, Route).
