-module(public_transport_SUITE).
-include("public_transport.hrl").
-include_lib("common_test/include/ct.hrl").

-export([all/0, test1/1]).

all() -> [test1].

test1(_Config) ->
  {ok, _PublicTransportSupervisor} = public_transport_supervisor:start_link(),
  {Route, Dur} = public_transport:?GET_ROUTE(a, o).