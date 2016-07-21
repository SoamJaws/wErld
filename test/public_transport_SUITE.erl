-module(public_transport_SUITE).
-include("public_transport.hrl").
-include("logger.hrl").
-include_lib("common_test/include/ct.hrl").

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2]).
-export([test1/1]).

all() -> [test1].

init_per_testcase(_TestCase, Config) ->
  put(id, test),
  put(module, "test"),
  logger:start_link("log"),
  {ok, PublicTransportSupervisor} = public_transport_supervisor:start_link(),
  Config.

end_per_testcase(_TestCase, Config) ->
  %% Let logger finish
  receive
    after 1000 ->
      ok
  end,
  Config.

test1(_Config) ->
  public_transport:?GET_ROUTE(a, o).
