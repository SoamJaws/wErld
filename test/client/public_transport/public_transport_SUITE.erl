-module(public_transport_SUITE).
-include("public_transport.hrl").
-include("logger.hrl").
-include_lib("common_test/include/ct.hrl").

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2]).
-export([test1/1]).

all() -> [test1].

init_per_testcase(TestCase, Config) ->
  put(id, ?MODULE),
  put(module, ?MODULE_STRING),
  logger:start_link(?MODULE_STRING ++ [$_|atom_to_list(TestCase)] ++ "_log"),
  {ok, PublicTransportSupervisor} = public_transport_supervisor:start_link(),
  Config.

end_per_testcase(TestCase, Config) ->
  logger:stop(),
  LogName = ?MODULE_STRING ++ [$_|atom_to_list(TestCase)] ++ "_log",
  ct:comment("<a href=\"../../logs/" ++ LogName ++ "/index.html\">" ++ LogName ++ "</a>"),
  Config.

test1(_Config) ->
  public_transport:?GET_ROUTE(a, o).
