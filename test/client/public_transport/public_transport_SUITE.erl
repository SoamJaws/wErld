-module(public_transport_SUITE).
-include("public_transport.hrl").
-include_lib("common_test/include/ct.hrl").

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2]).
-export([test1/1]).

all() -> [test1].

init_per_testcase(TestCase, Config) ->
  StopIds = [a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p],
  LineSpecs = [ {1, [a, 1, b, 2, c, 1, d, 4, e, 3, f, 2, g], bus}
              , {2, [i, 3, h, 2, b, 1, j, 2, k], tram}
              , {3, [l, 3, e, 2, m, 1, n], tram}
              , {4, [m, 2, o, 3, p], bus}
              ],
  public_transport_supervisor:start_link(StopIds, LineSpecs),
  Config.

end_per_testcase(_TestCase, Config) ->
  Config.

test1(_Config) ->
  public_transport:?GET_ROUTE(a, o).
