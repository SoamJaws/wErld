-module(public_transport_SUITE).
-include("public_transport.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). %% For assertion macros

-export([ get_route_case/1
        , get_other_end_case/1
        , get_next_stop_case/1]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2]).

all() -> [get_route_case].

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

get_route_case(_Config) ->
  timer:tc(fun() -> public_transport:?GET_ROUTE(a, o) end).


get_other_end_case(Config) ->
  LineStops = ets_utils:set_lookup(public_transport, ?LINE_ID(bus, 1)),
  [FirstStop|_] = LineStops,
  LastStop = lists:last(LineStops),
  ?assertEqual(line:?GET_OTHER_END(bus, 1, FirstStop), LastStop),
  ?assertEqual(line:?GET_OTHER_END(bus, 1, LastStop), FirstStop).


get_next_stop_case(Config) ->
  [Stop1, Dur1_2, Stop2, Dur2_3, Stop3] = ets_utils:set_lookup(public_transport, ?LINE_ID(bus, 4)),
  ?assertEqual(line:?GET_NEXT_STOP(bus, 4, Stop3, Stop1), {Stop2, Dur1_2}),
  ?assertEqual(line:?GET_NEXT_STOP(bus, 4, Stop3, Stop2), {Stop3, Dur2_3}),
  ?assertEqual(line:?GET_NEXT_STOP(bus, 4, Stop3, Stop3), none),
  ?assertEqual(line:?GET_NEXT_STOP(bus, 4, Stop1, Stop3), {Stop2, Dur2_3}),
  ?assertEqual(line:?GET_NEXT_STOP(bus, 4, Stop1, Stop2), {Stop1, Dur1_2}),
  ?assertEqual(line:?GET_NEXT_STOP(bus, 4, Stop1, Stop1), none).
