-module(stop_tests).
-include("infrastructure_state.hrl").
-include_lib("eunit/include/eunit.hrl").

passenger_check_in_test() ->
  {ok, Stop} = stop:start_link(stop1, 2),
  P1 = p1,
  P2 = p2,
  P3 = p3,
  
  ?assertEqual(ok, stop:passenger_check_in(Stop, P1)),
  ?assertMatch({nok, _}, stop:passenger_check_in(Stop, P1)),
  ?assertEqual(ok, stop:passenger_check_in(Stop, P2)),
  ?assertMatch({nok, _}, stop:passenger_check_in(Stop, P3)),

  stop:stop(Stop).

passenger_check_out_test() ->
  ?assert(false).

vehicle_check_in_test() ->
  ?assert(false).

vehicle_check_out_test() ->
  ?assert(false).


