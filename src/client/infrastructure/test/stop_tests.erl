-module(stop_tests).
-include("infrastructure_state.hrl").
-include("stop_sig.hrl").
-include_lib("eunit/include/eunit.hrl").

passenger_check_in_test() ->
  {ok, Stop} = stop:start_link(stop1, 2),
  {ok, P1} = gen_server_mock:start_link(p1),
  {ok, P2} = gen_server_mock:start_link(p2),
  {ok, P3} = gen_server_mock:start_link(p3),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertMatch({nok, _}, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertMatch({nok, _}, stop:?PASSENGER_CHECK_IN(Stop, P3)),

  ?assert(gen_server_mock:finalize(P1)),
  ?assert(gen_server_mock:finalize(P2)),
  ?assert(gen_server_mock:finalize(P3)),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  stop:stop(Stop).

passenger_check_out_test() ->
  {ok, Stop} = stop:start_link(stop1, 3),
  {ok, P1} = gen_server_mock:start_link(p1),
  {ok, P2} = gen_server_mock:start_link(p2),
  {ok, P3} = gen_server_mock:start_link(p3),
  {ok, V1} = gen_server_mock:start_link(v1),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),

  gen_server_mock:expect_cast(V1, {checkin_ok, Stop, false, Stop}),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, V1}, ok),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, V1}, ok),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, V1}, ok),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),

  stop:?PASSENGER_CHECK_OUT(Stop, P1, false),
  stop:?PASSENGER_CHECK_OUT(Stop, P2, false),

  gen_server_mock:expect_cast(V1, {boarding_complete, false, Stop}),

  stop:?PASSENGER_CHECK_OUT(Stop, P3, true),

  ?assert(gen_server_mock:finalize(P1)),
  ?assert(gen_server_mock:finalize(P2)),
  ?assert(gen_server_mock:finalize(P3)),
  ?assert(gen_server_mock:finalize(V1)),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(V1),
  stop:stop(Stop).

vehicle_check_in_test() ->
  {ok, Stop} = stop:start_link(stop1, 3),
  {ok, P1} = gen_server_mock:start_link(p1),
  {ok, P2} = gen_server_mock:start_link(p2),
  {ok, P3} = gen_server_mock:start_link(p3),
  {ok, V1} = gen_server_mock:start_link(v1),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),

  gen_server_mock:expect_cast(V1, {checkin_ok, Stop, false, Stop}),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, V1}, ok),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, V1}, ok),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, V1}, ok),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),

  ?assert(gen_server_mock:finalize(P1)),
  ?assert(gen_server_mock:finalize(P2)),
  ?assert(gen_server_mock:finalize(P3)),
  ?assert(gen_server_mock:finalize(V1)),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(V1),
  stop:stop(Stop).

vehicle_check_out_test() ->
  {ok, Stop} = stop:start_link(stop1, 3),
  {ok, P1} = gen_server_mock:start_link(p1),
  {ok, P2} = gen_server_mock:start_link(p2),
  {ok, P3} = gen_server_mock:start_link(p3),
  {ok, V1} = gen_server_mock:start_link(v1),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),

  gen_server_mock:expect_cast(V1, {checkin_ok, Stop, false, Stop}),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, V1}, ok),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, V1}, ok),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, V1}, ok),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),

  stop:?PASSENGER_CHECK_OUT(Stop, P1, false),
  stop:?PASSENGER_CHECK_OUT(Stop, P2, false),

  gen_server_mock:expect_cast(V1, {boarding_complete, false, Stop}),

  stop:?PASSENGER_CHECK_OUT(Stop, P3, true),

  %%TODO Next in line

  ?assert(gen_server_mock:finalize(P1)),
  ?assert(gen_server_mock:finalize(P2)),
  ?assert(gen_server_mock:finalize(P3)),
  ?assert(gen_server_mock:finalize(V1)),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(V1),
  stop:stop(Stop).
