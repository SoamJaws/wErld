-module(stop_tests).
-include("infrastructure.hrl").
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

  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
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

  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  ?assert(gen_server_mock:validate(V1)),
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

  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  ?assert(gen_server_mock:validate(V1)),
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
  {ok, V2} = gen_server_mock:start_link(v2),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),

  gen_server_mock:expect_cast(V1, {checkin_ok, Stop, false, Stop}),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, V1}, ok),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, V1}, ok),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, V1}, ok),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),
  stop:?VEHICLE_CHECK_IN(Stop, V2, true),

  stop:?PASSENGER_CHECK_OUT(Stop, P1, false),
  stop:?PASSENGER_CHECK_OUT(Stop, P2, false),

  gen_server_mock:expect_cast(V1, {boarding_complete, false, Stop}),

  ?assert(gen_server_mock:validate(V2)), %% Validate that the checkin_ok has not been sent prematurely
  gen_server_mock:expect_cast(V2, {checkin_ok, Stop, false, Stop}),

  stop:?PASSENGER_CHECK_OUT(Stop, P3, true),
  stop:?VEHICLE_CHECK_OUT(Stop, V1, true),
  stop:?VEHICLE_CHECK_OUT(Stop, V2, true), %% Exercise check out when no vehicles queued

  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  ?assert(gen_server_mock:validate(V1)),
  ?assert(gen_server_mock:validate(V2)),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(V1),
  gen_server_mock:stop(V2),
  stop:stop(Stop).
