-module(stop_tests).
-include("public_transport.hrl").
-include_lib("eunit/include/eunit.hrl").

passenger_check_in_test() ->
  stop_supervisor:start_link(),
  Stop = stop_supervisor:start_stop(stop1),
  P1 = gen_server_mock:start_link(citizen, p1, strict),
  P2 = gen_server_mock:start_link(citizen, p2, strict),
  P3 = gen_server_mock:start_link(citizen, p3, strict),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertMatch({nok, _}, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertMatch(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),

  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  stop_supervisor:stop_stop(Stop).

passenger_check_out_test() ->
  stop_supervisor:start_link(),
  Stop = stop_supervisor:start_stop(stop1),
  P1 = gen_server_mock:start_link(citizen, p1, strict),
  P2 = gen_server_mock:start_link(citizen, p2, strict),
  P3 = gen_server_mock:start_link(citizen, p3, strict),
  V1 = gen_server_mock:start_link(vehicle, v1, strict),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),

  gen_server_mock:expect_cast(V1, {?CHECKIN_OK, Stop, 3, false, Stop}),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, V1}, true),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),

  stop:?PASSENGER_CHECK_OUT(Stop, P1, false),
  stop:?PASSENGER_CHECK_OUT(Stop, P2, false),
  stop:?PASSENGER_CHECK_OUT(Stop, P3, true),

  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  ?assert(gen_server_mock:validate(V1)),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(V1),
  stop_supervisor:stop_stop(Stop).

vehicle_check_in_test() ->
  stop_supervisor:start_link(),
  Stop = stop_supervisor:start_stop(stop1),
  P1 = gen_server_mock:start_link(citizen, p1, strict),
  P2 = gen_server_mock:start_link(citizen, p2, strict),
  P3 = gen_server_mock:start_link(citizen, p3, strict),
  P4 = gen_server_mock:start_link(citizen, p4, strict),
  V1 = gen_server_mock:start_link(vehicle, v1, strict),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),

  gen_server_mock:expect_cast(V1, {?CHECKIN_OK, Stop, 1, false, Stop}),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, V1}, false),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),

  gen_server_mock:expect_call(P3, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P4, {vehicle_checked_in, V1}, false),
  gen_server_mock:expect_cast(V1, {?INCREMENT_BOARDING_PASSENGER, false, Stop}),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P4)),

  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  ?assert(gen_server_mock:validate(P4)),
  ?assert(gen_server_mock:validate(V1)),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(P4),
  gen_server_mock:stop(V1),
  stop_supervisor:stop_stop(Stop).

vehicle_check_out_test() ->
  stop_supervisor:start_link(),
  Stop = stop_supervisor:start_stop(stop1),
  P1 = gen_server_mock:start_link(citizen, p1, strict),
  P2 = gen_server_mock:start_link(citizen, p2, strict),
  P3 = gen_server_mock:start_link(citizen, p3, strict),
  V1 = gen_server_mock:start_link(vehicle, v1, strict),
  V2 = gen_server_mock:start_link(vehicle, v2, strict),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  stop:?PASSENGER_CHECK_OUT(Stop, P1, false),

  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),

  gen_server_mock:expect_cast(V1, {?CHECKIN_OK, Stop, 3, false, Stop}),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, V1}, true),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),
  stop:?VEHICLE_CHECK_IN(Stop, V2, true),

  stop:?PASSENGER_CHECK_OUT(Stop, P1, false),
  stop:?PASSENGER_CHECK_OUT(Stop, P2, false),

  ?assert(gen_server_mock:validate(V2)), %% Validate that the checkin_ok has not been sent prematurely
  gen_server_mock:expect_cast(V2, {?CHECKIN_OK, Stop, 0, false, Stop}),

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
  stop_supervisor:stop_stop(Stop).

invalid_vehicle_check_out_test() ->
  stop_supervisor:start_link(),
  Stop = stop_supervisor:start_stop(stop1),
  V1 = gen_server_mock:start_link(vehicle, v1, strict),
  V2 = gen_server_mock:start_link(vehicle, v2, strict),
  
  gen_server_mock:expect_cast(V1, {?CHECKIN_OK, Stop, 0, false, Stop}),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),
  stop:?VEHICLE_CHECK_OUT(Stop, V2, true),

  ?assert(gen_server_mock:validate(V1)),
  ?assert(gen_server_mock:validate(V2)),
  gen_server_mock:stop(V1),
  gen_server_mock:stop(V2),
  stop_supervisor:stop_stop(Stop).
