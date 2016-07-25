-module(stop_SUITE).
-include("public_transport.hrl").
-include("logger.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). %% For assertion macros

-export([ passenger_check_in_case/1
        , passenger_check_out_case/1
        , vehicle_check_in_case/1
        , vehicle_check_out_case/1
        , invalid_vehicle_check_out_case/1]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2]).


all() ->
  [ passenger_check_in_case
  , passenger_check_out_case
  , vehicle_check_in_case
  , vehicle_check_out_case
  , invalid_vehicle_check_out_case
  ].


init_per_testcase(_TestCase, Config) ->
  put(id, test),
  put(module, "test"),
  logger:start_link("log"),
  stop_supervisor:start_link(),
  P1 = gen_server_mock:start(citizen, p1, strict),
  P2 = gen_server_mock:start(citizen, p2, strict),
  P3 = gen_server_mock:start(citizen, p3, strict),
  P4 = gen_server_mock:start(citizen, p4, strict),
  V1 = gen_server_mock:start(vehicle, v1, strict),
  V2 = gen_server_mock:start(vehicle, v2, strict),
  Stop = stop_supervisor:start_stop(stop1),
  Config ++ [ {p1, P1}
            , {p2, P2}
            , {p3, P3}
            , {p4, P4}
            , {v1, V1}
            , {v2, V2}
            , {stop, Stop}
            ].


end_per_testcase(_TestCase, Config) ->
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  V1 = ?config(v1, Config),
  V2 = ?config(v2, Config),
  Stop = ?config(stop, Config),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  ?assert(gen_server_mock:validate(P4)),
  ?assert(gen_server_mock:validate(V1)),
  ?assert(gen_server_mock:validate(V2)),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(P4),
  gen_server_mock:stop(V1),
  gen_server_mock:stop(V2),
  logger:stop(),
  stop_supervisor:stop_stop(Stop),
  Config.


passenger_check_in_case(Config) ->
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  Stop = ?config(stop, Config),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertMatch({nok, _}, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertMatch(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)).

passenger_check_out_case(Config) ->
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  V1 = ?config(v1, Config),
  Stop = ?config(stop, Config),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),

  gen_server_mock:expect_cast(V1, {?CHECKIN_OK, Stop, 3, false, gen_server_utils:extract_pid(Stop)}),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, V1}, true),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),

  stop:?PASSENGER_CHECK_OUT(Stop, P1, false),
  stop:?PASSENGER_CHECK_OUT(Stop, P2, false),
  stop:?PASSENGER_CHECK_OUT(Stop, P3, true).

vehicle_check_in_case(Config) ->
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  V1 = ?config(v1, Config),
  Stop = ?config(stop, Config),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),

  gen_server_mock:expect_cast(V1, {?CHECKIN_OK, Stop, 1, false, gen_server_utils:extract_pid(Stop)}),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, V1}, false),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),

  gen_server_mock:expect_call(P3, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P4, {vehicle_checked_in, V1}, false),
  gen_server_mock:expect_cast(V1, {?INCREMENT_BOARDING_PASSENGER, false, gen_server_utils:extract_pid(Stop)}),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P4)).

vehicle_check_out_case(Config) ->
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  V1 = ?config(v1, Config),
  V2 = ?config(v2, Config),
  Stop = ?config(stop, Config),
  
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  stop:?PASSENGER_CHECK_OUT(Stop, P1, false),

  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P1)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P2)),
  ?assertEqual(ok, stop:?PASSENGER_CHECK_IN(Stop, P3)),

  gen_server_mock:expect_cast(V1, {?CHECKIN_OK, Stop, 3, false, gen_server_utils:extract_pid(Stop)}),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, V1}, true),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, V1}, true),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),
  stop:?VEHICLE_CHECK_IN(Stop, V2, true),

  stop:?PASSENGER_CHECK_OUT(Stop, P1, false),
  stop:?PASSENGER_CHECK_OUT(Stop, P2, false),

  ?assert(gen_server_mock:validate(V2)), %% Validate that the checkin_ok has not been sent prematurely
  gen_server_mock:expect_cast(V2, {?CHECKIN_OK, Stop, 0, false, gen_server_utils:extract_pid(Stop)}),

  stop:?PASSENGER_CHECK_OUT(Stop, P3, true),
  stop:?VEHICLE_CHECK_OUT(Stop, V1, true),
  stop:?VEHICLE_CHECK_OUT(Stop, V2, true). %% Exercise check out when no vehicles queued

invalid_vehicle_check_out_case(Config) ->
  V1 = ?config(v1, Config),
  V2 = ?config(v2, Config),
  Stop = ?config(stop, Config),

  gen_server_mock:expect_cast(V1, {?CHECKIN_OK, Stop, 0, false, gen_server_utils:extract_pid(Stop)}),

  stop:?VEHICLE_CHECK_IN(Stop, V1, true),
  stop:?VEHICLE_CHECK_OUT(Stop, V2, true).
