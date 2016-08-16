-module(vehicle_SUITE).
-include("public_transport.hrl").
-include("time.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). %% For assertion macros

-export([ checkin_ok_case/1
        , boarding_passenger_capacity_reached_case/1
        , boarding_passenger_below_capacity_case/1
        , next_stop_reached_case/1
        , next_stop_not_reached_case/1
        , target_stop_reached_case/1
        , increment_boarding_passenger_case/1
        , new_time_not_driving_case/1]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2]).

all() ->
  [ checkin_ok_case
  , boarding_passenger_capacity_reached_case
  , boarding_passenger_below_capacity_case
  , next_stop_reached_case
  , next_stop_not_reached_case
  , target_stop_reached_case
  , increment_boarding_passenger_case
  , new_time_not_driving_case
  ].

init_per_testcase(Config) ->
  vehicle_supervisor:start_link(),
  StartStop = gen_server_mock:start(stop, startstop, strict),
  TargetStop = gen_server_mock:start(stop, targetstop, strict),
  Time = gen_server_mock:start_global(time, time, strict),
  PT = gen_server_mock:start_local(public_transport, public_transport, strict),
  W  = gen_server_mock:start_local(weather, weather, strict),
  S1 = gen_server_mock:start(stop, s1, strict),
  P1 = gen_server_mock:start(citizen, p1, strict),
  P2 = gen_server_mock:start(citizen, p2, strict),
  P3 = gen_server_mock:start(citizen, p3, strict),
  P4 = gen_server_mock:start(citizen, p4, strict),

  gen_server_mock:expect_ets_lookup(public_transport, ?LINE_ID(bus, 1), [StartStop, 5, TargetStop]),

  Config ++ [ {startstop, StartStop}
            , {targetstop, TargetStop}
            , {time, Time}
            , {pt, PT}
            , {w, W}
            , {s1, S1}
            , {p1, P1}
            , {p2, P2}
            , {p3, P3}
            , {p4, P4}
            ].

init_per_testcase(TestCase, Config) ->
  UpdatedConfig = init_per_testcase(Config),
  StartStop = ?config(startstop, UpdatedConfig),
  TargetStop = ?config(targetstop, UpdatedConfig),
  Time = ?config(time, UpdatedConfig),
  PT = ?config(pt, UpdatedConfig),
  HasThreeCapacity = lists:member(TestCase, [ checkin_ok_case
                                            , boarding_passenger_capacity_reached_case
                                            , boarding_passenger_below_capacity_case
                                            , new_time_not_driving_case
                                            ]),
  HasFourCapacity = lists:member(TestCase, [ next_stop_reached_case
                                           , next_stop_not_reached_case
                                           , target_stop_reached_case
                                           , increment_boarding_passenger_case
                                           ]),
  Vehicle = vehicle_supervisor:start_vehicle(if
                                               HasThreeCapacity -> 3;
                                               HasFourCapacity -> 4
                                             end, 1, TargetStop, bus),
  gen_server_mock:expect_cast(Time, {?SUBSCRIBE, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  [{vehicle, Vehicle}|UpdatedConfig].



end_per_testcase(_TestCase, Config) ->
  StartStop = ?config(startstop, Config),
  TargetStop = ?config(targetstop, Config),
  Time = ?config(time, Config),
  PT = ?config(pt, Config),
  W = ?config(w, Config),
  S1 = ?config(s1, Config),
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  Vehicle = ?config(vehicle, Config),
  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(PT)),
  ?assert(gen_server_mock:validate(W)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  ?assert(gen_server_mock:validate(P4)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(PT),
  gen_server_mock:stop(W),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(P4),
  vehicle_supervisor:stop_vehicle(Vehicle),
  Config.


checkin_ok_case(Config) ->
  StartStop = ?config(startstop, Config),
  TargetStop = ?config(targetstop, Config),
  Time = ?config(time, Config),
  PT = ?config(pt, Config),
  S1 = ?config(s1, Config),
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  Vehicle = ?config(vehicle, Config),

  gen_server_mock:expect_ets_lookup(public_transport, ?LINE_ID(bus, 1), [StartStop, 5, S1, 7, TargetStop]),
  gen_server_mock:expect_ets_lookup(time, time, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  vehicle:?CHECKIN_OK(Vehicle, StartStop, 0, true).

boarding_passenger_capacity_reached_case(Config) ->
  StartStop = ?config(startstop, Config),
  TargetStop = ?config(targetstop, Config),
  Time = ?config(time, Config),
  PT = ?config(pt, Config),
  S1 = ?config(s1, Config),
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  Vehicle = ?config(vehicle, Config),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 4, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_ets_lookup(public_transport, ?LINE_ID(bus, 1), [StartStop, 5, S1, 7, TargetStop]),
  gen_server_mock:expect_ets_lookup(time, time, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P4), nok).

boarding_passenger_below_capacity_case(Config) ->
  StartStop = ?config(startstop, Config),
  TargetStop = ?config(targetstop, Config),
  Time = ?config(time, Config),
  PT = ?config(pt, Config),
  S1 = ?config(s1, Config),
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  Vehicle = ?config(vehicle, Config),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_ets_lookup(public_transport, ?LINE_ID(bus, 1), [StartStop, 5, S1, 7, TargetStop]),
  gen_server_mock:expect_ets_lookup(time, time, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok).

next_stop_reached_case(Config) ->
  StartStop = ?config(startstop, Config),
  TargetStop = ?config(targetstop, Config),
  Time = ?config(time, Config),
  PT = ?config(pt, Config),
  S1 = ?config(s1, Config),
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  Vehicle = ?config(vehicle, Config),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_ets_lookup(public_transport, ?LINE_ID(bus, 1), [StartStop, 5, S1, 7, TargetStop]),
  gen_server_mock:expect_ets_lookup(time, time, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  gen_server_mock:expect_ets_lookup(weather_controller, type, sunny),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, Vehicle}, leave),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_cast(S1, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  vehicle:?NEW_TIME(Vehicle, 1234+5, true).

next_stop_not_reached_case(Config) ->
  StartStop = ?config(startstop, Config),
  TargetStop = ?config(targetstop, Config),
  Time = ?config(time, Config),
  PT = ?config(pt, Config),
  S1 = ?config(s1, Config),
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  Vehicle = ?config(vehicle, Config),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_ets_lookup(public_transport, ?LINE_ID(bus, 1), [StartStop, 5, S1, 7, TargetStop]),
  gen_server_mock:expect_ets_lookup(time, time, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  gen_server_mock:expect_ets_lookup(weather_controller, type, sunny),

  vehicle:?NEW_TIME(Vehicle, 1234+2, true).

target_stop_reached_case(Config) ->
  StartStop = ?config(startstop, Config),
  TargetStop = ?config(targetstop, Config),
  Time = ?config(time, Config),
  PT = ?config(pt, Config),
  S1 = ?config(s1, Config),
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  Vehicle = ?config(vehicle, Config),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_ets_lookup(public_transport, ?LINE_ID(bus, 1), [StartStop, 5, TargetStop]),
  gen_server_mock:expect_ets_lookup(time, time, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  gen_server_mock:expect_ets_lookup(weather_controller, type, sunny),
  gen_server_mock:expect_ets_lookup(public_transport, ?LINE_ID(bus, 1), [StartStop, 5, TargetStop]),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, Vehicle}, leave),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_cast(TargetStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  vehicle:?NEW_TIME(Vehicle, 1234+5, true).

increment_boarding_passenger_case(Config) ->
  StartStop = ?config(startstop, Config),
  TargetStop = ?config(targetstop, Config),
  Time = ?config(time, Config),
  PT = ?config(pt, Config),
  S1 = ?config(s1, Config),
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  Vehicle = ?config(vehicle, Config),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 2, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),

  vehicle:?INCREMENT_BOARDING_PASSENGER(Vehicle, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_ets_lookup(public_transport, ?LINE_ID(bus, 1), [StartStop, 5, S1, 7, TargetStop]),
  gen_server_mock:expect_ets_lookup(time, time, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok).

new_time_not_driving_case(Config) ->
  StartStop = ?config(startstop, Config),
  TargetStop = ?config(targetstop, Config),
  Time = ?config(time, Config),
  PT = ?config(pt, Config),
  S1 = ?config(s1, Config),
  P1 = ?config(p1, Config),
  P2 = ?config(p2, Config),
  P3 = ?config(p3, Config),
  P4 = ?config(p4, Config),
  Vehicle = ?config(vehicle, Config),

  gen_server_mock:expect_ets_lookup(weather_controller, type, sunny),

  vehicle:?NEW_TIME(Vehicle, 1233, true),

  gen_server_mock:expect_ets_lookup(public_transport, ?LINE_ID(bus, 1), [StartStop, 5, S1, 7, TargetStop]),
  gen_server_mock:expect_ets_lookup(time, time, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  vehicle:?CHECKIN_OK(Vehicle, StartStop, 0, true).
