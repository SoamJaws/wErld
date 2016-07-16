-module(vehicle_tests).
-include("public_transport.hrl").
-include("time.hrl").
-include_lib("eunit/include/eunit.hrl").

checkin_ok_test() ->
  vehicle_supervisor:start_link(),
  StartStop = gen_server_mock:start_link(stop, startstop, strict),
  TargetStop = gen_server_mock:start_link(stop, targetstop, strict),
  Time = gen_server_mock:start_global(time, time, strict),
  L1 = gen_server_mock:start_link(line, l1, strict),
  S1 = gen_server_mock:start_link(stop, s1, strict),

  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  Vehicle = vehicle_supervisor:start_vehicle(3, L1, TargetStop, bus),
  gen_server_mock:expect_cast(Time, {?SUBSCRIBE, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(Time, ?GET_CURRENT_TIME, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  vehicle:?CHECKIN_OK(Vehicle, StartStop, 0, true),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  vehicle_supervisor:stop_vehicle(Vehicle).

boarding_passenger_capacity_reached_test() ->
  vehicle_supervisor:start_link(),
  StartStop = gen_server_mock:start_link(stop, startstop, strict),
  TargetStop = gen_server_mock:start_link(stop, targetstop, strict),
  Time = gen_server_mock:start_global(time, time, strict),
  L1 = gen_server_mock:start_link(line, l1, strict),
  S1 = gen_server_mock:start_link(stop, s1, strict),
  P1 = gen_server_mock:start_link(stop, p1, strict),
  P2 = gen_server_mock:start_link(citizen, p2, strict),
  P3 = gen_server_mock:start_link(citizen, p3, strict),
  P4 = gen_server_mock:start_link(citizen, p4, strict),

  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  Vehicle = vehicle_supervisor:start_vehicle(3, L1, TargetStop, bus),
  gen_server_mock:expect_cast(Time, {?SUBSCRIBE, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 4, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(Time, ?GET_CURRENT_TIME, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P4), nok),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  ?assert(gen_server_mock:validate(P4)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(P4),
  vehicle_supervisor:stop_vehicle(Vehicle).

boarding_passenger_below_capacity_test() ->
  vehicle_supervisor:start_link(),
  StartStop = gen_server_mock:start_link(stop, startstop, strict),
  TargetStop = gen_server_mock:start_link(stop, targetstop, strict),
  Time = gen_server_mock:start_global(time, time, strict),
  L1 = gen_server_mock:start_link(line, l1, strict),
  S1 = gen_server_mock:start_link(stop, s1, strict),
  P1 = gen_server_mock:start_link(citizen, p1, strict),
  P2 = gen_server_mock:start_link(citizen, p2, strict),
  P3 = gen_server_mock:start_link(citizen, p3, strict),

  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  Vehicle = vehicle_supervisor:start_vehicle(4, L1, TargetStop, bus),
  gen_server_mock:expect_cast(Time, {?SUBSCRIBE, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(Time, ?GET_CURRENT_TIME, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  vehicle_supervisor:stop_vehicle(Vehicle).

next_stop_reached_test() ->
  vehicle_supervisor:start_link(),
  StartStop = gen_server_mock:start_link(stop, startstop, strict),
  TargetStop = gen_server_mock:start_link(stop, targetstop, strict),
  Time = gen_server_mock:start_global(time, time, strict),
  L1 = gen_server_mock:start_link(line, l1, strict),
  S1 = gen_server_mock:start_link(stop, s1, strict),
  P1 = gen_server_mock:start_link(citizen, p1, strict),
  P2 = gen_server_mock:start_link(citizen, p2, strict),
  P3 = gen_server_mock:start_link(citizen, p3, strict),

  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  Vehicle = vehicle_supervisor:start_vehicle(4, L1, TargetStop, bus),
  gen_server_mock:expect_cast(Time, {?SUBSCRIBE, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(Time, ?GET_CURRENT_TIME, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  gen_server_mock:expect_call(P1, {vehicle_checked_in, Vehicle}, leave),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_cast(S1, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  vehicle:?NEW_TIME(Vehicle, 1234+5, true),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  vehicle_supervisor:stop_vehicle(Vehicle).

next_stop_not_reached_test() ->
  vehicle_supervisor:start_link(),
  StartStop = gen_server_mock:start_link(stop, startstop, strict),
  TargetStop = gen_server_mock:start_link(stop, targetstop, strict),
  Time = gen_server_mock:start_global(time, time, strict),
  L1 = gen_server_mock:start_link(line, l1, strict),
  S1 = gen_server_mock:start_link(stop, s1, strict),
  P1 = gen_server_mock:start_link(citizen, p1, strict),
  P2 = gen_server_mock:start_link(citizen, p2, strict),
  P3 = gen_server_mock:start_link(citizen, p3, strict),

  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  Vehicle = vehicle_supervisor:start_vehicle(4, L1, TargetStop, bus),
  gen_server_mock:expect_cast(Time, {?SUBSCRIBE, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(Time, ?GET_CURRENT_TIME, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  vehicle:?NEW_TIME(Vehicle, 1234+2, true),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  vehicle_supervisor:stop_vehicle(Vehicle).

target_stop_reached_test() ->
  vehicle_supervisor:start_link(),
  StartStop = gen_server_mock:start_link(stop, startstop, strict),
  TargetStop = gen_server_mock:start_link(stop, targetstop, strict),
  Time = gen_server_mock:start_global(time, time, strict),
  L1 = gen_server_mock:start_link(line, l1, strict),
  P1 = gen_server_mock:start_link(citizen, p1, strict),
  P2 = gen_server_mock:start_link(citizen, p2, strict),
  P3 = gen_server_mock:start_link(citizen, p3, strict),

  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  Vehicle = vehicle_supervisor:start_vehicle(4, L1, TargetStop, bus),
  gen_server_mock:expect_cast(Time, {?SUBSCRIBE, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {TargetStop, 5}),
  gen_server_mock:expect_call(Time, ?GET_CURRENT_TIME, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, Vehicle}, leave),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_cast(TargetStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  vehicle:?NEW_TIME(Vehicle, 1234+5, true),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  vehicle_supervisor:stop_vehicle(Vehicle).

increment_boarding_passenger_test() ->
  vehicle_supervisor:start_link(),
  StartStop = gen_server_mock:start_link(stop, startstop, strict),
  TargetStop = gen_server_mock:start_link(stop, targetstop, strict),
  Time = gen_server_mock:start_global(time, time, strict),
  L1 = gen_server_mock:start_link(line, l1, strict),
  S1 = gen_server_mock:start_link(stop, s1, strict),
  P1 = gen_server_mock:start_link(citizen, p1, strict),
  P2 = gen_server_mock:start_link(citizen, p2, strict),
  P3 = gen_server_mock:start_link(citizen, p3, strict),

  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  Vehicle = vehicle_supervisor:start_vehicle(4, L1, TargetStop, bus),
  gen_server_mock:expect_cast(Time, {?SUBSCRIBE, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 2, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),

  vehicle:?INCREMENT_BOARDING_PASSENGER(Vehicle, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(Time, ?GET_CURRENT_TIME, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  vehicle_supervisor:stop_vehicle(Vehicle).

new_time_not_driving_test() ->
  vehicle_supervisor:start_link(),
  StartStop = gen_server_mock:start_link(stop, startstop, strict),
  TargetStop = gen_server_mock:start_link(stop, targetstop, strict),
  Time = gen_server_mock:start_global(time, time, strict),
  L1 = gen_server_mock:start_link(line, l1, strict),
  S1 = gen_server_mock:start_link(stop, s1, strict),

  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  Vehicle = vehicle_supervisor:start_vehicle(3, L1, TargetStop, bus),
  gen_server_mock:expect_cast(Time, {?SUBSCRIBE, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  vehicle:?NEW_TIME(Vehicle, 1233, true),
  
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(Time, ?GET_CURRENT_TIME, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, gen_server_utils:extract_pid(Vehicle)}),
  vehicle:?CHECKIN_OK(Vehicle, StartStop, 0, true),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  vehicle_supervisor:stop_vehicle(Vehicle).
