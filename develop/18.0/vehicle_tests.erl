-module(vehicle_tests).
-include("public_transport.hrl").
-include("time.hrl").
-include_lib("eunit/include/eunit.hrl").

checkin_ok_test() ->
  {ok, StartStop} = gen_server_mock:start_link(startstop, strict),
  {ok, TargetStop} = gen_server_mock:start_link(targetstop, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),
  {ok, Time} = gen_server_mock:start_link(time, strict),
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, S1} = gen_server_mock:start_link(s1, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle} = vehicle:start_link(3, L1, TargetStop, bus),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(BlackBoard, {request, timePid}, Time),
  gen_server_mock:expect_call(Time, {request, currentTime}, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, Vehicle}),
  vehicle:?CHECKIN_OK(Vehicle, StartStop, 0, true),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(BlackBoard),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  vehicle:stop(Vehicle).

boarding_passenger_capacity_reached_test() ->
  {ok, StartStop} = gen_server_mock:start_link(startstop, strict),
  {ok, TargetStop} = gen_server_mock:start_link(targetstop, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),
  {ok, Time} = gen_server_mock:start_link(time, strict),
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, S1} = gen_server_mock:start_link(s1, strict),
  {ok, P1} = gen_server_mock:start_link(p1, strict),
  {ok, P2} = gen_server_mock:start_link(p2, strict),
  {ok, P3} = gen_server_mock:start_link(p3, strict),
  {ok, P4} = gen_server_mock:start_link(p4, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle} = vehicle:start_link(3, L1, TargetStop, bus),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 4, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(BlackBoard, {request, timePid}, Time),
  gen_server_mock:expect_call(Time, {request, currentTime}, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, Vehicle}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P4), nok),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  ?assert(gen_server_mock:validate(P4)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(BlackBoard),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(P4),
  vehicle:stop(Vehicle).

boarding_passenger_below_capacity_test() ->
  {ok, StartStop} = gen_server_mock:start_link(startstop, strict),
  {ok, TargetStop} = gen_server_mock:start_link(targetstop, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),
  {ok, Time} = gen_server_mock:start_link(time, strict),
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, S1} = gen_server_mock:start_link(s1, strict),
  {ok, P1} = gen_server_mock:start_link(p1, strict),
  {ok, P2} = gen_server_mock:start_link(p2, strict),
  {ok, P3} = gen_server_mock:start_link(p3, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle} = vehicle:start_link(4, L1, TargetStop, bus),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(BlackBoard, {request, timePid}, Time),
  gen_server_mock:expect_call(Time, {request, currentTime}, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, Vehicle}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(BlackBoard),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  vehicle:stop(Vehicle).

next_stop_reached_test() ->
  {ok, StartStop} = gen_server_mock:start_link(startstop, strict),
  {ok, TargetStop} = gen_server_mock:start_link(targetstop, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),
  {ok, Time} = gen_server_mock:start_link(time, strict),
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, S1} = gen_server_mock:start_link(s1, strict),
  {ok, P1} = gen_server_mock:start_link(p1, strict),
  {ok, P2} = gen_server_mock:start_link(p2, strict),
  {ok, P3} = gen_server_mock:start_link(p3, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle} = vehicle:start_link(4, L1, TargetStop, bus),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(BlackBoard, {request, timePid}, Time),
  gen_server_mock:expect_call(Time, {request, currentTime}, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, Vehicle}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  gen_server_mock:expect_call(P1, {vehicle_checked_in, Vehicle}, leave),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_cast(S1, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),
  vehicle:?NEW_TIME(Vehicle, 1234+5, true),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(BlackBoard),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  vehicle:stop(Vehicle).

next_stop_not_reached_test() ->
  {ok, StartStop} = gen_server_mock:start_link(startstop, strict),
  {ok, TargetStop} = gen_server_mock:start_link(targetstop, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),
  {ok, Time} = gen_server_mock:start_link(time, strict),
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, S1} = gen_server_mock:start_link(s1, strict),
  {ok, P1} = gen_server_mock:start_link(p1, strict),
  {ok, P2} = gen_server_mock:start_link(p2, strict),
  {ok, P3} = gen_server_mock:start_link(p3, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle} = vehicle:start_link(4, L1, TargetStop, bus),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(BlackBoard, {request, timePid}, Time),
  gen_server_mock:expect_call(Time, {request, currentTime}, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, Vehicle}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  vehicle:?NEW_TIME(Vehicle, 1234+2, true),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(BlackBoard),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  vehicle:stop(Vehicle).

target_stop_reached_test() ->
  {ok, StartStop} = gen_server_mock:start_link(startstop, strict),
  {ok, TargetStop} = gen_server_mock:start_link(targetstop, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),
  {ok, Time} = gen_server_mock:start_link(time, strict),
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, P1} = gen_server_mock:start_link(p1, strict),
  {ok, P2} = gen_server_mock:start_link(p2, strict),
  {ok, P3} = gen_server_mock:start_link(p3, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle} = vehicle:start_link(4, L1, TargetStop, bus),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 3, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {TargetStop, 5}),
  gen_server_mock:expect_call(BlackBoard, {request, timePid}, Time),
  gen_server_mock:expect_call(Time, {request, currentTime}, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, Vehicle}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  gen_server_mock:expect_call(P1, {vehicle_checked_in, Vehicle}, leave),
  gen_server_mock:expect_call(P2, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_call(P3, {vehicle_checked_in, Vehicle}, stay),
  gen_server_mock:expect_cast(TargetStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  vehicle:?NEW_TIME(Vehicle, 1234+5, true),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(BlackBoard),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  vehicle:stop(Vehicle).

increment_boarding_passenger_test() ->
  {ok, StartStop} = gen_server_mock:start_link(startstop, strict),
  {ok, TargetStop} = gen_server_mock:start_link(targetstop, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),
  {ok, Time} = gen_server_mock:start_link(time, strict),
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, S1} = gen_server_mock:start_link(s1, strict),
  {ok, P1} = gen_server_mock:start_link(p1, strict),
  {ok, P2} = gen_server_mock:start_link(p2, strict),
  {ok, P3} = gen_server_mock:start_link(p3, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle} = vehicle:start_link(4, L1, TargetStop, bus),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 2, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),

  vehicle:?INCREMENT_BOARDING_PASSENGER(Vehicle, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(BlackBoard, {request, timePid}, Time),
  gen_server_mock:expect_call(Time, {request, currentTime}, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, Vehicle}),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P3), ok),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(BlackBoard),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  vehicle:stop(Vehicle).

new_time_not_driving_test() ->
  {ok, StartStop} = gen_server_mock:start_link(startstop, strict),
  {ok, TargetStop} = gen_server_mock:start_link(targetstop, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),
  {ok, Time} = gen_server_mock:start_link(time, strict),
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, S1} = gen_server_mock:start_link(s1, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle} = vehicle:start_link(3, L1, TargetStop, bus),
  vehicle:?NEW_TIME(Vehicle, 1233, true),
  
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  gen_server_mock:expect_call(L1, {?GET_NEXT_STOP, TargetStop, StartStop}, {S1, 5}),
  gen_server_mock:expect_call(BlackBoard, {request, timePid}, Time),
  gen_server_mock:expect_call(Time, {request, currentTime}, 1234),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_OUT, Vehicle, false, Vehicle}),
  vehicle:?CHECKIN_OK(Vehicle, StartStop, 0, true),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(BlackBoard),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  vehicle:stop(Vehicle).

state_test() ->
  {ok, StartStop} = gen_server_mock:start_link(startstop, strict),
  {ok, TargetStop} = gen_server_mock:start_link(targetstop, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),
  {ok, Time} = gen_server_mock:start_link(time, strict),
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, S1} = gen_server_mock:start_link(s1, strict),
  {ok, P1} = gen_server_mock:start_link(p1, strict),
  {ok, P2} = gen_server_mock:start_link(p2, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle} = vehicle:start_link(4, L1, TargetStop, bus),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 2, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P1), ok),

  vehicle:?INCREMENT_BOARDING_PASSENGER(Vehicle, true),
  ?assertEqual(vehicle:?PASSENGER_BOARD(Vehicle, P2), ok),

  ?assertMatch(#vehicle_state{action={boarding, StartStop}, capacity=4, line={1, L1}, passengers=[P1,P2], boardingPassengers=1, target=TargetStop, type=bus}, vehicle:state(Vehicle)),

  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P1)),
  ?assert(gen_server_mock:validate(P2)),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(BlackBoard),
  gen_server_mock:stop(Time),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  vehicle:stop(Vehicle).
