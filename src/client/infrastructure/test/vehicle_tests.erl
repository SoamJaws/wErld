-module(vehicle_tests).
-include("infrastructure.hrl").
-include_lib("eunit/include/eunit.hrl").

checkin_ok_test() ->
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, StartStop} = gen_server_mock:start_link(s1, strict),
  {ok, TargetStop} = gen_server_mock:start_link(s2, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle} = vehicle:start_link(3, L1, TargetStop, bus),
  %% Hack, setting expected cast after call, will work since
  %% casts are not verified until validate is called
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false, Vehicle}),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 0, true),

  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  gen_server_mock:stop(BlackBoard),
  stop:stop(Vehicle).

boarding_complete_test() ->
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, StartStop} = gen_server_mock:start_link(s1, strict),
  {ok, TargetStop} = gen_server_mock:start_link(s2, strict),
  {ok, BlackBoard} = gen_server_mock:start_global(blackboard, strict),
  {ok, Time} = gen_server_mock:start_link(time, strict),
  {ok, S1} = gen_server_mock:start_link(s1, strict),
  {ok, S2} = gen_server_mock:start_link(s2, strict),
  {ok, P1} = gen_server_mock:start_link(p1, strict),
  {ok, P2} = gen_server_mock:start_link(p2, strict),
  {ok, P3} = gen_server_mock:start_link(p3, strict),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle1} = vehicle:start_link(3, L1, TargetStop, bus),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle2} = vehicle:start_link(3, L1, TargetStop, bus),

  gen_server_mock:expect_cast(BlackBoard, {subscribe, time}),
  gen_server_mock:expect_call(L1, ?GET_NUMBER, 1),
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  {ok, Vehicle3} = vehicle:start_link(3, L1, TargetStop, bus),

  vehicle:?CHECKIN_OK(S1, 0),
  gen_server_mock:expect_cast(L1, {?GET_NEXT_STOP, TargetStop, S1}, S2),
  gen_server_mock:expect_call(BlackBoard, {request, timePid}, Time),
  gen_server_mock:expect_call(Time, {request, currentTime}, 1234),
  gen_server_mock:expect_cast(S1, {?VEHICLE_CHECK_OUT, Vehicle1, false}),
  
  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  ?assert(gen_server_mock:validate(BlackBoard)),
  ?assert(gen_server_mock:validate(Time)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(S2)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(S1)),
  ?assert(gen_server_mock:validate(P2)),
  ?assert(gen_server_mock:validate(P3)),
  ?assert(gen_server_mock:stop(L1)),
  ?assert(gen_server_mock:stop(StartStop)),
  ?assert(gen_server_mock:stop(TargetStop)),
  ?assert(gen_server_mock:stop(BlackBoard)),
  ?assert(gen_server_mock:stop(Time)),
  ?assert(gen_server_mock:stop(S1)),
  ?assert(gen_server_mock:stop(S2)),
  ?assert(gen_server_mock:stop(S1)),
  ?assert(gen_server_mock:stop(S1)),
  ?assert(gen_server_mock:stop(P2)),
  ?assert(gen_server_mock:stop(P3)),
  stop:stop(Vehicle1),
  stop:stop(Vehicle2),
  stop:stop(Vehicle3).
