-module(vehicle_tests).
-include("infrastructure.hrl").
-include_lib("eunit/include/eunit.hrl").

checkin_ok_test() ->
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, StartStop} = gen_server_mock:start_link(s1, strict),
  {ok, TargetStop} = gen_server_mock:start_link(s2, strict),

  %% TODO Solve match PID from init
  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle, false}),
  {ok, Vehicle} = vehicle:start_link(3, L1, TargetStop, bus),

  vehicle:?CHECKIN_OK(Vehicle, StartStop, 0, true),

  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(StartStop)),
  ?assert(gen_server_mock:validate(TargetStop)),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(StartStop),
  gen_server_mock:stop(TargetStop),
  stop:stop(Vehicle).

%boarding_complete_test() ->
%  {ok, L1} = gen_server_mock:start_link(l1, strict),
%  {ok, StartStop} = gen_server_mock:start_link(s1, strict),
%  {ok, TargetStop} = gen_server_mock:start_link(s2, strict),
%  {ok, S1} = gen_server_mock:start_link(s1, strict),
%  {ok, P1} = gen_server_mock:start_link(p1, strict),
%  {ok, P2} = gen_server_mock:start_link(p2, strict),
%  {ok, P3} = gen_server_mock:start_link(p3, strict),
%
%  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
%  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle1, false}),
%  {ok, Vehicle1} = vehicle:start_link(3, L1, TargetStop, bus),
%
%  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
%  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle2, false}),
%  {ok, Vehicle2} = vehicle:start_link(3, L1, TargetStop, bus),
%
%  gen_server_mock:expect_call(L1, {?GET_OTHER_END, TargetStop}, StartStop),
%  gen_server_mock:expect_cast(StartStop, {?VEHICLE_CHECK_IN, Vehicle3, false}),
%  {ok, Vehicle3} = vehicle:start_link(3, L1, TargetStop, bus),
%
%  vehicle:?BOARDING_COMPLETE(Vehicle1, true),
%
%  gen_server_mock:expect_cast({?GET_NEXT_STOP, })
