-module(vehicle_tests).
-include("infrastructure.hrl").
-include_lib("eunit/include/eunit.hrl").

checkin_ok_test() ->
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, Vehicle} = vehicle:start_link(3, L1, bus),
  {ok, S1} = gen_server_mock:start_link(s1, strict),
  vehicle:?CHECKIN_OK(Vehicle, S1, 0, true),

  ?assert(gen_server_mock:validate(L1)),
  ?assert(gen_server_mock:validate(S1)),
  gen_server_mock:stop(L1),
  gen_server_mock:stop(S1),
  stop:stop(Vehicle).

boarding_complete_test() ->
  {ok, L1} = gen_server_mock:start_link(l1, strict),
  {ok, Vehicle1} = vehicle:start_link(3, L1, bus),
  {ok, Vehicle2} = vehicle:start_link(3, L1, bus),
  {ok, Vehicle3} = vehicle:start_link(3, L1, bus),
  {ok, S1} = gen_server_mock:start_link(s1, strict),
  {ok, P1} = gen_server_mock:start_link(p1, strict),
  {ok, P2} = gen_server_mock:start_link(p2, strict),
  {ok, P3} = gen_server_mock:start_link(p3, strict),

  vehicle:?BOARDING_COMPLETE(Vehicle1, true),

  gen_server_mock:expect_cast({?GET_NEXT_STOP, })
