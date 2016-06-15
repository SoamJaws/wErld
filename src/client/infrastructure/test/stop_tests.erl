-module(stop_tests).
-include("infrastructure_state.hrl").
-include_lib("eunit/include/eunit.hrl").

passenger_check_in_test() ->
  {ok, Stop} = stop:start_link(stop1, 2),
  {ok, P1} = gen_server_mock:start_link(p1),
  {ok, P2} = gen_server_mock:start_link(p2),
  {ok, P3} = gen_server_mock:start_link(p3),
  
  ?assertEqual(ok, stop:passenger_check_in(Stop, P1)),
  ?assertMatch({nok, _}, stop:passenger_check_in(Stop, P1)),
  ?assertEqual(ok, stop:passenger_check_in(Stop, P2)),
  ?assertMatch({nok, _}, stop:passenger_check_in(Stop, P3)),

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
  
  ?assertEqual(ok, stop:passenger_check_in(Stop, P1)),
  ?assertEqual(ok, stop:passenger_check_in(Stop, P2)),
  ?assertEqual(ok, stop:passenger_check_in(Stop, P3)),

  gen_server_mock:expect_cast(V1, {checkin_ok, Stop, false, Stop}),
  gen_server_mock:expect_cast(P1, {vehicle_check_in, V1}),
  gen_server_mock:expect_cast(P2, {vehicle_check_in, V1}),
  gen_server_mock:expect_cast(P3, {vehicle_check_in, V1}),

  stop:vehicle_check_in(Stop, V1, true),

  stop:passenger_check_out(Stop, P1, false),
  stop:passenger_check_out(Stop, P2, false),

  gen_server_mock:expect_cast(V1, {boarding_complete, false, Stop}),

  stop:passenger_check_out(Stop, P3, true),

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
  
  ?assertEqual(ok, stop:passenger_check_in(Stop, P1)),
  ?assertEqual(ok, stop:passenger_check_in(Stop, P2)),
  ?assertEqual(ok, stop:passenger_check_in(Stop, P3)),

  gen_server_mock:expect_cast(V1, {checkin_ok, Stop, false, Stop}),
  gen_server_mock:expect_cast(P1, {vehicle_check_in, V1}),
  gen_server_mock:expect_cast(P2, {vehicle_check_in, V1}),
  gen_server_mock:expect_cast(P3, {vehicle_check_in, V1}),

  stop:vehicle_check_in(Stop, V1, true),

  gen_server_mock:finalize(P1),
  gen_server_mock:finalize(P2),
  gen_server_mock:finalize(P3),
  gen_server_mock:finalize(V1),
  gen_server_mock:stop(P1),
  gen_server_mock:stop(P2),
  gen_server_mock:stop(P3),
  gen_server_mock:stop(V1),
  stop:stop(Stop).

vehicle_check_out_test() ->
  {ok, Stop} = stop:start_link(stop1, 3),
  {ok, V1} = gen_server_mock:start_link(v1),

  gen_server_mock:expect_cast(V1, {checkin_ok, Stop, false, Stop}),
  gen_server_mock:expect_cast(P1, {vehicle_check_in, V1}),
  gen_server_mock:expect_cast(P2, {vehicle_check_in, V1}),
  gen_server_mock:expect_cast(P3, {vehicle_check_in, V1}),

  stop:vehicle_check_in(Stop, V1, true),

  stop:passenger_check_out(Stop, P1, false),
  stop:passenger_check_out(Stop, P2, false),

  gen_server_mock:expect_cast(V1, {boarding_complete, false, Stop}),

  stop:passenger_check_out(Stop, P3, true),

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
