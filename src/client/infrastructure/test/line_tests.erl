-module(line_tests).
-include("infrastructure_state.hrl").
-include_lib("eunit/include/eunit.hrl").

get_next_stop_test() ->
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  Dur1_2 = 10,
  Dur2_3 = 20,
  {ok, Line} = line:start_link(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertEqual(line:get_next_stop(Line, Stop3, Stop1), {Stop2, Dur1_2}),
  ?assertEqual(line:get_next_stop(Line, Stop3, Stop2), {Stop3, Dur2_3}),
  ?assertEqual(line:get_next_stop(Line, Stop3, Stop3), none),
  ?assertEqual(line:get_next_stop(Line, Stop1, Stop3), {Stop2, Dur2_3}),
  ?assertEqual(line:get_next_stop(Line, Stop1, Stop2), {Stop1, Dur1_2}),
  ?assertEqual(line:get_next_stop(Line, Stop1, Stop1), none),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  line:stop(Line).

get_neighbors_test() ->
  ?assert(false).

get_other_end_test() ->
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  Dur1_2 = 10,
  Dur2_3 = 20,
  {ok, Line} = line:start_link(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertEqual(line:get_other_end(Line, Stop1), Stop3),
  ?assertEqual(line:get_other_end(Line, Stop3), Stop1),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  line:stop(Line).

contains_stop_test() ->
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  {ok, Stop4} = stop:start_link(stop4),
  Dur1_2 = 10,
  Dur2_3 = 20,
  {ok, Line} = line:start_link(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assert(line:contains_stop(Line, Stop1)),
  ?assert(line:contains_stop(Line, Stop2)),
  ?assert(line:contains_stop(Line, Stop3)),
  ?assertNot(line:contains_stop(Line, Stop4)),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  stop:stop(Stop4),
  line:stop(Line).

get_duration_test() ->
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  {ok, Stop4} = stop:start_link(stop4),
  {ok, Stop5} = stop:start_link(stop5),
  Dur1_2 = 7,
  Dur2_3 = 11,
  Dur3_4 = 13,
  Dur4_5 = 17,
  {ok, Line} = line:start_link(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3, Dur3_4, Stop4, Dur4_5, Stop5], bus),

  ?assertEqual(line:get_duration(Line, Stop1, Stop2), Dur1_2),
  ?assertEqual(line:get_duration(Line, Stop1, Stop3), Dur1_2 + Dur2_3),
  ?assertEqual(line:get_duration(Line, Stop1, Stop4), Dur1_2 + Dur2_3 + Dur3_4),
  ?assertEqual(line:get_duration(Line, Stop1, Stop5), Dur1_2 + Dur2_3 + Dur3_4 + Dur4_5),
  ?assertEqual(line:get_duration(Line, Stop2, Stop3), Dur2_3),
  ?assertEqual(line:get_duration(Line, Stop2, Stop4), Dur2_3 + Dur3_4),
  ?assertEqual(line:get_duration(Line, Stop2, Stop5), Dur2_3 + Dur3_4 + Dur4_5),
  ?assertEqual(line:get_duration(Line, Stop3, Stop4), Dur3_4),
  ?assertEqual(line:get_duration(Line, Stop3, Stop5), Dur3_4 + Dur4_5),
  ?assertEqual(line:get_duration(Line, Stop4, Stop5), Dur4_5),

  ?assertEqual(line:get_duration(Line, Stop2, Stop1), Dur1_2),
  ?assertEqual(line:get_duration(Line, Stop3, Stop1), Dur1_2 + Dur2_3),
  ?assertEqual(line:get_duration(Line, Stop4, Stop1), Dur1_2 + Dur2_3 + Dur3_4),
  ?assertEqual(line:get_duration(Line, Stop5, Stop1), Dur1_2 + Dur2_3 + Dur3_4 + Dur4_5),
  ?assertEqual(line:get_duration(Line, Stop3, Stop2), Dur2_3),
  ?assertEqual(line:get_duration(Line, Stop4, Stop2), Dur2_3 + Dur3_4),
  ?assertEqual(line:get_duration(Line, Stop5, Stop2), Dur2_3 + Dur3_4 + Dur4_5),
  ?assertEqual(line:get_duration(Line, Stop4, Stop3), Dur3_4),
  ?assertEqual(line:get_duration(Line, Stop5, Stop3), Dur3_4 + Dur4_5),
  ?assertEqual(line:get_duration(Line, Stop5, Stop4), Dur4_5),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  stop:stop(Stop4),
  stop:stop(Stop5),
  line:stop(Line).

is_end_stop_test() ->
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  Dur1_2 = 10,
  Dur2_3 = 20,

  {ok, Line} = line:start_link(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),
  ?assert(line:is_end_stop(Line, Stop1)),
  ?assertNot(line:is_end_stop(Line, Stop2)),
  ?assert(line:is_end_stop(Line, Stop3)),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  line:stop(Line).

get_intersection_test() ->
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  {ok, Stop4} = stop:start_link(stop4),
  {ok, Stop5} = stop:start_link(stop5),
  {ok, Stop6} = stop:start_link(stop6),
  {ok, Stop7} = stop:start_link(stop7),
  {ok, Stop8} = stop:start_link(stop8),
  {ok, Stop9} = stop:start_link(stop9),
  {ok, Line1} = line:start_link(1, [Stop1, 5, Stop2, 5, Stop3], bus),
  {ok, Line2} = line:start_link(2, [Stop4, 5, Stop2, 5, Stop5], bus),
  {ok, Line3} = line:start_link(3, [Stop1, 5, Stop6, 5, Stop7], bus),
  {ok, Line4} = line:start_link(4, [Stop8, 5, Stop9, 5, Stop7], bus),

  ?assertEqual(line:get_intersection(Line1, Line2), Stop2),
  ?assertEqual(line:get_intersection(Line2, Line1), Stop2),
  ?assertEqual(line:get_intersection(Line1, Line3), Stop1),
  ?assertEqual(line:get_intersection(Line3, Line1), Stop1),
  ?assertEqual(line:get_intersection(Line3, Line4), Stop7),
  ?assertEqual(line:get_intersection(Line4, Line3), Stop7),

  ?assertEqual(line:get_intersection(Line1, Line4), none),
  ?assertEqual(line:get_intersection(Line4, Line1), none),
  ?assertEqual(line:get_intersection(Line2, Line3), none),
  ?assertEqual(line:get_intersection(Line3, Line2), none),
  ?assertEqual(line:get_intersection(Line2, Line4), none),
  ?assertEqual(line:get_intersection(Line4, Line2), none),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  stop:stop(Stop4),
  stop:stop(Stop5),
  stop:stop(Stop6),
  stop:stop(Stop7),
  stop:stop(Stop8),
  stop:stop(Stop9),
  line:stop(Line1),
  line:stop(Line2),
  line:stop(Line3),
  line:stop(Line4).

state_test() ->
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  Dur1_2 = 10,
  Dur2_3 = 20,
  {ok, Line} = line:start_link(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertMatch(#line_state{number=1, stops=[Stop1, Dur1_2, Stop2, Dur2_3, Stop3], type=bus}, line:state(Line)),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  line:stop(Line).
