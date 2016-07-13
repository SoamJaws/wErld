-module(line_tests).
-include("public_transport.hrl").
-include_lib("eunit/include/eunit.hrl").

get_next_stop_test() ->
  line_supervisor:start_link(),
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  Dur1_2 = 10,
  Dur2_3 = 20,
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop3, Stop1), {Stop2, Dur1_2}),
  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop3, Stop2), {Stop3, Dur2_3}),
  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop3, Stop3), none),
  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop1, Stop3), {Stop2, Dur2_3}),
  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop1, Stop2), {Stop1, Dur1_2}),
  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop1, Stop1), none),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  line_supervisor:stop_line(Line).

get_neighbors_test() ->
  line_supervisor:start_link(),
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  {ok, Stop4} = stop:start_link(stop4),
  {ok, Stop5} = stop:start_link(stop5),
  Dur1_2 = 7,
  Dur2_3 = 11,
  Dur3_4 = 13,
  Dur4_5 = 17,
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3, Dur3_4, Stop4, Dur4_5, Stop5], bus),
  
  ?assertEqual(line:?GET_NEIGHBORS(Line, Stop1), [{Stop2, Dur1_2, Stop5, Line}]),
  ?assertEqual(line:?GET_NEIGHBORS(Line, Stop2), [{Stop1, Dur1_2, Stop1, Line}, {Stop3, Dur2_3, Stop5, Line}]),
  ?assertEqual(line:?GET_NEIGHBORS(Line, Stop3), [{Stop2, Dur2_3, Stop1, Line}, {Stop4, Dur3_4, Stop5, Line}]),
  ?assertEqual(line:?GET_NEIGHBORS(Line, Stop4), [{Stop3, Dur3_4, Stop1, Line}, {Stop5, Dur4_5, Stop5, Line}]),
  ?assertEqual(line:?GET_NEIGHBORS(Line, Stop5), [{Stop4, Dur4_5, Stop1, Line}]),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  stop:stop(Stop4),
  stop:stop(Stop5),
  line_supervisor:stop_line(Line).

get_other_end_test() ->
  line_supervisor:start_link(),
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  Dur1_2 = 10,
  Dur2_3 = 20,
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertEqual(line:?GET_OTHER_END(Line, Stop1), Stop3),
  ?assertEqual(line:?GET_OTHER_END(Line, Stop3), Stop1),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  line_supervisor:stop_line(Line).

contains_stop_test() ->
  line_supervisor:start_link(),
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  {ok, Stop4} = stop:start_link(stop4),
  Dur1_2 = 10,
  Dur2_3 = 20,
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertEqual(true, line:?CONTAINS_STOP(Line, Stop1)),
  ?assertEqual(true, line:?CONTAINS_STOP(Line, Stop2)),
  ?assertEqual(true, line:?CONTAINS_STOP(Line, Stop3)),
  ?assertEqual(false, line:?CONTAINS_STOP(Line, Stop4)),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  stop:stop(Stop4),
  line_supervisor:stop_line(Line).

get_duration_test() ->
  line_supervisor:start_link(),
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  {ok, Stop4} = stop:start_link(stop4),
  {ok, Stop5} = stop:start_link(stop5),
  Dur1_2 = 7,
  Dur2_3 = 11,
  Dur3_4 = 13,
  Dur4_5 = 17,
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3, Dur3_4, Stop4, Dur4_5, Stop5], bus),

  ?assertEqual(line:?GET_DURATION(Line, Stop1, Stop2), Dur1_2),
  ?assertEqual(line:?GET_DURATION(Line, Stop1, Stop3), Dur1_2 + Dur2_3),
  ?assertEqual(line:?GET_DURATION(Line, Stop1, Stop4), Dur1_2 + Dur2_3 + Dur3_4),
  ?assertEqual(line:?GET_DURATION(Line, Stop1, Stop5), Dur1_2 + Dur2_3 + Dur3_4 + Dur4_5),
  ?assertEqual(line:?GET_DURATION(Line, Stop2, Stop3), Dur2_3),
  ?assertEqual(line:?GET_DURATION(Line, Stop2, Stop4), Dur2_3 + Dur3_4),
  ?assertEqual(line:?GET_DURATION(Line, Stop2, Stop5), Dur2_3 + Dur3_4 + Dur4_5),
  ?assertEqual(line:?GET_DURATION(Line, Stop3, Stop4), Dur3_4),
  ?assertEqual(line:?GET_DURATION(Line, Stop3, Stop5), Dur3_4 + Dur4_5),
  ?assertEqual(line:?GET_DURATION(Line, Stop4, Stop5), Dur4_5),

  ?assertEqual(line:?GET_DURATION(Line, Stop2, Stop1), Dur1_2),
  ?assertEqual(line:?GET_DURATION(Line, Stop3, Stop1), Dur1_2 + Dur2_3),
  ?assertEqual(line:?GET_DURATION(Line, Stop4, Stop1), Dur1_2 + Dur2_3 + Dur3_4),
  ?assertEqual(line:?GET_DURATION(Line, Stop5, Stop1), Dur1_2 + Dur2_3 + Dur3_4 + Dur4_5),
  ?assertEqual(line:?GET_DURATION(Line, Stop3, Stop2), Dur2_3),
  ?assertEqual(line:?GET_DURATION(Line, Stop4, Stop2), Dur2_3 + Dur3_4),
  ?assertEqual(line:?GET_DURATION(Line, Stop5, Stop2), Dur2_3 + Dur3_4 + Dur4_5),
  ?assertEqual(line:?GET_DURATION(Line, Stop4, Stop3), Dur3_4),
  ?assertEqual(line:?GET_DURATION(Line, Stop5, Stop3), Dur3_4 + Dur4_5),
  ?assertEqual(line:?GET_DURATION(Line, Stop5, Stop4), Dur4_5),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  stop:stop(Stop4),
  stop:stop(Stop5),
  line_supervisor:stop_line(Line).

is_end_stop_test() ->
  line_supervisor:start_link(),
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  Dur1_2 = 10,
  Dur2_3 = 20,

  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),
  ?assertEqual(true, line:?IS_END_STOP(Line, Stop1)),
  ?assertEqual(false, line:?IS_END_STOP(Line, Stop2)),
  ?assertEqual(true, line:?IS_END_STOP(Line, Stop3)),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  line_supervisor:stop_line(Line).

get_intersection_test() ->
  line_supervisor:start_link(),
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  {ok, Stop4} = stop:start_link(stop4),
  {ok, Stop5} = stop:start_link(stop5),
  {ok, Stop6} = stop:start_link(stop6),
  {ok, Stop7} = stop:start_link(stop7),
  {ok, Stop8} = stop:start_link(stop8),
  {ok, Stop9} = stop:start_link(stop9),
  Line1 = line_supervisor:start_line(1, [Stop1, 5, Stop2, 5, Stop3], bus),
  Line2 = line_supervisor:start_line(2, [Stop4, 5, Stop2, 5, Stop5], bus),
  Line3 = line_supervisor:start_line(3, [Stop1, 5, Stop6, 5, Stop7], bus),
  Line4 = line_supervisor:start_line(4, [Stop8, 5, Stop9, 5, Stop7], bus),

  ?assertEqual(line:?GET_INTERSECTION(Line1, Line2), Stop2),
  ?assertEqual(line:?GET_INTERSECTION(Line2, Line1), Stop2),
  ?assertEqual(line:?GET_INTERSECTION(Line1, Line3), Stop1),
  ?assertEqual(line:?GET_INTERSECTION(Line3, Line1), Stop1),
  ?assertEqual(line:?GET_INTERSECTION(Line3, Line4), Stop7),
  ?assertEqual(line:?GET_INTERSECTION(Line4, Line3), Stop7),

  ?assertEqual(line:?GET_INTERSECTION(Line1, Line4), none),
  ?assertEqual(line:?GET_INTERSECTION(Line4, Line1), none),
  ?assertEqual(line:?GET_INTERSECTION(Line2, Line3), none),
  ?assertEqual(line:?GET_INTERSECTION(Line3, Line2), none),
  ?assertEqual(line:?GET_INTERSECTION(Line2, Line4), none),
  ?assertEqual(line:?GET_INTERSECTION(Line4, Line2), none),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  stop:stop(Stop4),
  stop:stop(Stop5),
  stop:stop(Stop6),
  stop:stop(Stop7),
  stop:stop(Stop8),
  stop:stop(Stop9),
  line_supervisor:stop_line(Line1),
  line_supervisor:stop_line(Line2),
  line_supervisor:stop_line(Line3),
  line_supervisor:stop_line(Line4).

get_number_test() ->
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  Line = line_supervisor:start_line(1, [Stop1, 5, Stop2, 5, Stop3], bus),

  ?assertEqual(line:?GET_NUMBER(Line), 1),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  line_supervisor:stop_line(Line).

get_target_test() ->
  line_supervisor:start_link(),
  {ok, Stop1} = stop:start_link(stop1),
  {ok, Stop2} = stop:start_link(stop2),
  {ok, Stop3} = stop:start_link(stop3),
  {ok, Stop4} = stop:start_link(stop4),
  {ok, Stop5} = stop:start_link(stop5),
  Dur1_2 = 7,
  Dur2_3 = 11,
  Dur3_4 = 13,
  Dur4_5 = 17,
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3, Dur3_4, Stop4, Dur4_5, Stop5], bus),

  ?assertEqual(line:?GET_TARGET(Line, Stop1, Stop3), Stop5),
  ?assertEqual(line:?GET_TARGET(Line, Stop5, Stop3), Stop1),
  ?assertEqual(line:?GET_TARGET(Line, Stop3, Stop4), Stop5),
  ?assertEqual(line:?GET_TARGET(Line, Stop3, Stop2), Stop1),

  stop:stop(Stop1),
  stop:stop(Stop2),
  stop:stop(Stop3),
  stop:stop(Stop4),
  stop:stop(Stop5),
  line_supervisor:stop_line(Line).
