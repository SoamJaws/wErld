-module(line_tests).
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
  ?assertEqual(line:get_next_stop(Line, Stop1, Stop1), none).

get_neighbors_test() ->
  ?assert(false).

get_other_end_test() ->
  ?assert(false).

contains_stop_test() ->
  ?assert(false).

get_duration_test() ->
  ?assert(false).

is_end_stop_test() ->
  ?assert(false).

get_intersection_test() ->
  ?assert(false).
