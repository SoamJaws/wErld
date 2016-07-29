-module(line_SUITE).
-include("public_transport.hrl").
-include("logger.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). %% For assertion macros

-export([ get_next_stop_case/1
        , get_other_end_case/1
        , is_end_stop_case/1
        , get_number_case/1
        , contains_stop_case/1
        , get_neighbors_case/1
        , get_duration_case/1
        , get_target_case/1
        , get_intersection_case/1]).

-export([ all/0
        , groups/0
        , init_per_group/2
        , end_per_group/2
        , init_per_testcase/2
        , end_per_testcase/2]).

all() ->
  [ {group, three_stops}
  , {group, four_stops}
  , {group, five_stops}
  , {group, nine_stops}
  ].

groups() ->
  [ { three_stops
    , []
    , [ get_next_stop_case
      , get_other_end_case
      , is_end_stop_case
      , get_number_case
      ]
    }
  , { four_stops
    , []
    , [ contains_stop_case
      ]
    }
  , { five_stops
    , []
    , [ get_neighbors_case
      , get_duration_case
      , get_target_case
      ]
    }
  , { nine_stops
    , []
    , [ get_intersection_case
      ]
    }
  ].

init_per_group(three_stops, Config) ->
  Stop1 = gen_server_mock:start(stop, stop1, strict),
  Stop2 = gen_server_mock:start(stop, stop2, strict),
  Stop3 = gen_server_mock:start(stop, stop3, strict),
  Dur1_2 = 7,
  Dur2_3 = 11,
  Config ++ [ {stop1, Stop1}
            , {stop2, Stop2}
            , {stop3, Stop3}
            , {dur1_2, Dur1_2}
            , {dur2_3, Dur2_3}
            ];

init_per_group(four_stops, Config) ->
  Stop4 = gen_server_mock:start(stop, stop4, strict),
  init_per_group(three_stops, Config) ++[{stop4, Stop4}];

init_per_group(five_stops, Config) ->
  Stop5 = gen_server_mock:start(stop, stop5, strict),
  Dur3_4 = 13,
  Dur4_5 = 17,
  init_per_group(four_stops, Config) ++ [ {stop5, Stop5}
                                        , {dur3_4, Dur3_4}
                                        , {dur4_5, Dur4_5}
                                        ];

init_per_group(nine_stops, Config) ->
  Stop6 = gen_server_mock:start(stop, stop6, strict),
  Stop7 = gen_server_mock:start(stop, stop7, strict),
  Stop8 = gen_server_mock:start(stop, stop8, strict),
  Stop9 = gen_server_mock:start(stop, stop9, strict),
  init_per_group(five_stops, Config) ++ [ {stop6, Stop6}
                                        , {stop7, Stop7}
                                        , {stop8, Stop8}
                                        , {stop9, Stop9}
                                        ];

init_per_group(_Group, Config) ->
  Config.


end_per_group(three_stops, Config) ->
  Stop1 = ?config(stop1, Config),
  Stop2 = ?config(stop2, Config),
  Stop3 = ?config(stop3, Config),
  ?assert(gen_server_mock:validate(Stop1)),
  ?assert(gen_server_mock:validate(Stop2)),
  ?assert(gen_server_mock:validate(Stop3)),
  gen_server_mock:stop(Stop1),
  gen_server_mock:stop(Stop2),
  gen_server_mock:stop(Stop3);

end_per_group(four_stops, Config) ->
  end_per_group(three_stops, Config),
  Stop4 = ?config(stop4, Config),
  ?assert(gen_server_mock:validate(Stop4)),
  gen_server_mock:stop(Stop4);

end_per_group(five_stops, Config) ->
  end_per_group(four_stops, Config),
  Stop5 = ?config(stop5, Config),
  ?assert(gen_server_mock:validate(Stop5)),
  gen_server_mock:stop(Stop5);

end_per_group(nine_stops, Config) ->
  end_per_group(five_stop, Config),
  Stop6 = ?config(stop6, Config),
  Stop7 = ?config(stop7, Config),
  Stop8 = ?config(stop8, Config),
  Stop9 = ?config(stop9, Config),
  ?assert(gen_server_mock:validate(Stop6)),
  ?assert(gen_server_mock:validate(Stop7)),
  ?assert(gen_server_mock:validate(Stop8)),
  ?assert(gen_server_mock:validate(Stop9)),
  gen_server_mock:stop(Stop6),
  gen_server_mock:stop(Stop7),
  gen_server_mock:stop(Stop8),
  gen_server_mock:stop(Stop9);

end_per_group(_Group, _Config) ->
  ok.


init_per_testcase(TestCase, Config) ->
  put(id, ?MODULE),
  put(module, ?MODULE_STRING),
  logger:start_link(?MODULE_STRING ++ [$_|atom_to_list(TestCase)] ++ "_log"),
  line_supervisor:start_link(),
  Config.


end_per_testcase(TestCase, Config) ->
  logger:stop(),
  LogName = ?MODULE_STRING ++ [$_|atom_to_list(TestCase)] ++ "_log",
  ct:comment("<a href=\"../../logs/" ++ LogName ++ "/index.html\">" ++ LogName ++ "</a>"),
  Config.


get_next_stop_case(Config) ->
  Stop1 = ?config(stop1, Config),
  Stop2 = ?config(stop2, Config),
  Stop3 = ?config(stop3, Config),
  Dur1_2 = ?config(dur1_2, Config),
  Dur2_3 = ?config(dur2_3, Config),
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop3, Stop1), {Stop2, Dur1_2}),
  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop3, Stop2), {Stop3, Dur2_3}),
  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop3, Stop3), none),
  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop1, Stop3), {Stop2, Dur2_3}),
  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop1, Stop2), {Stop1, Dur1_2}),
  ?assertEqual(line:?GET_NEXT_STOP(Line, Stop1, Stop1), none),

  line_supervisor:stop_line(Line).


get_other_end_case(Config) ->
  Stop1 = ?config(stop1, Config),
  Stop2 = ?config(stop2, Config),
  Stop3 = ?config(stop3, Config),
  Dur1_2 = ?config(dur1_2, Config),
  Dur2_3 = ?config(dur2_3, Config),
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertEqual(line:?GET_OTHER_END(Line, Stop1), Stop3),
  ?assertEqual(line:?GET_OTHER_END(Line, Stop3), Stop1),

  line_supervisor:stop_line(Line).


is_end_stop_case(Config) ->
  Stop1 = ?config(stop1, Config),
  Stop2 = ?config(stop2, Config),
  Stop3 = ?config(stop3, Config),
  Dur1_2 = ?config(dur1_2, Config),
  Dur2_3 = ?config(dur2_3, Config),
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertEqual(true, line:?IS_END_STOP(Line, Stop1)),
  ?assertEqual(false, line:?IS_END_STOP(Line, Stop2)),
  ?assertEqual(true, line:?IS_END_STOP(Line, Stop3)),

  line_supervisor:stop_line(Line).


get_number_case(Config) ->
  Stop1 = ?config(stop1, Config),
  Stop2 = ?config(stop2, Config),
  Stop3 = ?config(stop3, Config),
  Dur1_2 = ?config(dur1_2, Config),
  Dur2_3 = ?config(dur2_3, Config),
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertEqual(line:?GET_NUMBER(Line), 1),

  line_supervisor:stop_line(Line).


contains_stop_case(Config) ->
  Stop1 = ?config(stop1, Config),
  Stop2 = ?config(stop2, Config),
  Stop3 = ?config(stop3, Config),
  Stop4 = ?config(stop4, Config),
  Dur1_2 = ?config(dur1_2, Config),
  Dur2_3 = ?config(dur2_3, Config),
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3], bus),

  ?assertEqual(true, line:?CONTAINS_STOP(Line, Stop1)),
  ?assertEqual(true, line:?CONTAINS_STOP(Line, Stop2)),
  ?assertEqual(true, line:?CONTAINS_STOP(Line, Stop3)),
  ?assertEqual(false, line:?CONTAINS_STOP(Line, Stop4)),

  line_supervisor:stop_line(Line).


get_neighbors_case(Config) ->
  Stop1 = ?config(stop1, Config),
  Stop2 = ?config(stop2, Config),
  Stop3 = ?config(stop3, Config),
  Stop4 = ?config(stop4, Config),
  Stop5 = ?config(stop5, Config),
  Dur1_2 = ?config(dur1_2, Config),
  Dur2_3 = ?config(dur2_3, Config),
  Dur3_4 = ?config(dur3_4, Config),
  Dur4_5 = ?config(dur4_5, Config),
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3, Dur3_4, Stop4, Dur4_5, Stop5], bus),
  
  ?assertEqual(line:?GET_NEIGHBORS(Line, Stop1), [{Stop2, Dur1_2, Stop5, Line}]),
  ?assertEqual(line:?GET_NEIGHBORS(Line, Stop2), [{Stop1, Dur1_2, Stop1, Line}, {Stop3, Dur2_3, Stop5, Line}]),
  ?assertEqual(line:?GET_NEIGHBORS(Line, Stop3), [{Stop2, Dur2_3, Stop1, Line}, {Stop4, Dur3_4, Stop5, Line}]),
  ?assertEqual(line:?GET_NEIGHBORS(Line, Stop4), [{Stop3, Dur3_4, Stop1, Line}, {Stop5, Dur4_5, Stop5, Line}]),
  ?assertEqual(line:?GET_NEIGHBORS(Line, Stop5), [{Stop4, Dur4_5, Stop1, Line}]),

  line_supervisor:stop_line(Line).


get_duration_case(Config) ->
  Stop1 = ?config(stop1, Config),
  Stop2 = ?config(stop2, Config),
  Stop3 = ?config(stop3, Config),
  Stop4 = ?config(stop4, Config),
  Stop5 = ?config(stop5, Config),
  Dur1_2 = ?config(dur1_2, Config),
  Dur2_3 = ?config(dur2_3, Config),
  Dur3_4 = ?config(dur3_4, Config),
  Dur4_5 = ?config(dur4_5, Config),
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

  line_supervisor:stop_line(Line).


get_target_case(Config) ->
  Stop1 = ?config(stop1, Config),
  Stop2 = ?config(stop2, Config),
  Stop3 = ?config(stop3, Config),
  Stop4 = ?config(stop4, Config),
  Stop5 = ?config(stop5, Config),
  Dur1_2 = ?config(dur1_2, Config),
  Dur2_3 = ?config(dur2_3, Config),
  Dur3_4 = ?config(dur3_4, Config),
  Dur4_5 = ?config(dur4_5, Config),
  Line = line_supervisor:start_line(1, [Stop1, Dur1_2, Stop2, Dur2_3, Stop3, Dur3_4, Stop4, Dur4_5, Stop5], bus),

  ?assertEqual(line:?GET_TARGET(Line, Stop1, Stop3), Stop5),
  ?assertEqual(line:?GET_TARGET(Line, Stop5, Stop3), Stop1),
  ?assertEqual(line:?GET_TARGET(Line, Stop3, Stop4), Stop5),
  ?assertEqual(line:?GET_TARGET(Line, Stop3, Stop2), Stop1),

  line_supervisor:stop_line(Line).


get_intersection_case(Config) ->
  Stop1 = ?config(stop1, Config),
  Stop2 = ?config(stop2, Config),
  Stop3 = ?config(stop3, Config),
  Stop4 = ?config(stop4, Config),
  Stop5 = ?config(stop5, Config),
  Stop6 = ?config(stop6, Config),
  Stop7 = ?config(stop7, Config),
  Stop8 = ?config(stop8, Config),
  Stop9 = ?config(stop9, Config),
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

  line_supervisor:stop_line(Line1),
  line_supervisor:stop_line(Line2),
  line_supervisor:stop_line(Line3),
  line_supervisor:stop_line(Line4).
