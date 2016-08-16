-ifndef(__WEATHER_HRL).
-define(__WEATHER_HRL, true).

-include("time.hrl").

-type weather_type() :: sunny | rainy | snowy.
-type climate_type() :: coastal | inland | northern | southern.

-record(weather_controller_state, { city            :: atom()
                                  , climate         :: climate_type()
                                  , downpourUpdates :: non_neg_integer()
                                  , lastChange      :: time()
                                  , updateInterval  :: pos_integer()
                                  }).
-type weather_controller_state() :: #weather_controller_state{}.

-record(monthly_stats, { maxTemp  :: integer()
                       , minTemp  :: integer()
                       , sunHours :: integer()
                       , rainDays :: non_neg_integer()
                       , snowDays :: non_neg_integer()
                       }).
-type monthly_stats() :: #monthly_stats{}.

-define(GET_TYPE, get_type).
-define(GET_TEMP, get_temp).

%% Gothenburg
-define(COASTAL_STATS, [ #monthly_stats{maxTemp=1,  minTemp=-4, sunHours=1, rainDays=0,  snowDays=13}
                       , #monthly_stats{maxTemp=1,  minTemp=-4, sunHours=2, rainDays=2,  snowDays=9}
                       , #monthly_stats{maxTemp=5,  minTemp=-2, sunHours=3, rainDays=12, snowDays=0}
                       , #monthly_stats{maxTemp=9,  minTemp=2,  sunHours=5, rainDays=11, snowDays=0}
                       , #monthly_stats{maxTemp=16, minTemp=7,  sunHours=7, rainDays=11, snowDays=0}
                       , #monthly_stats{maxTemp=19, minTemp=11, sunHours=8, rainDays=11, snowDays=0}
                       , #monthly_stats{maxTemp=20, minTemp=12, sunHours=8, rainDays=11, snowDays=0}
                       , #monthly_stats{maxTemp=20, minTemp=12, sunHours=7, rainDays=12, snowDays=0}
                       , #monthly_stats{maxTemp=16, minTemp=9,  sunHours=5, rainDays=14, snowDays=0}
                       , #monthly_stats{maxTemp=11, minTemp=6,  sunHours=3, rainDays=15, snowDays=0}
                       , #monthly_stats{maxTemp=6,  minTemp=1,  sunHours=2, rainDays=16, snowDays=0}
                       , #monthly_stats{maxTemp=3,  minTemp=-2, sunHours=1, rainDays=11, snowDays=4}
                       ]).

%% Karlstad
-define(INLAND_STATS, [ #monthly_stats{maxTemp=-2, minTemp=-8, sunHours=1, rainDays=0,  snowDays=26}
                      , #monthly_stats{maxTemp=-1, minTemp=-9, sunHours=2, rainDays=0,  snowDays=28}
                      , #monthly_stats{maxTemp=3,  minTemp=-5, sunHours=3, rainDays=13, snowDays=9}
                      , #monthly_stats{maxTemp=8,  minTemp=-1, sunHours=5, rainDays=12, snowDays=0}
                      , #monthly_stats{maxTemp=15, minTemp=5,  sunHours=7, rainDays=12, snowDays=0}
                      , #monthly_stats{maxTemp=20, minTemp=9,  sunHours=9, rainDays=13, snowDays=0}
                      , #monthly_stats{maxTemp=21, minTemp=11, sunHours=8, rainDays=15, snowDays=0}
                      , #monthly_stats{maxTemp=20, minTemp=10, sunHours=7, rainDays=14, snowDays=0}
                      , #monthly_stats{maxTemp=15, minTemp=7,  sunHours=4, rainDays=16, snowDays=0}
                      , #monthly_stats{maxTemp=10, minTemp=3,  sunHours=3, rainDays=16, snowDays=0}
                      , #monthly_stats{maxTemp=4,  minTemp=-2, sunHours=1, rainDays=17, snowDays=0}
                      , #monthly_stats{maxTemp=0,  minTemp=-7, sunHours=1, rainDays=17, snowDays=3}
                      ]).

%% Luleå
-define(NORTHERN_STATS, [ #monthly_stats{maxTemp=-8, minTemp=-16, sunHours=0,  rainDays=0, snowDays=31}
                        , #monthly_stats{maxTemp=-8, minTemp=-16, sunHours=1,  rainDays=0, snowDays=28}
                        , #monthly_stats{maxTemp=-2, minTemp=-11, sunHours=3,  rainDays=0, snowDays=31}
                        , #monthly_stats{maxTemp=3,  minTemp=-4,  sunHours=5,  rainDays=10, snowDays=23}
                        , #monthly_stats{maxTemp=10, minTemp=2,   sunHours=8,  rainDays=10, snowDays=0}
                        , #monthly_stats{maxTemp=17, minTemp=8,   sunHours=10, rainDays=10, snowDays=0}
                        , #monthly_stats{maxTemp=19, minTemp=11,  sunHours=9,  rainDays=11, snowDays=0}
                        , #monthly_stats{maxTemp=17, minTemp=10,  sunHours=6,  rainDays=13, snowDays=0}
                        , #monthly_stats{maxTemp=12, minTemp=5,   sunHours=4,  rainDays=15, snowDays=0}
                        , #monthly_stats{maxTemp=6,  minTemp=1,   sunHours=2,  rainDays=15, snowDays=0}
                        , #monthly_stats{maxTemp=-1, minTemp=-7,  sunHours=1,  rainDays=16, snowDays=9}
                        , #monthly_stats{maxTemp=-5, minTemp=-13, sunHours=0,  rainDays=0, snowDays=28}
                        ]).

%% Malmö
-define(SOUTHERN_STATS, [ #monthly_stats{maxTemp=2,  minTemp=-2, sunHours=1, rainDays=0,  snowDays=1}
                        , #monthly_stats{maxTemp=2,  minTemp=-3, sunHours=2, rainDays=0,  snowDays=1}
                        , #monthly_stats{maxTemp=4,  minTemp=-1, sunHours=3, rainDays=14, snowDays=0}
                        , #monthly_stats{maxTemp=9,  minTemp=2,  sunHours=6, rainDays=13, snowDays=0}
                        , #monthly_stats{maxTemp=15, minTemp=7,  sunHours=8, rainDays=12, snowDays=0}
                        , #monthly_stats{maxTemp=19, minTemp=11, sunHours=8, rainDays=12, snowDays=0}
                        , #monthly_stats{maxTemp=20, minTemp=13, sunHours=8, rainDays=14, snowDays=0}
                        , #monthly_stats{maxTemp=20, minTemp=13, sunHours=7, rainDays=14, snowDays=0}
                        , #monthly_stats{maxTemp=17, minTemp=10, sunHours=5, rainDays=15, snowDays=0}
                        , #monthly_stats{maxTemp=12, minTemp=7,  sunHours=3, rainDays=15, snowDays=0}
                        , #monthly_stats{maxTemp=7,  minTemp=3,  sunHours=2, rainDays=17, snowDays=3}
                        , #monthly_stats{maxTemp=3,  minTemp=-1, sunHours=1, rainDays=0,  snowDays=5}
                        ]).

-endif.
