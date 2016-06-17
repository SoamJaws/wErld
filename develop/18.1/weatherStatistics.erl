-module(weatherStatistics).
-compile(export_all).

-record(monthlyStats, {maxTemp, minTemp, sunHours, rainDays}).

%% Gothenburg
get_coastal_stats() -> [ #monthlyStats{maxTemp=1,  minTemp=-4, sunHours=1, rainDays=15}
                       , #monthlyStats{maxTemp=1,  minTemp=-4, sunHours=2, rainDays=11}
                       , #monthlyStats{maxTemp=5,  minTemp=-2, sunHours=3, rainDays=12}
                       , #monthlyStats{maxTemp=9,  minTemp=2,  sunHours=5, rainDays=11}
                       , #monthlyStats{maxTemp=16, minTemp=7,  sunHours=7, rainDays=11}
                       , #monthlyStats{maxTemp=19, minTemp=11, sunHours=8, rainDays=11}
                       , #monthlyStats{maxTemp=20, minTemp=12, sunHours=8, rainDays=11}
                       , #monthlyStats{maxTemp=20, minTemp=12, sunHours=7, rainDays=12}
                       , #monthlyStats{maxTemp=16, minTemp=9,  sunHours=5, rainDays=14}
                       , #monthlyStats{maxTemp=11, minTemp=6,  sunHours=3, rainDays=15}
                       , #monthlyStats{maxTemp=6,  minTemp=1,  sunHours=2, rainDays=16}
                       , #monthlyStats{maxTemp=3,  minTemp=-2, sunHours=1, rainDays=15}
                       ].

%% Karlstad
get_inland_stats() -> [ #monthlyStats{maxTemp=-2, minTemp=-8, sunHours=1, rainDays=17}
                      , #monthlyStats{maxTemp=-1, minTemp=-9, sunHours=2, rainDays=13}
                      , #monthlyStats{maxTemp=3,  minTemp=-5, sunHours=3, rainDays=13}
                      , #monthlyStats{maxTemp=8,  minTemp=-1, sunHours=5, rainDays=12}
                      , #monthlyStats{maxTemp=15, minTemp=5,  sunHours=7, rainDays=12}
                      , #monthlyStats{maxTemp=20, minTemp=9,  sunHours=9, rainDays=13}
                      , #monthlyStats{maxTemp=21, minTemp=11, sunHours=8, rainDays=15}
                      , #monthlyStats{maxTemp=20, minTemp=10, sunHours=7, rainDays=14}
                      , #monthlyStats{maxTemp=15, minTemp=7,  sunHours=4, rainDays=16}
                      , #monthlyStats{maxTemp=10, minTemp=3,  sunHours=3, rainDays=16}
                      , #monthlyStats{maxTemp=4,  minTemp=-2, sunHours=1, rainDays=17}
                      , #monthlyStats{maxTemp=0,  minTemp=-7, sunHours=1, rainDays=17}
                      ].

%% Luleå
get_northern_stats() -> [ #monthlyStats{maxTemp=-8, minTemp=-16, sunHours=0,  rainDays=16}
                        , #monthlyStats{maxTemp=-8, minTemp=-16, sunHours=1,  rainDays=14}
                        , #monthlyStats{maxTemp=-2, minTemp=-11, sunHours=3,  rainDays=13}
                        , #monthlyStats{maxTemp=3,  minTemp=-4,  sunHours=5,  rainDays=10}
                        , #monthlyStats{maxTemp=10, minTemp=2,   sunHours=8,  rainDays=10}
                        , #monthlyStats{maxTemp=17, minTemp=8,   sunHours=10, rainDays=10}
                        , #monthlyStats{maxTemp=19, minTemp=11,  sunHours=9,  rainDays=11}
                        , #monthlyStats{maxTemp=17, minTemp=10,  sunHours=6,  rainDays=13}
                        , #monthlyStats{maxTemp=12, minTemp=5,   sunHours=4,  rainDays=15}
                        , #monthlyStats{maxTemp=6,  minTemp=1,   sunHours=2,  rainDays=15}
                        , #monthlyStats{maxTemp=-1, minTemp=-7,  sunHours=1,  rainDays=16}
                        , #monthlyStats{maxTemp=-5, minTemp=-13, sunHours=0,  rainDays=16}
                        ].

%% Malmö
get_southern_stats() -> [ #monthlyStats{maxTemp=2,  minTemp=-2, sunHours=1, rainDays=17}
                        , #monthlyStats{maxTemp=2,  minTemp=-3, sunHours=2, rainDays=13}
                        , #monthlyStats{maxTemp=4,  minTemp=-1, sunHours=3, rainDays=14}
                        , #monthlyStats{maxTemp=9,  minTemp=2,  sunHours=6, rainDays=13}
                        , #monthlyStats{maxTemp=15, minTemp=7,  sunHours=8, rainDays=12}
                        , #monthlyStats{maxTemp=19, minTemp=11, sunHours=8, rainDays=12}
                        , #monthlyStats{maxTemp=20, minTemp=13, sunHours=8, rainDays=14}
                        , #monthlyStats{maxTemp=20, minTemp=13, sunHours=7, rainDays=14}
                        , #monthlyStats{maxTemp=17, minTemp=10, sunHours=5, rainDays=15}
                        , #monthlyStats{maxTemp=12, minTemp=7,  sunHours=3, rainDays=15}
                        , #monthlyStats{maxTemp=7,  minTemp=3,  sunHours=2, rainDays=17}
                        , #monthlyStats{maxTemp=3,  minTemp=-1, sunHours=1, rainDays=17}
                        ].
