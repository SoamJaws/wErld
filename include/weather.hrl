-ifndef(__WEATHER_HRL).
-define(__WEATHER_HRL, true).

-include("time.hrl").

-type weather_type() :: sunny | rainy | snowy.

-record(weather_controller_state, { city           :: atom()
                                  , updateInterval :: pos_integer()
                                  , lastChange     :: time()
                                  }).
-type weather_controller_state() :: #weather_controller_state{}.

-record(monthly_stats, { maxTemp  :: integer()
                       , minTemp  :: integer()
                       , sunHours :: integer()
                       , rainDays :: non_neg_integer()
                       }).
-type monthly_stats() :: #monthly_stats{}.

-define(GET_TYPE, get_type).
-define(GET_TEMP, get_temp).

-endif.
