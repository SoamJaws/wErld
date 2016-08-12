-ifndef(__WEATHER_CONTROLLER_HRL).
-define(__WEATHER_CONTROLLER_HRL, true).

-include("time.hrl").

-type weather_type() :: sunny | rainy | snowy.

-record(weather_controller_state, { city           :: atom()
                       , type           :: weather_type()
                       , temp           :: integer()
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

-endif.
