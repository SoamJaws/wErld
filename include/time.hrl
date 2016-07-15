-ifndef(__TIME_HRL).
-define(__TIME_HRL, true).

-include("gen_server_utils.hrl").

-type time :: non_neg_integer().

-record(time_state, { delta      :: non_neg_integer()
                    , frequency  :: pos_integer()
                    , subcribers :: [gen_address()]
                    , time       :: non_neg_integer()
                    }).
-type time_state() :: #time_state{}.

-define(SUBSCRIBE,        subscribe).
-define(GET_CURRENT_TIME, get_current_time).
-define(NEW_TIME,         new_time).
