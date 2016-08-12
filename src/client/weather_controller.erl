-module(weather_controller).
-include("weather_controller.hrl").
-include("time.hrl").
-behaviour(gen_server).
-behaviour(time_subscriber).

%% Public API

%% Time subscriber
-export([ ?NEW_TIME/2
        , ?NEW_TIME/3]).

%% gen_server
-export([ start_link/2
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

%% Public API


%% Time subscriber

-spec ?NEW_TIME({global, weather_controller}, time()) -> ok.
?NEW_TIME(Address, Time) ->
  ?NEW_TIME(Address, Time, false).

-spec ?NEW_TIME({global, weather_controller}, time(), boolean()) -> ok.
?NEW_TIME(Address, Time, BlockCaller) ->
  gen_server_utils:cast(Address, {?NEW_TIME, Time}, BlockCaller).


%% gen_server

-spec start_link(atom(), pos_integer()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | any()}.
start_link(City, UpdateInterval) ->
  gen_server:start(?MODULE, [City, UpdateInterval], []).


-spec init({atom(), pos_integer()}) -> {ok, weather_controller_state()}.
init({City, UpdateInterval}) ->
  Pid = self(),
  Id = weather_controller,
  time:?SUBSCRIBE(?RECIPENT),
  Time = time:?GET_CURRENT_TIME(),
  { ok, #weather_controller_state{ city=City
                     , type=sunny
                     , temp=20
                     , updateInterval=UpdateInterval
                     , lastChange=Time
                     } }.


-spec handle_call(any(), {pid(), any()}, weather_controller_state()) -> {reply, ok, weather_controller_state()}.
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.


-spec handle_cast({?NEW_TIME, time(), boolean(), pid()}, weather_controller_state()) -> {noreply, weather_controller_state()}.
handle_cast({?NEW_TIME, Time, NotifyCaller, Caller}, State) ->
  UpdatedState = if
                   Time - State#weather_controller_state.lastChange >= State#weather_controller_state.updateInterval ->
                     %% Change weather if should be done
                     %% Change temp according to statistical temp and time of day
                     State#weather_controller_state{lastChange=Time};
                   true ->
                     State
                 end,
  gen_server_utils:notify_caller(NotifyCaller, Caller),
  {noreply, UpdatedState}.


-spec handle_info(timeout | any(), weather_controller_state()) -> {noreply, weather_controller_state()}.
handle_info(_Info, State) ->
  {noreply, State}.


-spec terminate(normal | shutdown | {shutdown, any()} | any(), weather_controller_state()) -> ok.
terminate(_Reason, _State) ->
  ok.


-spec code_change(term() | {down, term()}, weather_controller_state(), term()) -> {ok, weather_controller_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Backend

%% Gothenburg
-spec get_coastal_stats() -> [monthly_stats()].
get_coastal_stats() -> [ #monthly_stats{maxTemp=1,  minTemp=-4, sunHours=1, rainDays=15}
                       , #monthly_stats{maxTemp=1,  minTemp=-4, sunHours=2, rainDays=11}
                       , #monthly_stats{maxTemp=5,  minTemp=-2, sunHours=3, rainDays=12}
                       , #monthly_stats{maxTemp=9,  minTemp=2,  sunHours=5, rainDays=11}
                       , #monthly_stats{maxTemp=16, minTemp=7,  sunHours=7, rainDays=11}
                       , #monthly_stats{maxTemp=19, minTemp=11, sunHours=8, rainDays=11}
                       , #monthly_stats{maxTemp=20, minTemp=12, sunHours=8, rainDays=11}
                       , #monthly_stats{maxTemp=20, minTemp=12, sunHours=7, rainDays=12}
                       , #monthly_stats{maxTemp=16, minTemp=9,  sunHours=5, rainDays=14}
                       , #monthly_stats{maxTemp=11, minTemp=6,  sunHours=3, rainDays=15}
                       , #monthly_stats{maxTemp=6,  minTemp=1,  sunHours=2, rainDays=16}
                       , #monthly_stats{maxTemp=3,  minTemp=-2, sunHours=1, rainDays=15}
                       ].

%% Karlstad
-spec get_inland_stats() -> [monthly_stats()].
get_inland_stats() -> [ #monthly_stats{maxTemp=-2, minTemp=-8, sunHours=1, rainDays=17}
                      , #monthly_stats{maxTemp=-1, minTemp=-9, sunHours=2, rainDays=13}
                      , #monthly_stats{maxTemp=3,  minTemp=-5, sunHours=3, rainDays=13}
                      , #monthly_stats{maxTemp=8,  minTemp=-1, sunHours=5, rainDays=12}
                      , #monthly_stats{maxTemp=15, minTemp=5,  sunHours=7, rainDays=12}
                      , #monthly_stats{maxTemp=20, minTemp=9,  sunHours=9, rainDays=13}
                      , #monthly_stats{maxTemp=21, minTemp=11, sunHours=8, rainDays=15}
                      , #monthly_stats{maxTemp=20, minTemp=10, sunHours=7, rainDays=14}
                      , #monthly_stats{maxTemp=15, minTemp=7,  sunHours=4, rainDays=16}
                      , #monthly_stats{maxTemp=10, minTemp=3,  sunHours=3, rainDays=16}
                      , #monthly_stats{maxTemp=4,  minTemp=-2, sunHours=1, rainDays=17}
                      , #monthly_stats{maxTemp=0,  minTemp=-7, sunHours=1, rainDays=17}
                      ].

%% Luleå
-spec get_northern_stats() -> [monthly_stats()].
get_northern_stats() -> [ #monthly_stats{maxTemp=-8, minTemp=-16, sunHours=0,  rainDays=16}
                        , #monthly_stats{maxTemp=-8, minTemp=-16, sunHours=1,  rainDays=14}
                        , #monthly_stats{maxTemp=-2, minTemp=-11, sunHours=3,  rainDays=13}
                        , #monthly_stats{maxTemp=3,  minTemp=-4,  sunHours=5,  rainDays=10}
                        , #monthly_stats{maxTemp=10, minTemp=2,   sunHours=8,  rainDays=10}
                        , #monthly_stats{maxTemp=17, minTemp=8,   sunHours=10, rainDays=10}
                        , #monthly_stats{maxTemp=19, minTemp=11,  sunHours=9,  rainDays=11}
                        , #monthly_stats{maxTemp=17, minTemp=10,  sunHours=6,  rainDays=13}
                        , #monthly_stats{maxTemp=12, minTemp=5,   sunHours=4,  rainDays=15}
                        , #monthly_stats{maxTemp=6,  minTemp=1,   sunHours=2,  rainDays=15}
                        , #monthly_stats{maxTemp=-1, minTemp=-7,  sunHours=1,  rainDays=16}
                        , #monthly_stats{maxTemp=-5, minTemp=-13, sunHours=0,  rainDays=16}
                        ].

%% Malmö
-spec get_southern_stats() -> [monthly_stats()].
get_southern_stats() -> [ #monthly_stats{maxTemp=2,  minTemp=-2, sunHours=1, rainDays=17}
                        , #monthly_stats{maxTemp=2,  minTemp=-3, sunHours=2, rainDays=13}
                        , #monthly_stats{maxTemp=4,  minTemp=-1, sunHours=3, rainDays=14}
                        , #monthly_stats{maxTemp=9,  minTemp=2,  sunHours=6, rainDays=13}
                        , #monthly_stats{maxTemp=15, minTemp=7,  sunHours=8, rainDays=12}
                        , #monthly_stats{maxTemp=19, minTemp=11, sunHours=8, rainDays=12}
                        , #monthly_stats{maxTemp=20, minTemp=13, sunHours=8, rainDays=14}
                        , #monthly_stats{maxTemp=20, minTemp=13, sunHours=7, rainDays=14}
                        , #monthly_stats{maxTemp=17, minTemp=10, sunHours=5, rainDays=15}
                        , #monthly_stats{maxTemp=12, minTemp=7,  sunHours=3, rainDays=15}
                        , #monthly_stats{maxTemp=7,  minTemp=3,  sunHours=2, rainDays=17}
                        , #monthly_stats{maxTemp=3,  minTemp=-1, sunHours=1, rainDays=17}
                        ].
