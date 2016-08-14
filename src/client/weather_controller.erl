-module(weather_controller).
-include("weather.hrl").
-include("time.hrl").
-behaviour(gen_server).
-behaviour(time_subscriber).

%% Public API
-export([ ?GET_TYPE/0
        , ?GET_TEMP/0]).

%% Time subscriber
-export([ ?NEW_TIME/2
        , ?NEW_TIME/3]).

%% gen_server
-export([ start_link/3
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

%% Public API

-spec ?GET_TYPE() -> weather_type().
?GET_TYPE() ->
  ets_utils:set_lookup(?MODULE, type).

-spec ?GET_TEMP() -> integer().
?GET_TEMP() ->
  ets_utils:set_lookup(?MODULE, temp).


%% Time subscriber

-spec ?NEW_TIME({global, weather_controller}, time()) -> ok.
?NEW_TIME(Address, Time) ->
  ?NEW_TIME(Address, Time, false).

-spec ?NEW_TIME({global, weather_controller}, time(), boolean()) -> ok.
?NEW_TIME(Address, Time, BlockCaller) ->
  gen_server_utils:cast(Address, {?NEW_TIME, Time}, BlockCaller).


%% gen_server

-spec start_link(atom(), climate_type(), pos_integer()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | any()}.
start_link(City, Climate, UpdateInterval) ->
  gen_server:start_link(?MODULE, {City, Climate, UpdateInterval}, []).


-spec init({atom(), climate_type(), pos_integer()}) -> {ok, weather_controller_state()}.
init({City, Climate, UpdateInterval}) ->
  Pid = self(),
  Id = weather_controller,
  time:?SUBSCRIBE(?RECIPENT),
  Time = time:?GET_CURRENT_TIME(),
  ets:new(?MODULE, [named_table]),
  ets:insert(?MODULE, {type, sunny}),
  ets:insert(?MODULE, {temp, 20}),
  { ok, #weather_controller_state{ city=City
                                 , climate=Climate
                                 , lastChange=Time
                                 , updateInterval=UpdateInterval
                                 } }.


-spec handle_call(any(), {pid(), any()}, weather_controller_state()) -> {reply, ok, weather_controller_state()}.
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.


-spec handle_cast({?NEW_TIME, time(), boolean(), pid()}, weather_controller_state()) -> {noreply, weather_controller_state()}.
handle_cast({?NEW_TIME, Time, NotifyCaller, Caller}, State) ->
  UpdateInterval = State#weather_controller_state.updateInterval,
  UpdatedState = if
                   Time - State#weather_controller_state.lastChange >= UpdateInterval ->
                     %% Change weather if should be done
                     Climate = State#weather_controller_state.climate,
                     Stats = case Climate of
                               coastal  -> ?COASTAL_STATS;
                               inland   -> ?INLAND_STATS;
                               northern -> ?NORTHERN_STATS;
                               southern -> ?SOUTHERN_STATS
                             end,
                     {{Year, Month, _}, _} = calendar:gregorian_seconds_to_datetime(Time),
                     IsSnowing = is_snowing(Year, Month, UpdateInterval, Stats),
                     IsRaining = if
                                   not IsSnowing ->
                                     is_raining(Year, Month, UpdateInterval, Stats);
                                   true ->
                                     false
                                 end,
                     if
                       IsSnowing ->
                         ets:insert(?MODULE, {type, snowy});
                       IsRaining ->
                         ets:insert(?MODULE, {type, rainy});
                       true ->
                         ets:insert(?MODULE, {type, sunny})
                     end,
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

-spec is_snowing(non_neg_integer(), 1..12, pos_integer(), [monthly_stats()]) -> boolean().
is_snowing(Year, Month, UpdateInterval, Stats) ->
  MonthyStats = lists:nth(Month, Stats),
  is_downpour(Year, Month, UpdateInterval, MonthyStats#monthly_stats.snowDays).


-spec is_raining(non_neg_integer(), 1..12, pos_integer(), [monthly_stats()]) -> boolean().
is_raining(Year, Month, UpdateInterval, Stats) ->
  MonthyStats = lists:nth(Month, Stats),
  is_downpour(Year, Month, UpdateInterval, MonthyStats#monthly_stats.rainDays).


-spec is_downpour(non_neg_integer(), 1..12, pos_integer(), non_neg_integer()) -> boolean().
is_downpour(Year, Month, UpdateInterval, DownpourDays) ->
  Percentage = round(DownpourDays / calendar:last_day_of_the_month(Year, Month) / (86400/UpdateInterval) * 100),
  X = rand:uniform(101) - 1,
  X < Percentage.
