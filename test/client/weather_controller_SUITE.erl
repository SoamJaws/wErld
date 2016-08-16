-module(weather_controller_SUITE).
-include("weather.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl"). %% For assertion macros

-export([ coastal_case/1
        , inland_case/1
        , northern_case/1
        , southern_case/1]).

-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2]).

all() -> [ coastal_case
         , inland_case
         , northern_case
         , southern_case].

init_per_testcase(_TestCase, Config) ->
  Time = gen_server_mock:start_global(time, time, strict),
  StartTime = calendar:datetime_to_gregorian_seconds(calendar:local_time()),
  gen_server_mock:expect_ets_lookup(time, time, StartTime),
  Config ++ [ {time, Time}
            , {startTime, StartTime}
            , {updateInterval, 5}].

end_per_testcase(_TestCase, Config) ->
  Time = ?config(time, Config),
  ?assert(gen_server_mock:validate(Time)),
  gen_server_mock:stop(Time),
  Config.

coastal_case(Config) ->
  run_test(Config, coastal).

inland_case(Config) ->
  run_test(Config, inland).

northern_case(Config) ->
  run_test(Config, northern).

southern_case(Config) ->
  run_test(Config, southern).

run_test(Config, Type) ->
  Time = ?config(time, Config),
  StartTime = ?config(startTime, Config),
  UpdateInterval = ?config(updateInterval, Config),
  {ok, Supervisor} = client_supervisor:start_link(city, Type, UpdateInterval),
  [{weather_controller, Pid, _, _}] = supervisor:which_children(Supervisor),
  gen_server_mock:expect_cast(Time, {?SUBSCRIBE, {{weather_controller, weather_controller}, Pid}, false, Pid}),
  lists:map(fun(Delta) ->
              CurrentTime = StartTime + Delta,
              weather_controller:?NEW_TIME(Pid, CurrentTime, true),
              {calendar:gregorian_seconds_to_datetime(CurrentTime), weather_controller:?GET_TYPE(), weather_controller:?GET_TEMP()}
            end, lists:seq(60*60, 60*60*24*365, 60*60)).
