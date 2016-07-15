-module(time).
-include("time.hrl").
-include("logger.hrl").
-behaviour(gen_server).

%% Public API
-export([ ?SUBSCRIBE/1
        , ?GET_CURRENT_TIME/0]).

%% gen_server
-export([ start_link/2
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

%% Public API

-spec ?SUBSCRIBE(boolean()) -> ok.
?SUBSCRIBE(BlockCaller) ->
  gen_server_utils:cast({global, ?MODULE}, ?SUBSCRIBE, BlockCaller).

-spec ?GET_CURRENT_TIME() -> non_neg_integer().
?GET_CURRENT_TIME() ->
  gen_server:call({global, ?MODULE}, ?GET_CURRENT_TIME).


%% gen_server

start_link(Delta, Frequency) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [Delta, Frequency], []).


init([Delta, Frequency]) ->
  gen_server:cast(self(), tick),
  {ok, #time_state{delta=Delta, frequency=Frequency, subscribers=0, time=0}}. % Epoch in gregorian seconds


handle_call(?GET_CURRENT_TIME, _From, State) ->
  {reply, State#time_state.time, State}. 


handle_cast(subscribe, State) ->

handle_cast(tick, State) ->
  timer:sleep(State#time_state.frequency),
  gen_server:cast(self(), tick),
  %% Use apply to call the correct module
  NewTime = State#time_state.time + State#time_state.delta
  
  {noreply, State#time_state{time=NewTime}}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
