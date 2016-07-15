-module(time).
-include("time.hrl").
-include("logger.hrl").
-behaviour(gen_server).

%% Public API
-export([ ?SUBSCRIBE/1
        , ?SUBSCRIBE/2
        , ?GET_CURRENT_TIME/0]).

%% gen_server
-export([ start_link/2
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , code_change/3]).

%% Public API

-spec ?SUBSCRIBE(gen_address()) -> ok.
?SUBSCRIBE(Subscriber) ->
  ?SUBSCRIBE(Subscriber, false).

-spec ?SUBSCRIBE(gen_address(), boolean()) -> ok.
?SUBSCRIBE(Subscriber, BlockCaller) ->
  gen_server_utils:cast({global, ?MODULE}, {?SUBSCRIBE, Subscriber}, BlockCaller).

-spec ?GET_CURRENT_TIME() -> non_neg_integer().
?GET_CURRENT_TIME() ->
  gen_server:call({global, ?MODULE}, ?GET_CURRENT_TIME).


%% gen_server

-spec start_link(non_neg_integer(), pos_integer()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(Delta, Frequency) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, {Delta, Frequency}, []).


-spec init({non_neg_integer(), pos_integer()}) -> {ok, time_state()}.
init({Delta, Frequency}) ->
  gen_server:cast(self(), tick),
  {ok, #time_state{delta=Delta, frequency=Frequency, subscribers=0, time=0}}. % Epoch in gregorian seconds


-spec handle_call(?GET_CURRENT_TIME, {pid(), any()}, time_state()) -> {reply, time()}.
handle_call(?GET_CURRENT_TIME, _From, State) ->
  {reply, State#time_state.time, State}. 


-spec handle_cast({?SUBSCRIBE, gen_address()}, time_state()) -> {noreply, time_state()}
      ;          (tick, time_state()) -> {noreply, time_state()}.
handle_cast({?SUBSCRIBE, Subscriber}, State) ->
  Subscribers = State#time_state.subscribers,
  {noreply, State#time_state{subscribers=[Subscriber|Subscribers]}};

handle_cast(tick, State) ->
  timer:sleep(State#time_state.frequency),
  gen_server:cast(self(), tick),
  NewTime = State#time_state.time + State#time_state.delta,
  broadcast_time(State#time_state.subscribers, NewTime),
  {noreply, State#time_state{time=NewTime}}.


-spec handle_info(timeout | any(), time_state()) -> {noreply, time_state()}.
handle_info(_Info, State) ->
  {noreply, State}.


-spec code_change(term() | {down, term()}, time_state(), term()) -> {ok, time_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Helpers

-spec broadcast_time([gen_address()], time()) -> ok;
broadcast_time([], _Time) -> ok.
broadcast_time([Subscriber|Subscribers], Time) ->
  Module = gen_server_utils:extract_module(Subscriber),
  apply(Module, ?NEW_TIME, [Subscriber, Time]),
  broadcast_time(Subscribers, Time).
