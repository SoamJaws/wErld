-module(line).
-include("public_transport.hrl").
-behaviour(gen_server).

%% Public API
-export([ start_link/3
        , stop/1
        , state/1
        , ?GET_NEXT_STOP/3
        , ?GET_NEIGHBORS/2
        , ?GET_OTHER_END/2
        , ?CONTAINS_STOP/2
        , ?GET_DURATION/3
        , ?IS_END_STOP/2
        , ?GET_INTERSECTION/2
        , ?GET_NUMBER/1
        , ?GET_TARGET/3]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

-spec start_link(pos_integer(), [pid() | pos_integer()], vehicle_type()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(Number, Stops, Type) ->
  gen_server:start_link(?MODULE, {Number, Stops, Type}, []).

-spec stop(pid()) -> ok.
stop(Pid) ->
  gen_server:call(Pid, stop).

-spec state(pid()) -> line_state().
state(Pid) ->
  gen_server:call(Pid, state).

-spec ?GET_NEXT_STOP(pid(), pid(), pid()) -> {pid(), pos_integer()} | none.
?GET_NEXT_STOP(Pid, Target, Stop) ->
  gen_server:call(Pid, {?GET_NEXT_STOP, Target, Stop}).

-spec ?GET_NEIGHBORS(pid(), pid()) -> [{pid(), pos_integer(), pid(), pid()}].
?GET_NEIGHBORS(Pid, Stop) ->
  gen_server:call(Pid, {?GET_NEIGHBORS, Stop}).

-spec ?GET_OTHER_END(pid(), pid()) -> pid().
?GET_OTHER_END(Pid, Stop) ->
  gen_server:call(Pid, {?GET_OTHER_END, Stop}).

-spec ?CONTAINS_STOP(pid(), pid()) -> boolean().
?CONTAINS_STOP(Pid, Stop) ->
  gen_server:call(Pid, {?CONTAINS_STOP, Stop}).

-spec ?GET_DURATION(pid(), pid(), pid()) -> pos_integer().
?GET_DURATION(Pid, FromStop, ToStop) ->
  gen_server:call(Pid, {?GET_DURATION, FromStop, ToStop}).

-spec ?IS_END_STOP(pid(), pid()) -> boolean().
?IS_END_STOP(Pid, Stop) ->
  gen_server:call(Pid, {?IS_END_STOP, Stop}).

-spec ?GET_INTERSECTION(pid(), pid()) -> pid() | none.
?GET_INTERSECTION(Pid, OtherLine) ->
  gen_server:call(Pid, {?GET_INTERSECTION, OtherLine}).

-spec ?GET_NUMBER(pid()) -> pos_integer().
?GET_NUMBER(Pid) ->
  gen_server:call(Pid, ?GET_NUMBER).

-spec ?GET_TARGET(pid(), pid(), pid()) -> pid().
?GET_TARGET(Pid, FromStop, ToStop) ->
  gen_server:call(Pid, {?GET_TARGET, FromStop, ToStop}).


%% gen_server

-spec init({pos_integer(), [pid() | pos_integer()], vehicle_type()}) -> {ok, line_state()}.
init({Number, Stops, Type}) ->
  %gen_server:call(blackboard, {subscribe, time}),
  {ok, #line_state{number=Number, stops=Stops, type=Type}}.


-spec handle_call({?GET_NEXT_STOP, pid(), pid()}, {pid(), any()}, line_state()) -> {reply, {pid(), pos_integer()} | none, line_state()}
      ;          ({?GET_NEIGHBORS, pid()},        {pid(), any()}, line_state()) -> {reply, [{pid(), pos_integer(), pid(), pid()}], line_state()}
      ;          ({?GET_OTHER_END, pid()},        {pid(), any()}, line_state()) -> {reply, pid(), line_state()}
      ;          ({?CONTAINS_STOP, pid()},        {pid(), any()}, line_state()) -> {reply, boolean(), line_state()}
      ;          ({?GET_DURATION, pid(), pid()},  {pid(), any()}, line_state()) -> {reply, boolean(), line_state()}
      ;          ({?IS_END_STOP, pid()},          {pid(), any()}, line_state()) -> {reply, boolean(), line_state()}
      ;          ({?GET_INTERSECTION, pid()},     {pid(), any()}, line_state()) -> {reply, pid() | none, line_state()}
      ;          (?GET_NUMBER,                    {pid(), any()}, line_state()) -> {reply, pos_integer(), line_state()}
      ;          ({?GET_TARGET, pid(), pid()},    {pid(), any()}, line_state()) -> {reply, pid(), line_state()}
      ;          (stop,                           {pid(), any()}, line_state()) -> {stop, normal, stopped, line_state()}
      ;          (state,                          {pid(), any()}, line_state()) -> {reply, line_state(), line_state()}.
handle_call({?GET_NEXT_STOP, Target, Stop}, _From, State) ->
  [EndStop|_] = State#line_state.stops,
  Reply = case EndStop of
            Target -> get_next_stop_helper(Stop, pre, State#line_state.stops);
            _      -> get_next_stop_helper(Stop, post, State#line_state.stops)
          end,
  {reply, Reply, State};

handle_call({?GET_NEIGHBORS, Stop}, _From, State) ->
  {BeforeStop, [Stop|AfterStop]} = lists:splitwith(fun(X) -> X /= Stop end, State#line_state.stops),
  N1 = case BeforeStop of
         [FirstTarget|_] ->
           FirstNeighbor = lists:last(lists:droplast(BeforeStop)),
           [{FirstNeighbor, lists:last(BeforeStop), FirstTarget, self()}];
         [] ->
           []
       end,
  N2 = case AfterStop of
         [SecondDur|[SecondNeighbor|_]] ->
           [{SecondNeighbor, SecondDur, lists:last(AfterStop), self()}];
         [] ->
           []
       end,
  Reply = N1 ++ N2,
  {reply, Reply, State};

handle_call({?GET_OTHER_END, Stop}, _From, State) ->
  [H|T] = State#line_state.stops,
  Reply = case H of
            Stop -> lists:last(T);
            _    -> H
          end,
  {reply, Reply, State};

handle_call({?CONTAINS_STOP, Stop}, _From, State) ->
  Reply = lists:member(Stop, State#line_state.stops),
  {reply, Reply, State};

handle_call({?GET_DURATION, FromStop, ToStop}, _From, State) ->
  Reply = get_duration_helper(FromStop, ToStop, State#line_state.stops),
  {reply, Reply, State};

handle_call({?IS_END_STOP, Stop}, _From, State) ->
  Reply = lists:prefix([Stop], State#line_state.stops) or lists:suffix([Stop], State#line_state.stops),
  {reply, Reply, State};

handle_call({?GET_INTERSECTION, OtherLine}, _From, State) ->
  Reply = get_intersection_helper(OtherLine, State#line_state.stops),
  {reply, Reply, State};

handle_call(?GET_NUMBER, _From, State) ->
  Reply = State#line_state.number,
  {reply, Reply, State};

handle_call({?GET_TARGET, FromStop, ToStop}, _From, State) ->
  Reply = get_target_helper(FromStop, ToStop, State#line_state.stops),
  {reply, Reply, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, State, State}.


-spec handle_cast(any(), line_state()) -> {noreply, line_state()}.
handle_cast(_Cast, State) ->
  {noreply, State}.


-spec handle_info(timeout | any(), line_state()) -> {noreply, line_state()}.
handle_info(_Info, State) ->
  {noreply, State}.


-spec terminate(normal | shutdown | {shutdown, any()} | any(), line_state()) -> ok.
terminate(_Reason, _State) ->
  ok.


-spec code_change(term() | {down, term()}, stop_state(), term()) -> {ok, line_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Backend

-spec get_next_stop_helper(pid(), pre | post, [pid() | pos_integer()]) -> {pid(), pos_integer()} | none.
get_next_stop_helper(_Stop, _, [_|[]]) -> none; %%TODO ERROR log, since vehicles should always turn around when they arrive at their end stations. There should always be a next stop.
get_next_stop_helper(Stop, pre, [NextStop|[Dur|[Stop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, post, [Stop|[Dur|[NextStop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, Alignment, [_S|[_Dur|Stops]]) ->
  get_next_stop_helper(Stop, Alignment, Stops).

-spec get_duration_helper(pid(), pid(), [pid() | pos_integer()]) -> pos_integer().
get_duration_helper(FromStop, ToStop, Stops) ->
  get_duration_helper(FromStop, ToStop, false, Stops).

-spec get_duration_helper(pid(), pid(), boolean(), [pid() | pos_integer()]) -> non_neg_integer().
get_duration_helper(_FromStop, ToStop, true, [ToStop|_Stops]) -> 0;
get_duration_helper(FromStop, _ToStop, true, [FromStop|_Stops]) -> 0;
get_duration_helper(FromStop, ToStop, _OnPath, [FromStop|[Dur|Stops]]) ->
  Dur + get_duration_helper(FromStop, ToStop, true, Stops);
get_duration_helper(FromStop, ToStop, _OnPath, [ToStop|[Dur|Stops]]) ->
  Dur + get_duration_helper(FromStop, ToStop, true, Stops);
get_duration_helper(FromStop, ToStop, OnPath, [_S|[Dur|Stops]]) ->
  case OnPath of
    true ->
      Dur + get_duration_helper(FromStop, ToStop, true, Stops);
    false ->
      get_duration_helper(FromStop, ToStop, false, Stops)
  end.

-spec get_intersection_helper(pid(), [pid() | pos_integer()]) -> pid() | none.
get_intersection_helper(OtherLine, [Stop|Rest]) ->
  ContainsStop = ?CONTAINS_STOP(OtherLine, Stop),
  case ContainsStop of
    true ->
      Stop;
    false ->
      case Rest of
        [] ->
          none;
        [_|Stops] ->
          get_intersection_helper(OtherLine, Stops)
      end
  end.

-spec get_target_helper(pid(), pid(), [pid() | pos_integer()]) -> pid().
get_target_helper(FromStop, ToStop, [FirstEnd|[_|Stops]]) ->
  get_target_helper(FromStop, ToStop, FirstEnd, lists:filter(fun(Stop) -> is_pid(Stop) end, Stops)).

-spec get_target_helper(pid(), pid(), pid(), [pid() | pos_integer()]) -> pid().
get_target_helper(FromStop, _ToStop, FromStop, Stops) ->
  lists:last(Stops);
get_target_helper(FromStop, ToStop, FirstEnd, [Stop|Stops]) ->
  case Stop of
    FromStop ->
      lists:last(Stops);
    ToStop ->
      FirstEnd;
    Stop ->
      get_target_helper(FromStop, ToStop, FirstEnd, Stops)
  end.