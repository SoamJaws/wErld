-module(line).
-include("public_transport.hrl").
-include("logger.hrl").
-behaviour(gen_server).

%% Public API
-export([ ?GET_NEXT_STOP/3
        , ?GET_NEIGHBORS/2
        , ?GET_OTHER_END/2
        , ?CONTAINS_STOP/2
        , ?GET_DURATION/3
        , ?IS_END_STOP/2
        , ?GET_INTERSECTION/2
        , ?GET_NUMBER/1
        , ?GET_TARGET/3]).

%% gen_server
-export([ start_link/3
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

-spec ?GET_NEXT_STOP(line(), stop(), stop()) -> {stop(), pos_integer()} | none.
?GET_NEXT_STOP(?RECIPENT, Target, Stop) ->
  ?LOG_SEND(io_lib:format("GET_NEXT_STOP Line=~p TargetStop=~p CurrentStop=~p", [?RECIPENT, Target, Stop])),
  Reply = gen_server:call(Pid, {?GET_NEXT_STOP, Target, Stop}),
  ?LOG_RECEIVE(io_lib:format("REPLY GET_NEXT_STOP ~p", [Reply])),
  Reply.

-spec ?GET_NEIGHBORS(line(), stop()) -> [{stop(), pos_integer(), stop(), line()}].
?GET_NEIGHBORS(?RECIPENT, Stop) ->
  ?LOG_SEND(io_lib:format("GET_NEIGHBORS Line=~p Stop=~p", [?RECIPENT, Stop])),
  Reply = gen_server:call(Pid, {?GET_NEIGHBORS, Stop}),
  ?LOG_RECEIVE(io_lib:format("REPLY GET_NEIGHBORS ~p", [Reply])),
  Reply.

-spec ?GET_OTHER_END(line(), stop()) -> stop().
?GET_OTHER_END(?RECIPENT, Stop) ->
  ?LOG_SEND(io_lib:format("GET_OTHER_END Line=~p Stop=~p", [?RECIPENT, Stop])),
  Reply = gen_server:call(Pid, {?GET_OTHER_END, Stop}),
  ?LOG_RECEIVE(io_lib:format("REPLY GET_OTHER_END ~p", [Reply])),
  Reply.

-spec ?CONTAINS_STOP(line(), stop()) -> boolean().
?CONTAINS_STOP(?RECIPENT, Stop) ->
  ?LOG_SEND(io_lib:format("CONTAINS_STOP Line=~p Stop=~p", [?RECIPENT, Stop])),
  Reply = gen_server:call(Pid, {?CONTAINS_STOP, Stop}),
  ?LOG_RECEIVE(io_lib:format("REPLY CONTAINS_STOP ~p", [Reply])),
  Reply.

-spec ?GET_DURATION(line(), stop(), stop()) -> pos_integer().
?GET_DURATION(?RECIPENT, FromStop, ToStop) ->
  ?LOG_SEND(io_lib:format("GET_DURATION Line=~p FromStop=~p ToStop=~p", [?RECIPENT, FromStop, ToStop])),
  Reply = gen_server:call(Pid, {?GET_DURATION, FromStop, ToStop}),
  ?LOG_RECEIVE(io_lib:format("REPLY GET_DURATION ~p", [Reply])),
  Reply.

-spec ?IS_END_STOP(line(), stop()) -> boolean().
?IS_END_STOP(?RECIPENT, Stop) ->
  ?LOG_SEND(io_lib:format("IS_END_STOP Line=~p Stop=~p", [?RECIPENT, Stop])),
  Reply = gen_server:call(Pid, {?IS_END_STOP, Stop}),
  ?LOG_RECEIVE(io_lib:format("REPLY IS_END_STOP ~p", [Reply])),
  Reply.

-spec ?GET_INTERSECTION(line(), line()) -> stop() | none.
?GET_INTERSECTION(?RECIPENT, OtherLine) ->
  ?LOG_SEND(io_lib:format("GET_INTERSECTION Line=~p OtherLine=~p", [?RECIPENT, OtherLine])),
  Reply = gen_server:call(Pid, {?GET_INTERSECTION, OtherLine}),
  ?LOG_RECEIVE(io_lib:format("REPLY GET_INTERSECTION ~p", [Reply])),
  Reply.

-spec ?GET_NUMBER(line()) -> pos_integer().
?GET_NUMBER(?RECIPENT) ->
  ?LOG_SEND(io_lib:format("GET_NUMBER Line=~p", [?RECIPENT])),
  Reply = gen_server:call(Pid, ?GET_NUMBER),
  ?LOG_RECEIVE(io_lib:format("REPLY GET_NUMBER ~p", [Reply])),
  Reply.

-spec ?GET_TARGET(line(), stop(), stop()) -> stop().
?GET_TARGET(?RECIPENT, FromStop, ToStop) ->
  ?LOG_SEND(io_lib:format("GET_TARGET Line=~p FromStop=~p ToStop=~p", [?RECIPENT, FromStop, ToStop])),
  Reply = gen_server:call(Pid, {?GET_TARGET, FromStop, ToStop}),
  ?LOG_RECEIVE(io_lib:format("REPLY GET_TARGET ~p", [Reply])),
  Reply.

%% gen_server

-spec start_link(pos_integer(), [pid() | pos_integer()], vehicle_type()) -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link(Number, Stops, Type) ->
  gen_server:start_link(?MODULE, {Number, Stops, Type}, []).


-spec init({pos_integer(), [pid() | pos_integer()], vehicle_type()}) -> {ok, line_state()}.
init({Number, Stops, Type}) ->
  Id = list_to_atom(atom_to_list(Type) ++ "_" ++ integer_to_list(Number)),
  put(id, Id),
  put(module, ?MODULE_STRING),
  ?LOG_INFO("Line started"),
  {ok, #line_state{id=Id, number=Number, stops=Stops, type=Type}}.


-spec handle_call({?GET_NEXT_STOP, stop(), stop()}, {pid(), any()}, line_state()) -> {reply, {stop(), pos_integer()} | none, line_state()}
      ;          ({?GET_NEIGHBORS, stop()},         {pid(), any()}, line_state()) -> {reply, [{stop(), pos_integer(), stop(), line()}], line_state()}
      ;          ({?GET_OTHER_END, stop()},         {pid(), any()}, line_state()) -> {reply, stop(), line_state()}
      ;          ({?CONTAINS_STOP, stop()},         {pid(), any()}, line_state()) -> {reply, boolean(), line_state()}
      ;          ({?GET_DURATION, stop(), stop()},  {pid(), any()}, line_state()) -> {reply, pos_integer(), line_state()}
      ;          ({?IS_END_STOP, stop()},           {pid(), any()}, line_state()) -> {reply, boolean(), line_state()}
      ;          ({?GET_INTERSECTION, line()},      {pid(), any()}, line_state()) -> {reply, stop() | none, line_state()}
      ;          (?GET_NUMBER,                      {pid(), any()}, line_state()) -> {reply, pos_integer(), line_state()}
      ;          ({?GET_TARGET, stop(), stop()},    {pid(), any()}, line_state()) -> {reply, stop(), line_state()}.
handle_call({?GET_NEXT_STOP, Target, Stop}, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("GET_NEXT_STOP Target=~p Stop=~p", [Target, Stop])),
  [EndStop|_] = State#line_state.stops,
  Reply = case EndStop of
            Target -> get_next_stop_helper(Stop, pre, State#line_state.stops);
            _      -> get_next_stop_helper(Stop, post, State#line_state.stops)
          end,
  {reply, Reply, State};

handle_call({?GET_NEIGHBORS, Stop}, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("GET_NEIGHBORS Stop=~p", [Stop])),
  Id = State#line_state.id,
  {BeforeStop, [Stop|AfterStop]} = lists:splitwith(fun(X) -> X /= Stop end, State#line_state.stops),
  Pid = self(),
  N1 = case BeforeStop of
         [FirstTarget|_] ->
           FirstNeighbor = lists:last(lists:droplast(BeforeStop)),
           [{FirstNeighbor, lists:last(BeforeStop), FirstTarget, ?RECIPENT}];
         [] ->
           []
       end,
  N2 = case AfterStop of
         [SecondDur|[SecondNeighbor|_]] ->
           [{SecondNeighbor, SecondDur, lists:last(AfterStop), ?RECIPENT}];
         [] ->
           []
       end,
  Reply = N1 ++ N2,
  {reply, Reply, State};

handle_call({?GET_OTHER_END, Stop}, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("GET_OTHER_END Stop=~p", [Stop])),
  [H|T] = State#line_state.stops,
  Reply = case H of
            Stop -> lists:last(T);
            _    -> H
          end,
  {reply, Reply, State};

handle_call({?CONTAINS_STOP, Stop}, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("CONTAINS_STOP Stop=~p", [Stop])),
  Reply = lists:member(Stop, State#line_state.stops),
  {reply, Reply, State};

handle_call({?GET_DURATION, FromStop, ToStop}, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("GET_DURATION FromStop=~p ToStop=~p", [FromStop, ToStop])),
  Reply = get_duration_helper(FromStop, ToStop, State#line_state.stops),
  {reply, Reply, State};

handle_call({?IS_END_STOP, Stop}, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("IS_END_STOP Stop=~p", [Stop])),
  Reply = lists:prefix([Stop], State#line_state.stops) or lists:suffix([Stop], State#line_state.stops),
  {reply, Reply, State};

handle_call({?GET_INTERSECTION, OtherLine}, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("GET_INTERSECTION OtherLine=~p", [OtherLine])),
  Reply = get_intersection_helper(OtherLine, lists:filter(fun(Stop) ->
                                                            case Stop of
                                                              ?ADDRESS(stop) ->
                                                                true;
                                                              _ ->
                                                                false
                                                            end
                                                          end, State#line_state.stops)),
  {reply, Reply, State};

handle_call(?GET_NUMBER, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("GET_NUMBER", [])),
  Reply = State#line_state.number,
  {reply, Reply, State};

handle_call({?GET_TARGET, FromStop, ToStop}, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("GET_TARGET FromStop=~p ToStop=~p", [FromStop, ToStop])),
  Reply = get_target_helper(FromStop, ToStop, State#line_state.stops),
  {reply, Reply, State}.


-spec handle_cast(any(), line_state()) -> {noreply, line_state()}.
handle_cast(_Cast, State) ->
  {noreply, State}.


-spec handle_info(timeout | any(), line_state()) -> {noreply, line_state()}.
handle_info(_Info, State) ->
  {noreply, State}.


-spec terminate(normal | shutdown | {shutdown, any()} | any(), line_state()) -> ok.
terminate(_Reason, _State) ->
  ok.


-spec code_change(term() | {down, term()}, line_state(), term()) -> {ok, line_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Backend

-spec get_next_stop_helper(stop(), pre | post, [stop() | pos_integer()]) -> {stop(), pos_integer()} | none.
get_next_stop_helper(_Stop, _, [_|[]]) -> none; %%TODO ERROR log, since vehicles should always turn around when they arrive at their end stations. There should always be a next stop.
get_next_stop_helper(Stop, pre, [NextStop|[Dur|[Stop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, post, [Stop|[Dur|[NextStop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, Alignment, [_S|[_Dur|Stops]]) ->
  ?LOG_INFO(io_lib:format("get_next_stop_helper Stop=~p Alignment=~p Stops=~p", [Stop, Alignment, Stops])),
  get_next_stop_helper(Stop, Alignment, Stops).

-spec get_duration_helper(stop(), stop(), [stop() | pos_integer()]) -> pos_integer().
get_duration_helper(FromStop, ToStop, Stops) ->
  get_duration_helper(FromStop, ToStop, false, Stops).

-spec get_duration_helper(stop(), stop(), boolean(), [stop() | pos_integer()]) -> non_neg_integer().
get_duration_helper(_FromStop, ToStop, true, [ToStop|_Stops]) -> 0;
get_duration_helper(FromStop, _ToStop, true, [FromStop|_Stops]) -> 0;
get_duration_helper(FromStop, ToStop, _OnPath, [FromStop|[Dur|Stops]]) ->
  ?LOG_INFO(io_lib:format("get_duration_helper Stops head is FromStop, FromStop=~p ToStop=~p Dur=~p Stops=~p", [FromStop, ToStop, Dur, Stops])),
  Dur + get_duration_helper(FromStop, ToStop, true, Stops);
get_duration_helper(FromStop, ToStop, _OnPath, [ToStop|[Dur|Stops]]) ->
  ?LOG_INFO(io_lib:format("get_duration_helper Stops head is ToStop, FromStop=~p ToStop=~p Dur=~p Stops=~p", [FromStop, ToStop, Dur, Stops])),
  Dur + get_duration_helper(FromStop, ToStop, true, Stops);
get_duration_helper(FromStop, ToStop, OnPath, [_S|[Dur|Stops]]) ->
  ?LOG_INFO(io_lib:format("get_duration_helper Stops head does not match FromStop or ToStop, FromStop=~p ToStop=~p OnPath~p Dur=~p Stops=~p", [FromStop, ToStop, OnPath, Dur, Stops])),
  case OnPath of
    true ->
      Dur + get_duration_helper(FromStop, ToStop, true, Stops);
    false ->
      get_duration_helper(FromStop, ToStop, false, Stops)
  end.

-spec get_intersection_helper(line(), [stop()]) -> stop() | none.
get_intersection_helper(OtherLine, []) -> none;
get_intersection_helper(OtherLine, [Stop|Rest]) ->
  ?LOG_INFO(io_lib:format("get_intersection_helper OtherLine=~p Stop=~p Rest=~p", [OtherLine, Stop, Rest])),
  ContainsStop = ?CONTAINS_STOP(OtherLine, Stop),
  case ContainsStop of
    true ->
      Stop;
    false ->
      get_intersection_helper(OtherLine, Rest)
  end.

-spec get_target_helper(stop(), stop(), [stop() | pos_integer()]) -> stop().
get_target_helper(FromStop, ToStop, [FirstEnd|[_|Stops]]) ->
  get_target_helper(FromStop, ToStop, FirstEnd, lists:filter(fun(Stop) -> not is_integer(Stop) end, Stops)).

-spec get_target_helper(stop(), stop(), stop(), [stop() | pos_integer()]) -> stop().
get_target_helper(FromStop, _ToStop, FromStop, Stops) ->
  lists:last(Stops);
get_target_helper(FromStop, ToStop, FirstEnd, [Stop|Stops]) ->
  ?LOG_INFO(io_lib:format("get_target_helper FromStop=~p ToStop=~p FirstEnd=~p Stop=~p Stops=~p", [FromStop, ToStop, FirstEnd, [Stop|Stops]])),
  case Stop of
    FromStop ->
      lists:last(Stops);
    ToStop ->
      FirstEnd;
    Stop ->
      get_target_helper(FromStop, ToStop, FirstEnd, Stops)
  end.
