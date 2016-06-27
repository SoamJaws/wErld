-module(line).
-include("infrastructure.hrl").
-behaviour(gen_server).

%% Public API
%% TODO Add ?GET_LINE_NUMBER API function and signal handling
-export([ start_link/3
        , stop/1
        , state/1
        , ?GET_NEXT_STOP/3
        , ?GET_NEIGHBORS/2
        , ?GET_OTHER_END/2
        , ?CONTAINS_STOP/2
        , ?GET_DURATION/3
        , ?IS_END_STOP/2
        , ?GET_INTERSECTION/2]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

start_link(Number, Stops, Type) ->
  gen_server:start_link(?MODULE, #line_state{number=Number, stops=Stops, type=Type}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

?GET_NEXT_STOP(Pid, Target, Stop) ->
  gen_server:call(Pid, {?GET_NEXT_STOP, Target, Stop}).

?GET_NEIGHBORS(Pid, Stop) ->
  gen_server:call(Pid, {?GET_NEIGHBORS, Stop}).

?GET_OTHER_END(Pid, Stop) ->
  gen_server:call(Pid, {?GET_OTHER_END, Stop}).

?CONTAINS_STOP(Pid, Stop) ->
  gen_server:call(Pid, {?CONTAINS_STOP, Stop}).

?GET_DURATION(Pid, FromStop, ToStop) ->
  gen_server:call(Pid, {?GET_DURATION, FromStop, ToStop}).

?IS_END_STOP(Pid, Stop) ->
  gen_server:call(Pid, {?IS_END_STOP, Stop}).

?GET_INTERSECTION(Pid, OtherLine) ->
  gen_server:call(Pid, {?GET_INTERSECTION, OtherLine}).


%% gen_server

init(State) ->
  %gen_server:call(blackboard, {subscribe, time}),
  {ok, State}.


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

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, State, State}.


handle_cast(_Cast, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Backend

get_next_stop_helper(_Stop, _, [_|[]]) -> none; %%TODO ERROR log, since vehicles should always turn around when they arrive at their end stations. There should always be a next stop.
get_next_stop_helper(Stop, pre, [NextStop|[Dur|[Stop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, post, [Stop|[Dur|[NextStop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, Alignment, [_S|[_Dur|Stops]]) ->
  get_next_stop_helper(Stop, Alignment, Stops).

get_duration_helper(FromStop, ToStop, Stops) ->
  get_duration_helper(FromStop, ToStop, false, Stops).

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
