-module(public_transport).
-include("public_transport.hrl").
-behaviour(gen_server).

%% Public API
-export([ ?GET_ROUTE/2
        , ?GET_OTHER_END/3
        , ?GET_NEXT_STOP/4]).

%% gen_server and internally spawned functions
-export([ start_link/2
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        , get_route_concurrent/7]).


%% Public API

-spec ?GET_ROUTE(atom(), atom()) -> route() | none.
?GET_ROUTE(FromId, ToId) ->
  gen_server:call(?MODULE, {?GET_ROUTE, FromId, ToId}).

-spec ?GET_OTHER_END(atom(), pos_integer(), stop()) -> stop().
?GET_OTHER_END(VehicleType, LineNumber, Stop) ->
  [H|T] = ets_utils:set_lookup(?MODULE, ?LINE_ID(VehicleType, LineNumber)),
  case H of
    Stop -> lists:last(T);
    _    -> H
  end.

-spec ?GET_NEXT_STOP(atom(), pos_integer(), stop(), stop()) -> {stop(), pos_integer()} | none.
?GET_NEXT_STOP(VehicleType, LineNumber, Target, Stop) ->
  Stops = ets_utils:set_lookup(?MODULE, ?LINE_ID(VehicleType, LineNumber)),
  [EndStop|_] = Stops,
  case EndStop of
    Target -> get_next_stop_helper(Stop, pre, Stops);
    _      -> get_next_stop_helper(Stop, post, Stops)
  end.


%% gen_server

-spec start_link([atom()], [{pos_integer(), [stop() | pos_integer()], vehicle_type()}]) -> {ok, pid()} | ignore | {error, {already_started, pid()} | any()}.
start_link(StopIds, LineSpecs) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {StopIds, LineSpecs}, []).


-spec init({[atom()], [{pos_integer(), [stop() | pos_integer()], vehicle_type()}]}) -> {ok, public_transport_state()}.
init({StopIds, LineSpecs}) ->
  %% StopIds = [atom()]
  %% LineSpecs = [{non_neg_integer(), [atom()], vehicle_type()}]
  StopDict = init_stops(StopIds),
  ets:new(?MODULE, [named_table]),
  init_lines(LineSpecs, StopDict),
  {ok, #public_transport_state{stops=StopDict}}.


-spec handle_call({?GET_ROUTE, atom(), atom()}, {pid(), any()}, public_transport_state()) -> {reply, route() | none, public_transport_state()}.
handle_call({?GET_ROUTE, FromId, ToId}, _From, State) ->
  Reply = get_route_helper(FromId, ToId, State),
  {reply, Reply, State}.


-spec handle_cast(any(), public_transport_state()) -> {noreply, public_transport_state()}.
handle_cast(_Cast, State) ->
  {noreply, State}.


-spec handle_info(timeout | any(), public_transport_state()) -> {noreply, public_transport_state()}.
handle_info(_Info, State) ->
  {noreply, State}.


-spec terminate(normal | shutdown | {shutdown, any()} | any(), stop_state()) -> ok.
terminate(_Reason, _State) ->
  ok.


-spec code_change(any() | {down, any()}, stop_state(), any()) -> {ok, stop_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Backend
-spec init_stops([atom()]) -> dict:dict(stop_id(), pid()).
init_stops(StopIds) ->
  init_stops(StopIds, dict:new()).

-spec init_stops([atom()], dict:dict(stop_id(), pid())) -> dict:dict(stop_id(), pid()).
init_stops([], StopDict) -> StopDict;
init_stops([StopId|StopIds], StopDict) ->
  {{stop, StopId}, Pid} = stop_supervisor:start_stop(StopId),
  init_stops(StopIds, dict:store({stop, StopId}, Pid, StopDict)).


-spec init_lines([{non_neg_integer(), [atom()], vehicle_type()}], dict:dict(stop_id(), pid())) -> ok.
init_lines([], _StopDict) -> ok;
init_lines([{Number, Stops, Type}|LineSpecs], StopDict) ->
  UpdatedStops = lists:map(fun(Element) ->
                             if
                               is_integer(Element) ->
                                 Element;
                               true ->
                                 {{stop, Element}, dict:fetch({stop, Element}, StopDict)}
                             end
                           end, Stops),
  ets:insert(?MODULE, {?LINE_ID(Type, Number), UpdatedStops, line}),
  init_lines(LineSpecs, StopDict).


%% Instructionsformat: list of tuples {[{Line, Target, Destination}, {Line, Target, Destination}...], Dur}
%% Citizen goes from From to Destination by line in the Target direction, repeat until arrived at To
-spec get_route_helper(atom(), atom(), public_transport_state()) -> route() | none.
get_route_helper(FromId, ToId, State) ->
  Matches = ets:select(?MODULE, [{{'$1', '$2', line}, [], ['$$']}]),
  AllLines = [{LineId, LineStops} || [LineId, LineStops] <- Matches],
  From = {{stop, FromId}, dict:fetch({stop, FromId}, State#public_transport_state.stops)},
  To = {{stop, ToId}, dict:fetch({stop, ToId}, State#public_transport_state.stops)},
  ToLines = lists:filter(fun({_LineId, LineStops}) -> lists:member(To, LineStops) end, AllLines),
  Invoker = self(),
  spawn(fun() ->
            get_route_concurrent(From, To, ToLines, {[], 1}, [], AllLines, Invoker)
         end
        ),
  receive
    {RouteSteps, Dur} ->
      {lists:reverse(RouteSteps), Dur}
  end.


-spec get_route_concurrent(stop(), stop(), [{atom(), [stop() | pos_integer()]}], route(), [stop()], [{atom(), [stop() | pos_integer()]}], pid()) -> route() | none.
get_route_concurrent(From, To, ToLines, {Route, Dur}, VisitedStops, AllLines, Invoker) ->
  FromLines = lists:filter(fun({_LineId, LineStops}) -> lists:member(From, LineStops) end, AllLines),
  IntersectingLines = get_intersecting_lines(FromLines, ToLines),
  case IntersectingLines of
    [] ->
      Neighbors = lists:append([get_neighbors(LineId, LineStops, From) || {LineId, LineStops} <- FromLines]),
      case Neighbors of
        [] ->
          Invoker ! none;
        _ ->
          NewRoute = spawn_get_route_calls(Neighbors, To, ToLines, {Route, Dur}, [From|VisitedStops], AllLines),
          case NewRoute of
            none ->
              Invoker ! none;
            {NewRouteSteps, NewDur} ->
              Invoker ! {NewRouteSteps, NewDur}
          end
      end;
    _  ->
      IntersectingLinesWithDurations = [{{FromLineId, FromLineStops}, {ToLineId, ToLineStops}, IntersectingStop, get_duration(From, IntersectingStop, FromLineStops) + get_duration(IntersectingStop, To, ToLineStops)} || {{FromLineId, FromLineStops}, {ToLineId, ToLineStops}, IntersectingStop} <- IntersectingLines],
    {FromLine, ToLine, IntersectingStop, LastDur} = get_best_intersecting_lines(IntersectingLinesWithDurations),
    {FromLineId, FromLineStops} = FromLine,
    {ToLineId, _ToLineStops} = ToLine,
    Target = get_target(From, IntersectingStop, FromLineStops),
    Invoker ! {[{FromLineId, Target, IntersectingStop}, {ToLineId, IntersectingStop, To} | Route], Dur + LastDur}
  end.


-spec spawn_get_route_calls([{stop(), pos_integer(), stop(), {atom(), [stop() | pos_integer()]}}], stop(), [{atom(), [stop() | pos_integer()]}], route(), [stop()], [{atom(), [stop() | pos_integer()]}]) -> route() | none.
spawn_get_route_calls(Neighbors, To, ToLines, Route, VisitedStops, AllLines) ->
  spawn_get_route_calls(Neighbors, To, ToLines, Route, VisitedStops, AllLines, 0).

-spec spawn_get_route_calls([{stop(), pos_integer(), stop(), {atom(), [stop() | pos_integer()]}}], stop(), [{atom(), [stop() | pos_integer()]}], route(), [stop()], [{atom(), [stop() | pos_integer()]}], non_neg_integer()) -> route() | none.
spawn_get_route_calls([], _To, _ToLines, _Route, _VisitedStops, _AllLines, NoCalls) ->
  receive_route(NoCalls);
spawn_get_route_calls([Neighbor|Neighbors], To, ToLines, {RouteSteps, TotalDur}, VisitedStops, AllLines, NoCalls) ->
  {From, Dur, Target, Line} = Neighbor,
  VisitedNeighbor = lists:member(From, VisitedStops),
  if
    not VisitedNeighbor ->
      Invoker = self(),
      UpdatedRouteSteps = case RouteSteps of
                            [] ->
                              [{Line, Target, From}];
                            _ ->
                              LastStep = lists:last(RouteSteps),
                              [{Line, Target, From} | case LastStep of
                                                        {Line, _, _} ->
                                                          lists:droplast(RouteSteps);
                                                        _ ->
                                                          RouteSteps
                                                      end]
                          end,
      spawn(fun() ->
              get_route_concurrent(From, To, ToLines, {UpdatedRouteSteps, TotalDur + Dur}, VisitedStops, AllLines, Invoker)
            end
           ),
      spawn_get_route_calls(Neighbors, To, ToLines, {RouteSteps, TotalDur}, VisitedStops, AllLines, NoCalls + 1);
    true ->
      spawn_get_route_calls(Neighbors, To, ToLines, {RouteSteps, TotalDur}, VisitedStops, AllLines, NoCalls)
  end.

-spec receive_route(non_neg_integer()) -> route() | none.
receive_route(NoCalls) ->
  receive_route(NoCalls, none).

-spec receive_route(non_neg_integer(), route() | none) -> route() | none.
receive_route(0, Route) -> Route;
receive_route(NoCalls, Route) ->
  receive
    none ->
      receive_route(NoCalls-1, Route);
    {NewRouteSteps, NewDur} ->
      case Route of
        {_, Dur} ->
          if
            NewDur < Dur ->
              receive_route(NoCalls-1, {NewRouteSteps, NewDur});
            true ->
              receive_route(NoCalls-1, Route)
          end;
        none ->
          receive_route(NoCalls-1, {NewRouteSteps, NewDur})
      end
  end.


%% [{FromLine, ToLine, IntersectingStop}]
-spec get_intersecting_lines([{atom(), [stop() | pos_integer()]}], [{atom(), [stop() | pos_integer()]}]) -> [{{atom(), [stop() | pos_integer()]}, {atom(), [stop() | pos_integer()]}, stop()}].
get_intersecting_lines(FromLines, ToLines) ->
  get_intersecting_lines([{FromLine, ToLine, get_intersection(FromLine, ToLine)} || FromLine <- FromLines, ToLine <- ToLines, FromLine /= ToLine]).

-spec get_intersecting_lines([{{atom(), [stop() | pos_integer()]}, {atom(), [stop() | pos_integer()]}, stop()}]) -> [{{atom(), [stop() | pos_integer()]}, {atom(), [stop() | pos_integer()]}, stop()}].
get_intersecting_lines([]) -> [];
get_intersecting_lines([{_,_,none}|IntersectingLines]) ->
  get_intersecting_lines(IntersectingLines);
get_intersecting_lines([IntersectingLine|IntersectingLines]) ->
  [IntersectingLine|get_intersecting_lines(IntersectingLines)].


-spec get_best_intersecting_lines([{{atom(), [stop() | pos_integer()]}, {atom(), [stop() | pos_integer()]}, stop(), pos_integer()}]) -> {{atom(), [stop() | pos_integer()]}, {atom(), [stop() | pos_integer()]}, stop(), pos_integer()}.
get_best_intersecting_lines([IntersectingLineWithDuration|IntersectingLinesWithDurations]) ->
  get_best_intersecting_lines(IntersectingLinesWithDurations, IntersectingLineWithDuration).

-spec get_best_intersecting_lines([{{atom(), [stop() | pos_integer()]}, {atom(), [stop() | pos_integer()]}, stop(), pos_integer()}], {{atom(), [stop() | pos_integer()]}, {atom(), [stop() | pos_integer()]}, stop(), pos_integer()}) -> {{atom(), [stop() | pos_integer()]}, {atom(), [stop() | pos_integer()]}, stop(), pos_integer()}.
get_best_intersecting_lines([], BestIntersectingLines) -> BestIntersectingLines;
get_best_intersecting_lines([{FromLine, ToLine, IntersectingStop, Dur}|IntersectingLinesWithDurations], {BestFromLine, BestToLine, BestIntersectingStop, BestDur}) ->
  if
    Dur < BestDur ->
      get_best_intersecting_lines(IntersectingLinesWithDurations, {FromLine, ToLine, IntersectingStop, Dur});
    true ->
      get_best_intersecting_lines(IntersectingLinesWithDurations, {BestFromLine, BestToLine, BestIntersectingStop, BestDur})
  end.


-spec get_next_stop_helper(stop(), pre | post, [stop() | pos_integer()]) -> {stop(), pos_integer()} | none.
get_next_stop_helper(_Stop, _, [_|[]]) -> none; %%TODO ERROR log, since vehicles should always turn around when they arrive at their end stations. There should always be a next stop.
get_next_stop_helper(Stop, pre, [NextStop|[Dur|[Stop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, post, [Stop|[Dur|[NextStop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, Alignment, [_S|[_Dur|Stops]]) ->
  get_next_stop_helper(Stop, Alignment, Stops).


-spec get_neighbors(atom(), [stop() | pos_integer()], stop()) -> [{stop(), pos_integer(), stop(), atom()}].
get_neighbors(LineId, LineStops, Stop) ->
  {BeforeStop, [Stop|AfterStop]} = lists:splitwith(fun(X) -> X /= Stop end, LineStops),
  N1 = case BeforeStop of
         [FirstTarget|_] ->
           FirstNeighbor = lists:last(lists:droplast(BeforeStop)),
           [{FirstNeighbor, lists:last(BeforeStop), FirstTarget, LineId}];
         [] ->
           []
       end,
  N2 = case AfterStop of
         [SecondDur|[SecondNeighbor|_]] ->
           [{SecondNeighbor, SecondDur, lists:last(AfterStop), LineId}];
         [] ->
           []
       end,
  N1 ++ N2.


-spec get_duration(stop(), stop(), [stop() | pos_integer()]) -> pos_integer().
get_duration(FromStop, ToStop, Stops) ->
  get_duration(FromStop, ToStop, false, Stops).

-spec get_duration(stop(), stop(), boolean(), [stop() | pos_integer()]) -> non_neg_integer().
get_duration(_FromStop, ToStop, true, [ToStop|_Stops]) -> 0;
get_duration(FromStop, _ToStop, true, [FromStop|_Stops]) -> 0;
get_duration(FromStop, ToStop, _OnPath, [FromStop|[Dur|Stops]]) ->
  Dur + get_duration(FromStop, ToStop, true, Stops);
get_duration(FromStop, ToStop, _OnPath, [ToStop|[Dur|Stops]]) ->
  Dur + get_duration(FromStop, ToStop, true, Stops);
get_duration(FromStop, ToStop, OnPath, [_S|[Dur|Stops]]) ->
  case OnPath of
    true ->
      Dur + get_duration(FromStop, ToStop, true, Stops);
    false ->
      get_duration(FromStop, ToStop, false, Stops)
  end.


-spec get_target(stop(), stop(), [stop() | pos_integer()]) -> stop().
get_target(FromStop, ToStop, [FirstEnd|[_|Stops]]) ->
  get_target(FromStop, ToStop, FirstEnd, lists:filter(fun(Stop) -> not is_integer(Stop) end, Stops)).

-spec get_target(stop(), stop(), stop(), [stop() | pos_integer()]) -> stop().
get_target(FromStop, _ToStop, FromStop, Stops) ->
  lists:last(Stops);
get_target(FromStop, ToStop, FirstEnd, [Stop|Stops]) ->
  case Stop of
    FromStop ->
      lists:last(Stops);
    ToStop ->
      FirstEnd;
    Stop ->
      get_target(FromStop, ToStop, FirstEnd, Stops)
  end.


-spec get_intersection({atom(), [stop() | pos_integer()]}, {atom(), [stop() | pos_integer()]}) -> stop() | none
      ;               ([stop() | pos_integer()], [stop() | pos_integer()]) -> stop() | none.
get_intersection({_, FirstLineStops}, {_, SecondLineStops}) ->
  get_intersection(FirstLineStops, SecondLineStops);
get_intersection(FirstLineStops, [Stop|[]]) ->
  ContainsStop = lists:member(Stop, FirstLineStops),
  case ContainsStop of
    true ->
      Stop;
    false ->
      none
  end;
get_intersection(FirstLineStops, [Stop|[_Dur|Rest]]) ->
  ContainsStop = lists:member(Stop, FirstLineStops),
  case ContainsStop of
    true ->
      Stop;
    false ->
      get_intersection(FirstLineStops, Rest)
  end.
