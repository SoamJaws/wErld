-module(public_transport).
-include("public_transport.hrl").
-include("logger.hrl").
-behaviour(gen_server).

%% Public API
-export([ ?GET_ROUTE/2]).

%% gen_server and internally spawned functions
-export([ start_link/0
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
  ?LOG_SEND(io_lib:format("GET_ROUTE ~p FromId=~p ToId=~p", [?MODULE, FromId, ToId])),
  Reply = gen_server:call({global, ?MODULE}, {?GET_ROUTE, FromId, ToId}),
  ?LOG_RECEIVE(io_lib:format("REPLY GET_ROUTE ~p", [Reply])),
  Reply.


%% gen_server

-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).


-spec init([]) -> {ok, public_transport_state()}.
init([]) ->
  put(id, public_transport),
  put(module, ?MODULE_STRING),
  %% StopIds = [atom()]
  %% LineSpecs = [{non_neg_integer(), [atom()], vehicle_type()}]
  {ok, {{stops, StopIds}, {lines, LineSpecs}}} = file:script(?PUBLIC_TRANSPORT_DATA_PATH),
  ?LOG_INFO(io_lib:format("Read stops ~p", [StopIds])),
  ?LOG_INFO(io_lib:format("Read lines ~p", [LineSpecs])),
  StopDict = lists:foldl(fun(StopId, Dict) ->
                           ?LOG_INFO(io_lib:format("Starting stop ~p", [StopId])),
                           {{stop, StopId}, Pid} = stop_supervisor:start_stop(StopId),
                           dict:store({stop, StopId}, Pid, Dict)
                         end , dict:new() , StopIds),
  Lines = lists:map(fun({Number, Stops, Type}) ->
                      UpdatedStops = lists:map(fun(Element) ->
                                                 if
                                                   is_integer(Element) ->
                                                     Element;
                                                   true ->
                                                     dict:fetch({stop, Element}, StopDict)
                                                 end
                                               end, Stops),
                      ?LOG_INFO(io_lib:format("Starting line ~w ~p", [Number, Type])),
                      Line = line_supervisor:start_line(Number, UpdatedStops, Type),
                      Line
                    end, LineSpecs),
  ?LOG_INFO("Puplic transport started"),
  {ok, #public_transport_state{lines=Lines, stops=StopDict}}.


-spec handle_call({?GET_ROUTE, atom(), atom()}, {pid(), any()}, public_transport_state()) -> {reply, route() | none, public_transport_state()}.
handle_call({?GET_ROUTE, FromId, ToId}, _From, State) ->
  ?LOG_RECEIVE(io_lib:format("GET_ROUTE FromId=~p ToId=~p", [FromId, ToId])),
  Reply = get_route_helper(FromId, ToId, State),
  {reply, Reply, State}.


-spec handle_cast(any(), public_transport_state()) -> {noreply, public_transport_state()}.
handle_cast(_, State) ->
  {noreply, State}.


-spec handle_info(timeout | any(), public_transport_state()) -> {noreply, public_transport_state()}.
handle_info(_Info, State) ->
  {noreply, State}.


-spec terminate(normal | shutdown | {shutdown, any()} | any(), stop_state()) -> ok.
terminate(_Reason, _State) ->
  ok.


-spec code_change(term() | {down, term()}, stop_state(), term()) -> {ok, stop_state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Backend

%% Instructionsformat: list of tuples {[{Line, Target, Destination}, {Line, Target, Destination}...], Dur}
%% Citizen goes from From to Destination by line in the Target direction, repeat until arrived at To
-spec get_route_helper(atom(), atom(), public_transport_state()) -> route() | none.
get_route_helper(FromId, ToId, State) ->
  ?LOG_INFO(io_lib:format("get_route_helper FromId=~p ToId=~p State=~p", [FromId, ToId, State])),
  AllLines = State#public_transport_state.lines,
  From = {FromId, dict:fetch({stop, FromId}, State#public_transport_state.stops)},
  To = {ToId, dict:fetch({stop, ToId}, State#public_transport_state.stops)},
  ToLines = lists:filter(fun(Line) -> line:?CONTAINS_STOP(Line, To) end, AllLines),
  spawn(public_transport, get_route_concurrent, [From, To, ToLines, {[], 0}, [], AllLines, self()]),
  receive
    {Route, Dur} ->
      {compress_route(Route), Dur};
    none ->
      none
  end.


-spec get_route_concurrent(stop(), stop(), [line()], route(), [stop()], [line()], pid()) -> route() | none.
get_route_concurrent(From, To, ToLines, {Route, Dur}, VisitedStops, AllLines, Invoker) ->
  FromLines = [Line || Line <- AllLines, line:?CONTAINS_STOP(Line, From)],
  IntersectingLines = get_intersecting_lines(FromLines, ToLines),
  case IntersectingLines of
    [] ->
      Neighbors = lists:append([line:?GET_NEIGHBORS(Line, From) || Line <- FromLines]),
      case Neighbors of
        [] ->
          Invoker ! none;
        _ ->
          Routes = spawn_get_route_calls(Neighbors, To, ToLines, {Route, Dur}, VisitedStops, AllLines),
          {BestRoute, TotalDur} = get_best_route(lists:filter(fun(R) -> R /= none end, Routes)),
          Invoker ! {Route ++ BestRoute, Dur + TotalDur}
      end;
    _  ->
      IntersectingLinesWithDurations = [{FromLine, ToLine, IntersectingStop, line:?GET_DURATION(FromLine, From, IntersectingStop) + line:?GET_DURATION(ToLine, IntersectingStop, To)} || {FromLine, ToLine, IntersectingStop} <- IntersectingLines],
    {FromLine, ToLine, IntersectingStop, LastDur} = get_best_intersecting_lines(IntersectingLinesWithDurations),
    Target = line:?GET_TARGET(FromLine, From, IntersectingStop),
    Invoker ! {Route ++ [{FromLine, Target, IntersectingStop}, {ToLine, IntersectingStop, To}], Dur + LastDur}
  end.


-spec spawn_get_route_calls([stop()], stop(), [line()], route(), [stop()], [line()]) -> [route()].
spawn_get_route_calls(Neighbors, To, ToLines, Route, VisitedStops, AllLines) ->
  spawn_get_route_calls(Neighbors, To, ToLines, Route, VisitedStops, AllLines, 0).

-spec spawn_get_route_calls([stop()], stop(), [line()], route(), [stop()], [line()], non_neg_integer()) -> [route()].
spawn_get_route_calls([], _To, _ToLines, _Route, _VisitedStops, _AllLines, NoCalls) ->
  receive_routes(NoCalls);
spawn_get_route_calls([Neighbor|Neighbors], To, ToLines, {Route, TotalDur}, VisitedStops, AllLines, NoCalls) ->
  VisitedNeighbor = lists:member(Neighbor, VisitedStops),
  if
    not VisitedNeighbor ->
      {From, Dur, Target, Line} = Neighbor,
      spawn(?MODULE, get_route_concurrent, [From, To, ToLines, {Route ++ [{Line, Target, From}], TotalDur + Dur}, [From|VisitedStops], AllLines, self()]),
      spawn_get_route_calls(Neighbors, To, ToLines, {Route, TotalDur}, VisitedStops, AllLines, NoCalls + 1);
    true ->
      spawn_get_route_calls(Neighbors, To, ToLines, {Route, TotalDur}, VisitedStops, AllLines, NoCalls)
  end.

-spec receive_routes(non_neg_integer()) -> [route()].
receive_routes(NoCalls) ->
  receive_routes(NoCalls, []).

-spec receive_routes(non_neg_integer(), [route()]) -> [route()].
receive_routes(0, Routes) -> Routes;
receive_routes(NoCalls, Routes) ->
  receive
    none ->
      receive_routes(NoCalls-1, Routes);
    Route ->
      receive_routes(NoCalls-1, [Route|Routes])
  end.

-spec get_best_route([route()]) -> route().
get_best_route([Route|Routes]) ->
  get_best_route(Routes, Route).

-spec get_best_route([route()], route()) -> route().
get_best_route([], BestRoute) -> BestRoute;
get_best_route([{Route, Dur}|Routes], {BestRoute, BestDur}) ->
  if
    Dur < BestDur ->
      get_best_route(Routes, {Route, Dur});
    true ->
      get_best_route(Routes, {BestRoute, BestDur})
  end.


%% [{FromLine, ToLine, IntersectingStop}]
-spec get_intersecting_lines([line()], [line()]) -> [{line(), line(), stop()}].
get_intersecting_lines(FromLines, ToLines) ->
  get_intersecting_lines([{FromLine, ToLine, line:?GET_INTERSECTION(FromLine, ToLine)} || FromLine <- FromLines, ToLine <- ToLines]).

-spec get_intersecting_lines([{line(), line(), stop()}]) -> [{line(), line(), stop()}].
get_intersecting_lines([]) -> [];
get_intersecting_lines([{_,_,none}|IntersectingLines]) ->
  get_intersecting_lines(IntersectingLines);
get_intersecting_lines([IntersectingLine|IntersectingLines]) ->
  [IntersectingLine|get_intersecting_lines(IntersectingLines)].


-spec get_best_intersecting_lines([{line(), line(), stop(), pos_integer()}]) -> {line(), line(), stop(), pos_integer()}.
get_best_intersecting_lines([IntersectingLineWithDuration|IntersectingLinesWithDurations]) ->
  get_best_intersecting_lines(IntersectingLinesWithDurations, IntersectingLineWithDuration).

-spec get_best_intersecting_lines([{line(), line(), stop(), pos_integer()}], {line(), line(), stop(), pos_integer()}) -> {line(), line(), stop(), pos_integer()}.
get_best_intersecting_lines([], BestIntersectingLines) -> BestIntersectingLines;
get_best_intersecting_lines([{FromLine, ToLine, IntersectingStop, Dur}|IntersectingLinesWithDurations], {BestFromLine, BestToLine, BestIntersectingStop, BestDur}) ->
  if
    Dur < BestDur ->
      get_best_intersecting_lines(IntersectingLinesWithDurations, {FromLine, ToLine, IntersectingStop, Dur});
    true ->
      get_best_intersecting_lines(IntersectingLinesWithDurations, {BestFromLine, BestToLine, BestIntersectingStop, BestDur})
  end.


-spec compress_route([route_step()]) -> [route_step()].
compress_route([Stop|Stops]) ->
  compress_route(Stops, [Stop]).

-spec compress_route([route_step()], [route_step()]) -> [route_step()].
compress_route([], Route) -> Route;
compress_route([{Line, Target, Destination}|Stops], Route) ->
  Last = lists:last(Route),
  case Last of
    {Line,_} -> compress_route(Stops, lists:droplast(Route) ++ [{Line, Target, Destination}]);
    _        -> compress_route(Stops, Route ++ [{Line, Target, Destination}])
  end.
