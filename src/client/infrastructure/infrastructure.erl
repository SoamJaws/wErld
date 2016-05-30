-module(infrastructure).
-include("infrastructure_state.hrl").
-behaviour(gen_server).

%% Public API
-export([ start_link/1
        , stop/1
        , state/1
        , get_route/3
        , get_route_concurrent/7]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%% Public API

start_link(Lines) ->
  gen_server:start_link(?MODULE, [Lines], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

get_route(Pid, From, To) ->
  gen_server:call(Pid, {get_route, From, To}).


%% gen_server

init([Lines]) ->
  {ok, #infrastructure_state{lines=Lines}}.


handle_call({get_route, From, To}, _From, Lines) ->
  Reply = get_route_helper(From, To, Lines),
  {reply, Reply, Lines};

handle_call(state, _From, Lines) ->
  {reply, Lines, Lines};

handle_call(stop, _From, Lines) ->
  {stop, normal, stopped, Lines}.


handle_cast(_, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%% Backend

%% Instructionsformat: list of tuples {[{Line, Target, Destination}, {Line, Target, Destination}...], Dur}
%% Citizen goes from From to Destination by line in the Target direction, repeat until arrived at To
get_route_helper(From, To, AllLines) ->
  ToLines = [Line || Line <- AllLines, line:contains_stop(Line, To)],
  spawn(infrastructure, get_route_concurrent, [From, To, ToLines, {[], 0}, [], AllLines, self()]),
  receive
    {Route, Dur} -> {compress_route(Route), Dur}
  end.


get_route_concurrent(From, To, ToLines, {Route, Dur}, VisitedStops, AllLines, Invoker) ->
  FromLines = [Line || Line <- AllLines, line:contains_stop(Line, From)],
  IntersectingLines = get_intersecting_lines(FromLines, ToLines),
  case IntersectingLines of
    [] ->
      Neighbors = lists:append([line:get_neighbors(Line, From) || Line <- FromLines]),
      case Neighbors of
        [] ->
          Invoker ! none;
        _ ->
          Routes = spawn_get_route_calls(Neighbors, To, ToLines, Route, VisitedStops, AllLines),
          {BestRoute, TotalDur} = get_best_route(Routes),
          Invoker ! {Route ++ BestRoute, Dur + TotalDur}
      end;
    _  ->
      IntersectingLinesWithDurations = [{FromLine, ToLine, IntersectingStop, line:get_duration(FromLine, From, IntersectingStop) + line:get_duration(ToLine, IntersectingStop, To)} || {FromLine, ToLine, IntersectingStop} <- IntersectingLines],
    {FromLine, ToLine, IntersectingStop, LastDur} = get_best_intersecting_lines(IntersectingLinesWithDurations),
    Invoker ! {Route ++ [{FromLine, IntersectingStop}, {ToLine, To}], Dur + LastDur}
  end.


spawn_get_route_calls(Neighbors, To, ToLines, Route, VisitedStops, AllLines) ->
  spawn_get_route_calls(Neighbors, To, ToLines, Route, VisitedStops, AllLines, 0).

spawn_get_route_calls([], _To, _ToLines, _Route, _VisitedStops, _AllLines, NoCalls) ->
  receive_routes(NoCalls);
spawn_get_route_calls([Neighbor|Neighbors], To, ToLines, {Route, TotalDur}, VisitedStops, AllLines, NoCalls) ->
  VisitedNeighbor = lists:member(Neighbor, VisitedStops),
  if
    not VisitedNeighbor ->
      {From, Dur, Target, Line} = Neighbor,
      spawn(infrastructure, get_route_concurrent, [From, To, ToLines, {Route ++ [{Line, Target, From}], TotalDur + Dur}, [From|VisitedStops], AllLines, self()]),
      spawn_get_route_calls(Neighbors, To, ToLines, {Route, TotalDur}, VisitedStops, AllLines, NoCalls + 1);
    true ->
      spawn_get_route_calls(Neighbors, To, ToLines, {Route, TotalDur}, VisitedStops, AllLines, NoCalls)
  end.


receive_routes(NoCalls) ->
  receive_routes(NoCalls, []).

receive_routes(0, Routes) -> Routes;
receive_routes(NoCalls, Routes) ->
  receive
    none ->
      receive_routes(NoCalls-1, Routes);
    Route ->
      receive_routes(NoCalls-1, [Route|Routes])
  end.


get_best_route(Routes) ->
  get_best_route(Routes, {none, -1}).

get_best_route([], BestRoute) -> BestRoute;
get_best_route([Route|Routes], {none, -1}) ->
  get_best_route(Routes, Route);
get_best_route([{Route, Dur}|Routes], {BestRoute, BestDur}) ->
  if
    Dur < BestDur ->
      get_best_route(Routes, {Route, Dur});
    true ->
      get_best_route(Routes, {BestRoute, BestDur})
  end.


%% [{FromLine, ToLine, IntersectingStop}]
get_intersecting_lines(FromLines, ToLines) ->
  get_intersecting_lines([{FromLine, ToLine, line:get_intersection(FromLine, ToLine)} || FromLine <- FromLines, ToLine <- ToLines]).

get_intersecting_lines([]) -> [];
get_intersecting_lines([{_,_,none}|IntersectingLines]) ->
  get_intersecting_lines(IntersectingLines);
get_intersecting_lines([IntersectingLine|IntersectingLines]) ->
  [IntersectingLine|get_intersecting_lines(IntersectingLines)].


get_best_intersecting_lines(IntersectingLinesWithDurations) ->
  get_best_intersecting_lines(IntersectingLinesWithDurations, {none, none, none, 0}).

get_best_intersecting_lines([], BestIntersectingLines) -> BestIntersectingLines;
get_best_intersecting_lines([{FromLine, ToLine, IntersectingStop, Dur}|IntersectingLinesWithDurations], {BestFromLine, BestToLine, BestIntersectingStop, BestDur}) ->
  if
    Dur < BestDur ->
      get_best_intersecting_lines(IntersectingLinesWithDurations, {FromLine, ToLine, IntersectingStop, Dur});
    true ->
      get_best_intersecting_lines(IntersectingLinesWithDurations, {BestFromLine, BestToLine, BestIntersectingStop, BestDur})
  end.


compress_route(Stops) -> compress_route(Stops, []).

compress_route([], Route) -> Route;
compress_route([{Line, Stop}|Stops], Route) ->
  Last = lists:last(Route),
  case Last of
    {Line,_} -> compress_route(Stops, lists:droplast(Route) ++ {Line, Stop});
    _        -> compress_route(Stops, Route ++ [{Line, Stop}])
  end.
