-module(public_transport).
-include("public_transport.hrl").
-behaviour(gen_server).

%% Public API
-export([ start_link/0
        , stop/1
        , state/1
        , ?GET_ROUTE/3]).

%% gen_server and internally spawned functions
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        , get_route_concurrent/7]).


%% Public API

-spec start_link() -> {ok, pid()} | ignore | {error, {already_started, pid()} | term()}.
start_link() ->
  gen_server:start_link(?MODULE, [], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
  gen_server:call(Pid, stop).

-spec state(pid()) -> public_transport_state().
state(Pid) ->
  gen_server:call(Pid, state).

-spec ?GET_ROUTE(pid(), pid(), pid()) -> {[{pid(), pid(), pid()}], pos_integer()} | none.
?GET_ROUTE(Pid, From, To) ->
  gen_server:call(Pid, {?GET_ROUTE, From, To}).


%% gen_server

-spec init([]) -> {ok, public_transport_state()}.
init([]) ->
  {ok, {{stops, StopSpecs}, {lines, LineSpecs}}} = file:script(?INFRASTRUCTURE_DATA_PATH),
  %%TODO Start a stop for each stopspec.
  %%TODO Start a line for each linespec, using stops as input
  Stops = [],
  Lines = [],
  {ok, #public_transport_state{lines=Lines}}.

-spec handle_call({?GET_ROUTE, pid(), pid()}, {pid(), any()}, public_transport_state()) -> {reply, {[{pid(), pid(), pid()}], pos_integer()} | none, public_transport_state()}
      ;          (stop,                       {pid(), any()}, public_transport_state()) -> {stop, normal, stopped, stop_state()}
      ;          (state,                      {pid(), any()}, public_transport_state()) -> {reply, public_transport_state(), public_transport_state()}.
handle_call({?GET_ROUTE, From, To}, _From, Lines) ->
  Reply = get_route_helper(From, To, Lines),
  {reply, Reply, Lines};

handle_call(state, _From, Lines) ->
  {reply, Lines, Lines};

handle_call(stop, _From, Lines) ->
  {stop, normal, stopped, Lines}.


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
-spec get_route_helper(pid(), pid(), [pid()]) -> {[{pid(), pid(), pid()}], pos_integer()} | none.
get_route_helper(From, To, AllLines) ->
  ToLines = [Line || Line <- AllLines, line:?CONTAINS_STOP(Line, To)],
  spawn(public_transport, get_route_concurrent, [From, To, ToLines, {[], 0}, [], AllLines, self()]),
  receive
    {Route, Dur} ->
      {compress_route(Route), Dur};
    none ->
      none
  end.


-spec get_route_concurrent(pid(), pid(), [pid()], {[{pid(), pid(), pid()}], pos_integer()}, [pid()], [pid()], pid()) -> {[{pid(), pid(), pid()}], pos_integer()} | none.
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
    Invoker ! {Route ++ [{FromLine, IntersectingStop}, {ToLine, To}], Dur + LastDur}
  end.


-spec spawn_get_route_calls([pid()], pid(), [pid()], {[{pid(), pid(), pid()}], pos_integer()}, [pid()], [pid()]) -> [{[{pid(), pid(), pid()}], pos_integer()}].
spawn_get_route_calls(Neighbors, To, ToLines, Route, VisitedStops, AllLines) ->
  spawn_get_route_calls(Neighbors, To, ToLines, Route, VisitedStops, AllLines, 0).

-spec spawn_get_route_calls([pid()], pid(), [pid()], {[{pid(), pid(), pid()}], pos_integer()}, [pid()], [pid()], non_neg_integer()) -> [{[{pid(), pid(), pid()}], pos_integer()}].
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

-spec receive_routes(non_neg_integer()) -> [{[{pid(), pid(), pid()}], pos_integer()}].
receive_routes(NoCalls) ->
  receive_routes(NoCalls, []).

-spec receive_routes(non_neg_integer(), [{[{pid(), pid(), pid()}], pos_integer()}]) -> [{[{pid(), pid(), pid()}], pos_integer()}].
receive_routes(0, Routes) -> Routes;
receive_routes(NoCalls, Routes) ->
  receive
    none ->
      receive_routes(NoCalls-1, Routes);
    Route ->
      receive_routes(NoCalls-1, [Route|Routes])
  end.

-spec get_best_route([{[{pid(), pid(), pid()}], pos_integer()}]) -> {[{pid(), pid(), pid()}], pos_integer()}.
get_best_route([Route|Routes]) ->
  get_best_route(Routes, Route).

-spec get_best_route([{[{pid(), pid(), pid()}], pos_integer()}], {[{pid(), pid(), pid()}], pos_integer()}) -> {[{pid(), pid(), pid()}], pos_integer()}.
get_best_route([], BestRoute) -> BestRoute;
get_best_route([{Route, Dur}|Routes], {BestRoute, BestDur}) ->
  if
    Dur < BestDur ->
      get_best_route(Routes, {Route, Dur});
    true ->
      get_best_route(Routes, {BestRoute, BestDur})
  end.


%% [{FromLine, ToLine, IntersectingStop}]
-spec get_intersecting_lines([pid()], [pid()]) -> [{pid(), pid(), pid()}].
get_intersecting_lines(FromLines, ToLines) ->
  get_intersecting_lines([{FromLine, ToLine, line:?GET_INTERSECTION(FromLine, ToLine)} || FromLine <- FromLines, ToLine <- ToLines]).

-spec get_intersecting_lines([{pid(), pid(), pid()}]) -> [{pid(), pid(), pid()}].
get_intersecting_lines([]) -> [];
get_intersecting_lines([{_,_,none}|IntersectingLines]) ->
  get_intersecting_lines(IntersectingLines);
get_intersecting_lines([IntersectingLine|IntersectingLines]) ->
  [IntersectingLine|get_intersecting_lines(IntersectingLines)].


-spec get_best_intersecting_lines([{pid(), pid(), pid(), pos_integer()}]) -> {pid(), pid(), pid(), pos_integer()}.
get_best_intersecting_lines([IntersectingLineWithDuration|IntersectingLinesWithDurations]) ->
  get_best_intersecting_lines(IntersectingLinesWithDurations, IntersectingLineWithDuration).

-spec get_best_intersecting_lines([{pid(), pid(), pid(), pos_integer()}], {pid(), pid(), pid(), pos_integer()}) -> {pid(), pid(), pid(), pos_integer()}.
get_best_intersecting_lines([], BestIntersectingLines) -> BestIntersectingLines;
get_best_intersecting_lines([{FromLine, ToLine, IntersectingStop, Dur}|IntersectingLinesWithDurations], {BestFromLine, BestToLine, BestIntersectingStop, BestDur}) ->
  if
    Dur < BestDur ->
      get_best_intersecting_lines(IntersectingLinesWithDurations, {FromLine, ToLine, IntersectingStop, Dur});
    true ->
      get_best_intersecting_lines(IntersectingLinesWithDurations, {BestFromLine, BestToLine, BestIntersectingStop, BestDur})
  end.


-spec compress_route([{pid(), pid(), pid()}]) -> [{pid(), pid(), pid()}].
compress_route([Stop|Stops]) ->
  compress_route(Stops, [Stop]).

-spec compress_route([{pid(), pid(), pid()}], [{pid(), pid(), pid()}]) -> [{pid(), pid(), pid()}].
compress_route([], Route) -> Route;
compress_route([{Line, Target, Destination}|Stops], Route) ->
  Last = lists:last(Route),
  case Last of
    {Line,_} -> compress_route(Stops, lists:droplast(Route) ++ [{Line, Target, Destination}]);
    _        -> compress_route(Stops, Route ++ [{Line, Target, Destination}])
  end.
