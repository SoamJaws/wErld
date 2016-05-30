-module(infrastructure).
-include("infrastructure_state.hrl").
-behaviour(gen_server).

%% Public API
-export([ start_link/1
        , stop/1
        , state/1
        , get_route/3]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).


%%TODO
%% Make pathfinding algorithm work for more than one vehicle switch, see todo in get_route_helper
%% Or consider storing neighbor info in all stops and implement a regular Dijkstras

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

%% Instructionsformat: list of tuples {[{Line, Destination}, {Line, Destination}...], Dur}
%% Citizen goes from From to Destination by line, repeat until arrived at To
get_route_helper(From, To, Lines) ->
  ToLines = [Line || Line <- AllLines, line:contains_stop(Line, To)],
  spawn(infrastructure, get_route_concurrent, [From, To, ToLines, [[], 0], [], Lines, self()]),
  receive
    {Route, Dur} -> {compress_route(Route), Dur}
  end.

get_route_concurrent(From, To, ToLines, {Route, Dur}, VisitedStops, AllLines, Invoker) ->
  FromLines = [Line || Line <- AllLines, line:contains_stop(Line, From)],
  IntersectingLines = get_intersecting_lines(FromLines, ToLines),
  case IntersectingLines of
    [] ->
      %% TODO Need to add target to instructions, to get correct direction
      Neighbors = [line: || Line <- FromLines]%% Get all nonvisited neighbors, spawn a subcall for each.
    _  ->
      IntersectingLinesWithDurations = [{FromLine, ToLine, IntersectingStop, line:get_duration(FromLine, From, IntersectingStop) + line:get_duration(ToLine, IntersectingStop, To)} || {FromLine, ToLine, IntersectingStop} <- IntersectingLines],
    {FromLine, ToLine, IntersectingStop, LastDur} = get_best_intersecting_lines(IntersectingLinesWithDurations),
    Invoker ! {Route ++ [{FromLine, IntersectingStop}, {ToLine, To}], Dur + LastDur}.


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


assemble_route(Stops) -> assemble_route(Stops, []).

assemble_route([]¸ Route) -> Route;
assemble_route([{Line, Stop}|Stops], Route) ->
  Last = lists:last(Route),
  case Last of
    {Line,_} -> assemble_route(Stops, lists:droplast(Route) ++ {Line, Stop};
    _        -> assemble_route(Stops, Route ++ [{Line, Stop}])
  end

%% Pathfinding algorithm
%%
%% Wrap function
%%	Input: From, To, AllLines
%%	Flow:
%%		Spawn concurrent pathfinder function and receive result
%%
%% Concurrent pathfinder function
%%	Input: From, To, VisitedStops, Spawner
%%	Flow: 
%%		Get all lines containing From
%%		Get all lines containing To
