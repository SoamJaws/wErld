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
  FromLines = [Line || Line <- Lines, line:contains_stop(Line, From)],
  ToLines = [Line || Line <- Lines, line:contains_stop(Line, To)],
  get_route_helper(From, To, FromLines, ToLines, Lines).

get_route_helper(From, To, FromLines, ToLines, AllLines) ->
  IntersectingLines = get_intersecting_lines(FromLines, ToLines),
  case IntersectingLines of
    [] -> ok; %%TODO Try to get route to the closest neighbor of From
    _  ->
      IntersectingLinesWithDurations = [{FromLine, ToLine, IntersectingStop, line:get_duration(FromLine, From, IntersectingStop) + line:get_duration(ToLine, IntersectingStop, To)} || {FromLine, ToLine, IntersectingStop} <- IntersectingLines],
    {FromLine, ToLine, IntersectingStop, Dur} = get_best_intersecting_lines(IntersectingLinesWithDurations),
    {[{FromLine, IntersectingStop}, {ToLine, To}], Dur}.


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
