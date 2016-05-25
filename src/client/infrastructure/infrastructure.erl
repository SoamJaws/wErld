-module(infrastructure).
-include("infrastructure_state.hrl").
-behaviour(gen_server).

-export([get_route/3]).

-export([ start_link/1
        , stop/1
        , state/1
        , init/1
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

init([Lines]) ->
  {ok, #infrastructure_state{lines=Lines}}.

get_route(Pid, From, To) ->
  gen_server:call(Pid, {get_route, From, To}).

handle_call({get_line, StartStop}, _From, Lines) ->
  Line = get_line_helper(StartStop, Lines),
  {reply, Line, Lines};

handle_call({get_route, From, To}, _From, Lines) ->
  FromLines = [Line || Line <- Lines, line:contains_stop(Line, From)],
  ToLines = [Line || Line <- Lines, line:contains_stop(Line, To)],
  Reply = get_route_helper(From, To, FromLines, ToLines),
  {reply, Reply, Lines};

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

get_line_helper(_StartStop, []) -> none; %%TODO Error log and crash, not supposed to happen
get_line_helper(StartStop, [Line|Lines]) ->
  IsStartStop = line:is_start_stop(Line, StartStop),
  if
    IsStartStop ->
      Line;
    true ->
      get_line_helper(StartStop, Lines)
  end.

%% Instructionsformat: list of tuples {[{Line, Destination}, {Line, Destination}...], Dur}
%% Citizen goes from From to Destination by line, repeat until arrived at To
get_route_helper(From, To, FromLines, ToLines) ->
  IntersectingLines = get_intersecting_lines(FromLines, ToLines),
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
