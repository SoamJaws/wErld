-module(infrastructure).
-include("infrastructure_state.hrl").

-behaviour(gen_server).
-export([ get_line/2]).
-export([ start_link/1
        , stop/1
        , state/1
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

%% TODO
%% Add api for citizens to use, i.e. {get_route, From, To}
%% Implement pathfinding algorithm. Includes quering lines
%% for stop existence and getting the duration of routes.

%% Public API

start_link(Lines) ->
  gen_server:start_link(?MODULE, [Lines], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

init([Lines]) ->
  {ok, #infrastructure_state{lines=Lines}}.

get_line(Pid, StartStop) ->
  gen_server:call(Pid, {get_line, StartStop}).

get_route(Pid, From, To) ->
  gen_server:call(Pid, {get_route, From, To}).

handle_call({get_line, StartStop}, _From, Lines) ->
  Line = get_line_helper(StartStop, Lines),
  {reply, Line, Lines};

handle_call({get_route, From, To}, _From, Lines) ->
  ok. %TODO IMPLEMENT
  %Instructionsformat: list of tuples [{Line, Destination}, {Line, Destination}...]
  %Citizen goes from From to Destionation by line, repeat until arrived at To

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
