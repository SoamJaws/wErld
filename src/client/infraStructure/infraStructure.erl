-module(infraStructure).

-behaviour(gen_server).
-export([ start_link/1
        , stop/1
        , state/1
        , init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

%% The infrastructure state should hold a graph. The graph is a list of weighted edges.
%% The weight represents the duration it takes to travel the edge. The nodes are stops.
%% Edges should hold a list of tuples, left value is an atom stating type (bus, tram etc),
%% right value is line number.

%% Public API

start_link(Name) ->
  gen_server:start_link(?MODULE, [Name], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

init([Name]) ->
  gen_server:call({subscribe, time}),
  {ok, [Name]}.


handle_call(stop, _From, Subscriptions) ->
  {stop, normal, stopped, Subscriptions}.

handle_cast({time, _Time}, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% Call this function. Will return none if no connection is found.
%% Will return {Type, Weight} for the fastest connection
%% To be used internally in pathfinding algorithms
getBestConnection([ {edge, {vertex, A}, {vertex, B}} | _ ], {vertex, A}, {vertex, B}, Connections) ->
  getBestConnection(Connections, {none, -1});
getBestConnection([ {edge, {vertex, B}, {vertex, A}} | _ ], {vertex, A}, {vertex, B}, Connections) ->
  getBestConnection(Connections, {none, -1});
getBestConnection([ _ | Graph ], {vertex, A}, {vertex, B}, Connections) ->
  getBestConnection(Graph, {vertex, A}, {vertex, B}, Connections);
getBestConnection([], _, _, _) -> none.

%% Second part of the function, gets the best connection type
getBestConnection([ {Type, Weight} | Rest], {_BestType, -1}) ->
  getBestConnection(Rest, {Type, Weight});
getBestConnection([ {Type, Weight} | Rest], {BestType, BestWeight}) ->
  case Weight > BestWeight of
    true  -> getBestConnection(Rest, {Type, Weight});
    false -> getBestConnection(Rest, {BestType, BestWeight})
  end;
getBestConnection([], Connection) -> Connection.
