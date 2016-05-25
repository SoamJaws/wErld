-module(line).
-compile(export_all).
-include("infrastructure_state.hrl").

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link(Number, Stops, Type) ->
  gen_server:start_link(?MODULE, #line_state{number=Number, stops=Stops, type=Type}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

init(State) ->
  gen_server:call(blackboard, {subscribe, time}),
  {ok, State}.

get_next_stop(Pid, Target, Stop) ->
  gen_server:call(Pid, {get_next_stop, Target, Stop}).

get_other_end(Pid, Stop) ->
  gen_server:call(Pid, {get_other_end, Stop}).

contains_stop(Pid, Stop) ->
  gen_server:call(Pid, {contains_stop, Stop}).

get_duration(Pid, FromStop, ToStop) ->
  gen_server:call(Pid, {get_duration, FromStop, ToStop}).

is_end_stop(Pid, Stop) ->
  gen_server:call(Pid, {is_end_stop, Stop}).

get_intersection(Pid, OtherLine) ->
  get_server:call(Pid, {get_intersection, OtherLine}).

handle_call({get_next_stop, Target, Stop}, _From, State) ->
  EndStop = lists:head(State#line_state.stops),
  Reply = case EndStop of
            Target -> get_next_stop_helper(Stop, pre, State#line_state.stops);
            _      -> get_next_stop_helper(Stop, post, State#line_state.stops)
          end,
  {reply, Reply, State};

handle_call({get_other_end, Stop}, _From, State) ->
  [H|T] = State#line_state.stops,
  Reply = case H of
            Stop -> lists:last(T);
            _    -> H
          end,
  {reply, Reply, State};

handle_call({contains_stop, Stop}, _From, State) ->
  Reply = lists:member(Stop, State#line_state.stops),
  {reply, Reply, State};

handle_call({get_duration, FromStop, ToStop}, _From, State) ->
  Reply = get_duration_helper(FromStop, ToStop, State#line_state.stops),
  {reply, Reply, State};

handle_call({is_end_stop, Stop}, _From, State) ->
  Reply = lists:prefix([Stop], State#line_state.stops) or lists:suffix([Stop], State#line_state.stops),
  {reply, Reply, State};

handle_call({get_intersection, OtherLine}, _From, State) ->
  Reply = get_intersection_helper(OtherLine, State#line_state.stops),
  {reply, Reply, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State}.


handle_cast(_Cast, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_next_stop_helper(_Stop, _, []) -> none; %%TODO ERROR log, since vehicles should always turn around when they arrive at their end stations. There should always be a next stop.
get_next_stop_helper(Stop, pre, [NextStop|[Dur|[Stop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, post, [Stop|[Dur|[NextStop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, Alignment, [_S|[_Dur|[Stops]]]) ->
  get_next_stop_helper(Stop, Alignment, Stops).

get_duration_helper(FromStop, ToStop, Stops) ->
  get_duration_helper(FromStop, ToStop, false, Stops).

get_duration_helper(_FromStop, ToStop, true, [ToStop|_Stops]) -> 0;
get_duration_helper(FromStop, ToStop, _OnPath, [FromStop|[Dur|[Stops]]]) ->
  Dur + get_duration_helper(FromStop, ToStop, true, Stops);
get_duration_helper(FromStop, ToStop, OnPath, [_S|[Dur|[Stops]]]) ->
  case OnPath of
    true ->
      Dur + get_duration_helper(FromStop, ToStop, true, Stops);
    false ->
      get_duration_helper(FromStop, ToStop, false, Stops)
  end.

get_intersection_helper(_OtherLine, []) -> none;
get_intersection_helper(OtherLine, [Stop|[_Dur|[Stops]]]) ->
  ContainsStop = contains_stop(OtherLine, Stop),
  case ContainsStop of
    true ->
      Stop;
    false ->
      get_intersection_helper(OtherLine, Stops)
  end.
