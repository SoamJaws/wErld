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

get_next_stop(Pid, Stop) ->
  gen_server:call(Pid, {get_next_stop, Stop}).

get_end_stop(Pid) ->
  gen_server:call(Pid, get_end_stop).

contains_stop(Pid, Stop) ->
  gen_server:call(Pid, {contains_stop, Stop}).

get_duration(Pid, FromStop, ToStop) ->
  gen_server:call(Pid, {get_duration, FromStop, ToStop}).

is_start_stop(Pid, Stop) ->
  get_server:call(Pid, {is_start_stop, Stop}).

handle_call({get_next_stop, Stop}, _From, State) ->
  Reply = get_next_stop_helper(Stop, State#line_state.stops),
  {reply, Reply, State};

handle_call(get_end_stop, _From, State) ->
  Reply = lists:last(State#line_state.stops),
  {reply, Reply, State};

handle_call({contains_stop, Stop}, _From, State) ->
  Reply = contains_stop_helper(Stop, State#line_state.stops),
  {reply, Reply, State};

handle_call({get_duration, FromStop, ToStop}, _From, State) ->
  Reply = get_duration_helper(FromStop, ToStop, State#line_state.stops),
  {reply, Reply, State};

handle_call({is_start_stop, Stop}, _From, State) ->
  Reply = lists:prefix([Stop], State#line_state.stops),
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

get_next_stop_helper(_Stop, []) -> none;
get_next_stop_helper(Stop, [Stop|[Dur|[NextStop|_]]]) ->
  {NextStop, Dur};
get_next_stop_helper(Stop, [_S|[_Dur|[Stops]]]) ->
  get_next_stop_helper(Stop, Stops).

contains_stop_helper(_Stop, []) -> false;
contains_stop_helper(Stop, [Stop|_]) -> true;
contains_stop_helper(Stop, [_S|[_Dur|[Stops]]]) ->
  contains_stop_helper(Stop, Stops).

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
