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

contains_stop(Pid, Stop) ->
  gen_server:call(Pid, {contains_stop, Stop}).

get_duration(Pid, From, To) ->
  gen_server:call(Pid, {get_duration, From, To}).

handle_call({get_next_stop, Stop}, _From, State) ->
  Reply = get_next_stop_helper(Stop, State#line_state.stops),
  {reply, Reply, State};

handle_call({contains_stop, Stop}, _From, State) ->
  Reply = contains_stop_helper(Stop, State#line_state.stops),
  {reply, Reply, State};

handle_call({get_duration, From, To}, _Sender, State) ->
  undefined;

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State}.


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

get_duration_helper(From, To, _OnPath, [From|[Dur|[Stops]]]) ->
  Dur + get_duration_helper(From, To, true, Stops);
get_duration_helper(From, To, true, [_S|[Dur|[Stops]]])
  Dur + get_duration_helper(From, To, true, Stops);
get_duration_helper(From, To, false, [_S|[Dur|[Stops]]])
  get_duration_helper(From, To, false, Stops);
