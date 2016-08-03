-module(citizen).
-include("gen_server_utils.hrl").
-behaviour(gen_server).


%% Public API
-export([ start_link/1
        , stop/1
        , state/1
        , vehicle_checked_in/2]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

start_link(Name) ->
  gen_server:start_link(?MODULE, [Name], []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

vehicle_checked_in(?RECIPENT_NO_ID, Vehicle) ->
  gen_server:call(Pid, {vehicle_checked_in, Vehicle}).

init([Name]) ->
  {ok, [Name]}.


handle_call({vehicle_checked_in, _Vehicle}, _From, State) ->
  {reply, undefined, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, State, State}.


handle_cast(_Cast, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
