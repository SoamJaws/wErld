-module(gen_server_mock).
-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

%% Public API
-export([ start_link/1
        , stop/1
        , state/1
        , expect_call/3
        , expect_cast/2
        , finalize/1]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

-record(gen_server_mock_state, {id, calls = [], casts = [], expectedCalls = [], expectedCasts = [], callReturns = []}).

%% Public API

start_link(Id) ->
  gen_server:start_link(?MODULE, #gen_server_mock_state{id=Id}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

expect_call(Pid, Msg, Reply) ->
  gen_server:cast(Pid, {expectCall, Msg, Reply}).

expect_cast(Pid, Msg) ->
  gen_server:cast(Pid, {expectCast, Msg}).

finalize(Pid) ->
  gen_server:call(Pid, finalize).

%% gen_server

init(State) ->
  {ok, State}.


handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State};

handle_call(finalize, _From, State) ->
  Calls = State#gen_server_mock_state.calls,
  Casts = State#gen_server_mock_state.casts,
  ExpectedCalls = State#gen_server_mock_state.expectedCalls,
  ExpectedCasts = State#gen_server_mock_state.expectedCasts,
  CallReturns = State#gen_server_mock_state.callReturns,
  if
    (Calls == ExpectedCalls) and (Casts == ExpectedCasts) and (CallReturns == []) ->
      {reply, true, State};
    true ->
      Id = State#gen_server_mock_state.id,
      ?debugFmt("Casts and/or calls does not match expectations for mock with Id: ~p.~n---- Casts:~n~p~n---- Expected Casts:~n~p~n---- Calls~n~p~n---- Expected Calls~n~p~n", [Id, Casts, ExpectedCasts, Calls, ExpectedCalls]),
      {reply, false, State}
  end;

handle_call(Msg, _From, State) ->
  Calls = State#gen_server_mock_state.calls,
  CallReturns = State#gen_server_mock_state.callReturns,
  case CallReturns of
    [] ->
      Id = State#gen_server_mock_state.id,
      Casts = State#gen_server_mock_state.casts,
      ExpectedCasts = State#gen_server_mock_state.expectedCasts,
      ExpectedCalls = State#gen_server_mock_state.expectedCalls,
      ?debugFmt("No expected call return when called with ~p in gen_server with Id: ~p~n---- Casts:~n~p~n---- Expected Casts:~n~p~n---- Calls~n~p~n---- Expected Calls~n~p~n---- Call Returns~n~p~n", [Msg, Id, Casts, ExpectedCasts, Calls, ExpectedCalls, CallReturns]),
      ?assert(false);
    _ ->
      {reply, lists:last(CallReturns), State#gen_server_mock_state{calls=[Msg|Calls], callReturns=lists:droplast(CallReturns)}}
  end.


handle_cast({expectCall, Msg, Reply}, State) ->
  ExpectedCalls = State#gen_server_mock_state.expectedCalls,
  CallReturns = State#gen_server_mock_state.callReturns,
  {noreply, State#gen_server_mock_state{expectedCalls=[Msg|ExpectedCalls], callReturns=[Reply|CallReturns]}};

handle_cast({expectCast, Msg}, State) ->
  ExpectedCasts = State#gen_server_mock_state.expectedCasts,
  {noreply, State#gen_server_mock_state{expectedCasts=[Msg|ExpectedCasts]}};

handle_cast(Msg, State) ->
  Casts = State#gen_server_mock_state.casts,
  {noreply, State#gen_server_mock_state{casts=[Msg|Casts]}}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.