-module(gen_server_mock).
-behaviour(gen_server).
-include("gen_server_utils.hrl").
-include_lib("eunit/include/eunit.hrl").

%% Public API
-export([ start/3
        , start_global/3
        , stop/1
        , state/1
        , expect_call/3
        , expect_cast/2
        , validate/1]).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

-record(gen_server_mock_state, {module, id, type, calls = [], casts = [], expectedCalls = [], expectedCasts = [], callReturns = []}).

%% Public API

start(Module, Id, Type) ->
  {ok, Pid} = gen_server:start(?MODULE, #gen_server_mock_state{module=Module, id=Id, type=Type}, []),
  ?ADDRESS(Module).

start_global(Module, Id, Type) ->
  {ok, Pid} = gen_server:start({global, Id}, ?MODULE, #gen_server_mock_state{module=Module, id=Id, type=Type}, []),
  ?ADDRESS(Module).

stop(?ADDRESS(Module)) ->
  gen_server:call(Pid, stop).

state(?ADDRESS(Module)) ->
  gen_server:call(Pid, state).

expect_call(?ADDRESS(Module), Msg, Reply) ->
  gen_server:cast(Pid, {expectCall, Msg, Reply}).

expect_cast(?ADDRESS(Module), Msg) ->
  gen_server:cast(Pid, {expectCast, Msg}).

validate(?ADDRESS(Module)) ->
  gen_server:call(Pid, validate).

%% gen_server

init(State) ->
  put(id, State#gen_server_mock_state.id),
  put(module, atom_to_list(State#gen_server_mock_state.module) ++ "_mock"),
  {ok, State}.


handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State};

handle_call(validate, _From, State) ->
  Calls = State#gen_server_mock_state.calls,
  Casts = State#gen_server_mock_state.casts,
  ExpectedCalls = State#gen_server_mock_state.expectedCalls,
  ExpectedCasts = State#gen_server_mock_state.expectedCasts,
  Type = State#gen_server_mock_state.type,
  case Type of
    strict ->
      if
        (Calls == ExpectedCalls) and (Casts == ExpectedCasts) ->
          {reply, true, State};
        true ->
          Id = State#gen_server_mock_state.id,
          ?debugFmt("Casts and/or calls does not match expectations for strict mock with Id: ~p.~n---- Casts:~n~p~n---- Expected Casts:~n~p~n---- Calls~n~p~n---- Expected Calls~n~p~n", [Id, Casts, ExpectedCasts, Calls, ExpectedCalls]),
          {reply, false, State}
      end;
    nice ->
      AllExpectedCallsMet = lists:all(fun(X) -> lists:member(X, Calls) end, ExpectedCalls),
      AllExpectedCastsMet = lists:all(fun(X) -> lists:member(X, Casts) end, ExpectedCasts),
      if
        AllExpectedCallsMet and AllExpectedCastsMet ->
          {reply, true, State};
        true ->
          Id = State#gen_server_mock_state.id,
          ?debugFmt("One or more expected casts and/or calls were not matched for nice mock with Id: ~p.~n---- Casts:~n~p~n---- Expected Casts:~n~p~n---- Calls~n~p~n---- Expected Calls~n~p~n", [Id, Casts, ExpectedCasts, Calls, ExpectedCalls]),
          {reply, false, State}
      end
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
      ?debugFmt("No expected call return when called with ~p in mock with Id: ~p~n---- Casts:~n~p~n---- Expected Casts:~n~p~n---- Calls~n~p~n---- Expected Calls~n~p~n---- Call Returns~n~p~n", [Msg, Id, Casts, ExpectedCasts, Calls, ExpectedCalls, CallReturns]),
      error(unexpected_call_error);
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
