-module(gen_server_mock).
-behaviour(gen_server).

%% Public API
-export([ start_link/1
        , stop/1
        , state/1).

%% gen_server
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

-record(gen_server_mock_state, {id, calls = [], expectations = [], callReturns = []}).

%% Public API

start_link(Id) ->
  gen_server:start_link(?MODULE, #gen_server_mock_state{id=Id}, []).

stop(Pid) ->
  gen_server:call(Pid, stop).

state(Pid) ->
  gen_server:call(Pid, state).

expect_call(Pid, Msg, Reply) ->
  gen_server:cast(Pid, {expectCall, Msg, Reply}).

%% gen_server

init(State) ->
  {ok, State}.


handle_call({passenger_check_in, Passenger}, _From, State) ->
  {reply, {ok, State}, State};

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State};

handle_call(state, _From, State) ->
  {reply, {ok, State}, State}.


handle_cast({expectCall, Msg, Reply}, State) ->
  Calls = State#gen_server_mock_state.calls,
  Expectations = State#gen_server_mock_state.expectations,
  CallReturns = State#gen_server_mock_state.callReturns,


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
