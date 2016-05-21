-module(logger).
-compile(export_all).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link(LogFile) ->
  gen_server:start_link({global, ?MODULE}, ?MODULE, [LogFile], []).

stop(Module) ->
  gen_server:call(Module, stop).

stop() ->
  stop(?MODULE).

state(Module) ->
  gen_server:call(Module, state).

state() ->
  state(?MODULE).

init([LogFile]) ->
  {ok, LogFile}.

log_info(Content) ->
  log(Content, "INFO").

log_warning(Content) ->
  log(Content, "WARNING").

log_error(Content) ->
  log(Content, "ERROR").

log(Content, Mode) ->
  {_Date, {H, M, S}} = calendar:local_time(),
  gen_server:cast(?MODULE, {log, io_lib:fwrite("--- ~s ~s:~s:~s - ~s --- ~s~n--- END ~s ---~n", [Mode, H, M, S, self(), Content, Mode])}).

handle_call(stop, _From, State) ->
  {stop, normal, stopped, State}.

handle_cast({log, Content}, LogFile) ->
  file:write_file(LogFile, Content, [append]),
  {noreply, LogFile}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
